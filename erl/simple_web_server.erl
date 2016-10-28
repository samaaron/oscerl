-module(simple_web_server).

%% -compile(export_all).
-export([start/2, pre/1, default_action/3, decode_multipart/1,
	 parse_uri/1,
	 bin_to_websocket_frame/1,
	 make_websocket_handshake/1,
	 deframe/1]).

-import(lists, [reverse/1]).

start(Port, Mod) ->
    S = self(),
    Pid = spawn(fun() -> start_web_server(S, Port, Mod)  end),
    receive
	{Pid, Reply} ->
	    Reply
    end.

start_web_server(Parent, Port, Mod) ->
    %% spawn two process 
    %% the web_server_loop manages the callbacks
    PidWS = spawn_link(fun() -> web_server_loop(Mod) end),
    %% the port sets up middle men
    spawn_link(fun() -> start_port(Port, PidWS) end),
    Parent ! {self(), ok}.

%%----------------------------------------------------------------------
%% start_port connects to the Port
%% listens for a connection - then sends messages to Pid
%% when anything happens

start_port(Port, Pid) ->
    io:format("Port processor will send all messages to Pid=~p~n",[Pid]),
    io:format("starting a web server Port=~p~n",[Port]),
    {ok, Listen} = gen_tcp:listen(Port, [{packet,0}, 
					 binary,
					 {reuseaddr,true},
					 {active, true}]),
    spawn(fun() -> par_connect(Listen, Pid) end),
    %% If this process dies the Listening port is closed 
    %% so we make sure we don't die
    receive
	after infinity ->
		true
	end.

par_connect(Listen, Pid) ->
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    spawn(fun() -> par_connect(Listen, Pid) end),
	    mm(Socket, Pid, <<>>);
	Other ->
	    io:format("par_connect:Other=~p ~p~n",
		      [Other,erlang:get_stacktrace()])
    end.

mm(Socket, Pid, Cont) ->
    receive
	{tcp, _Socket, Bin} ->
	    case (catch parse_request(Cont, Bin)) of
		{ok, Cmd, Next} ->
		    %% io:format("Parse request::Cmd=~p ~n",[Cmd]),
		    Pid ! {self(), Cmd},
		    %% io:format("mm Next=~p~n",[Next]),
		    mm(Socket, Pid, Next);
		{more, _} = Cont1 ->
		    mm(Socket, Pid, Cont1);
		Bin when is_binary(Bin) ->
		    mm(Socket, Pid, Bin);
		Other ->
		    io:format("Parse request bonkers:Other:~p~n",
			      [Other]),
		    exit(bonkers)
	    end;
	{send, Data} ->
	    gen_tcp:send(Socket, Data),
	    mm(Socket, Pid, Cont);
	{tcp_closed,Socket} ->
	    io:format("Socket closed ~p~n",[self()]),
	    Pid ! {self(), closed};
	Any ->
	    io:format("mm received ~p ~p~n",[Socket, Any]),
	    mm(Socket, Pid, Cont)
    end.

%%----------------------------------------------------------------------
%% 

%% web_server_loop just gets messages from *sombody*
%% it does the callback and replies to *sombody*

web_server_loop(Mod) ->
    receive
	{Client,{request, ["GET", "/mfa/"++C,_], _Headers,_Body}} ->
	    [MS,FS|Args] = string:tokens(C,"/"),
	    Mod1 = s2a(MS),
	    Func1 = s2a(FS),
	    Reply = do_mfa_callback(Mod1, Func1, [Args]),
	    Client ! {send, Reply},
	    web_server_loop(Mod);
	
	{Client,{request, ["GET", "/cgi?"++C,_], _Headers, _Body}} ->
	    Norm = parse_uri_args(C),
	    Reply = do_cgi_callback(Mod, Norm),
	    Client ! {send, Reply},
	    web_server_loop(Mod);

	{Client,{request, ["GET", File,_], Headers, Body}} ->
	    %% io:format("web_server_loop get ~p ~p~n",[Mod, File]),
	    Reply = do_callback(Mod, File, Headers, Body),
	    Client ! {send, Reply},
	    web_server_loop(Mod);

	{Client,{request, ["POST", "/erl_call",_], _Headers, Body}} ->
	    io:format("erl_call from json:Body=~p~n",[Body]),
	    Norm = normalise_args(Body),
	    io:format("Norm=~p~n",[Norm]),
	    {Tag, Res} = cgi_erl_call(Norm),
	    %% io:format("res=~p~n",[Res]),
	    Client ! {send, make_response(Tag, Res)},
	    web_server_loop(Mod);
	
	{Client,{request, ["POST", "/cgi",_], _Headers, Body}} ->
	    %% io:format("POST CGI ~p~n",[Body]),
	    Norm = normalise_args(Body),
	    %% io:format("Cgi:~p~n",[Norm]),
	    {Tag, Res} = cgi(Norm),
	    %% io:format("res=~p~n",[Res]),
	    Client ! {send, make_response(Tag, Res)},
	    web_server_loop(Mod);

	{Client,{request, ["POST", "/jsoncgi"|_], _Headers, Body}} ->
	    %% io:format("POST Jsoncgi ~p~n",[Body]),
	    Norm = normalise_args(Body),
	    %% io:format("Cgi:~p~n",[Norm]),
	    {Tag, Res} = Z = cgi(Norm),
	    %% io:format("response=~p~n",[Z]),
	    Client ! {send, make_response(Tag, Res)},
	    web_server_loop(Mod);


	{Client,{request, ["POST", File,_], Headers, Body}} ->
	    Reply = do_callback(Mod, File, Headers, Body),
	    Client ! {send, Reply},
	    web_server_loop(Mod);
	Any ->
	    io:format("web_server_loop client got unexpected:~p ~n",[Any]),
	    web_server_loop(Mod)
    end.

cgi_erl_call(#{mod := ModB, func := FuncB, args := Args}) ->
    Mod  = b2a(ModB),
    Func = b2a(FuncB),
    case (catch apply(Mod, Func, Args)) of
	{'EXIT', Why} ->
	    io:format("Error1:~p ~p ~p => ~p~n",[Mod,Func,Args,Why]),
	    {html, pre({'EXIT', Why})};
	Ok ->
	    Ok
    end.

cgi(#{mod := ModB, func := FuncB} = All) ->
    Mod  = b2a(ModB),
    Func = b2a(FuncB),
    case (catch apply(Mod, Func, [All])) of
	{'EXIT', Why} ->
	    io:format("Error:~p ~p ~p => ~p~n",[Mod,Func,All,Why]),
	    {html, pre({'EXIT', Why})};
	Ok ->
	    Ok
    end.


do_mfa_callback(Mod, Func, Args) ->
    io:format("do mfa callback:~p ~p ~p~n",[Mod,Func,Args]),
    case (catch Mod:Func(Args)) of
	{'EXIT', Why} ->
	    io:format("mfa ERROR => ~p~n",[Why]),
	    make_response(json, #{error => pre({"error", Why})});
	{Tag, IO} = _X ->
	    %% io:format("mfa callback => ~p~n",[X]),
	    make_response(Tag, IO)
    end.


do_cgi_callback(Mod, Args) ->
    case (catch Mod:handle_cgi_request(Args)) of
	{'EXIT', Why} ->
	    make_response(html, pre({"error", Why}));
	{Tag, IO} ->
	    make_response(Tag, erlang:iolist_to_binary(IO))
    end.

do_callback(Mod, File, Headers, Body) ->
    case (catch Mod:handle_get_request(File, Headers, Body)) of
	default ->
	    default_read_file(File);
	{'EXIT', Why} ->
	    io:format("ooo~p~n",[Why]),
	    make_response(html, pre({"error", Why}));
	{Tag, Value} ->
	    make_response(Tag, iolist_to_binary(Value))
		
    end.


b2i(B) ->
    list_to_integer(binary_to_list(B)).

%%----------------------------------------------------------------------

default_read_file(File) ->
    io:format("get ~p~n",[File]),
    File1 = "./" ++ File,
    case file:read_file(File1) of
	{ok, Bin} ->
	    make_response(classify(File), Bin);
	{error, _} ->
	    case filelib:is_dir(File1) of
		true ->
		    list_dir(File1);
		false ->
		    make_response(html, pre({"no such file", File}))
	    end
    end.




%%----------------------------------------------------------------------
%% parse_request -- reentrant parser for HTTP requests
%% returns {ok, Cmd, Bin}      Cmd is the completed command Bin the Next comand
%%         {more, {Cmd,N,Bin}} The command <CMD> is being collected we need N bytes
%%                             we have collected Bin     
%%         Bin                 haven't even go the first command


parse_request({more,{Req,N,B0}}, B1) ->
    Bin2 = <<B0/binary, B1/binary>>,
    collect_request_content(N, Req, Bin2);
parse_request(Bin0, Bin1) ->
    Bin2 = <<Bin0/binary, Bin1/binary>>,
    case binary:split(Bin2, <<"\r\n\r\n">>) of
	[Before, After] ->
	    {Cmd, Headers} =  parse_request_cmd(Before),
	    %% io:format("Headers=~p~n",[Headers]),
	    case maps:find('Content-Length', Headers) of
		error ->
		    %% io:format("no content length~n"),
		    %% we have a complete
		    %% request
		    Req =  {request, Cmd, Headers, <<>>},
		    {ok, Req, After};
		{ok, Val} ->
		    %% io:format("content length=~p~n",[Val]),
		    N = b2i(Val),
		    %% io:format("content length=~p~n",[N]),
		    Req =  {request2, Cmd, Headers},
		    collect_request_content(N, Req, After)
	    end;
	[_] ->
	    Bin2
    end.


%%----------------------------------------------------------------------
%% collect_request_content
%% returns {ok, Cmd, Bin}      Cmd is the completed command Bin the Next comand
%%         {more, {Cmd,N,Bin}} The command <CMD> is being collected we need N bytes
%%                             we have collected Bin     
%%         Bin                 haven't even go the first command


collect_request_content(N, {request2,Cmd,Headers}, Bin0) when size(Bin0) >= N ->
    {B1, B2} = split_binary(Bin0, N),
    {ok, {request,Cmd,Headers,B1}, B2};
collect_request_content(N, Req, Bin0) ->
    {more, {Req,N,Bin0}}.

parse_request_cmd(Bin) ->
    [Line|Parts] = binary:split(Bin, <<"\r\n">>, [global]),
    Cmd = string:tokens(binary_to_list(Line)," "),
    %% io:format("Cmd::~p~n",[Cmd]),
    Headers = parse_request_headers(Parts),
    %% io:format("Headers::~p~n",[Headers]),
    {Cmd, Headers}.

parse_request_headers(L) ->
    maps:from_list([parse_header(I) || I<- L]).

parse_header(B) ->
    [Key,Val] = binary:split(B, <<": ">>),
    {b2a(Key), Val}.


classify(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
	".gif"  -> gif;
	".jpg"  -> jpg;
	".jpeg" -> jpg;
	".css"  -> css;
	".js"   -> js;
	".svg"  -> svg;
	".xul"  -> xul;
	".html" -> html;
	".xhtml" -> xhtml;
	".htm"  -> html;
	_       -> txt
    end.


pre(X) ->
    list_to_binary(["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"]).

make_response(json, M) when is_map(M) ->
    make_response(json, jsone:encode(M));
make_response(Tag, B1) ->
    Len = size(B1),
    Mime = mime_type(Tag),
    ["HTTP/1.1 200 Ok\r\n",
     "Content-Type: ", Mime, "\r\n",
     "Content-Length: ", integer_to_list(Len),"\r\n\r\n",
     B1].


mime_type(gif)               -> "image/gif";
mime_type(jpg)               -> "image/jpeg";
mime_type(png)               -> "image/png";
mime_type(css)               -> "text/css";
mime_type(json)              -> "application/json";
mime_type(swf)               -> "application/x-shockwave-flash";
mime_type(html)              -> "text/html";
mime_type(xhtml)             -> "application/xhtml+xml";
mime_type(xul)               -> "application/vnd.mozilla.xul+xml";
mime_type(js)                -> "application/x-javascript";
mime_type(svg)               -> "image/svg+xml";
mime_type(X) when is_atom(X) -> mime_type(html);
mime_type(FileName)          -> mime_type(classify(FileName)).

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T]) -> [H|quote(T)];
quote([]) -> [].

make_websocket_handshake(Headers) ->
    %% io:format("websocket connect Headers=~p~n",[Headers]),
    {ok, SecWebSocketKey} = maps:find('Sec-WebSocket-Key',Headers),
    Sha1 = crypto:hash(sha,[SecWebSocketKey, 
			    <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
    Base64 = base64:encode(Sha1),
    Handshake = [
		 <<"HTTP/1.1 101 Switching Protocols\r\n">>,
		 <<"Upgrade: websocket\r\n">>,
		 <<"Connection: Upgrade\r\n">>,
		 <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
		 <<"\r\n">>
		],
    list_to_binary(Handshake).

deframe(<<Fin:1,_Rsv:3,Opcode:4,Mask:1,
	  PayloadLength:7, Rest/binary>>) ->
    Do = {Fin, Opcode},
    case PayloadLength of
	N when N =< 125 ->
	    %% we're done
	    %% no additonal length
	    deframe1(Do, Mask, N, Rest);
	126 ->
	    %% Next 16 bits is the length
	    <<K:16,B1/binary>> = Rest,
	    deframe1(Do, Mask, K, B1);
	127 ->
	    <<K:64,B1/binary>> = Rest,
	    deframe1(Do, Mask, K, B1)
    end.

deframe1(Do, 0, K, B) ->
    %% no mask
    deframe2(Do, no, K, B);
deframe1(Do, 1, Len, <<Mask:4/binary, B1/binary>>) ->
    io:format("Len ~p size:~p Mask:~p B1:~p~n",[Len,size(B1), Mask, B1]),
    deframe2(Do, {yes, Mask}, Len, B1).

deframe2({1,1}, Mask, _Len, Data) ->
    %% final text frame
    Bin = unmask(Data, Mask),
    unicode:characters_to_list(Bin);
deframe2(Do, Mask, Len, Data) ->
    io:format("deframe2: do:~p Mask:~p Length:~p Data:~p~n",
	      [Do, Mask, Len, size(Data)]),
    exit({deframe2,Do}).

unmask(Data, no) ->
    Data;
unmask(Data, {yes, Mask}) ->
    M = binary_to_list(Mask),
    unmask(binary_to_list(Data), M, M, []).

unmask([],_,_,L)              -> reverse(L);
unmask(X, [], M, L)           -> unmask(X, M, M, L);
unmask([H1|T1], [H2|T2], M,L) -> unmask(T1, T2, M, [H1 bxor H2|L]).

bin_to_websocket_frame(Bin) ->
    Size = size(Bin),
    if Size < 126 ->
	    <<1:1, 0:3, 1:4, 0:1, Size:7, Bin/binary>>;
       true ->
	    io:format("*** too large frame !!!!~n"),
	    exit(large)
    end.

normalise_args(Body) ->
    Cmd = jsone:decode(Body,[{keys,atom}]),
    %% io:format("Normalized=~p~n",[Cmd]),
    Cmd.

b2a(B) ->
    list_to_atom(binary_to_list(B)).

parse_uri(URI) ->
    case string:tokens(URI, "?") of
	[Root] ->
	    {Root, []};
	[Root, Args] ->
	    {Root, parse_uri_args(Args)}
    end.

parse_uri_args(Args) ->
    Args1 = string:tokens(Args, "&;"),
    L1 = lists:map(fun(KeyVal) ->
			   case string:tokens(KeyVal, "=") of
			       [Key, Val] ->
				   {list_to_atom(urlencoded2str(Key)), 
				    list_to_binary(urlencoded2str(Val))};
			       [Key] ->
				   {list_to_atom(urlencoded2str(Key)), <<>>};
			       _ ->
				   io:format("Invalid str:~p~n",[KeyVal]),
				   {error, <<"error">>}
		      end
	      end, Args1),
    maps:from_list(L1).

urlencoded2str([$%,$u,A,B,C,D|T]) -> [decode_hex(A,B,C,D)|urlencoded2str(T)];
urlencoded2str([$%,Hi,Lo|T])      -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])            -> [$ |urlencoded2str(T)];
urlencoded2str([H|T])             -> [H|urlencoded2str(T)];
urlencoded2str([])                -> [].

%% decode_hex ...

decode_hex(Hex1, Hex2) -> hex2dec(Hex1)*16 + hex2dec(Hex2).

decode_hex(Hex1, Hex2, Hex3, Hex4) -> 
    hex2dec(Hex1)*4096 + hex2dec(Hex2)*256 + hex2dec(Hex3)*16 + hex2dec(Hex4).

hex2dec(X) when X >=$0, X =<$9 -> X-$0;
hex2dec($A) -> 10;
hex2dec($B) -> 11;
hex2dec($C) -> 12;
hex2dec($D) -> 13;
hex2dec($E) -> 14;
hex2dec($F) -> 15;
hex2dec($a) -> 10;
hex2dec($b) -> 11;
hex2dec($c) -> 12;
hex2dec($d) -> 13;
hex2dec($e) -> 14;
hex2dec($f) -> 15.

list_dir(D) ->
    {ok, L} = file:list_dir(D),
    L2 = [modify(D, I) || I <- L],
    L1 = [["<li>",a(I, I),"\n"] ||  I<- L2],
    make_response(html, list_to_binary(L1)).

modify(D,I) ->
    F = filename:join(D,I),
    case filelib:is_dir(F) of
	true ->
	    I ++ "/";
	false ->
	    I
    end.

a(I,I) ->
    ["<a href='",I,"'>",I,"</a>"].

s2a(I) ->
    list_to_atom(I).

%%----------------------------------------------------------------------

default_action("/jsoncgi", _, Body) ->
    %% io:format("Body:~p~n",[Body]),
    Cmd = jsone:decode(Body,[{keys,atom}]),
    %% io:format("Cmd:~p~n",[Cmd]),
    Reply = do_json(Cmd),
    Ret = jsone:encode(Reply),
    %% io:format("Ret=~p~n",[Ret]),
    {json, Ret};
default_action(_File, _Headers, _Body) ->
    %% io:format("My server:~p~n",[_File]),
    default.


decode_multipart(<<"------WebKitFormBoundary", B0/binary>>=_All) ->
    %% Step 1) Find out the boundary
    {Start, _Len} = binary:match(B0,<<13>>),
    {B1, B2} = split_binary(B0, Start),
    Boundary = <<"------WebKitFormBoundary",B1/binary>>,
    io:format("End Boundary=~p~n",[Boundary]),
    Parts = binary:split(B2, Boundary,[global]),
    Parts1 = lists:flatten([parse_part(I) || I <- Parts]),
    [extract_data_from_part(I) || I <- Parts1].

extract_data_from_part({
			 [
			  #{'Content-Disposition' := 'form-data',filename := Name},
			  #{'Content-Type' := Type}
			 ], 
			 ImageBin
		       }) ->
    #{filename => Name, content_type => Type, data => ImageBin};
extract_data_from_part({[#{'Content-Disposition' := 'form-data',
			   name := "json"}],JsonBin}) ->
    io:format("Json=~p~n",[JsonBin]),
    Meta = jsone:decode(JsonBin,[{keys,atom}]),
    #{type=> meta, data => Meta}.

parse_part(<<"--\r\n">>) ->
    [];
parse_part(Bin) ->    
    case binary:split(Bin, <<"\r\n\r\n">>) of
	[Before, Data] ->
	    Headers = parse_headers(Before),
	    Data1 = remove_trailing_characters(Data),
	    {Headers, Data1}
    end.

remove_trailing_characters(Bin) ->
    Size = size(Bin),
    binary_part(Bin,0,Size-2).



parse_headers(Bin) ->
    %% OO = erlang:simple_web_server:decode_packet(httph,<<Bin/binary,"\r\n">>,[]),
    %% io:format("OO=~p~n",[OO]),
    %% the headers are separted by \r\n
    [<<>>|Lines] = binary:split(Bin, <<"\r\n">>, [global]),    
    [parse_line(I) || I <- Lines].

%% really an RFC822 line

parse_line(Bin) ->	    
    rfc822:parse_header(Bin).

do_json(#{mod := M, func := F, args := Args}) ->
    Mod = b2a(M),
    Func = b2a(F),
    Json = apply(Mod, Func, Args),
    Json.
   
