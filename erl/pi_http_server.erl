-module(pi_http_server).

-compile(export_all).

start() ->
    %% simple_db:start_link("db1"),
    simple_web_server:start(8080, ?MODULE).

handle_get_request(_File, _Headers, _Body) ->
    %% io:format("My server:~p~n",[_File]),
    default.

handle_cgi_request(#{mod := BMod, func := BFunc} = All) ->
    Mod = b2a(BMod),
    Func = b2a(BFunc),
    apply(Mod, Func, [All]).

fac(0) -> 1;
fac(N) -> N*fac(N-1).

b2a(B) ->
    list_to_atom(binary_to_list(B)).

play_remote(#{args := [Ip, Prog]}) ->
    %% io:format("Ip=~p Prog=~p~n",[Ip, Prog]),
    %% all we have to do is send this to Sonic Pi
    run_code(binary_to_list(Ip), binary_to_list(Prog)),
    {json, #{status => ok}}.

test() ->
    run_code("localhost", "play 80\n").

run_code(IP, Prog) ->
    io:format("Run:~s~n",[Prog]),
    E = osc:encode(["/run-code", "erl_id", Prog]),
    {ok, Socket} = gen_udp:open(0,[binary]),
    %% io:format("socket=~p message=~p~n",[Socket,E]),
    ok = gen_udp:send(Socket, IP, 4557, E),
    gen_udp:close(Socket).
   
    
