-module(pi_server).
-compile(export_all).

%% Just run pi_server:start() in a separate shell

%% Assumptions

server_port() -> 8014.

start() ->
    S = self(),
    register(?MODULE, spawn(fun() -> go(S) end)),
    receive
	ack ->
	    true
    end.

go(P) ->
    {ok, Socket} = gen_udp:open(server_port(), [binary]),
    io:format("Udp server listening for SONIC PI on port:~p~n",[server_port()]),
    P ! ack,
    Monitor = spawn(fun() -> monitor() end),
    loop(Socket, 1, Monitor, {0,0}).

monitor() ->
    receive
	alive ->
	    monitor()
    after infinity ->
	    init:stop()
    end.

loop(Socket, N, Monitor, Clock) ->
    receive
	{udp, Socket, _Ip, _Port, Bin} ->
	    case (catch osc:decode(Bin)) of
		{bundle, Time, X} ->
		    do_bundle(Socket, Time, X, Clock),
		    loop(Socket, N, Monitor, Clock);
		{cmd, ["/clock/sync", X, Y]} ->
		    RemoteTimeBase = X + Y/1000000000,
		    MyTimeBase = osc:now(),
		    Clock1 = {RemoteTimeBase, MyTimeBase},
		    io:format("/clock/sync:~p ~p~n",[N, Clock1]),
		    loop(Socket, N+1, Monitor, Clock1);
		{cmd, XX} ->
		    do_cmd(Socket, Clock, XX);
		{'EXIT', Why} ->
		    io:format("Error decoding:~p ~p~n",[Bin, Why])
	    end,
	    loop(Socket, N+1, Monitor, Clock);
	Any ->
	    io:format("Any:~p~n",[Any]),
	    loop(Socket, N+1, Monitor, Clock)
    after 50000 ->
	    io:format("udp server timeout:~p~n",[N]),
	    loop(Socket, N+1, Monitor, Clock)
    end.

do_bundle(Socket, Time, [{_,B}], Clock) ->
    {cmd, Cmd} = osc:decode(B),
    io:format("bundle cmd:~p~n",[Cmd]),
    case Cmd of
	["/send_after",Host,Port|Cmd1] ->
	    spawn(fun() ->
                          io:format("bundle cmd1:~p~n",[Cmd1]),
			  send_later(Time, Clock, Socket, Host, Port, Cmd1)
		  end);
	_ ->
	    io:format("unexpected bundle:~p~n",[Cmd])
    end.

send_later(BundleTime, {Tremote,Tlocal}, Socket, Host, Port, Cmd) ->
    io:format(Cmd),

io:format("yo"),
io:format("----"),
    Bin = osc:encode(Cmd),
    %% RemoteDelay = BundleTime - Tremote,
    %% LocalAbsTime = Tlocal + RemoteDelay,
    RealDelay = BundleTime - osc:now(),
    MsDelay = trunc(RealDelay*1000+0.5), %% nearest
    sleep(MsDelay),
    ok = gen_udp:send(Socket, Host, Port, Bin),
    io:format("Sending to ~p:~p => ~p~n",[Host, Port, Cmd]).

%% send_later(BundleTime, {Tremote,Tlocal}, Socket, Host, Port, Cmd) ->
%%     Bin = osc:encode(Cmd),
%%     RemoteDelay = BundleTime - Tremote,
%%     LocalAbsTime = Tlocal + RemoteDelay,
%%     RealDelay = LocalAbsTime - osc:now(),
%%     MsDelay = trunc(RealDelay*1000+0.5), %% nearest
%%     sleep(MsDelay),
%%     ok = gen_udp:send(Socket, Host, Port, Bin),
%%     io:format("Sending to ~p:~p => ~p~n",[Host, Port, Cmd]).

sleep(T) ->
    receive
	after
	    T ->
		true
	end.

do_cmd(_Socket, _Clock, Cmd) ->
    io:format("Cannot do:~p~n",[Cmd]).
