-module(udp_server).
-compile(export_all).

%% Just run udp_server:start() in a separate shell

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
    io:format("Udp server listening on port:~p~n",[server_port()]),
    P ! ack,
    Monitor = spawn(fun() -> monitor() end),
    send_bootstrap_code(Socket, "localhost", 4559),
    loop(Socket, 1, Monitor).

send_bootstrap_code(Socket, Host, Port) ->
    [gen_udp:send(Socket, Host, Port, I) || I<- code()].

code() ->
    [osc:encode(["/load_buffer", 2, 
		 "# Click to play a tune\nosc_sender.send \"/play_tune\",\"localhost\",4559\n"]),
     osc:encode(["/load_buffer", 3, "play 80\nsleep 1\nplay 81\nsleep 1\nplay 82\n"]),
     osc:encode(["/run_buffer", 3])].

monitor() ->
    receive
	alive ->
	    monitor()
    after infinity ->
	    init:stop()
    end.

loop(Socket, N, Monitor) ->
    receive
	{udp, Socket, Ip, Port, Bin} ->
	    case (catch osc:decode(Bin)) of
		{bundle, T, X} ->
		    io:format("ignored bundle message:~n"),
		    print_times(T),
		    print_bundle(1, X);
		{cmd, ["/alive"]} ->
		    io:format("got alive message~n"),
		    Monitor ! alive;
		{cmd, ["/ping"]} ->
		    io:format("got ping sending pong~n"),
		    gen_udp:send(Socket, Ip, Port, osc:encode(["/pong"]));
		{cmd, X} ->
		    do_cmd(Socket, X);
		{'EXIT', Why} ->
		    io:format("Error decoding:~p ~p~n",[Bin, Why])
	    end,
	    loop(Socket, N+1, Monitor);
	Any ->
	    io:format("Any:~p~n",[Any]),
	    loop(Socket, N+1, Monitor)
    after 50000 ->
	    io:format("udp server timeout:~p~n",[N]),
	    loop(Socket, N+1, Monitor)
    end.


do_cmd(Socket, ["/send_after", Tsec, Tnano, When, Host, Port | Cmd] = X) ->
    Tnow = time_now(),
    SonicTnow = Tsec + Tnano/1000000000,
    PropagationDelay = Tnow - SonicTnow,
    io:format("~nTnow       = ~p~nSonic Tnow = ~p~nDelay      = ~p (ms)~n",
	      [Tnow, SonicTnow, PropagationDelay*1000]),
    B = osc:encode(Cmd),
    Delay = trunc(When  - PropagationDelay),
    io:format("Delay=~p~n",[Delay]),
    send_after(Socket, Host, Port, Delay, B);
do_cmd(Socket, ["/send_at", Time, Host, Port | Cmd] = X) ->
    io:format("do:~p (Now=~p) (sys=~p)~n",[X, time_now(), erlang:system_time()]),
    Now = erlang:convert_time_unit(erlang:system_time(),native,seconds),
    io:format("Now=~p~n",[Now]),
    B = osc:encode(Cmd),
    Delay = Time - time_now(),
    io:format("Delay=~p~n",[Delay]),
    send_after(Socket, Host, Port, Delay, B);
do_cmd(Socket, ["/after1",TInt,TNano,DelayMs,Host,Port|Cmd]=Msg) ->
    Tme = time_now(),
    F = TNano/1000000000,
    Tpi = TInt + F,
    Delay = Tme - Tpi,
    io:format("Tme:~p Tpi:~p Delay:~p~ncannot do:~p~n",[Tme,Tpi,Delay,Msg]),
    TDelayCalibrated = trunc(DelayMs - Delay*1000),
    io:format("DelayMs:~p TDelayCalibrated:~p~n",[DelayMs,TDelayCalibrated]),
    B = osc:encode(Cmd),
    send_after(Socket, Host, Port, TDelayCalibrated, B);
do_cmd(Socket, ["/play_tune",Host,Port]) ->
    io:format("play tune~n"),
    L = tune(),
    spawn(fun() -> play(L, Socket, Host, Port) end);
do_cmd(_Socket, Cmd) ->
    io:format("Cannot do:~p~n",[Cmd]).


%% time_now() returns time past epoch as a float
%% should be same as Sonic Pi's Time.now
time_now() ->
    erlang:system_time() / 1000000000.


play([{sleep,Time}|T], Socket, Host, Port) ->
    T1 = trunc(Time/256)*1000,
    receive
	after T1 ->
		play(T, Socket, Host, Port)
	end;
play([{play,N,duration,R}|T], Socket, Host, Port) ->
    B = osc:encode(["/foo/bar", N, trunc(R/256)]),
    ok = gen_udp:send(Socket, Host, Port, B),
    play(T, Socket, Host, Port);
play([], _, _, _) ->
    void.

print_times(T) ->
    T1 = time_now(),
    Delay = T1 - T,
    io:format("Bundle time   = ~p~nReceived time = ~p~n(delay:~p)~n",[T, T1, Delay]).

print_bundle(N, [{_Len,B}|T]) ->
    io:format("packet:~p content:~p~n",[N,catch osc:decode(B)]),
    print_bundle(N+1, T);
print_bundle(_, []) ->
    true.
    
send_after(Socket, Host, Port, Delay, B) ->
    io:format("delay for ~p ms~n",[Delay]),
    receive
	after Delay ->
		io:format("sending now:~p~n",[osc:decode(B)]),
		ok = gen_udp:send(Socket, Host, Port, B)
	end.

	
tune() ->
    [{sleep,256},
     {play,30,duration,128},
     {play,42,duration,128},
     {sleep,128},
     {play,69,duration,384},
     {play,74,duration,384},
     {play,78,duration,384},
     {play,81,duration,384},
 {play,54,duration,384},
 {play,57,duration,384},
 {play,62,duration,384},
 {play,66,duration,384},
 {sleep,130},
 {play,69,duration,130},
 {play,54,duration,130},
 {sleep,119},
 {play,55,duration,121},
 {sleep,7},
 {play,35,duration,384},
 {play,47,duration,384},
 {sleep,5},
 {play,50,duration,261},
 {sleep,62},
 {play,67,duration,195},
 {sleep,54},
 {play,63,duration,121},
 {sleep,3},
 {play,66,duration,60},
 {sleep,4},
 {play,60,duration,128},
 {play,33,duration,128},
 {play,45,duration,128},
 {sleep,128},
 {play,64,duration,128},
 {sleep,128},
 {play,31,duration,256},
 {play,43,duration,256},
 {play,47,duration,128},
 {play,52,duration,128},
 {play,59,duration,256},
 {sleep,128},
 {play,71,duration,384},
 {play,64,duration,256},
 {play,69,duration,128},
 {sleep,128},
 {play,30,duration,256},
 {play,42,duration,256},
 {play,47,duration,128},
 {play,51,duration,128},
 {play,59,duration,128},
 {sleep,2},
 {play,62,duration,130},
 {play,69,duration,130},
 {sleep,128},
 {play,67,duration,130},
 {play,73,duration,130},
 {play,64,duration,130},
 {sleep,3},
 {play,71,duration,261},
 {sleep,116},
 {play,70,duration,121},
 {play,76,duration,121},
 {sleep,7},
 {play,28,duration,256},
 {play,40,duration,256},
 {play,55,duration,128},
 {play,58,duration,128},
 {play,64,duration,0},
 {sleep,130},
 {play,76,duration,130},
 {sleep,113},
 {play,66,duration,243},
 {play,71,duration,243},
 {sleep,6},
 {play,62,duration,121},
 {play,74,duration,121},
 {sleep,7},
 {play,35,duration,256},
 {play,47,duration,256},
 {sleep,128},
 {play,61,duration,128},
 {sleep,2},
 {play,73,duration,130},
 {sleep,113},
 {play,62,duration,243},
 {play,66,duration,243},
 {sleep,6},
 {play,71,duration,121},
 {sleep,7},
 {play,54,duration,384},
 {play,50,duration,256},
 {play,59,duration,128},
 {sleep,128},
 {play,50,duration,128},
 {sleep,2},
 {play,62,duration,130},
 {sleep,62},
 {play,57,duration,192},
 {sleep,3},
 {play,69,duration,195},
 {sleep,54},
 {play,64,duration,121},
 {sleep,3},
 {play,67,duration,60},
 {sleep,4},
 {play,43,duration,256},
 {play,52,duration,128},
 {play,55,duration,64},
 {sleep,256},
 {play,38,duration,256},
 {play,45,duration,256},
 {play,54,duration,256},
 {sleep,128},
 {play,57,duration,384},
 {play,62,duration,384},
 {play,66,duration,384},
 {play,69,duration,384},
 {sleep,128},
 {play,54,duration,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,50,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,64,duration,128},
 {play,69,duration,128},
 {play,26,duration,512},
 {play,49,duration,128},
 {sleep,128},
 {play,47,duration,128},
 {sleep,128},
 {play,38,duration,768},
 {play,45,duration,128},
 {sleep,5},
 {play,66,duration,261},
 {sleep,103},
 {play,62,duration,364},
 {play,71,duration,364},
 {sleep,13},
 {play,67,duration,121},
 {sleep,135},
 {play,74,duration,128},
 {play,59,duration,128},
 {sleep,128},
 {play,62,duration,256},
 {play,67,duration,256},
 {play,71,duration,128},
 {play,31,duration,384},
 {play,43,duration,384},
 {sleep,64},
 {play,69,duration,64},
 {sleep,64},
 {play,62,duration,128},
 {play,64,duration,128},
 {play,67,duration,64},
 {play,50,duration,256},
 {play,35,duration,128},
 {play,47,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {sleep,128},
 {play,66,duration,128},
 {sleep,2},
 {play,57,duration,130},
 {sleep,126},
 {play,61,duration,128},
 {play,64,duration,128},
 {sleep,2},
 {play,55,duration,130},
 {sleep,119},
 {play,54,duration,121},
 {sleep,7},
 {play,69,duration,512},
 {play,62,duration,128},
 {play,38,duration,512},
 {play,50,duration,512},
 {sleep,128},
 {play,71,duration,128},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,55,duration,128},
 {sleep,64},
 {play,69,duration,64},
 {sleep,64},
 {play,62,duration,128},
 {play,67,duration,64},
 {play,43,duration,256},
 {play,50,duration,256},
 {play,59,duration,128},
 {sleep,128},
 {play,69,duration,128},
 {sleep,128},
 {play,57,duration,256},
 {play,62,duration,256},
 {play,66,duration,128},
 {play,42,duration,256},
 {play,50,duration,256},
 {sleep,261},
 {play,64,duration,261},
 {sleep,123},
 {play,35,duration,384},
 {play,47,duration,384},
 {sleep,102},
 {play,54,duration,486},
 {play,59,duration,486},
 {sleep,13},
 {play,62,duration,243},
 {sleep,13},
 {play,33,duration,128},
 {play,45,duration,128},
 {sleep,256},
 {play,31,duration,256},
 {play,43,duration,256},
 {sleep,128},
 {play,50,duration,384},
 {play,55,duration,384},
 {play,59,duration,384},
 {sleep,128},
 {play,62,duration,128},
 {play,43,duration,256},
 {sleep,2},
 {play,59,duration,130},
 {sleep,119},
 {play,57,duration,121},
 {play,62,duration,121},
 {play,66,duration,121},
 {sleep,7},
 {play,42,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,69,duration,128},
 {play,38,duration,128},
 {play,50,duration,128},
 {sleep,128},
 {play,47,duration,128},
 {sleep,2},
 {play,62,duration,130},
 {play,66,duration,130},
 {sleep,126},
 {play,64,duration,128},
 {play,35,duration,256},
 {play,52,duration,128},
 {sleep,2},
 {play,61,duration,130},
 {sleep,126},
 {play,52,duration,128},
 {play,55,duration,128},
 {sleep,2},
 {play,64,duration,130},
 {play,67,duration,130},
 {sleep,5},
 {play,71,duration,391},
 {sleep,114},
 {play,74,duration,121},
 {play,62,duration,121},
 {play,66,duration,121},
 {sleep,7},
 {play,47,duration,256},
 {play,50,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,66,duration,128},
 {sleep,2},
 {play,69,duration,130},
 {play,73,duration,130},
 {sleep,63},
 {play,71,duration,65},
 {sleep,56},
 {play,61,duration,121},
 {play,66,duration,121},
 {sleep,3},
 {play,69,duration,60},
 {sleep,4},
 {play,42,duration,256},
 {play,49,duration,256},
 {play,57,duration,256},
 {sleep,256},
 {play,66,duration,256},
 {play,54,duration,256},
 {play,59,duration,256},
 {sleep,128},
 {play,62,duration,384},
 {play,71,duration,384},
 {sleep,128},
 {play,68,duration,256},
 {play,62,duration,128},
 {play,47,duration,512},
 {play,56,duration,256},
 {sleep,128},
 {play,69,duration,128},
 {play,73,duration,128},
 {sleep,128},
 {play,61,duration,256},
 {play,66,duration,128},
 {play,45,duration,256},
 {play,49,duration,256},
 {play,57,duration,256},
 {sleep,128},
 {play,69,duration,128},
 {sleep,128},
 {play,62,duration,256},
 {play,68,duration,128},
 {play,35,duration,256},
 {play,47,duration,256},
 {sleep,128},
 {play,37,duration,128},
 {play,49,duration,128},
 {sleep,128},
 {play,57,duration,256},
 {play,61,duration,256},
 {play,66,duration,256},
 {play,38,duration,128},
 {play,50,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {sleep,128},
 {play,56,duration,256},
 {play,64,duration,256},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,64,duration,128},
 {sleep,128},
 {play,57,duration,384},
 {play,69,duration,384},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,33,duration,384},
 {play,45,duration,384},
 {sleep,128},
 {play,61,duration,128},
 {play,64,duration,128},
 {play,57,duration,128},
 {play,45,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,59,duration,128},
 {play,64,duration,128},
 {play,43,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,63,duration,128},
 {play,66,duration,128},
 {play,42,duration,128},
 {play,47,duration,128},
 {sleep,128},
 {play,64,duration,128},
 {sleep,128},
 {play,59,duration,128},
 {play,47,duration,128},
 {sleep,128},
 {play,55,duration,384},
 {play,67,duration,384},
 {play,64,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,71,duration,128},
 {play,62,duration,128},
 {play,28,duration,512},
 {play,40,duration,512},
 {play,55,duration,128},
 {sleep,192},
 {play,69,duration,192},
 {sleep,64},
 {play,60,duration,256},
 {play,64,duration,256},
 {play,45,duration,256},
 {play,52,duration,256},
 {play,57,duration,256},
 {sleep,256},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,128},
 {play,59,duration,384},
 {play,64,duration,384},
 {play,67,duration,384},
 {sleep,128},
 {play,55,duration,128},
 {play,59,duration,128},
 {play,64,duration,128},
 {play,28,duration,256},
 {play,40,duration,256},
 {sleep,128},
 {play,59,duration,128},
 {play,64,duration,128},
 {play,67,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,67,duration,128},
 {play,69,duration,128},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,128},
 {play,50,duration,128},
 {sleep,128},
 {play,50,duration,128},
 {sleep,128},
 {play,62,duration,384},
 {play,67,duration,384},
 {play,71,duration,384},
 {play,55,duration,128},
 {play,59,duration,128},
 {sleep,128},
 {play,66,duration,128},
 {play,69,duration,128},
 {play,74,duration,128},
 {play,38,duration,512},
 {play,54,duration,128},
 {play,57,duration,128},
 {sleep,128},
 {play,64,duration,128},
 {play,67,duration,128},
 {play,72,duration,128},
 {play,52,duration,128},
 {play,55,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,69,duration,128},
 {play,50,duration,256},
 {play,54,duration,128},
 {play,57,duration,128},
 {sleep,128},
 {play,69,duration,128},
 {sleep,128},
 {play,67,duration,128},
 {sleep,128},
 {play,62,duration,384},
 {play,71,duration,384},
 {sleep,128},
 {play,31,duration,512},
 {play,43,duration,512},
 {sleep,2},
 {play,67,duration,130},
 {play,59,duration,130},
 {play,62,duration,130},
 {sleep,119},
 {play,71,duration,121},
 {sleep,7},
 {play,43,duration,128},
 {play,55,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,69,duration,128},
 {play,74,duration,128},
 {play,42,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,67,duration,128},
 {sleep,64},
 {play,71,duration,192},
 {play,76,duration,192},
 {sleep,64},
 {play,66,duration,128},
 {play,74,duration,64},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,256},
 {play,69,duration,256},
 {play,73,duration,256},
 {play,64,duration,256},
 {play,45,duration,256},
 {play,57,duration,256},
 {sleep,128},
 {play,69,duration,128},
 {sleep,64},
 {play,61,duration,192},
 {play,73,duration,192},
 {sleep,64},
 {play,64,duration,128},
 {play,59,duration,64},
 {play,71,duration,64},
 {play,43,duration,256},
 {play,55,duration,256},
 {sleep,128},
 {play,57,duration,128},
 {play,42,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,67,duration,128},
 {play,40,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,69,duration,384},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,38,duration,128},
 {play,50,duration,128},
 {sleep,128},
 {play,69,duration,128},
 {play,64,duration,128},
 {play,37,duration,128},
 {play,49,duration,128},
 {sleep,128},
 {play,74,duration,128},
 {play,35,duration,128},
 {play,47,duration,128},
 {sleep,128},
 {play,62,duration,256},
 {play,66,duration,256},
 {play,67,duration,128},
 {play,71,duration,128},
 {play,31,duration,128},
 {play,43,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,69,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,30,duration,256},
 {play,42,duration,256},
 {sleep,256},
 {play,59,duration,256},
 {play,62,duration,256},
 {play,66,duration,256},
 {play,31,duration,256},
 {play,43,duration,256},
 {sleep,256},
 {play,55,duration,256},
 {play,61,duration,256},
 {play,64,duration,256},
 {play,33,duration,256},
 {play,45,duration,256},
 {sleep,256},
 {play,54,duration,256},
 {play,57,duration,256},
 {play,62,duration,256},
 {play,38,duration,256},
 {play,50,duration,256},
 {sleep,256},
 {play,30,duration,128},
 {play,42,duration,128},
 {sleep,128},
 {play,69,duration,384},
 {play,74,duration,384},
 {play,78,duration,384},
 {play,81,duration,384},
 {play,54,duration,384},
 {play,57,duration,384},
 {play,62,duration,384},
 {play,66,duration,384},
 {sleep,128},
 {play,69,duration,128},
 {play,50,duration,128},
 {play,54,duration,128},
 {play,57,duration,128},
 {sleep,128},
 {play,67,duration,128},
 {play,35,duration,384},
 {play,47,duration,384},
 {play,55,duration,128},
 {play,59,duration,128},
 {sleep,128},
 {play,63,duration,128},
 {play,66,duration,128},
 {play,60,duration,128},
 {play,33,duration,128},
 {play,45,duration,128},
 {sleep,128},
 {play,63,duration,128},
 {play,66,duration,128},
 {sleep,128},
 {play,59,duration,256},
 {play,71,duration,256},
 {play,64,duration,128},
 {play,31,duration,256},
 {play,43,duration,256},
 {sleep,128},
 {play,52,duration,128},
 {sleep,67},
 {play,61,duration,195},
 {play,73,duration,195},
 {sleep,48},
 {play,67,duration,243},
 {play,71,duration,243},
 {sleep,9},
 {play,64,duration,60},
 {play,76,duration,60},
 {sleep,4},
 {play,40,duration,256},
 {play,55,duration,128},
 {play,59,duration,128},
 {sleep,130},
 {play,64,duration,130},
 {play,76,duration,130},
 {sleep,113},
 {play,66,duration,243},
 {play,71,duration,243},
 {sleep,6},
 {play,62,duration,121},
 {play,74,duration,121},
 {sleep,7},
 {play,35,duration,256},
 {play,47,duration,256},
 {sleep,130},
 {play,61,duration,130},
 {play,73,duration,130},
 {sleep,113},
 {play,66,duration,243},
 {sleep,6},
 {play,59,duration,121},
 {play,71,duration,121},
 {sleep,7},
 {play,50,duration,256},
 {play,54,duration,256},
 {sleep,195},
 {play,69,duration,195},
 {play,57,duration,195},
 {sleep,48},
 {play,59,duration,243},
 {play,62,duration,243},
 {play,43,duration,243},
 {play,50,duration,243},
 {sleep,9},
 {play,67,duration,60},
 {play,55,duration,60},
 {sleep,260},
 {play,57,duration,256},
 {play,62,duration,256},
 {play,66,duration,256},
 {play,69,duration,256},
 {play,38,duration,256},
 {play,45,duration,256},
 {play,54,duration,256},
 {sleep,256},
 {play,57,duration,256},
 {play,62,duration,256},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,50,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,64,duration,128},
 {play,69,duration,128},
 {play,26,duration,512},
 {play,49,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,59,duration,128},
 {play,62,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,64,duration,128},
 {play,38,duration,256},
 {play,47,duration,256},
 {play,50,duration,256},
 {sleep,128},
 {play,71,duration,384},
 {play,62,duration,128},
 {play,66,duration,128},
 {sleep,128},
 {play,74,duration,128},
 {play,59,duration,128},
 {play,66,duration,128},
 {play,38,duration,256},
 {play,54,duration,128},
 {sleep,128},
 {play,71,duration,128},
 {play,62,duration,128},
 {play,67,duration,128},
 {sleep,64},
 {play,69,duration,64},
 {sleep,64},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,67,duration,64},
 {play,47,duration,256},
 {play,50,duration,256},
 {play,55,duration,256},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,38,duration,128},
 {play,45,duration,128},
 {play,54,duration,128},
 {sleep,130},
 {play,64,duration,130},
 {play,55,duration,130},
 {sleep,128},
 {play,67,duration,130},
 {play,59,duration,130},
 {sleep,119},
 {play,66,duration,121},
 {play,57,duration,121},
 {sleep,7},
 {play,69,duration,512},
 {sleep,128},
 {play,71,duration,128},
 {play,62,duration,128},
 {sleep,64},
 {play,69,duration,64},
 {sleep,64},
 {play,59,duration,256},
 {play,62,duration,128},
 {play,67,duration,64},
 {play,43,duration,256},
 {play,50,duration,256},
 {sleep,128},
 {play,69,duration,128},
 {sleep,128},
 {play,57,duration,256},
 {play,62,duration,256},
 {play,66,duration,128},
 {play,42,duration,256},
 {play,50,duration,256},
 {sleep,128},
 {play,57,duration,128},
 {sleep,128},
 {play,61,duration,256},
 {play,64,duration,256},
 {play,55,duration,128},
 {sleep,128},
 {play,35,duration,384},
 {play,47,duration,384},
 {sleep,128},
 {play,54,duration,256},
 {play,59,duration,256},
 {play,62,duration,256},
 {play,33,duration,128},
 {play,45,duration,128},
 {sleep,256},
 {play,57,duration,256},
 {sleep,128},
 {play,50,duration,384},
 {play,59,duration,384},
 {play,55,duration,128},
 {play,31,duration,384},
 {play,43,duration,384},
 {sleep,128},
 {play,55,duration,128},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,50,duration,128},
 {play,43,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,42,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,69,duration,128},
 {play,50,duration,256},
 {play,38,duration,128},
 {sleep,256},
 {play,59,duration,256},
 {play,62,duration,256},
 {play,66,duration,256},
 {play,71,duration,256},
 {play,35,duration,256},
 {play,47,duration,256},
 {sleep,128},
 {play,66,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,62,duration,256},
 {play,71,duration,256},
 {play,74,duration,256},
 {play,68,duration,128},
 {play,59,duration,256},
 {play,56,duration,128},
 {sleep,128},
 {play,66,duration,128},
 {play,69,duration,128},
 {play,73,duration,128},
 {play,61,duration,128},
 {sleep,64},
 {play,71,duration,64},
 {sleep,64},
 {play,61,duration,128},
 {play,66,duration,128},
 {play,69,duration,64},
 {play,42,duration,256},
 {play,49,duration,256},
 {play,57,duration,256},
 {sleep,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {sleep,128},
 {play,64,duration,128},
 {sleep,128},
 {play,71,duration,384},
 {play,66,duration,128},
 {play,47,duration,384},
 {play,54,duration,384},
 {play,59,duration,384},
 {sleep,128},
 {play,71,duration,128},
 {play,62,duration,128},
 {play,68,duration,128},
 {play,35,duration,128},
 {play,47,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,69,duration,128},
 {play,73,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,66,duration,128},
 {play,33,duration,256},
 {play,45,duration,256},
 {sleep,192},
 {play,69,duration,192},
 {sleep,64},
 {play,57,duration,256},
 {play,62,duration,256},
 {play,68,duration,64},
 {play,35,duration,256},
 {play,47,duration,256},
 {sleep,128},
 {play,37,duration,128},
 {play,49,duration,128},
 {sleep,128},
 {play,66,duration,256},
 {play,57,duration,256},
 {play,61,duration,256},
 {play,38,duration,128},
 {play,50,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,59,duration,128},
 {sleep,128},
 {play,64,duration,256},
 {play,56,duration,128},
 {play,62,duration,128},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,64,duration,128},
 {sleep,128},
 {play,57,duration,384},
 {play,69,duration,384},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,33,duration,384},
 {play,45,duration,384},
 {sleep,128},
 {play,64,duration,128},
 {play,57,duration,128},
 {play,61,duration,128},
 {play,45,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,59,duration,128},
 {play,64,duration,128},
 {play,43,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,63,duration,128},
 {play,66,duration,128},
 {play,42,duration,128},
 {play,47,duration,128},
 {sleep,256},
 {play,47,duration,256},
 {sleep,128},
 {play,55,duration,384},
 {play,64,duration,384},
 {play,67,duration,384},
 {sleep,128},
 {play,71,duration,128},
 {play,62,duration,128},
 {play,64,duration,128},
 {play,40,duration,512},
 {play,47,duration,256},
 {sleep,192},
 {play,67,duration,192},
 {sleep,64},
 {play,60,duration,256},
 {play,64,duration,256},
 {play,45,duration,256},
 {play,52,duration,256},
 {sleep,256},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,128},
 {play,55,duration,384},
 {play,59,duration,384},
 {play,64,duration,384},
 {play,67,duration,384},
 {sleep,128},
 {play,55,duration,128},
 {play,59,duration,128},
 {play,64,duration,128},
 {play,28,duration,256},
 {play,40,duration,256},
 {sleep,128},
 {play,59,duration,128},
 {play,64,duration,128},
 {play,67,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,67,duration,128},
 {play,69,duration,128},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,128},
 {play,50,duration,128},
 {sleep,128},
 {play,62,duration,256},
 {play,67,duration,256},
 {play,71,duration,256},
 {play,38,duration,256},
 {play,50,duration,128},
 {sleep,128},
 {play,67,duration,128},
 {play,71,duration,128},
 {play,55,duration,128},
 {sleep,128},
 {play,74,duration,256},
 {play,62,duration,256},
 {play,66,duration,128},
 {play,59,duration,256},
 {play,54,duration,128},
 {play,57,duration,128},
 {sleep,128},
 {play,72,duration,128},
 {play,64,duration,128},
 {play,67,duration,128},
 {play,60,duration,128},
 {play,52,duration,128},
 {play,55,duration,128},
 {sleep,128},
 {play,69,duration,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,57,duration,128},
 {play,60,duration,128},
 {play,50,duration,128},
 {sleep,128},
 {play,69,duration,128},
 {play,57,duration,128},
 {sleep,128},
 {play,67,duration,128},
 {play,55,duration,128},
 {sleep,128},
 {play,62,duration,384},
 {play,71,duration,384},
 {play,43,duration,384},
 {play,50,duration,384},
 {sleep,128},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,67,duration,128},
 {play,31,duration,128},
 {play,43,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,67,duration,128},
 {play,71,duration,128},
 {play,43,duration,128},
 {play,55,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,69,duration,128},
 {play,74,duration,128},
 {play,42,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,64,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,40,duration,256},
 {play,52,duration,256},
 {sleep,128},
 {play,67,duration,384},
 {play,71,duration,384},
 {play,76,duration,384},
 {play,61,duration,128},
 {play,69,duration,128},
 {sleep,128},
 {play,62,duration,128},
 {play,71,duration,128},
 {play,74,duration,128},
 {play,59,duration,128},
 {sleep,128},
 {play,57,duration,384},
 {sleep,7},
 {play,45,duration,391},
 {sleep,57},
 {play,61,duration,192},
 {play,64,duration,192},
 {play,73,duration,192},
 {sleep,57},
 {play,43,duration,121},
 {play,55,duration,121},
 {sleep,7},
 {play,71,duration,64},
 {sleep,128},
 {play,62,duration,128},
 {play,42,duration,128},
 {play,54,duration,128},
 {sleep,128},
 {play,61,duration,128},
 {play,67,duration,128},
 {play,40,duration,128},
 {play,52,duration,128},
 {sleep,128},
 {play,66,duration,384},
 {play,69,duration,384},
 {play,62,duration,128},
 {play,38,duration,128},
 {play,50,duration,128},
 {sleep,128},
 {play,69,duration,128},
 {play,64,duration,128},
 {play,37,duration,128},
 {play,49,duration,128},
 {sleep,128},
 {play,66,duration,128},
 {play,74,duration,128},
 {play,35,duration,128},
 {play,47,duration,128},
 {sleep,128},
 {play,62,duration,256},
 {play,67,duration,128},
 {play,71,duration,128},
 {play,31,duration,128},
 {play,43,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,69,duration,128},
 {sleep,128},
 {play,59,duration,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,71,duration,128},
 {play,30,duration,256},
 {play,42,duration,256},
 {sleep,256},
 {play,59,duration,256},
 {play,62,duration,256},
 {play,66,duration,256},
 {play,31,duration,256},
 {play,43,duration,256},
 {sleep,256},
 {play,55,duration,256},
 {play,61,duration,256},
 {play,64,duration,256},
 {play,33,duration,256},
 {play,45,duration,256},
 {sleep,384},
 {play,54,duration,384},
 {play,57,duration,384},
 {play,62,duration,384},
 {sleep,128},
 {play,54,duration,128},
 {play,62,duration,128},
 {sleep,128},
 {play,57,duration,128},
 {play,62,duration,128},
 {play,66,duration,128},
 {play,50,duration,384},
 {sleep,128},
 {play,57,duration,128},
 {play,64,duration,128},
 {play,69,duration,128},
 {play,26,duration,768},
 {play,38,duration,768},
 {play,49,duration,128},
 {sleep,128},
 {play,47,duration,128},
 {sleep,128},
 {play,38,duration,256},
 {play,50,duration,128},
 {sleep,5},
 {play,66,duration,261},
 {sleep,103},
 {play,62,duration,364},
 {play,71,duration,364},
 {sleep,13},
 {play,67,duration,121},
 {sleep,135},
 {play,67,duration,128},
 {play,79,duration,128},
 {play,47,duration,256},
 {play,55,duration,256},
 {sleep,130},
 {play,67,duration,130},
 {play,79,duration,130},
 {sleep,30},
 {play,66,duration,32},
 {play,78,duration,32},
 {sleep,62},
 {play,64,duration,30},
 {play,76,duration,30},
 {sleep,21},
 {play,71,duration,243},
 {play,74,duration,243},
 {sleep,13},
 {play,43,duration,256},
 {play,50,duration,256},
 {play,59,duration,256},
 {sleep,768},
 {play,66,duration,768},
 {play,69,duration,768},
 {play,74,duration,768},
 {play,78,duration,768},
 {play,38,duration,768},
 {play,50,duration,768}].




