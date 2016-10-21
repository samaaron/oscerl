-module(pi_relay).
-compile(export_all).

%%  set Sonic.app/server/bin/port-discovery.rb
%%     gui_send_to_server = 6000 (was 4557)
%%     server_send_to_gui = 6001 (was 4558)
%%     gui_listen_to_server = 4558
%%     server_listen_to_gui = 4557


%% Listen to port 6000 -> 4558 (from gui to server)
%%                6001 -> 4557 (from server to gui)

server_port() -> 8014.

%% started with Make relay

start() ->

    Server_send_to_gui   = 6001,
    Gui_listen_to_server = 4558,    
    Server_listen_to_gui = 4557,
    Gui_send_to_server   = 6000,
    
    spawn(fun() -> relay('Server -> GUI', 
			 Server_send_to_gui,
			 Gui_listen_to_server) end),
    spawn(fun() -> relay('Gui -> Server', 
			 Gui_send_to_server,
			 Server_listen_to_gui) end),
    receive
	ack ->
	    true
    end.

relay(Tag, FromPort, ToPort) ->
    io:format("relaying ~w -> ~w~n",[FromPort, ToPort]),
    {ok, Socket} = gen_udp:open(FromPort, [binary]),
    io:format("Listen on port:~p~n",[FromPort]),
    loop(Tag, Socket, 0, ToPort).

loop(Tag, Socket, N, ToPort) ->
    receive
	{udp, Socket, _Ip, _Port, Bin} ->
	    gen_udp:send(Socket,"localhist", ToPort, Bin),
	    Decoded = (catch osc:decode(Bin)),
	    io:format("~p ~p~n",[Tag,Decoded]),
	    loop(Tag, Socket, N+1, ToPort);
	O ->
	    io:format("unexpected message:~p~n",[O]),
	    loop(Tag, Socket, N+1, ToPort)
    end.
