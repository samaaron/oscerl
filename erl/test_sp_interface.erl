-module(test_sp_interface).
-compile(export_all).

%% play note immediately

play_note_now(Note) ->
    Cmd = ["/foo/bar", Note],
    E = osc:encode(Cmd),
    io:format("sending:~p~n",[E]),
    {ok, Socket} = gen_udp:open(0,[binary]),
    ok = gen_udp:send(Socket, "localhost", 4559, E),
    gen_udp:close(Socket).

%% do play_note_now(Note) after a Delay ms

play_note_in_future(Note, Delay) ->
    spawn(fun() ->
		  sleep(Delay),
		  play_note_now(Note)
	  end).

sleep(T) ->
    receive
	after T ->
		true
	end.

test1() ->
    Cmd = ["/run-code" , "erl-id", "play 80\n"],
    E = osc:encode(Cmd),
    io:format("sending:~p~n",[E]),
    {ok, Socket} = gen_udp:open(0,[binary]),
    ok = gen_udp:send(Socket, "localhost", 4557, E),
    gen_udp:close(Socket).



    
