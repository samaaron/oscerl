-module(midi_out_driver).

-export([start/1, stop/0, send_midi/1,
	 test_internal/0, test_external/0]).
	 

test_internal() ->
    start(internal),
    play_scale(60,80),
    init:stop().

test_external() ->
    io:format("make sure to have an external midi player running~n"),
    start(external),
    play_scale(60,80),
    init:stop().

play_scale(I,I) ->
    play_note(I);
play_scale(I,J) ->
    play_note(I),
    play_scale(I+1,J).

play_note(I) ->    
    midi_out_driver:send_midi([144,I,80]),
    timer:sleep(500),
    midi_out_driver:send_midi([144,I,0]).

start(Type) ->
    Exec = driver(Type),
    Prog = filename:dirname(code:which(?MODULE)) ++ Exec,
    register(?MODULE, 
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Port = open_port({spawn, Prog},
					    [{packet, 2}]),
		     loop(Port)
		   end)),
    sleep(1500).  %% why since drivers takes a while to start

driver(internal) -> "/midi_synt_driver";
driver(external) -> "/midi_event_gen".

sleep(T) ->
    receive
	after T ->
		true
	end.

stop() ->
    ?MODULE ! stop.

send_midi(M) -> call_port([1|M]).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{?MODULE, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {?MODULE, Data}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason})
    end.
	
