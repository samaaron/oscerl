-module(midi_recorder).

-compile(export_all).

spy() ->
    midi_in_driver:start(),
    midi_in_driver:subscribe(self()),
    show().

show() ->
    receive
	{midi_event, _Time, Event}=X ->
	    io:format("event:~p ",[X]),
	    io:format("~p~n",[midi:parse_event(Event)]),
	    show();
	Other ->
	    io:format("Other:~p~n",[Other]),
	    show()
    end.

start() ->
    midi_in_driver:start(),
    spawn(fun() ->
		  record()
	  end).

record() ->
    midi_in_driver:subscribe(self()),
    go().

go() ->
    C = get_note("play control note"),
    say("Thanks"),
    say("Recording"),
    receive
	Any ->
	    io:format("event:~p~n",[Any]),
	    loop(C, [])
    end.

loop(Stop, L) ->
    receive
	{midi_event, _Time, <<128,Stop,_>>} ->
	    say("sequence finished"),
	    save_sequence(L),
	    loop(Stop, []);
	Any ->
	    io:format("event:~p~n",[Any]),
	    loop(Stop, [Any|L])
    end.

save_sequence(L) ->
    F = file_name(0),
    file:write_file(F, term_to_binary(lists:reverse(L))),
    say_format("written ~p events to:~p~n",[length(L), F]).

say_format(F, D) ->
    Z = lists:flatten(io_lib:format(F, D)),
    say(Z).


file_name(K) ->
    F = "rec_" ++ integer_to_list(K) ++ ".rec",
    case filelib:is_file(F) of
	true ->
	    file_name(K+1);
	false ->
	    F
    end.


get_note(X) ->
    say(X),
    get_single_note(X).

say(X) ->
    os:cmd("say " ++ X).


get_single_note(Say) ->
    receive
	{midi_event, _Time, <<144,K,_>>=P} ->
	    io:format("midi:~p~n",[midi:parse_event(P)]),
	    get_stop_note(Say, K);
	Other ->
	    io:format("got:~p~n",[Other])
    end.

get_stop_note(Say, K) ->
    receive
	{midi_event, _Time, <<128,K,_>>=P} ->
	    io:format("midi:~p~n",[midi:parse_event(P)]),
	    K;
	Other ->
	    io:format("oops:~p~n",[Other]),
	    get_note(Say)
    end.


	
