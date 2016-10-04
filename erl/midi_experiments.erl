-module(midi_experiments).
-compile(export_all).
-import(lists, [reverse/1]).

test1() ->
    io:format("ruby transcoder~n"),
    L = midi_parser:parse_file("../midi_files/jerusalem.mid"),
    %% elib2_misc:dump("midi.tmp", L),
    M = add_tracknames(L),
    T = transcribe(M, <<"Piano">>),
    elib2_misc:dump("notes.tmp", T),
    N1 = to_ruby(T),
    elib2_misc:dump("tune.cmds", N1),
    N2 = [ruby_render(I) || I <- N1],
    file:write_file("sonic.txt", N2).

test_seq() ->
    io:format("sequential rendering engine~n"),  
    midi_out_driver:start(internal),
    L = midi_parser:parse_file("../midi_files/jerusalem.mid"),
    %% elib2_misc:dump("midi.tmp", L),
    Map = add_tracknames(L),
    case maps:find(<<"Piano">>, Map) of
	{ok, #{data := L1}} ->
	    elib2_misc:dump("note_list.tmp", L1),
	    play_seq(L1);
	error ->
	    void
    end.

play_seq([{0,D}|T]) ->
    midi:send(D),
    play_seq(T);
play_seq([{Dt,D}|T]) ->
    delay(trunc(Dt*2.8)),
    midi:send(D),
    play_seq(T);
play_seq([]) ->
    [].

delay(T) ->
    io:format("delay:~p~n",[T]),
    timer:sleep(T).

test_par() ->
    io:format("parallel rendering engine~n"),
    midi_out_driver:start(internal),
    L = midi_parser:parse_file("../midi_files/jerusalem.mid"),
    %% elib2_misc:dump("midi.tmp", L),
    M = add_tracknames(L),
    T = transcribe(M, <<"Piano">>),
    %% elib2_misc:dump("notes.tmp", T),
    L1 = elib2_misc:merge_kv(T),
    %% elib2_misc:dump("merged.tmp", L1),
    io:format("creating ~p proceses~n",[length(L1)]),
    [make_player(I) || I <- L1],
    true.
    
make_player(X) ->
    spawn(fun() ->
		  player(X)
	  end).

player({Time, L}) ->
    T1 = trunc(Time*3),
    receive
	after T1 ->
		[play_one_note(I) || I <- L]
	end.

play_one_note({play,Note,Dur,Vol}=X) ->
    io:format("play one note~p~n",[X]),
    midi:send({noteOn,0,Note,Vol}),
    spawn(fun() ->
		  D1 = trunc(Dur*3),
		  receive
		  after D1 ->
			  midi:send({noteOff,0,Note,0})
		  end
	  end).
		  
clock() ->
    0.003.

ruby_render({sleep,K}) ->
    io_lib:format("sleep ~6.3f~n", [K*clock()]);
ruby_render({play,N,duration,R}) ->
    io_lib:format("play ~p, sustain:~6.3f~n", [N,R*clock()]).

to_ruby(L) ->
    to_ruby(L, 0, []).

to_ruby([{T1,{play,Note,Dur,_Vol}}|T], T1, L) ->
    %% samme time
    L1 = [{play,Note,duration,Dur}|L],
    to_ruby(T, T1, L1);
to_ruby([{T1,_}|_]=A, T2, L) ->
    L1 = [{sleep,T1-T2}|L],
    to_ruby(A, T1, L1);
to_ruby([], _, L) ->
    reverse(L).

transcribe(Map, Key) ->
    case maps:find(Key, Map) of
	{ok, #{data := L}} ->
	    transcribe(L, 0, dict:new(), []);	    
	error ->
	    exit({notrack_called, Key})
    end.

add_tracknames(L) ->
    Names = [track_names(I) || I<- L],
    L1 = lists:zip(Names, L),
    maps:from_list(L1).

track_names(#{data := L}) -> first([Name || {_,{trackName,Name}} <- L]);
track_names(_)            -> none.

first([X|_]) -> X;
first(_)     -> none.
    
transcribe([{Dt,{noteOn,Channel,Note,Vol}}|T], T0, On, L) ->
    T1 = T0 + Dt,
    case dict:find(Note, On) of
	{ok, _} ->
	    io:format("NoteOn ~p when it's ON!!!~n",[Note]),
	    transcribe(T, T1, On, L);
	error ->
	    %% Goody
	    On1 = dict:store(Note, {Channel, T1,Vol}, On),
	    transcribe(T, T1, On1, L)
    end;
transcribe([{Dt,{noteOff,Channel,Note,_}}|T], T0, On, L) ->
    T1 = T0 + Dt,
    case dict:find(Note, On) of
	{ok, {Channel, Ton, Vol}} ->
	    Duration = T1 - Ton,
	    L1 = [{T1, {play,Note,Duration,Vol}}|L],
	    On1 = dict:erase(Note, On),
	    transcribe(T, T1, On1, L1);
	error ->
	    io:format("NoteOff ~p but it's not on!!!~n",[Note]),
	    transcribe(T, T1, On, L);
	Other ->
	    io:format("Other *** channel error:~p~n",[Other]),
	    transcribe(T, T1, On, L)
    end;
transcribe([{Dt,X}|T], T0, On, L) ->
    io:format("dropping:~p~n",[X]),
    T1 = T0 + Dt,
    transcribe(T, T1, On, L);
transcribe([], _, On, L) ->
    case dict:to_list(On) of
	[] ->
	    void;
	L1 ->
	    io:format("Some notes on at eos:~p~n",[L1])
    end,
    reverse(L).
