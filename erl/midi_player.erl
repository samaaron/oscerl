-module(midi_player).
-compile(export_all).

test1() ->
    io:format("play misic on garage bad~n"),
    midi_out_driver:start(external),
    %% play("jerusalem.mid"),
    play("rach-pc1-1.mid"),
    true.

test2() ->
    midi_out_driver:start(internal),
    %% play("jerusalem.mid"),
    play("rach-pc1-1.mid"),
    true.
    
play(F) ->
    L = midi_parser:parse_file(F),
    Tmap = make_tempo_map(L),
    [_,_|L0] = L,
    L1 = [normalise(I, Tmap) || I <- L0],
    L2 = lists:append(L1),
    L3 = lists:sort(L2),
    %% elib2_misc:dump("tmp", L3),
    play1(0, L3),
    true.

make_tempo_map([#{time := {ticks_per_quarter_note, PPQN}},
		#{data := D} | _]) ->
    case find_time_sig(D) of
	no ->
	    io:format("no time signature found~n");
	{{timeSig,N,M,C,B}, Rest} ->
	    io:format("PPQNQ  = ~p~n",[PPQN]),
	    io:format("Time Sig = ~p/~p~n",[N,M]),
	    io:format("Ticks per clock = ~p~n",[C]),
	    io:format("number of 1/32 note per quarter (=8) = ~p~n",[B]),
	    make_tempo_map(Rest, 0, 0, PPQN, [])
    end.

find_time_sig([{_,{timeSig,_,_,_,_}=Ts}|T]) ->
    {Ts, T};
find_time_sig([_|T]) ->
    find_time_sig(T);
find_time_sig(_) ->
    no.
    
make_tempo_map([{Dt,{setTempo, Tempo}}|T], Tabs, OldTick, PPQN, L) -> 
    Now = Tabs + Dt*OldTick,    
    BPM = 600000000/Tempo,
    Tick = 600/(BPM*PPQN),
    io:format("BPM = ~p~n",[BPM]),
    io:format("Tick = ~p~n",[Tick]),
    make_tempo_map(T, Now, Tick, PPQN, [{Dt, Tick}|L]);
make_tempo_map([{Dt,X}|T], Tabs, Tick, PPQN, L) -> 
    io:format("dropping~p~n",[X]),
    make_tempo_map(T, Tabs + Dt*Tick, Tick, PPQN, L);
make_tempo_map([], _, _, _, L) -> 
    lists:reverse(L).

play1(Time, [{Time,D}|T]) ->
    midi:send(D),
    play1(Time, T);
play1(Time1, [{Time2,D}|T]) ->
    delay(Time2-Time1),
    midi:send(D),
    play1(Time2, T).

delay(T) ->
    timer:sleep(T).

normalise(#{type := track, data := Data}, Tmap) ->
    [{_,Delta}|_] = Tmap,
    Abs = 0,
    add_times(Data, Abs, Delta, Tmap, []).

add_times([{T1,E}|T2], Abs, Delta, [], L) ->
    Abs1 = Abs + T1 * Delta,
    add_times(T2, Abs1, Delta, [], [{trunc(Abs1),E}|L]);
add_times([{T1,E}|T2], Abs, Delta, [{Tnext,Delta1}|T3]=A1, L) ->
    Abs1 = T1*Delta + Abs,
    if 
	Abs1 >= Tnext ->
	    %% change delta for next event
	    add_times(T2, Abs1, Delta1, T3, [{trunc(Abs1*1000),E}|L]);
	true ->
	    add_times(T2, Abs1, Delta, A1, [{trunc(Abs1*1000),E}|L])
    end;
add_times([], _, _, _, L) ->
    lists:reverse(L).

add_times(T1, [{T2,D}|T], L) ->
    T3 = T1 + T2,
    add_times(T3, T, [{T3,D}|L]);
add_times(_, [], L) ->
    lists:reverse(L).

