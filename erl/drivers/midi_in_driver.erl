-module(midi_in_driver).
%% -compile(export_all).

-export([start/0, stop/0, test/0, subscribe/1, send_event/1]).

test() ->
    midi_in_driver:start(),
    spawn(fun add_listener/0).

add_listener() ->
    midi_in_driver:subscribe(self()),
    event_loop().

event_loop() ->
    receive
	Any ->
	    io:format("received:~p~n",[Any]),
	    event_loop()
    end.

start() ->
    Parent = self(),
    Prog = filename:dirname(code:which(?MODULE)) ++ "/midi_in_driver",
    Pid = spawn(fun() ->
			register(?MODULE, self()),
			process_flag(trap_exit, true),
			Port = open_port({spawn, Prog}, 
					 [{packet, 2}]),
			io:format("Port=~p~n",[Port]),
			Parent ! {self(), ack},
			loop(Port, [])
		end),
    receive
	{Pid, ack} ->
	    true
    end.

stop() ->
    ?MODULE ! stop.

send_event(M) -> call_port([1|M]).

subscribe(Pid) ->
    ?MODULE ! {subscribe, Pid}.

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{?MODULE, Result} ->
	    Result
    end.

loop(Port, L) ->
    receive
	{subscribe, Pid} ->
	    link(Pid),
	    loop(Port, [Pid|L]);
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {?MODULE, Data}
	    end,
	    loop(Port, L);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason});
	{Port,{data,Data}} ->
	    %% io:format("~n~p bytes~n",[length(Data)]),
	    Event = cvt(list_to_binary(Data)),
	    %% io:format("event:~p ~p~n",[Event, L]),
	    [Pid ! Event || Pid <- L],
	    loop(Port, L);
	Any ->
	    io:format("*** Dropped:~p~n",[Any]),
	    loop(Port, L)
    end.
	
cvt(<<1,T:64/unsigned-little-integer,B/binary>>) ->
    {midi_event, T, B}.

