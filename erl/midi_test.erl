-module(midi_test).
-compile(export_all).

test1() ->
    midi_out_driver:start(external),
    a.

