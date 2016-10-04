-module(midi).
-compile(export_all).

test1() ->
    %% test the internal driver
    midi_out_driver:start(internal),
    scale(60,70,100),
    instruments(20,30).

instruments(Min, Max) ->
    for(Min, Max,
	fun(I) ->
		set_instrument(1, I),
		scale(60,70,50)
	end).

set_instrument(Channel, Instrument) ->
    io:format("changing instrument to:~p~n",
	      [instrument_name(Instrument)]),
    send({programChange,Channel,Instrument}).


send({programChange, Channel, K}) ->
    io:format("instrument =~p~n",[K]),
    Name = instrument_name(K),
    io:format("setting Channel:~p to ~p:~s~n",[Channel,K,Name]),
    midi_out_driver:send_midi([16#C0 + Channel, K, 0]);
send({controllerChange, Channel, Pitch, Vol}) ->
    midi_out_driver:send_midi([16#B0 + Channel, Pitch, Vol]);
send({noteOn, Channel, Pitch, Vol}) ->
    midi_out_driver:send_midi([16#90 + Channel, Pitch, Vol]);
send({noteOff, Channel, Pitch, Vol}) ->
    midi_out_driver:send_midi([16#80 + Channel, Pitch, Vol]);
send({lyric, X}) ->
    io:format("~s ~n",[X]);
send(X) ->
    io:format("dropping:~p~n",[X]).

parse_event(<<8:4,C:4,P,V>>) ->
    {noteOff, {channel, C}, {pitch, P}, {col, V}};
parse_event(<<9:4,C:4,P,V>>) ->
    {noteOn, {channel, C}, {pitch, P}, {vol, V}};
parse_event(Other) ->
    {other, Other}.

scale(_, _K, T) ->
    for(60,70,
	fun(I) -> play_note(I,T) end
       ).

for(I,I,F) -> F(I);
for(I,J,F) -> F(I), for(I+1,J,F).

play_note(I,T) ->
    send({noteOn,1,I,80}),
    timer:sleep(T),
    send({noteOn,1,I,0}).

instrument_name(I) ->
    element(I+1, instrument_names()).

instrument_names() ->
    {"Acoustic Grand Piano", "Bright Acoustic Piano",
     "Electric Grand Piano", "Honky-tonk Piano",
     "Electric Piano 1", "Electric Piano 2", "Harpsichord",
     "Clavi", "Celesta", "Glockenspiel", "Music Box",
     "Vibraphone", "Marimba", "Xylophone", "Tubular Bells",
     "Dulcimer", "Drawbar Organ", "Percussive Organ",
     "Rock Organ", "Church Organ", "Reed Organ",
     "Accordion", "Harmonica", "Tango Accordion",
     "Acoustic Guitar (nylon)", "Acoustic Guitar (steel)",
     "Electric Guitar (jazz)", "Electric Guitar (clean)",
     "Electric Guitar (muted)", "Overdriven Guitar",
     "Distortion Guitar", "Guitar harmonics",
     "Acoustic Bass", "Electric Bass (finger)",
     "Electric Bass (pick)", "Fretless Bass",
     "Slap Bass 1", "Slap Bass 2", "Synth Bass 1",
     "Synth Bass 2", "Violin", "Viola", "Cello",
     "Contrabass", "Tremolo Strings", "Pizzicato Strings",
     "Orchestral Harp", "Timpani", "String Ensemble 1",
     "String Ensemble 2", "SynthStrings 1", "SynthStrings 2",
     "Choir Aahs", "Voice Oohs", "Synth Voice",
     "Orchestra Hit", "Trumpet", "Trombone", "Tuba",
     "Muted Trumpet", "French Horn", "Brass Section",
     "SynthBrass 1", "SynthBrass 2", "Soprano Sax",
     "Alto Sax", "Tenor Sax", "Baritone Sax", "Oboe",
     "English Horn", "Bassoon", "Clarinet", "Piccolo",
     "Flute", "Recorder", "Pan Flute", "Blown Bottle",
     "Shakuhachi", "Whistle", "Ocarina", "Lead 1 (square)",
     "Lead 2 (sawtooth)", "Lead 3 (calliope)", "Lead 4 (chiff)",
     "Lead 5 (charang)", "Lead 6 (voice)", "Lead 7 (fifths)",
     "Lead 8 (bass + lead)", "Pad 1 (new age)", "Pad 2 (warm)",
     "Pad 3 (polysynth)", "Pad 4 (choir)", "Pad 5 (bowed)",
     "Pad 6 (metallic)", "Pad 7 (halo)", "Pad 8 (sweep)",
     "FX 1 (rain)", "FX 2 (soundtrack)", "FX 3 (crystal)",
     "FX 4 (atmosphere)", "FX 5 (brightness)", "FX 6 (goblins)",
     "FX 7 (echoes)", "FX 8 (sci-fi)", "Sitar", "Banjo",
     "Shamisen", "Koto", "Kalimba", "Bag pipe", "Fiddle",
     "Shanai", "Tinkle Bell", "Agogo", "Steel Drums",
     "Woodblock", "Taiko Drum", "Melodic Tom", "Synth Drum",
     "Reverse Cymbal", "Guitar Fret Noise", "Breath Noise",
     "Seashore", "Bird Tweet", "Telephone Ring",
     "Helicopter", "Applause", "Gunshot"}.

