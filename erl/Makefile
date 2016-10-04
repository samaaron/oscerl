.SUFFIXES: .erl .beam 

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

all: beams 
	erl -s udp_server start

seq: beams
	erl -pa drivers -s midi_experiments test_seq

par: beams
	erl -pa drivers -s midi_experiments test_par



beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam





