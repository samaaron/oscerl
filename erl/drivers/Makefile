.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<


all: beams midi_event_gen midi_in_driver midi_synt_driver

read: all
	erl -s midi_in_driver test

test: all
	erl -s midi_out_driver test_internal	

test1: all
	erl -s midi_out_driver test_external



midi_in_driver: midi_in_driver.c erl_comm.c
	gcc -o midi_in_driver midi_in_driver.c erl_comm.c \
               -framework CoreMidi -framework CoreServices

midi_event_gen: midi_event_gen.c erl_comm.c
	gcc -o midi_event_gen midi_event_gen.c erl_comm.c \
               -framework CoreMidi -framework CoreServices

midi_synt_driver: midi_synt_driver.c erl_comm.c
	gcc -o midi_synt_driver midi_synt_driver.c erl_comm.c \
               -framework CoreMidi -framework CoreServices \
               -framework AudioToolbox -framework CoreAudio -framework AudioUnit 


beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf midi_event_gen midi_synt_driver *.beam midi_in_driver


