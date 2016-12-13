.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<


remote: beams
	erl -pa ./drivers -s pi_http_server start

all: beams
	erl -s pi_server start

key25: beams
	erl -pa ./drivers -s midi_key25 start

relay: beams
	erl -s pi_relay start


doc.pdf: doc.tex
	latexmk -pdf doc.tex
	latexmk -c

doc.tex: doc.org
	erl -pa ../../doc_latex -oldshell -s convert_org_to_latex file doc -s init stop

seq: beams
	erl -pa drivers -s midi_experiments test_seq

par: beams
	erl -pa drivers -s midi_experiments test_par

demo_seq1: beams
	erl -pa drivers -s midi_experiments demo_seq1

demo_seq2: beams
	erl -pa drivers -s midi_experiments demo_seq2

demo_par1: beams
	erl -pa drivers -s midi_experiments demo_par1

demo_par2: beams
	erl -pa drivers -s midi_experiments demo_par2

demo_par3: beams
	erl -pa drivers -s midi_experiments demo_par2

par3a: beams
	erl -pa drivers -s midi_experiments2 test_par3

par3b: beams
	erl -pa drivers -s midi_experiments3 test_par3

par3c: beams
	erl +K true -pa drivers -s midi_experiments test_par3

clip_player: beams
	erl -pa drivers -s midi_clip run

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam
