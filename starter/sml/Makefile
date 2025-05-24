
.PHONY: c0c c0c-sml c0c-mlton bin
c0c: c0c-smlnj

# SML/NJ
SML = sml

bin:
	mkdir -p ../bin

c0c-smlnj: bin parse
	ml-lex parse/c0.lex
	ml-yacc parse/c0.grm
	echo 'use "compile-c0c.sml";' | ${SML}
	cp bin/c0c.sh ../bin/c0c

# MLton
MLTON = mlton -const "Exn.keepHistory true" -default-ann "redundantMatch warn" -default-ann "sequenceNonUnit warn" -output
MLTON_TARG = ../bin/c0c

c0c-mlton: bin
	mllex parse/c0.lex
	mlyacc parse/c0.grm
	$(MLTON) $(MLTON_TARG) sources.mlb

clean:
	find . -type d -name .cm | xargs rm -rf
	${RM} parse/*.lex.* parse/*.grm.*
	find . -type f | grep '~$$' | xargs ${RM}
	${RM} bin/*.heap.* $(MLTON_TARG)
