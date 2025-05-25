all: c0c

c0c: ../bin/c0c
../bin/c0c: always
	dune build
	mkdir -p ../bin
	install _build/default/bin/c0c.exe $@

.PHONY: test
test: c0c
	dune runtest

always:

clean:
	dune clean

.PHONY: c0c clean native
