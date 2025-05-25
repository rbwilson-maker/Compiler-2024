# Running 'make labN' should cause the compiler for labN to be placed
# in the bin directory as bin/c0c.

# Just modify default each lab.
DEFAULT = 6
TEST_DEFAULT = 4

.PHONY: lab* grade test

default: lab$(DEFAULT)

grade-basic: default
	../gradecompiler ../tests/l$(TEST_DEFAULT)-basic/ -a -O1

grade-basic-unsafe:
	../gradecompiler ../tests/l$(TEST_DEFAULT)-basic/ --safe-only -a --unsafe -O1

grade-large: default
	../gradecompiler ../tests/l$(TEST_DEFAULT)-large/ -a -O1

grade-large-unsafe:
	../gradecompiler ../tests/l$(TEST_DEFAULT)-large/ -O1 --safe-only -a --unsafe

# outputs x86-64 and abstract assembly files for a specific test
%.l$(TEST_DEFAULT): default
	bin/c0c -l ../runtime/15411-l4.h0 -eabs $@
	bin/c0c -l ../runtime/15411-l4.h0 -ex86-64 -v $@

# if you want to see assembly output run this, it will also output the files
%.l$(TEST_DEFAULT)_gcc: %.l$(TEST_DEFAULT).s
	sed -i "" 's/_c0/__c0/g' $<
	sed -i "" 's/calloc/_calloc/g' $<
	sed -i "" 's/raise/_raise/g' $<
	sed -i "" 's/abort/_abort/g' $<
	arch -x86_64 gcc -m64 $< ../runtime/run411.c
	./a.out

%.l$(DEFAULT): default
	bin/c0c -l ../runtime/15411-l4.h0 -eabs $@ 
	bin/c0c -l ../runtime/15411-l4.h0 -ex86-64 -v $@

# if you want to see assembly output run this, it will also output the files
%.l$(DEFAULT)_gcc: %.l$(DEFAULT).s
	sed -i "" 's/_c0/__c0/g' $<
	sed -i "" 's/calloc/_calloc/g' $<
	sed -i "" 's/raise/_raise/g' $<
	sed -i "" 's/abort/_abort/g' $<
	arch -x86_64 gcc -m64 $< ../runtime/run411.c
	./a.out

test:
	../gradecompiler tests/lab6-tests -O1

run-bench:
	echo "Unsafe Mode Tests"
	for file in ../tests/bench/*.* ; do \
		bin/c0c -l ../runtime/15411-l4.h0 -O1 --count-lines -ex86-64 --unsafe $${file} ; \
	done
	echo "Safe Mode Tests"
	rm ../tests/bench/*.s
	for file in ../tests/bench/*.* ; do \
		bin/c0c -l ../runtime/15411-l4.h0 -O1 --count-lines -ex86-64 $${file} ; \
	done
	rm ../tests/bench/*.s

lab*: bin
	$(MAKE) -C $@

bin:
	mkdir bin

clean:
	rm -Rf bin
	for l in lab*; do $(MAKE) -C $$l clean; done
	rm ../tests/*.{abs,s}

# update the second argument with the name of the branch!!!
name:
	echo "github.com/15-411-S24/MaryamMirzakhani.git" "l6-base"

