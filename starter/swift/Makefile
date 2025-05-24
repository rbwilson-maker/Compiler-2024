SUBDIRS = Util Parse Trans Type Codegen
MAIN = main.swift

ifeq ($(shell uname -s),Darwin)
  SWIFTC=xcrun -sdk macosx swiftc
  CLANG=MACOSX_DEPLOYMENT_TARGET=10.9 clang
else
  SWIFTC=swiftc
  CLANG=clang
endif

CFLAGS = -std=c11 -fPIC
SWIFTFLAGS = -module-name c0c -Onone -g -Xfrontend -serialize-debugging-options -j4

SWIFT_SOURCES = $(SUBDIRS:%=%/*.swift)
C_SOURCES = Util/*.c

all: c0c

c0c: ../bin/c0c
../bin/c0c: Util/lex.yy.c Util/y.tab.c Util/y.tab.h $(wildcard $(SWIFT_SOURCES)) $(MAIN)
	mkdir -p ../bin
	mkdir -p _build
	cd _build; $(CLANG) $(CFLAGS) -c $(C_SOURCES:%=../%)
	cd _build; $(SWIFTC) $(SWIFTFLAGS) -c -import-objc-header ../c0c-Bridging-Header.h $(SWIFT_SOURCES:%=../%) ../$(MAIN)
	$(SWIFTC) -o $@ _build/*.o

Util/lex.yy.c: Parse/c0Lexer.l Util/y.tab.h
	flex -o Util/lex.yy.c Parse/c0Lexer.l

Util/y.tab%c Util/y.tab%h: Parse/c0Parser%y
	bison -d -o Util/y.tab.c Parse/c0Parser.y

clean:
	rm -Rf _build
	rm -Rf ../bin

.INTERMEDIATE: Util/lex.yy.c Util/y.tab.c Util/y.tab.h
.PHONY: c0c clean
