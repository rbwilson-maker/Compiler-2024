CC = g++
CXX = g++
CFLAGS = -O1 -Wall -Wconversion -g -std=c++11
CXXFLAGS = $(CFLAGS)
OBJDIR := objs

.PHONY:	setup depend

top:	setup c0c
	/bin/cp c0c ../bin

# List of source files to build. When creating new files,
# you should only have to modify this list *correctly*.
CSRCS = c0c.cc 				\
				bb.cc 				\
				cst.cc 				\
				symbols.cc 		\
				symtab.cc 		\
				tokennames.c	\
				c0.tab.c 			\
				lex.yy.c 			\
				type.cc 			\
				munch.cc 			\
				util.cc 			\
				output.cc 		\
				arg.cc 				\
				srcfile.cc 		\
				ir.cc 				\
				asm.cc 				\
				typecheck.cc 	\
				cfg.cc 				\
				l1ckpt.cc			\

CSRCNAMES = $(basename $(CSRCS))
OBJS = $(addprefix $(OBJDIR)/,$(addsuffix .o,$(CSRCNAMES)))

# Generating depends file depends on the parser step so g++ doesn't
# just error out at a missing tokentypes.h file which can cause
# an incorrect Makefile.depend
-include Makefile.depend

c0c:	$(OBJS)
	$(CXX) -rdynamic -g -o $@ $^

$(OBJDIR)/%.o: %.c
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(OBJDIR)/%.o: %.cc
	$(CXX) $(CXXFLAGS) -c -o $@ $<

tokennames.c:	tokentypes.h
	./tokens.py $< $@ 

lex.yy.c:	c0.l
	flex -o $@ $<

tokentypes.h:	c0.tab.h
	./gen_tokentypes.py c0.tab.h tokentypes.h

c0.tab.h:	c0.y
	bison -t -d $<

# actually built by bison, but we need both rules so make gets dependency correct
c0.tab.c:	c0.tab.h
	@if test -f $@; then :; else \
          rm -f $<; \
          $(MAKE) $(AM_MAKEFLAGS) $<; \
	fi

depend: c0.tab.c lex.yy.c tokentypes.h tokennames.c
	@/bin/rm -f Makefile.depend
	@/bin/rm -f Makefile.temp
	@for i in $(CSRCS); do $(CXX) $(CXXFLAGS) -MM -MT "$(OBJDIR)/$${i%.*}.o" $$i >> Makefile.temp; done;
	@mv Makefile.temp Makefile.depend
	@echo "Depend created"

setup: depend
	mkdir -p ../bin
	mkdir -p $(sort $(dir $(OBJS)))

clean:
	/bin/rm -f c0.tab.[cho] echo.o lex.yy.[co] tokennames.[oc] is.lex.[co] is.tab.[hco] Makefile.depend tokentypes.h
	/bin/rm -rf $(OBJDIR)
	/bin/rm -rf ../bin
