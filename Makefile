# mGtk - Makefile
# (c) Ken Friis Larsen and Henning Niss 1999, 2000.

include src/Makefile.inc

SUBDIRS=src examples

all: 
	@dirs='$(SUBDIRS)'; for dir in $$dirs; do \
	   cd $$dir; \
	   $(MAKE) all; \
           cd ..; \
        done

clean:
	rm -rf *~

realclean: clean
	@dirs='$(SUBDIRS)'; for dir in $$dirs; do \
	   cd $$dir; \
	   $(MAKE) realclean; \
           cd ..; \
        done
