.SILENT:
.PRECIOUS: %.res

TRANSLATE=../mgtkgen

all: gtk-adj gtk-fonts gtk-boxed gtk-output gtk-split-words gtk

fresh: clean all

%.c: %.defs
	$(TRANSLATE) -c --no-header -o $@ $< > /dev/null 2>&1 

%.sig: %.defs
	$(TRANSLATE) -sig --no-header -o $@ $< > /dev/null 2>&1 

%.sml: %.defs
	$(TRANSLATE) -sml --no-header -o $@ $< > /dev/null 2>&1 

%.res: %.c %.sig %.sml
	rm -f $@; \
	files='$^'; for file in $$files; do \
	  echo "------------------------------------------------------------" >> $@; \
	  cat $$file >> $@; \
	done

%: %.res
	if diff $@.trusted $<; then \
	  echo "$@: success!"; \
	else \
	  echo "$@: failure."; \
	fi

clean:
	rm -f *~
	rm -f *.res
