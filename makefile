all: bin/gash

GUILE = $(shell $(SHELL) -c "command -v guile")

bin/gash: bin/gash.in
	sed s,@GUILE@,$(GUILE), $< > $@
	chmod +x $@

clean:
	rm -f bin/gash
