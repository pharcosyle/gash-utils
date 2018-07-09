.PHONY: all all-go check clean install
-include .config.make

default: all

.config.make: makefile
	./configure --prefix=$(PREFIX)

bin/gash: bin/gash.in
	./configure --prefix=$(PREFIX)

all: all-go bin/gash

all-go:
	build-aux/build-guile.sh

clean:
	git clean -fdx

clean-go:
	rm -f $(shell find . -name '*.go')

check: all check-bash check-gash

check-bash: all
ifneq ($(BASH),)
	SHELL=$(BASH) ./test.sh
endif

check-gash: all
	SHELL=bin/gash ./test.sh

install: all
	mkdir -p $(DESTDIR)$(BINDIR)
	cp bin/gash $(DESTDIR)$(BINDIR)/gash
	mkdir -p $(DESTDIR)$(GUILE_SITE_DIR)
	tar -cf- gash/*.scm | tar -C $(DESTDIR)$(GUILE_SITE_DIR) -xf-
	mkdir -p $(DESTDIR)$(GUILE_SITE_CCACHE_DIR)
	cp bin/gash.go  $(DESTDIR)$(GUILE_SITE_CCACHE_DIR)
	tar -cf- gash/*.go | tar -C $(DESTDIR)$(GUILE_SITE_CCACHE_DIR) -xf-
	mkdir -p $(DESTDIR)$(DOCDIR)
	cp -f COPYING README TODO $(DOCDIR)

define HELP_TOP
Usage: make [OPTION]... [TARGET]...

Targets:
  all             update everything
  all-go          update .go files
  check           run ./test.sh
  clean           run git clean -dfx
  clean-go        clean .go files
  install         install in $(PREFIX)
endef
export HELP_TOP
help:
	@echo "$$HELP_TOP"

export BUILD_DEBUG
export GUILE
export GUILE_TOOLS
export GUILE_LOAD_PATH
export GUILE_LOAD_COMPILED_PATH


