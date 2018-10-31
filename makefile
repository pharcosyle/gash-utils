.PHONY: all all-go check clean install
-include .config.make

default: all

.config.make: makefile

bin/gash: bin/gash.in
	$(MAKE) do-configure

gash/config.scm:
	$(MAKE) do-configure

do-configure:
	./configure --prefix=$(prefix)

all: all-go

all-go: | gash/config.scm
	build-aux/build-guile.sh

clean:
	git clean -fdx

clean-go:
	rm -f $(shell find . -name '*.go')

check: all check-bash check-gash

check-bash: all
ifneq ($(BASH),)
	PATH=$(PATH):bin SHELL=$(BASH) ./check.sh
endif

check-gash: all
	SHELL=bin/gash ./check.sh

check-geesh: all
	SHELL='bin/gash --geesh' ./check.sh

install: all
	mkdir -p $(DESTDIR)$(bindir)
	cp bin/gash $(DESTDIR)$(bindir)/gash
	mkdir -p $(DESTDIR)$(guile_site_dir)
	tar -cf- gash/*.scm | tar -C $(DESTDIR)$(guile_site_dir) -xf-
	mkdir -p $(DESTDIR)$(guile_site_ccache_dir)
	cp bin/gash.go  $(DESTDIR)$(guile_site_ccache_dir)
	tar -cf- gash/*.go | tar -C $(DESTDIR)$(guile_site_ccache_dir) -xf-
	mkdir -p $(DESTDIR)$(docdir)
	cp -f COPYING README TODO $(docdir)
	$(MAKE) install-info

install-info: info
	mkdir -p $(DESTDIR)$(prefix)/share/info
	tar -cf- doc/gash.info* | tar -xf- --strip-components=1 -C $(DESTDIR)$(prefix)/share/info
	install-info --info-dir=$(DESTDIR)$(prefix)/share/info doc/gash.info

doc/version.texi: doc/gash.texi makefile
	(set `LANG= date -r $< +'%d %B %Y'`;\
	echo "@set UPDATED $$1 $$2 $$3"; \
	echo "@set UPDATED-MONTH $$2 $$3"; \
	echo "@set EDITION $(VERSION)"; \
	echo "@set VERSION $(VERSION)") > $@

info: doc/gash.info

doc/gash.info: doc/gash.texi doc/version.texi makefile
	$(MAKEINFO) -o $@ -I doc $<

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
export guile_load_path
export guile_load_compiled_path


