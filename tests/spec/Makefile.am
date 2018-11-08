oil-link: oil.scm
	if test -L oil-link; then unlink oil-link; fi
	guix build --root=oil-link --file=oil.scm

oil: oil-link
	rm -rf oil
	cp -R -H oil-link "$@"
	chmod -R u+w "$@"

oil/bin/geesh: oil
	echo "#!/bin/sh" > "$@"
	echo "$(abs_top_builddir)/pre-inst-env geesh" >> "$@"
	chmod a+x "$@"

check: oil/bin/geesh ;

TEST_EXTENSIONS = .sh
SH_LOG_COMPILER = ./check-spec

TESTS =                                         \
    oil/spec/smoke.test.sh

clean-local:
	rm -rf oil
	if test -L oil-link; then unlink oil-link; fi
