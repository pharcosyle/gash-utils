sed \
    -e "s,^#! /bin/sh,#! /bin/gash," \
    test/data/diff.scm > $DESTDIR/tmp/diff.scm
cat $DESTDIR/tmp/diff.scm
rm $DESTDIR/tmp/diff.scm
