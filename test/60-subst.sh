subst () {
    sed \
        -e s",foo,bar,"\
        $1 > $2
}

subst tests/data/foo foo.tmp
cat foo.tmp
rm foo.tmp
