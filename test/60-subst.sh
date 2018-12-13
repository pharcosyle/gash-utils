subst () {
    sed \
        -e s",foo,bar,"\
        $1 > $2
}

subst test/data/foo foo.tmp
cat foo.tmp
rm foo.tmp
