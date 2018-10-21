if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi
#SHELL=${SHELL-bin/gash}
SHELL=bin/gash
for f in test/*.sh; do
    echo -n "$f: "
    b=test/$(basename $f .sh)
#    $SHELL --geesh -e $f
    $SHELL -e $f
    r=$?
    if [ -f $b.exit ]; then
        e=$(cat $b.exit)
    else
        e=0
    fi
    [ $r = $e ] || exit 1
    echo pass
done
