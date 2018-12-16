set -e
if [ -n "$V" ]; then
    set -x
fi
DIFF=${DIFF-diff}
SHELL=${SHELL-scripts/gash}

t="$1"
b=tests/$(basename "$t" .sh)
set +e
timeout 1 $SHELL -e "$b".sh -s --long file0 file1 > "$b".1 2> "$b".2
r=$?
set -e
if [ -f "$b".exit ]; then
    e=$(cat "$b".exit)
else
    e=0
fi
[ $r = $e ] || exit 1
if [ -f "$b".stdout ]; then
        $DIFF -u "$b".stdout $b.1
fi
if [ -f "$b".stderr ]; then
    $DIFF -u "$b".stderr "$b".2
fi
