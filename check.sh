if [ -n "$V" ]; then
    set -x
fi
DIFF=diff
SHELL=${SHELL-bin/gash}

tests="
assign
assign2
for
for-split-sequence
find
if2
iohere
list
ls
nesting
pipe
substitution
00-exit
01-exit-0
02-exit-1
03-echo
04-echo-var
05-assignment-doublequoted-doublequotes
05-assignment
06-assignment-echo
06-assignment-singlequote
07-assignment-double-quote
08-assignment-variable-word
09-compound-word
0a-assign-substitute
0b-command-compound-word
10-if
11-if-false
20-pipe-exit-0
21-pipe-exit-1
22-semi-pipe-exit-0
30-assignment-substitution
30-eval
31-eval-echo-variable
32-for-substitute
33-string-args
35-assignment-eval-echo

00-sed
00-sed-once
00-sed-global
00-sed-case
00-sed-group
00-sed-group-extended
00-sed-twice
00-sed-undo
"

broken="
"

if [ "$(basename $SHELL)" = bash ]; then
    broken="
00-sed
"
fi

expect=$(echo $broken | wc -w)
pass=0
fail=0
total=0
for t in $tests; do
    sh test.sh "test/$t" &> test/"$t".log
    r=$?
    total=$((total+1))
    if [ $r = 0 ]; then
        echo $t: [OK]
        pass=$((pass+1))
    else
        echo $t: [FAIL]
        fail=$((fail+1))
    fi
done

[ $expect != 0 ] && echo "expect: $expect"
[ $fail != 0 ] && echo "failed: $fail"
[ $fail -lt $expect ] && echo "solved: $(($expect - $fail))"
echo "passed: $pass"
echo "total:  $total"
if [ $fail != 0 -a $fail -gt $expect ]; then
    echo FAILED: $fail/$total
    exit 1
elif [ $fail != 0 ]; then
    echo PASS: $pass/$total
else
    echo PASS: $total
fi
