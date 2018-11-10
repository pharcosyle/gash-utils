if [ -n "$V" ]; then
    set -x
fi
DIFF=diff
SHELL=${SHELL-bin/gash}

tests="
00-exit
00-exit-0
00-exit-1

01-script-newline
01-script-semi
01-script-backslash
01-script-backslash-space
01-script-backslash-twice

03-echo
03-echo-doublequotes
03-echo-nesting
03-echo-escaped-doublequotes
03-echo-quoted-doublequotes

04-echo-var
04-echo-equal

05-assignment
05-assignment-echo
05-assignment-empty
05-assignment-singlequote
05-assignment-double-quote
05-assignment-variable-word
05-assignment-word-variable
05-assignment-doublequoted-doublequotes

06-compound-word
06-command-compound-word

07-variable-or
07-variable-not-or

08-variable-and
08-variable-not-and

10-if
10-if-false
10-if-word-variable
11-for
11-for-split-sequence

20-semi.sh
20-or.sh
20-and.sh
20-pipe-exit-0
20-pipe-exit-1
20-pipe-sed

30-substitution
30-substitution-backtick
30-substitution-assignment
30-for-substitution

40-eval
40-eval-echo-variable
40-assignment-eval-echo

41-dot.sh

50-iohere

60-function
60-subst

70-hash
70-hash-hash
70-percent
70-percent-percent
70-percent-space
70-slash
70-slash-string
70-slash-string-slash

100-sed
100-sed-once
100-sed-global
100-sed-case
100-sed-group
100-sed-group-extended
100-sed-twice
100-sed-undo
100-sed-file

100-tar
100-tar-Z
100-tar-Z-old
100-tar-Z-pipe
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
