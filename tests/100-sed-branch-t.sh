seq 3 | sed -e '
2 s/\(.*\)/\1/
: repeat
a foo
t repeat'
