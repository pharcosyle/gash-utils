seq 3 | sed -e '
b skip
s/./0/
: skip'
