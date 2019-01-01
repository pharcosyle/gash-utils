input='1 bar
2 bar
3 bar
4 bar
5 bar'

echo "$input" | \sed '2,4s/a/x/'
