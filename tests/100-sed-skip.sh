input='1
2
3
4
5
6'

echo "$input" | \sed 'n;n;s/./x/'
