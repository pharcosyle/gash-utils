input='bar
baz
bam'

echo "$input" | \sed  -e '/baz/,$ s/a/i/'
