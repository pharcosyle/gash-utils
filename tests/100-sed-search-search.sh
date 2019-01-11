input='bar
baz
bla
bam'

echo "$input" | \sed '/baz/,/bla/ s/a/i/'
