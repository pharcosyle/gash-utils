input='bar
baz
bam'

echo "$input" | \sed '/baz/ s/a/i/'
