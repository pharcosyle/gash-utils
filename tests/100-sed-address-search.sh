input='bar
baz
bam'

echo "$input" | \sed '1,/baz/ s/a/i/'
