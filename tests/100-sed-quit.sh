input='foo
bar'

echo "$input" | \sed 's/foo/baz/ ; q ; s/baz/foo/'
