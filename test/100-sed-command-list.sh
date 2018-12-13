input='foo
bar'

echo "$input" | \sed '/foo/ { s/foo/baz/ s/baz/bar/ } s/bar/baz/'
