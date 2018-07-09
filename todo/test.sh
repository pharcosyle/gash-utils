foo=bar
bar=foo
if [ "${foo}" != "foo" -a "${bar}" != "bar" ]; then echo foobar; fi
