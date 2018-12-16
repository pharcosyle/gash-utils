set +e
ls /bin/sh /bin/foo > bar 2>&1
echo foo
cat bar
rm bar
