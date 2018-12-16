foo () {
    echo $1
}

echo before
foo bar
foo baz
echo after
