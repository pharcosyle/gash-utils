if $SHELL --version | grep foobar 2>/dev/null; then
    exit 1
else
    exit 0
fi
