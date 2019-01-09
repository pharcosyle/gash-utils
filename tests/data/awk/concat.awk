BEGIN {
    line = "foo"
    value = "bar"
    #line = substr(line, 1, len) "" value "" substr(line, len + keylen + 3)
    line = substr(line, 1, len) "" value "" substr(line, len + keylen + 3)
    print line
}
