BEGIN {
    array["foo"]="0"
    array["bar"]="1"
    array["baz"]=""

    if ("foo" in array)
        print "have foo"

    for (key in array)
        print key " => " array[key]
}
