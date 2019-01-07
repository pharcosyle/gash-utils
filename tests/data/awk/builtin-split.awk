BEGIN {
    line = "foo @bar@ baz"
    nfields = split(line, field, "@")
    for (i = 1; i < nfields; i++)
        print i ":" field[i]
}
