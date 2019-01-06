echo -e "foo\nbar" | awk -f tests/data/awk/array.awk | LANG=C LC_ALL=C sort
