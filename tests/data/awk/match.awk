$0 ~ /^#!/ && NR == 1 { print "#! be bang" }
$0 !~ /^#/ && NF == 3 { print "!#" }
/^[^#]/ { print }

