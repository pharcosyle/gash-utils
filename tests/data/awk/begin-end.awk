{ print foo }
BEGIN { print "begin"; foo = "bar" }
END { print "end" }
BEGIN { print "again"; foo = "baz" }
