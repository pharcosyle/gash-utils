basename='/^.*\/\([^/][^/]*\)\/*$/{
	    s//\1/
	    q
	  }
  /^X\/\(\/\/\)$/{
	    s//\1/
	    q
  }
  /^X\/\(\/\).*/{
	    s//\1/
	    q
  }
  s/.*/./; q'

echo 'X/foo/bar' | \sed "$basename"
