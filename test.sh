for f in test/*; do echo $f; ./anguish -p $f; ./anguish $f; done
