#!/bin/bash
for f in test/*; do echo $f; ./anguish $f; done
