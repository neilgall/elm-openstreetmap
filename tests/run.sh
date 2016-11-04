#!/bin/bash
set -e
if [ "`basename ${PWD}`" == "tests" ]; then
  cd ..
fi
test -d tests/node_modules/elm-test || (cd tests && npm install elm-test)
tests/node_modules/.bin/elm-test
