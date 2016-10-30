#!/bin/bash
set -e
if [ "`basename ${PWD}`" == "tests" ]; then
  cd ..
fi
(cd tests && npm install elm-test)
tests/node_modules/.bin/elm-test
