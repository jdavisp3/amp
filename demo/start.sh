#!/bin/sh

exec \
  erl \
    -pa ../demo/ebin deps/*/ebin \
    -s demo
