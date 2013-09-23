#!/bin/sh
cd `dirname $0`

export MNESIA_DIR="$HOME/.occi/"

exec erl -pa $PWD/ebin \
    $PWD/deps/*/ebin \
    -boot start_sasl \
    -config examples/hello_occi.config \
    -s reloader \
    -s hello_occi
