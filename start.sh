#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/apps/occi/ebin $PWD/deps/*/ebin \
		-boot start_sasl \
		-s occi
