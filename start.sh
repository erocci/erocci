#!/bin/sh
cd `dirname $0`

export OCCI_XMPP_JID="node-1@localhost"
export OCCI_XMMP_PASSWD="node"

exec erl -pa $PWD/apps/occi/ebin $PWD/deps/*/ebin \
		-boot start_sasl \
		-s occi
