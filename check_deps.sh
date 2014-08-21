#!/bin/bash

DEPS="dbus erim erim_xml erocci_authnz erocci_backend_dbus erocci_core erocci_listener_xmpp erocci_authnz_htpasswd erocci_backend_mnesia erocci_listener_http"

function _status() {
    dirty=$(git -C deps/$1 status -s)
    if [ "x" = "x${dirty}" ]; then
	return 1
    else
	return 0
    fi
}

for dep in ${DEPS}; do
    echo -n "Check status for ${dep}: "
    if _status ${dep}; then
	echo "dirty, please commit."
	exit 1
    else
	if [ "xpush" = "x$1" ]; then
	    echo "pushing..."
	    git -C deps/${dep} push > /dev/null 2>&1
	else
	    echo "ok."
	fi
    fi
done
