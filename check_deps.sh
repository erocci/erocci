#!/bin/bash

function _status() {
    dirty=$(git -C deps/$1 status -s)
    if [ "x" = "x${dirty}" ]; then
	return 1
    else
	return 0
    fi
}

for dep in $(cd deps && ls -d *); do
    if [ -d deps/${dep}/.git ]; then
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
    fi
done
