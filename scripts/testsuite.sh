#!/bin/bash -e

basedir=$(dirname $0)

export occi_srv=http://localhost:8080

for script in $basedir/testsuite/*.sh; do
    echo "### "$(basename $script)
    [ -x $script ] && $script
done
