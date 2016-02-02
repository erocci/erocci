#!/bin/sh

set -e

v=$1

git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null | \
    sed -e 's/^v//' || \
    echo $v

exit 0
