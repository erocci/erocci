#!/bin/sh

set -e

autoreconf
$(dirname $0)/configure $@
