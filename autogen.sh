#!/bin/sh

set -e

autoreconf -ivf
$(dirname $0)/configure $@
