#!/bin/sh

set -e

(
	cd $(dirname $0) && \
		autoreconf -ivf && \
		$(dirname $0)/configure $@
)

exit 0
