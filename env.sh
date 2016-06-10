#!/bin/sh

BASEDIR=$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)
DEPS="occi erocci_core erocci_listener_http erocci_backend_mnesia mixer"

CFLAGS=$(for d in ${DEPS}; do echo -n "-I${BASEDIR}/deps/$d/include "; done)
CFLAGS="${CFLAGS} -DTEST"
export CFLAGS

LDFLAGS=$(for d in ${DEPS}; do echo -n "-pa ${BASEDIR}/deps/$d/ebin "; done)
export LDFLAGS
