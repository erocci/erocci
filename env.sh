#!/bin/sh

BASEDIR=$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)
DEPS="occi erocci_core erocci_listener_http erocci_backend_mnesia mixer dbus erocci_backend_dbus"

FLYCHECK_CFLAGS=$(for d in ${DEPS}; do echo -n "-I${BASEDIR}/deps/$d/include "; done)
FLYCHECK_CFLAGS="${FLYCHECK_CFLAGS} -DTEST"
export FLYCHECK_CFLAGS

FLYCHECK_LDFLAGS=$(for d in ${DEPS}; do echo -n "-pa ${BASEDIR}/deps/$d/ebin "; done)
export FLYCHECK_LDFLAGS
