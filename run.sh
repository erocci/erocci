#!/bin/sh

/opt/erocci/bin/erocci -detached -config /tmp/sys.config && \
    tail -F /var/log/erocci/{kernel,sasl}.log
