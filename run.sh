#!/bin/bash

mkdir -p /var/log/erocci
touch /var/log/erocci/kernel.log
touch /var/log/erocci/sasl.log
/opt/erocci/bin/erocci -detached -config /tmp/sys.config && \
    tail -f /var/log/erocci/{kernel,sasl}.log
