#!/bin/bash

mkdir -p /var/log/erocci
touch /var/log/erocci/kernel.log
touch /var/log/erocci/sasl.log
cd /erocci && \
    erl -pa apps/erocci/ebin -pa deps/*/ebin \
	-boot start_sasl \
	-config /tmp/sys.config \
	-detached \
        -s erocci && \
    tail -f /var/log/erocci/{kernel,sasl}.log
