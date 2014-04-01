#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/compute/
ct=( 'text/plain' 'text/uri-list' 'text/occi' 'application/json' 'application/xml' )

for ct in "${ct[@]}"; do
    get 200 ${id} "${ct}"
done
