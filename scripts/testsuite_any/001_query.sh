#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/-/
ct=( 'text/plain', 'text/uri-list' 'text/plain' 'application/json' 'application/xml' )

for ct in "${ct[@]}"; do
    get 200 ${id} "${ct}"
done
