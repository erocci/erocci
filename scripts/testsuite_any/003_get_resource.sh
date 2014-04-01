#!/bin/bash

. $(dirname $0)/../testenv.sh

id=$(curl -s -H "accept: text/uri-list" ${occi_srv}/store/compute/ | head -1)
ct=( 'text/plain' 'text/occi' 'application/json' 'application/xml' )

for ct in "${ct[@]}"; do
    get 200 ${id} "${ct}"
done
