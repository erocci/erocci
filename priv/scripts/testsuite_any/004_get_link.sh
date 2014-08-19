#!/bin/bash

. $(dirname $0)/../testenv.sh

id=$(curl -s -H "accept: text/uri-list" ${occi_srv}/collections/networkinterface/ | head -1)
ct=( 'text/plain' 'text/occi' 'application/json' 'application/xml' )

for ct in "${ct[@]}"; do
    get 200 ${id} "${ct}" || exit 1
done
