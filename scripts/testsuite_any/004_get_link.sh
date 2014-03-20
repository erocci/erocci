#!/bin/bash

. $(dirname $0)/../testenv.sh

id=$(curl -s -H "accept: text/uri-list" ${occi_srv}/networkinterface/ | head -1)
ct=( 'text/plain' 'text/plain' 'application/json' 'application/xml' )

for ct in "${ct[@]}"; do
    get 200 ${id} "${ct}"
done
