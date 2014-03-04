#!/bin/bash

. $(dirname $0)/../testenv.sh

entity=$(curl -s -H "accept: text/uri-list" ${occi_srv}/compute/ | head -1)
content=$(cat <<EOF
x-occi-location: ${entity}
EOF
       )

post 204 /os_tpl/ "text/plain" "$content"
