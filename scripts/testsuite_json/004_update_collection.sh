#!/bin/bash

. $(dirname $0)/../testenv.sh

entity=$(curl -s -H "accept: text/uri-list" ${occi_srv}/compute/ | head -1)
content=$(cat <<EOF
[ "${entity}" ]
EOF
       )

post 204 /os_tpl/ "application/json" "$content"

exit  0
