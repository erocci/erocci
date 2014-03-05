#!/bin/bash

. $(dirname $0)/../testenv.sh

id=$(curl -s -H "accept: text/uri-list" ${occi_srv}/compute/ | head -1)
content=$(cat <<EOF
category: stop; scheme="http://schemas.ogf.org/occi/infrastructure/compute/action#"; class="action"
x-occi-attribute: method="graceful"
EOF
       )
post 204 ${id}?action=stop "text/plain" "$content"
