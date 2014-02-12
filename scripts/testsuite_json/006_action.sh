#!/bin/bash

. $(dirname $0)/../testenv.sh

id=$(curl -s -H "accept: text/uri-list" ${occi_srv}/compute/ | head -1)
content=$(cat <<EOF
{
  "action": "http://schemas.ogf.org/occi/infrastructure/compute/action#stop"
  "attributes" : {
     "method": "graceful"
  }
}
EOF
       )
post 204 ${id}?action=stop "application/json" "$content"
