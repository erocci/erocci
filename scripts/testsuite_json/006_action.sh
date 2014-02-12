#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<'EOF'
{
  "action": "http://schemas.ogf.org/occi/infrastructure/compute/action#stop"
  "attributes" : {
     "method": "graceful"
  }
}
EOF
       )
post 204 /myresources/xml/compute/id01?action=stop "application/json" "$content"

exit  0
