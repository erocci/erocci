#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
x-occi-location: http://localhost:8080/store/badresoruce
EOF
       )

post 422 /store/os_tpl/ "text/plain" "$content"
