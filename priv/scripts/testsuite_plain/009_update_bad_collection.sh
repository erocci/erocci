#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
x-occi-location: http://localhost:8080/store/badresoruce
EOF
       )

post 400 /collections/os_tpl/ "text/plain" "$content"
