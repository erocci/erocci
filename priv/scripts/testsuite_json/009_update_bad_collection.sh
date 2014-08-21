#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
[ "http://localhost:8080/store/badresoruce" ]
EOF
       )

post 400 /collections/os_tpl/ "application/json" "$content"
