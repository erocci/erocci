#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
[ "http://localhost:8080/badresoruce" ]
EOF
       )

post 422 /os_tpl/ "application/json" "$content"
