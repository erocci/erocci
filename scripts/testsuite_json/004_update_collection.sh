#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<'EOF'
[ "http://localhost:8080/myresources/json/compute/id01" ]
EOF
       )

post 200 /os_tpl/ "application/json" "$content"

exit  0
