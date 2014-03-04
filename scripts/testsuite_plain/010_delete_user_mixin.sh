#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
category: jsonmixin; scheme="http://schemas.example.org/occi#"; class="mixin"; location="/store/usermixins/jsonmixin/"
EOF
       )

delete 200 /-/ "text/plain" "$content"
