#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
category: plainmixin; scheme="http://schemas.example.org/occi#"; class="mixin"; location="/store/usermixins/plainmixin/"
EOF
       )

delete 200 /-/ "text/plain" "$content"
