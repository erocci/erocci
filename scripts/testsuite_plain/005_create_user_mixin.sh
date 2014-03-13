#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
category: plainmixin; scheme="http://schemas.example.org/occi#"; location="/store/usermixins/plainmixin/"
EOF
       )

post 200 /-/ "text/plain" "$content"
