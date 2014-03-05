#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
category: plainmixin; scheme="http://schemas.example.org/occi#"; location="/store/usermixins/jsonmixin/"
EOF
       )

post 200 /-/ "text/plain" "$content"
