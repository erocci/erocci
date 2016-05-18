#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
{
  "mixins": [
   {
      "term": "jsonmixin",
      "scheme": "http://schemas.example.org/occi#",
      "location": "/store/usermixins/jsonmixin/"
   }
  ]
}
EOF
       )

post 303 /-/ "application/json" "$content"
