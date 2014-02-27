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

delete 200 /-/ "application/json" "$content"
