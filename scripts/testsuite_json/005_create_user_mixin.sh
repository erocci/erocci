#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
{
  "mixins": [
   {
      "term": "jsonmixin",
      "scheme": "http://schemas.example.org/occi#",
      "location": "/usermixins/jsonmixin"
   }
  ]
}
EOF
       )

post 200 /-/ "application/json" "$content"

exit  0
