#!/bin/sh

. $(dirname $0)/../testenv.sh

echo -n "Creates user mixin... "

(
    cat <<EOF
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
) | curl ${curl_opts} -X POST --data @- -H 'content-type: application/json' ${occi_srv}/-/
echo

exit  0
