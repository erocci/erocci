#!/bin/sh

. $(dirname $0)/../testenv.sh

echo -n "Creates user mixin... "

(
    cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<mixin xmlns="http://schemas.ogf.org/occi" 
    term="xmlmixin" scheme="http://schemas.example.org/occi#" 
    location="/usermixins/xmlmixin" />
EOF
) | curl ${curl_opts} -X POST --data @- -H 'content-type: application/xml' ${occi_srv}/-/
echo

exit  0
