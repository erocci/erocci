#!/bin/sh

. $(dirname $0)/../testenv.sh

echo -n "Adding resource to mixin: /myresources/compute/id01... "

(
    cat <<EOF
[ "http://localhost:8080/myresources/json/compute/id01" ]
EOF
) | curl ${curl_opts} -X POST --data @- -H 'content-type: application/json' ${occi_srv}/os_tpl/
echo

exit  0
