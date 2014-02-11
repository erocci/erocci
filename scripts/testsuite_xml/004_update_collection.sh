#!/bin/sh

. $(dirname $0)/../testenv.sh

echo -n "Adding resource to mixin: /myresources/compute/id01... "

(
    cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<collection xmlns="http://schemas.ogf.org/occi" xmlns:xl="http://www.w3.org/2008/06/xlink" >
  <entity xl:href="http://localhost:8080/myresources/compute/id01" />
</collection>
EOF
) | curl ${curl_opts} -X POST --data @- -H 'content-type: application/xml' ${occi_srv}/os_tpl/
echo

exit  0
