#!/bin/bash

. $(dirname $0)/../testenv.sh

entity=$(curl -s -H "accept: text/uri-list" ${occi_srv}/collections/compute/ | head -1)
content=$(cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<collection xmlns="http://schemas.ogf.org/occi" xmlns:xl="http://www.w3.org/2008/06/xlink" >
  <entity xl:href="${entity}" />
</collection>
EOF
       )

post 204 /collections/os_tpl/ "application/xml" "$content"
