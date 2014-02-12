#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<collection xmlns="http://schemas.ogf.org/occi" xmlns:xl="http://www.w3.org/2008/06/xlink" >
  <entity xl:href="http://localhost:8080/myresources/xml/compute/id01" />
</collection>
EOF
       )

post 200 /os_tpl/ "application/xml" "$content"


