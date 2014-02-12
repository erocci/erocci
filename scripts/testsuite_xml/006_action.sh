#!/bin/bash

. $(dirname $0)/../testenv.sh

id=$(curl -s -H "accept: text/uri-list" ${occi_srv}/compute/ | head -1)
content=$(cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<action xmlns="http://schemas.ogf.org/occi" 
    term="stop" scheme="http://schemas.ogf.org/occi/infrastructure/compute/action#" >
  <attribute name="method" value="graceful" />
</action>
EOF
       )
post 204 ${id}?action=stop "application/xml" "$content"

exit  0
