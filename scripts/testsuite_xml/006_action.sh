#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<action xmlns="http://schemas.ogf.org/occi" 
    term="stop" scheme="http://schemas.ogf.org/occi/infrastructure/compute/action#" >
  <attribute name="method" value="graceful" />
</action>
EOF
       )
post 204 /myresources/xml/compute/id01?action=stop "application/xml" "$content"

exit  0
