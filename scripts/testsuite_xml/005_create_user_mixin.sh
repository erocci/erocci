#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<mixin xmlns="http://schemas.ogf.org/occi" 
    term="xmlmixin" scheme="http://schemas.example.org/occi#" 
    location="/usermixins/xmlmixin" />
EOF
       )

post 200 /-/ "application/xml" "$content"

exit  0
