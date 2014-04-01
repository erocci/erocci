#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" 
         type="caps" />
    <mixin xmlns="http://schemas.ogf.org/occi" 
        term="xmlmixin" scheme="http://schemas.example.org/occi#" 
        location="/store/usermixins/xmlmixin/" />
  </query>
</iq>
EOF
       )

iq_set result /-/ "$content"


