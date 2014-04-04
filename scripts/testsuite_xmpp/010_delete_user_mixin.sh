#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" type="caps" op="delete" node="http://schemas.example.org/occi#xmlmixin" />
</iq>
EOF
       )

iq_set result /-/ "$content"
