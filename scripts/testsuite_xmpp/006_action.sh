#!/bin/bash

. $(dirname $0)/../testenv.sh

id=$(curl -s -H "accept: text/uri-list" ${occi_srv}/store/compute/ | head -1)
content=$(cat <<EOF
<iq to="${occi_jid} type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" node="${id}" type="action" >
    <action xmlns="http://schemas.ogf.org/occi" 
        term="stop" scheme="http://schemas.ogf.org/occi/infrastructure/compute/action#" >
      <attribute name="method" value="graceful" />
    </action>
  </query>
</iq>
EOF
       )
iq_set result ${id}?action=stop "$content"
