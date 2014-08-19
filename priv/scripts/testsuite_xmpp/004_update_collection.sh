#!/bin/bash

. $(dirname $0)/../testenv.sh

entity=$(curl -s -H "accept: text/uri-list" ${occi_srv}/store/compute/ | head -1)
content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" node="http://schemas.ogf.org/occi/infrastructure#os_tpl" type="col" >
    <collection xmlns="http://schemas.ogf.org/occi" xmlns:xl="http://www.w3.org/2008/06/xlink" >
      <entity xl:href="${entity}" />
    </collection>
  </query>
</iq>
EOF
       )

iq_set result /store/os_tpl/ "$content"
