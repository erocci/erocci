#!/bin/bash

. $(dirname $0)/../testenv.sh

content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" type="col" node="/os_tpl/" >
    <collection xmlns="http://schemas.ogf.org/occi" xmlns:xl="http://www.w3.org/2008/06/xlink" >
      <entity xl:href="http://localhost:8080/store/unknownresource" />
    </collection>
  </query>
</iq>
EOF
       )

iq_set 'bad-request' /os_tpl/ "$content"
