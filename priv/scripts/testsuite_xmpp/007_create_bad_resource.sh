#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/xml/badcompute/id

content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" node="${id}" >
    <occi:resource xmlns:occi="http://schemas.ogf.org/occi" title="Machine a toto" >
      <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="badterm" />
    </occi:resource>
  </query>
</iq>
EOF
       )
iq_set 'bad-request' ${id} "$content"


