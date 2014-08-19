#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/xmpp/network/$(uuidgen)

content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" node="${id}" >
    <occi:resource xmlns:occi="http://schemas.ogf.org/occi" >
      <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="network" />
      <occi:attribute name="occi.network.vlan" value="1" />
      <occi:attribute name="occi.network.label" value="mylan" />
    </occi:resource>
  </query>
</iq>
EOF
	   )
iq_set result ${id} "$content"
