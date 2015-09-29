#!/bin/bash

. $(dirname $0)/../testenv.sh

source=$(curl -s -H "accept: text/uri-list" ${occi_srv}/store/compute/ | head -1)
target=$(curl -s -H "accept: text/uri-list" ${occi_srv}/store/network/ | head -1)

id=/store/mylinks/xmpp/networkinterfaces/id$(uuidgen)
content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" node="${id}" >
    <link xmlns="http://schemas.ogf.org/occi" 
          xmlns:xl="http://www.w3.org/2008/06/xlink" >
      <kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="networkinterface" />
      <mixin scheme="http://schemas.ogf.org/occi/infrastructure/networkinterface#" term="ipnetworkinterface" />
      <attribute name="occi.networkinterface.interface" value="eth0" />
      <attribute name="occi.networkinterface.mac" value="00:80:41:ae:fd:32" />
      <attribute name="occi.networkinterface.address" value="192.168.3.4" />
      <attribute name="occi.networkinterface.gateway" value="192.168.3.0" />
      <attribute name="occi.networkinterface.allocation" value="dynamic" />
      <attribute name="occi.core.target" xl:href="${target}" />
      <attribute name="occi.core.source" xl:href="${source}" />
    </link>
  </query>
</iq>
EOF
	   )

iq_set result ${id} "$content"


