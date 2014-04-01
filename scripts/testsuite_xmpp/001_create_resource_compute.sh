#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/xmpp/compute/$(uuidgen)

content=$(cat <<EOF
<iq to="${occi_jid}" type="set" >
  <query xmlns="http://schemas.ogf.org/occi-xmpp" node="${id}" >
    <occi:resource xmlns:occi="http://schemas.ogf.org/occi" title="Machine a toto" >
      <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="compute" />
      <occi:attribute name="occi.compute.architecture" value="x86" />
      <occi:attribute name="occi.compute.cores" value="2" />
      <occi:attribute name="occi.compute.hostname" value="pc_toto" />
      <occi:attribute name="occi.compute.memory" value="5" />
      <occi:attribute name="occi.compute.speed" value="4000" />
    </occi:resource>
  </query>
</iq>
EOF
       )
iq_set result ${id} "$content"
