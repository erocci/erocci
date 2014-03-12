#!/bin/bash

. $(dirname $0)/../testenv.sh

source=$(curl -s -H "accept: text/uri-list" ${occi_srv}/compute/ | head -1)
target=$(curl -s -H "accept: text/uri-list" ${occi_srv}/network/ | head -1)

id=/store/mylinks/plain/networkinterfaces/$(uuidgen)
content=$(cat <<EOF
category: networkinterface; scheme="http://schemas.ogf.org/occi/infrastructure#"; class="kind"
category: ipnetworkinterface; scheme="http://schemas.ogf.org/occi/infrastructure/networkinterface#"; class="mixin"
x-occi-attribute: occi.networkinterface.interface="eth0"
x-occi-attribute: occi.networkinterface.mac="00:80:41:ae:fd:32"
x-occi-attribute: occi.networkinterface.address="192.168.3.4"
x-occi-attribute: occi.networkinterface.gateway="192.168.3.1"
x-occi-attribute: occi.networkinterface.allocation="dynamic"
x-occi-attribute: occi.core.target="${target}"
x-occi-attribute: occi.core.source="${source}"
EOF
       )

put 201 ${id} "text/plain" "$content"
