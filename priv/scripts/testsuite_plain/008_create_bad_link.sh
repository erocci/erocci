#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/mylinks/plain/networkinterfaces/$(uuidgen)
content=$(cat <<EOF
category: networkinterface; scheme="http://schemas.ogf.org/occi/infrastructure#"; class="kind"
category: ipnetworkinterface; scheme="http://schemas.ogf.org/occi/infrastructure/networkinterface#"; class="mixin"
x-occi-attribute: occi.network.interface="eth0"
x-occi-attribute: occi.network.mac="00:80:41:ae:fd:32"
x-occi-attribute: occi.network.address="192.168.3.4"
x-occi-attribute: occi.network.gateway="192.168.3.1"
x-occi-attribute: occi.network.allocation="dynamic"
x-occi-attribute: occi.core.target="bad_value"
x-occi-attribute: occi.core.source="bad_value"
EOF
       )

put 400 ${id} "text/plain" "$content"
