#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/plain/network/$(uuidgen)
content=$(cat <<EOF
category: network; scheme="http://schemas.ogf.org/occi/infrastructure#"; class="kind"
x-occi-attribute: occi.network.vlan=1
x-occi-attribute: occi.network.label: "mylan"
EOF
       )
put 201 ${id} "text/plain" "$content"
