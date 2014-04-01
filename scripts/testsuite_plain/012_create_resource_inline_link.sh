#!/bin/bash

. $(dirname $0)/../testenv.sh

target=$(curl -s -H "accept: text/uri-list" ${occi_srv}/store/network/ | head -1)

id=/store/myresources/plain/compute/$(uuidgen)
linkid=$(uuidgen)
content=$(cat <<EOF
category: compute; scheme="http://schemas.ogf.org/occi/infrastructure#"; class="kind"
x-occi-attribute: occi.core.title="Machine super"
x-occi-attribute: occi.compute.architecture="x86_64"
x-occi-attribute: occi.compute.cores=12
x-occi-attribute: occi.compute.hostname="pc_super"
x-occi-attribute: occi.compute.memory=64
x-occi-attribute: occi.compute.speed=4000
link: <${target}>; rel="http://schemas.ogf.org/occi/infrastructure#network"; category="http://schemas.ogf.org/occi/infrastructure#networkinterface"; category="http://schemas.ogf.org/occi/infrastructure/networkinterface#ipnetworkinterface";self="/store/mylinks/plain/networkinterface/${linkid}"; occi.networkinterface.interface="eth0"; occi.networkinterface.mac="00:80:41:ff:ff:32"; occi.networkinterface.address="192.168.100.4"; occi.networkinterface.gateway="192.168.100.1"
EOF
       )

put 201 ${id} "text/plain" "$content"
