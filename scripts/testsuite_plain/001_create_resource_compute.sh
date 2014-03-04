#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/plain/compute/$(uuidgen)

content=$(cat <<EOF
category: compute; scheme="http://schemas.ogf.org/occi/infrastructure#"; class="kind"
x-occi-attribute: occi.core.title="Machine"
x-occi-attribute: occi.compute.architecture="x86"
x-occi-attribute: occi.compute.cores=1
x-occi-attribute: occi.compute.hostname="pc"
x-occi-attribute: occi.compute.memory=5
x-occi-attribute: occi.compute.speed=4000
EOF
	   )
put 201 ${id} "text/plain" "$content"
