#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/plain/badcompute/id

content=$(cat <<EOF
category: badterm; scheme="http://schemas.ogf.org/occi/infrastructure#"; class="kind"
EOF
	   )
put 400 ${id} "text/plain" "$content"
