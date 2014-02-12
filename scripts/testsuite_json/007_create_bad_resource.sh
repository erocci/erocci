#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/myresources/json/badcompute/id

content=$(cat <<'EOF'
{
    "resources": [
	{
            "kind": "http://schemas.ogf.org/occi/infrastructure#badterm",
	    "attributes": {
	    }
	}
    ]
}
EOF
	   )
put 400 ${id} "application/json" "$content"

exit 0
