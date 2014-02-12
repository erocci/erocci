#!/bin/bash

. $(dirname $0)/../testenv.sh

for i in {1..10}; do
    idx=$(printf '%02d' $i)
    id=/myresources/json/network/id${idx}
    content=$(cat <<'EOF'
{
    "resources": [
	{
            "kind": "http://schemas.ogf.org/occi/infrastructure#network",
	    "attributes": {
		"occi": {
		    "network": {
			"vlan": "1",
			"label": "mylan${idx}"
		    }
		}
	    }
	}
    ]
}
EOF
	   )
    put 200 ${id} "application/json" "$content"
done

exit 0
