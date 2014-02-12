#!/bin/bash

. $(dirname $0)/../testenv.sh

for i in {1..10}; do
    idx=$(printf '%02d' $i)
    id=/myresources/json/compute/id${idx}

    content=$(cat <<'EOF'
{
    "resources": [
	{
            "kind": "http://schemas.ogf.org/occi/infrastructure#compute",
	    "attributes": {
		"occi": {
		    "core": {
			"title": "Machine${idx}"
		    },
		    "compute": {
			"architecture": "x86",
			"cores": 1,
			"hostname": "pc${idx}",
			"memory": 5,
			"speed": 4000
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
