#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/myresources/json/compute/$(uuidgen)

content=$(cat <<EOF
{
    "resources": [
	{
            "kind": "http://schemas.ogf.org/occi/infrastructure#compute",
	    "attributes": {
		"occi": {
		    "core": {
			"title": "Machine"
		    },
		    "compute": {
			"architecture": "x86",
			"cores": 1,
			"hostname": "pc",
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
put 201 ${id} "application/json" "$content"
