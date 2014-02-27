#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/json/network/$(uuidgen)
content=$(cat <<EOF
{
    "resources": [
	{
            "kind": "http://schemas.ogf.org/occi/infrastructure#network",
	    "attributes": {
		"occi": {
		    "network": {
			"vlan": "1",
			"label": "mylan"
		    }
		}
	    }
	}
    ]
}
EOF
       )
put 201 ${id} "application/json" "$content"
