#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/mylinks/json/networkinterfaces/$(uuidgen)
content=$(cat <<EOF
{
    "links": [
	{
	    "kind": "http://schemas.ogf.org/occi/infrastructure#networkinterface",
	    "mixins": [
		"http://schemas.ogf.org/occi/infrastructure/networkinterface#ipnetworkinterface"
	    ],
	    "attributes": {
		"occi": {
		    "networkinterface": {
			"interface": "eth0",
			"mac": "00:80:41:ae:fd:32",
			"address": "192.168.3.4",
			"gateway": "192.168.3.0",
			"allocation": "dynamic"
		    }
		}
	    },
	    "target": "bad_value",
	    "source": "bad_value"
	}
    ]
}
EOF
       )

put 400 ${id} "application/json" "$content"
