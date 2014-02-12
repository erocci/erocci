#!/bin/bash

. $(dirname $0)/../testenv.sh

source=$(curl -s -H "accept: text/uri-list" ${occi_srv}/compute/ | head -1)
target=$(curl -s -H "accept: text/uri-list" ${occi_srv}/network/ | head -1)

id=/mylinks/json/networkinterfaces/$(uuidgen)
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
	    "target": "${target}",
	    "source": "${source}"
	}
    ]
}
EOF
       )

put 201 ${id} "application/json" "$content"
