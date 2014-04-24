#!/bin/bash

. $(dirname $0)/../testenv.sh

target=$(curl -s -H "accept: text/uri-list" ${occi_srv}/collections/network/ | head -1)

id=/store/myresources/json/compute/$(uuidgen)
linkid=$(uuidgen)
content=$(cat <<EOF
{
    "resources": [
	{
            "kind": "http://schemas.ogf.org/occi/infrastructure#compute",
            "links": [
                  { "kind": "http://schemas.ogf.org/occi/infrastructure#networkinterface",
                    "mixins": [ "http://schemas.ogf.org/occi/infrastructure/networkinterface#ipnetworkinterface" ],
                    "attributes" : {
                        "occi" : {
                            "networkinterface" : {
                                 "address" : "192.168.100.4",
                                 "allocation" : "dynamic",
                                 "gateway" : "192.168.3.0",
                                 "interface" : "eth0",
                                 "mac" : "00:80:41:ff:ff:32"
                            }
                        }
                    },
                    "target": "$target",
                    "self": "/store/mylinks/json/networkinterface/${linkid}"
                 }
            ],
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
