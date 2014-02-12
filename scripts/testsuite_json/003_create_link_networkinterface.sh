#!/bin/bash

. $(dirname $0)/../testenv.sh

for i in {1..10}; do
    idx=$(printf '%02d' $i)
    id=/mylinks/json/networkinterfaces/id${idx}
    content=$(cat <<'EOF'
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
			"mac": "00:80:41:ae:fd:${idx}",
			"address": "192.168.3.4{idx}",
			"gateway": "192.168.3.0",
			"allocation": "dynamic"
		    }
		}
	    },
	    "target": "http://localhost:8080/myresources/json/network/id01",
	    "source": "http://localhost:8080/myresources/json/compute/id${idx}"
	}
    ]
}
EOF
	   )

    put 200 ${id} "application/json" "$content"
done

exit 0
