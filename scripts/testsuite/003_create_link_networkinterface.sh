#!/bin/sh

for i in $(seq 1 10); do
    idx=$(printf '%02d' $i)
    id=/mylinks/networkinterfaces/id${idx}
    echo -n "Creating link "${id}"... "

    (
	cat <<EOF
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
	    "target": "http://localhost:8080/myresources/network/id01",
	    "source": "http://localhost:8080/myresources/compute/id${idx}"
	}
    ]
}
EOF
    ) | curl -s -f -X PUT --data @- -H 'content-type: application/json' --data @- ${occi_srv}${id}

    if [ $? = 0 ]; then
	echo OK
    else
	echo FAIL
    fi
done
