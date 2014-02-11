#!/bin/sh

. $(dirname $0)/../testenv.sh

for i in $(seq 1 10); do
    idx=$(printf '%02d' $i)
    id=/myresources/json/network/id${idx}
    echo -n "Creating resource "${id}"... "

    (
	cat <<EOF
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
    ) | curl ${curl_opts} -X PUT --data @- -H 'content-type: application/json' ${occi_srv}${id}
    echo
done

exit 0
