#!/bin/sh

for i in $(seq 1 10); do
    idx=$(printf '%02d' $i)
    id=/myresources/network/id${idx}
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
    ) | curl -s -f -X PUT --data @- -H 'content-type: application/json' --data @- ${occi_srv}${id} > /dev/null 2>&1

    if [ $? = 0 ]; then
	echo OK
    else
	echo FAIL
    fi    
done
