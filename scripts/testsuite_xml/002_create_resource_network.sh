#!/bin/sh

for i in $(seq 1 10); do
    idx=$(printf '%02d' $i)
    id=/myresources/network/id${idx}
    echo -n "Creating resource "${id}"... "

    (
	cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<occi:resource xmlns:occi="http://schemas.ogf.org/occi" >
  <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="network" />
  <occi:attribute name="occi.network.vlan" value="1" />
  <occi:attribute name="occi.network.label" value="mylan${idx}" />
</occi:resource>
EOF
    ) | curl -s -w "%{http_code}\n" -f -X PUT --data @- -H 'content-type: application/xml' -o /dev/null ${occi_srv}${id}
done

exit 0
