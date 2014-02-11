#!/bin/sh

. $(dirname $0)/../testenv.sh

for i in $(seq 1 10); do
    idx=$(printf '%02d' $i)
    id=/mylinks/networkinterfaces/id${idx}
    echo -n "Creating link "${id}"... "

    (
	cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<occi:link xmlns:occi="http://schemas.ogf.org/occi" >
  <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="networkinterface" />
  <occi:mixin scheme="http://schemas.ogf.org/occi/infrastructure/networkinterface#" term="ipnetworkinterface" />
  <occi:attribute name="occi.networkinterface.interface" value="eth0" />
  <occi:attribute name="occi.networkinterface.mac" value="00:80:41:ae:fd:${idx}" />
  <occi:attribute name="occi.networkinterface.address" value="192.168.3.4{idx}" />
  <occi:attribute name="occi.networkinterface.gateway" value="192.168.3.0" />
  <occi:attribute name="occi.networkinterface.allocation" value="dynamic" />
  <occi:target href="http://localhost:8080/myresources/network/id01" />
  <occi:source href="http://localhost:8080/myresources/network/id01" />
</occi:link>
EOF
    ) | curl ${curl_opts} -X PUT --data @- -H 'content-type: application/xml' ${occi_srv}${id}
    echo
done

exit  0
