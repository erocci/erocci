#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/mylinks/xml/networkinterfaces/id$(uuidgen)
content=$(cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<link xmlns="http://schemas.ogf.org/occi" 
      xmlns:xl="http://www.w3.org/2008/06/xlink" >
  <kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="networkinterface" />
  <mixin scheme="http://schemas.ogf.org/occi/infrastructure/networkinterface#" term="ipnetworkinterface" />
  <attribute name="occi.networkinterface.interface" value="eth0" />
  <attribute name="occi.networkinterface.mac" value="00:80:41:ae:fd:32" />
  <attribute name="occi.networkinterface.address" value="192.168.3.4" />
  <attribute name="occi.networkinterface.gateway" value="192.168.3.0" />
  <attribute name="occi.networkinterface.allocation" value="dynamic" />
  <attribute name="occi.core.target" xl:href="bad_target" />
  <attribute name="occi.core.source" xl:href="bad_source" />
</link>
EOF
	   )

put 422 ${id} "application/xml" "$content"

exit  0
