#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/myresources/xml/network/$(uuidgen)

content=$(cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<occi:resource xmlns:occi="http://schemas.ogf.org/occi" >
  <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="network" />
  <occi:attribute name="occi.network.vlan" value="1" />
  <occi:attribute name="occi.network.label" value="mylan" />
</occi:resource>
EOF
	   )
put 201 ${id} "application/xml" "$content"


