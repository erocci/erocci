#!/bin/bash

. $(dirname $0)/../testenv.sh

target=$(curl -s -H "accept: text/uri-list" ${occi_srv}/collections/network/ | head -1)

id=/store/myresources/xml/compute/$(uuidgen)
linkid=$(uuidgen)
content=$(cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<occi:resource xmlns:occi="http://schemas.ogf.org/occi" xmlns:xl="http://www.w3.org/2008/06/xlink"
      title="Machine a toto" >
  <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="compute" />
  <occi:link id="/store/mylinks/json/networkinterface/${linkid}" >
    <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="networkinterface" />
    <occi:mixin scheme="http://schemas.ogf.org/occi/infrastructure/networkinterface#" term="ipnetworkinterface" />
    <occi:attribute name="occi.networkinterface.interface" value="eth0" />
    <occi:attribute name="occi.networkinterface.mac" value="00:80:41:ae:fd:32" />
    <occi:attribute name="occi.networkinterface.address" value="192.168.3.4" />
    <occi:attribute name="occi.networkinterface.gateway" value="192.168.3.0" />
    <occi:attribute name="occi.networkinterface.allocation" value="dynamic" />
    <occi:attribute name="occi.core.target" xl:href="${target}" />
  </occi:link>
  <occi:attribute name="occi.compute.architecture" value="x86" />
  <occi:attribute name="occi.compute.cores" value="2" />
  <occi:attribute name="occi.compute.hostname" value="pc_toto" />
  <occi:attribute name="occi.compute.memory" value="5" />
  <occi:attribute name="occi.compute.speed" value="4000" />
</occi:resource>
EOF
       )

put 201 ${id} "application/xml" "$content"
