#!/bin/bash

. $(dirname $0)/../testenv.sh

for i in {1..10}; do
    idx=$(printf '%02d' $i)
    id=/myresources/xml/network/id${idx}

    content=$(cat <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<occi:resource xmlns:occi="http://schemas.ogf.org/occi" >
  <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="network" />
  <occi:attribute name="occi.network.vlan" value="1" />
  <occi:attribute name="occi.network.label" value="mylan${idx}" />
</occi:resource>
EOF
	   )
    put 200 ${id} "application/xml" "$content"
done

exit 0
