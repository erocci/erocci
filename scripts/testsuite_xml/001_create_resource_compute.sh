#!/bin/bash

. $(dirname $0)/../testenv.sh

for i in {1..10}; do
    idx=$(printf '%02d' $i)
    id=/myresources/xml/compute/id${idx}

    content=$(cat <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<occi:resource xmlns:occi="http://schemas.ogf.org/occi" title="Machine a toto" >
  <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="compute" />
  <occi:attribute name="occi.compute.architecture" value="x86" />
  <occi:attribute name="occi.compute.cores" value="2" />
  <occi:attribute name="occi.compute.hostname" value="pc_toto" />
  <occi:attribute name="occi.compute.memory" value="5" />
  <occi:attribute name="occi.compute.speed" value="4000" />
</occi:resource>
EOF
	   )
    put 200 ${id} "application/xml" "$content"
done

exit 0
