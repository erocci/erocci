#!/bin/bash

basedir=$(dirname $0)
url=http://localhost:8080/compute/

(
    cat <<EOF
category: compute; scheme="http://schemas.ogf.org/occi/infrastructure#"; class="kind"
x-occi-attribute: occi.compute.cores=2, occi.compute.hostname="foofoo"
EOF
) | curl -v -X POST --data @- --header "content-type: text/plain" $url
