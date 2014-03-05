#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/occi/compute/$(uuidgen)

content=( '"category: compute; scheme=\"http://schemas.ogf.org/occi/infrastructure#\"; class=\"kind\""'
	  '"x-occi-attribute: occi.core.title=\"Machine\", occi.compute.architecture=\"x86\", occi.compute.cores=1, occi.compute.hostname=\"pc\", occi.compute.memory=5, occi.compute.speed=4000"' )

put_h 201 ${id} "text/occi" "${content}"
