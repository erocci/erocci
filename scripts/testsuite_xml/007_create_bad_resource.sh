#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/myresources/xml/badcompute/id

content=$(cat <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<occi:resource xmlns:occi="http://schemas.ogf.org/occi" title="Machine a toto" >
  <occi:kind scheme="http://schemas.ogf.org/occi/infrastructure#" term="badterm" />
</occi:resource>
EOF
       )
put 400 ${id} "application/xml" "$content"

exit 0
