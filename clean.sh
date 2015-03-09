#!/bin/sh

clean="aclocal.m4 ar-lib autom4te.cache compile config.guess config.sub configure depcomp install-sh ltmain.sh m4/libtool.m4 m4/lt~obsolete.m4 m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4 missing Makefile.in apps/Makefile.in apps/core/Makefile.in apps/authnz/Makefile.in apps/listener_http/Makefile.in apps/listener_xmpp/Makefile.in apps/authnz_htpasswd/Makefile.in apps/authnz_htpasswd/c_src/Makefile.in apps/backend_mnesia/Makefile.in apps/backend_dbus/Makefile.in doc/Makefile.in doc/guide/Makefile.in test/Makefile.in"
	
for file in $clean; do
    rm -rf $file
done

exit 0
