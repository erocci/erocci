AC_PREREQ([2.64])
AC_INIT([erocci],
	[0.5],
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 -Wall -Wno-portability foreign no-define tar-ustar no-dist-gzip dist-xz])

AX_ERLANG_INIT
AX_ERLANG_DEP([jiffy],                  [git], [git://github.com/davisp/jiffy.git], [0.9.0])
AX_ERLANG_DEP([erim_xml],               [git], [git://github.com/lizenn/erim_xml.git])
AX_ERLANG_DEP([uuid],                   [git], [git://gitorious.org/avtobiff/erlang-uuid.git], [v0.4.6])
AX_ERLANG_DEP([uri],                    [git], [git://github.com/heroku/uri.git])
AX_ERLANG_DEP([erocci_listener_http],   [git], [git://github.com/erocci/erocci_listener_http.git])
AX_ERLANG_DEP([erocci_listener_xmpp],   [git], [git://github.com/erocci/erocci_listener_xmpp.git])
AX_ERLANG_DEP([erocci_backend_mnesia],  [git], [git://github.com/erocci/erocci_backend_mnesia.git])
AX_ERLANG_DEP([erocci_authnz_htpasswd], [git], [git://github.com/erocci/erocci_authnz_htpasswd.git])

AC_CHECK_PROGS([REBAR], [rebar])

AC_CONFIG_FILES([
	Makefile
	ebin/occi.app:src/occi.app.in
	apps/Makefile
	apps/core/Makefile
	apps/core/ebin/occi_core.app:apps/core/src/occi_core.app.in
])

AC_OUTPUT

echo "************************************************************************"
echo " erocci configuration is now complete."
echo ""
echo " Run 'make deps' for fetching dependancies and build them."
echo "************************************************************************"
