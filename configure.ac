AC_PREREQ([2.64])
AC_INIT([erocci],
	[0.5],
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 -Wall -Wno-portability foreign no-define tar-ustar no-dist-gzip dist-xz])

AX_ERLANG_INIT()

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_core], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_authnz], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_http], [${PACKAGE_VERSION}])

AC_ARG_ENABLE([xmpp],
	      [AS_HELP_STRING([--disable-xmpp],
	                      [Disable XMPP listener @<:@default: enable@:>@])],
	      [],
	      [enable_xmpp=yes])
AM_CONDITIONAL([XMPP], [test "$enable_xmpp" = "yes"])
if test x$enable_xmpp = xyes; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_xmpp], [${PACKAGE_VERSION}])
   AX_ERLANG_DEP([erim], [git], [git://github.com/lizenn/erim.git])
   AX_ERLANG_DEP([dnssd], [git], [git://github.com/benoitc/dnssd_erlang.git])
fi

AX_ERLANG_DEP([cowboy],                 [git], [git://github.com/extend/cowboy.git], [1.0.0])
AX_ERLANG_DEP([jiffy],                  [git], [git://github.com/davisp/jiffy.git], [0.9.0])
AX_ERLANG_DEP([erim_xml],               [git], [git://github.com/lizenn/erim_xml.git])
AX_ERLANG_DEP([uuid],                   [git], [git://gitorious.org/avtobiff/erlang-uuid.git], [v0.4.6])
AX_ERLANG_DEP([uri],                    [git], [git://github.com/heroku/uri.git])
AX_ERLANG_DEP([erocci_backend_mnesia],  [git], [git://github.com/erocci/erocci_backend_mnesia.git])
AX_ERLANG_DEP([erocci_authnz_htpasswd], [git], [git://github.com/erocci/erocci_authnz_htpasswd.git])

AC_CHECK_PROGS([REBAR], [rebar])

AC_CONFIG_FILES([
	Makefile
	apps/Makefile
	apps/core/Makefile
	apps/authnz/Makefile
	apps/listener_http/Makefile
	apps/listener_xmpp/Makefile
])

AC_OUTPUT

echo "************************************************************************"
echo " erocci configuration is now complete."
echo ""
echo " Run 'make deps' for fetching dependancies and build them."
echo "************************************************************************"
