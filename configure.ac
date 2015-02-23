AC_PREREQ([2.64])
AC_INIT([erocci],
	[0.5],
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 -Wall -Wno-portability foreign no-define tar-ustar no-dist-gzip dist-xz])

AC_ARG_ENABLE([xmpp],
	      [AS_HELP_STRING([--disable-xmpp],
	                      [Disable XMPP listener @<:@default: enable@:>@])],
	      [],
	      [enable_xmpp=yes])
AM_CONDITIONAL([XMPP], [test "$enable_xmpp" = "yes"])

AC_ARG_ENABLE([erlang-deps],
	      [AS_HELP_STRING([--enable-erlang-deps],
                              [Enable erlang dependancies fetch and build @<:@default: disable@:>@])],
              [],
	      [enable_erlang_deps=no])
AM_CONDITIONAL([ERL_DEPS], [test "$enable_erlang_deps" = "yes"])

AX_ERLANG_INIT()

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_core], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_authnz], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_http], [${PACKAGE_VERSION}])

AS_IF([test x$enable_xmpp = xyes], [
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_xmpp], [${PACKAGE_VERSION}])
   AX_ERLANG_DEP([erim],  [$enable_erlang_deps],
                 [git], [git://github.com/lizenn/erim.git])
   AX_ERLANG_DEP([dnssd], [$enable_erlang_deps],
                 [git], [git://github.com/benoitc/dnssd_erlang.git])

   PKG_CHECK_MODULES([avahi_compat_libdns_sd], [avahi-compat-libdns_sd])
])

AX_ERLANG_DEP([cowboy],                 [$enable_erlang_deps],
	      [git], [git://github.com/extend/cowboy.git], [1.0.0])
AX_ERLANG_DEP([jiffy],                  [$enable_erlang_deps],
              [git], [git://github.com/davisp/jiffy.git], [0.9.0])
AX_ERLANG_DEP([erim_xml],               [$enable_erlang_deps],
              [git], [git://github.com/lizenn/erim_xml.git])
AX_ERLANG_DEP([uuid],                   [$enable_erlang_deps],
              [git], [git://gitorious.org/avtobiff/erlang-uuid.git], [v0.4.6])
AX_ERLANG_DEP([uri],                    [$enable_erlang_deps],
              [git], [git://github.com/heroku/uri.git])
AX_ERLANG_DEP([erocci_backend_mnesia],  [$enable_erlang_deps],
              [git], [git://github.com/erocci/erocci_backend_mnesia.git])
AX_ERLANG_DEP([erocci_authnz_htpasswd], [$enable_erlang_deps],
              [git], [git://github.com/erocci/erocci_authnz_htpasswd.git])

AC_CHECK_PROGS([REBAR], [rebar])
AS_IF([test x$enable_erlang_deps = xyes], [AC_CHECK_PROGS([GIT], [git])])

AC_CONFIG_FILES([
	Makefile
	apps/Makefile
	apps/core/Makefile
	apps/authnz/Makefile
	apps/listener_http/Makefile
	apps/listener_xmpp/Makefile
])

AC_OUTPUT

get_apps () {
    for app in $(echo -e "subdirs:\n\t@echo \$(SUBDIRS)\n\ninclude ./apps/Makefile" | make -f - subdirs); do
      echo $app
    done
}

get_plugins () {
    get_apps | awk -e '/^'$1'_/ { sub(/^'$1'_/,""); print }'
}

listeners=$(get_plugins listener)
backends=$(get_plugins backend)
authnz=$(get_plugins authnz)
echo "************************************************************************"
echo " erocci configuration is now complete."
echo ""
echo "************************************************************************"
echo ""
echo "Enabled components:"
echo "    listeners : "$listeners
echo "    backends  : "$backends
echo "    authnz    : "$authnz
echo ""
