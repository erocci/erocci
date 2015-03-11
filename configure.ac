AC_PREREQ([2.69])
AC_INIT([erocci],
	[0.5],
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.14 -Wall -Wno-portability foreign no-define tar-ustar no-dist-gzip dist-xz])

AX_ERLANG_INIT()

AC_PROG_CC
AC_PROG_CXX
AM_PROG_AR
LT_INIT

AC_ARG_ENABLE([listener-xmpp],
	      [AS_HELP_STRING([--disable-listener-xmpp],
	                      [Disable XMPP listener @<:@default: enable@:>@])],
	      [],
	      [enable_listener_xmpp=yes])
AM_CONDITIONAL([LISTENER_XMPP], [test "$enable_listener_xmpp" = "yes"])

AC_ARG_ENABLE([authnz-htpasswd],
	      [AS_HELP_STRING([--disable-authnz-htpasswd],
	                      [Disable htpasswd based authentication @<:@default: enable@:>@])],
	      [],
	      [enable_authnz_htpasswd=yes])
AM_CONDITIONAL([AUTHNZ_HTPASSWD], [test "$enable_authnz_htpasswd" = "yes"])

AC_ARG_ENABLE([backend-dbus],
	      [AS_HELP_STRING([--enable-backend-dbus],
	                      [Enable D-BUS backend @<:@default: disable@:>@])],
	      [],
	      [enable_backend_dbus=no])
AM_CONDITIONAL([BACKEND_DBUS], [test "$enable_backend_dbus" = "yes"])


AC_ARG_ENABLE([backend-admin],
	      [AS_HELP_STRING([--disable-backend-admin],
	                      [Disable admin backend @<:@default: enable@:>@])],
	      [],
	      [enable_backend_admin=yes])
AM_CONDITIONAL([BACKEND_ADMIN], [test "$enable_backend_admin" = "yes"])

AC_ARG_ENABLE([backend-fs],
	      [AS_HELP_STRING([--disable-backend-fs],
	                      [Disable storage backend @<:@default: enable@:>@])],
	      [],
	      [enable_backend_fs=yes])
AM_CONDITIONAL([BACKEND_FS], [test "$enable_backend_fs" = "yes"])

AC_ARG_ENABLE([erlang-deps],
	      [AS_HELP_STRING([--disable-erlang-deps],
                              [Disable erlang dependancies fetch and build @<:@default: enable@:>@])],
              [],
	      [enable_erlang_deps=yes])
AM_CONDITIONAL([ERL_DEPS], [test "$enable_erlang_deps" = "yes"])

AC_ARG_ENABLE([libxml2],
	      [AS_HELP_STRING([--enable-libxml2],
                              [Enable libxml2 XML parser backend @<:@default: disable@:>@])],
              [],
	      [enable_libxml2=no])

AC_ARG_ENABLE([debug],
	      [AS_HELP_STRING([--enable-debug],
                              [Enable debugging infos @<:@default: disable@:>@])],
              [],
	      [enable_debug=no])
AM_CONDITIONAL([DEBUG], [test "$enable_debug" = "yes"])

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_core], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_authnz], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_authnz_htpasswd], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_http], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_mnesia], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_admin], [${PACKAGE_VERSION}])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_fs], [${PACKAGE_VERSION}])

if test "x$enable_listener_xmpp" = "xyes"; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_xmpp], [${PACKAGE_VERSION}])
   AX_ERLANG_DEP([erim],  [$enable_erlang_deps],
                 [git], [git://github.com/lizenn/erim.git])
   AX_ERLANG_DEP([dnssd], [$enable_erlang_deps],
                 [git], [git://github.com/benoitc/dnssd_erlang.git])

   PKG_CHECK_MODULES([avahi_compat_libdns_sd], [avahi-compat-libdns_sd])
   PKG_CHECK_MODULES([ssl], [openssl])
   AS_IF([test "$enable_libxml2" = "yes"], [
     PKG_CHECK_MODULES([xml2], [libxml-2.0])
   ], [
     enable_libxml2=no
   ])
   AX_LIB_EXPAT
fi
AM_CONDITIONAL([HAVE_EXPAT], [test "$HAVE_EXPAT" = "yes"])
AM_CONDITIONAL([LIBXML2], [test "$enable_libxml2" = "yes"])

if test "x$enable_backend_dbus" = "xyes"; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_dbus], [${PACKAGE_VERSION}])
   AX_ERLANG_DEP([dbus],  [$enable_erlang_deps],
                 [git], [git://github.com/lizenn/erlang-dbus.git])

   PKG_CHECK_MODULES([avahi_compat_libdns_sd], [avahi-compat-libdns_sd])
fi

if test "x$enable_backend_fs" = "xyes"; then
   AX_ERLANG_DEP([statfs], [$enable_erlang_deps],
   	         [git], [https://github.com/lizenn/statfs.git])
fi

if test "x$enable_htpasswd" = "xyes"; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_authnz_htpasswd], [${PACKAGE_VERSION}])

   AC_MSG_CHECKING([for libaprutil1 (apr_md5.h)])
   if which apu-config > /dev/null; then
      AC_SUBST([APRUTIL_CFLAGS], [`apu-config --includes`])
      AC_SUBST([APRUTIL_LDFLAGS], [`apu-config --link-ld`])
      AC_MSG_RESULT([ok])
   else
      AC_MSG_ERROR([not found])
   fi
fi

AX_ERLANG_DEP([cowboy],                 [$enable_erlang_deps],
	      [git], [git://github.com/extend/cowboy.git], [1.0.0])
AX_ERLANG_DEP([jiffy],                  [$enable_erlang_deps],
              [git], [git://github.com/davisp/jiffy.git], [0.9.0])
AX_ERLANG_DEP([erim_xml],               [$enable_erlang_deps],
              [git], [git://github.com/lizenn/erim_xml.git])
AX_ERLANG_DEP([uuid],                   [$enable_erlang_deps],
              [git], [git://github.com/avtobiff/erlang-uuid.git], [v0.4.6])
AX_ERLANG_DEP([uri],                    [$enable_erlang_deps],
              [git], [git://github.com/heroku/uri.git])

if test "$enable_erlang_deps" = "yes"; then
   AC_CHECK_PROGS([GIT], [git], [no])
   if test "$GIT" = "no"; then
      AC_MSG_ERROR(['git' is required but can not be found])
   fi
fi
AC_CHECK_PROGS([REBAR], [rebar], [no])
if test "$REBAR" = "no"; then
   AC_MSG_ERROR(['rebar' is required but can not be found])
fi
AC_CHECK_PROGS([MKDOCS], [mkdocs])
if test ! x$MKDOCS = x; then
   enable_doc=yes
else
   enable_doc=no
fi
AM_CONDITIONAL([BUILDDOC], [test x$enable_doc = xyes])

AS_IF([test x$enable_erlang_deps = xyes], [AC_CHECK_PROGS([GIT], [git])])

AS_IF([test x$enable_htpasswd = xyes], [
   AC_ERLANG_CHECK_LIB([erl_interface])
   AC_SUBST([ERL_CFLAGS], ["-I${ERLANG_ROOT_DIR}/erts-${ERLANG_ERTS_VER}/include -I${ERLANG_LIB_DIR_erl_interface}/include"])
   AC_SUBST([ERL_LDFLAGS], ["-L${ERLANG_LIB_DIR_erl_interface}/lib -lerl_interface -lei"])
])

AC_CONFIG_FILES([
	Makefile
	apps/Makefile
	apps/core/Makefile
	apps/authnz/Makefile
	apps/listener_http/Makefile
	apps/listener_xmpp/Makefile
	apps/authnz_htpasswd/Makefile
	apps/authnz_htpasswd/c_src/Makefile
	apps/backend_mnesia/Makefile
	apps/backend_dbus/Makefile
	apps/backend_admin/Makefile
	apps/backend_fs/Makefile
	doc/Makefile
	doc/guide/Makefile
	test/Makefile
])

AC_OUTPUT

get_apps () {
    for app in $(echo -e "subdirs:\n\t@echo \$(SUBDIRS)\n\ninclude ./apps/Makefile" | make -f - subdirs); do
      echo $app
    done
}

get_plugins () {
    get_apps | awk '/^'$1'_/ { sub(/^'$1'_/,""); print }'
}

listeners=$(get_plugins listener)
backends=$(get_plugins backend)
authnz=$(get_plugins authnz)
echo "************************************************************************"
echo " erocci configuration is now complete."
echo ""
echo "************************************************************************"
echo "Options:"
echo "    debug         : "$enable_debug
echo "    xml backends  : "$(test "$HAVE_EXPAT" = "yes" && echo expat) " " $(test "$enable_libxml2" = "yes" && echo libxml2)
echo ""
echo "Components:"
echo "    listeners     : "$listeners
echo "    backends      : "$backends
echo "    authnz        : "$authnz
echo "    documentation : "$enable_doc
echo ""
