dnl
dnl Autoconf initialization
dnl
AC_PREREQ([2.69])
AC_INIT([erocci],
	m4_esyscmd([(git describe 2>/dev/null || echo 0.5) | sed 's/^v//' |
        sed 's/-\([0-9]\)-/-0\1-/' | tr -d '\n' || echo 1]),
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_SRCDIR([src/occi.erl])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.14 -Wall -Wno-portability foreign no-define tar-ustar no-dist-gzip dist-xz])

AX_ERLANG_INIT()

dnl ------------------------------------------------------------------
dnl Versioning.
dnl ------------------------------------------------------------------

dnl Is this a final release?
is_release=0
GITDESC1=`git describe --abbrev=0 || echo 1`
GITDESC2=`git describe --abbrev=4 || echo 1`
if test "${GITDESC1}" = "${GITDESC2}"; then
       is_release=1
fi

dnl
dnl Compilers and all
dnl
AC_PROG_CC
AC_PROG_CXX
AC_PROG_LD
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_SED
AM_PROG_AR

dnl
dnl Libtool
dnl
dnl Hack to skip C++/Fortran tests (stolen from Beep Media Player)
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

LT_INIT

AM_DISABLE_STATIC
AM_ENABLE_SHARED

AM_PROG_LIBTOOL

dnl
dnl Options
dnl
check_ei=

dnl fetch and build dependancies
AC_ARG_ENABLE([erlang-deps],
	      [AS_HELP_STRING([--disable-erlang-deps],
                              [Disable erlang dependancies fetch and build @<:@default: enable@:>@])],
              [],
	      [enable_erlang_deps=yes])
AM_CONDITIONAL([ERL_DEPS], [test "$enable_erlang_deps" = "yes"])

dnl main app
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi], [${PACKAGE_VERSION}])
append_to_RELXAPPS "occi"

dnl core app
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_core], [${PACKAGE_VERSION}])
append_to_RELXAPPS "occi_core"

dnl authnz framework
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_authnz], [${PACKAGE_VERSION}])
append_to_RELXAPPS "occi_authnz"

dnl web server
AX_ERLANG_DEP([cowboy],                 [$enable_erlang_deps],
	      [git], [git://github.com/extend/cowboy.git], [1.0.0])
append_to_RELXAPPS "{cowboy, \"1.0.0\", '>='}"

dnl xml lib and backends
AX_LIB_EXPAT
AM_CONDITIONAL([HAVE_EXPAT], [test "$HAVE_EXPAT" = "yes"])

enable_xml2="no"
AC_ARG_WITH([libxml2],
	    [AS_HELP_STRING([--with-libxml2],
                            [use libxml2 Parser from given prefix (ARG=path); check standard prefixes (ARG=yes) or disable (ARG=no)])],
            [
	    if test "$withval" = "yes"; then
	       PKG_CHECK_MODULES([xml2], [libxml-2.0])
	       enable_xml2="yes"
	    elif test -d "$withval"; then
	       xml2_CFLAGS="-I$withval/include"
	       xml2_LIBS="-L$withval/lib -lxml2"
	       enable_xml2="yes"
	    else
	       xml2_CFLAGS=""
	       xml2_LIBS=""
	    fi
	    ],
	    [
	    PKG_CHECK_MODULES([xml2], [libxml-2.0])
	    enable_xml2="yes"
	    ])
AM_CONDITIONAL([LIBXML2], [test "$enable_xml2" = "yes"])

if test "$HAVE_EXPAT" != "yes" -a "$enable_xml2" != "yes"; then
   AC_MSG_ERROR([At least one XML parser backend is required (expat or libxml2) !])
fi
AX_ERLANG_DEP([erim_xml],               [$enable_erlang_deps],
              [git], [git://github.com/lizenn/erim_xml.git])
append_to_RELXAPPS "{erim_xml, \"0.1\", '>='}"

dnl uuid erlang dep
AX_ERLANG_DEP([uuid],                   [$enable_erlang_deps],
              [git], [git://github.com/avtobiff/erlang-uuid.git], [v0.4.6])
append_to_RELXAPPS "{uuid, \"0.4.6\", '>='}"

dnl uri erlang dep
AX_ERLANG_DEP([uri],                    [$enable_erlang_deps],
              [git], [git://github.com/heroku/uri.git])
append_to_RELXAPPS "{uri, \"0.1.0\", '>='}"

dnl jiffy erlang dep
AX_ERLANG_DEP([jiffy],                  [$enable_erlang_deps],
              [git], [git://github.com/davisp/jiffy.git], [0.9.0])
append_to_RELXAPPS "{jiffy, \"0.9.0\", '>='}"

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_http], [${PACKAGE_VERSION}])
append_to_RELXAPPS "occi_listener_http"

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_mnesia], [${PACKAGE_VERSION}])
append_to_RELXAPPS "occi_backend_mnesia"
append_to_RELXAPPS "mnesia"

dnl option --enable-listner-xmpp
AC_ARG_ENABLE([listener-xmpp],
	      [AS_HELP_STRING([--disable-listener-xmpp],
	                      [Disable XMPP listener @<:@default: enable@:>@])],
	      [],
	      [enable_listener_xmpp=yes])
if test "x$enable_listener_xmpp" = "xyes"; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_listener_xmpp], [${PACKAGE_VERSION}])
   AX_ERLANG_DEP([erim],  [$enable_erlang_deps],
                 [git], [git://github.com/lizenn/erim.git])
   AX_ERLANG_DEP([dnssd], [$enable_erlang_deps],
                 [git], [git://github.com/benoitc/dnssd_erlang.git])

   PKG_CHECK_MODULES([avahi_compat_libdns_sd], [avahi-compat-libdns_sd])
   PKG_CHECK_MODULES([ssl], [openssl])
   append_to_RELXAPPS "occi_listener_xmpp"
   append_to_RELXAPPS "erim"
   append_to_RELXAPPS "dnssd"
fi
AM_CONDITIONAL([LISTENER_XMPP], [test "$enable_listener_xmpp" = "yes"])

AC_ARG_ENABLE([authnz-htpasswd],
	      [AS_HELP_STRING([--disable-authnz-htpasswd],
	                      [Disable htpasswd based authentication @<:@default: enable@:>@])],
	      [],
	      [enable_authnz_htpasswd=yes])
AS_IF([test x$enable_authnz_htpasswd = xyes], [
   check_ei=1
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_authnz_htpasswd], [${PACKAGE_VERSION}])

   AC_MSG_CHECKING([for libaprutil1 (apr_md5.h)])
   if which apu-config > /dev/null; then
      AC_SUBST([APRUTIL_CFLAGS], [`apu-config --includes`])
      AC_SUBST([APRUTIL_LDFLAGS], [`apu-config --link-ld`])
      AC_MSG_RESULT([ok])
   else
      AC_MSG_ERROR([not found])
   fi

   append_to_RELXAPPS "occi_authnz_htpasswd"
])
AM_CONDITIONAL([AUTHNZ_HTPASSWD], [test "$enable_authnz_htpasswd" = "yes"])

AC_ARG_ENABLE([backend-dbus],
	      [AS_HELP_STRING([--enable-backend-dbus],
	                      [Enable D-BUS backend @<:@default: disable@:>@])],
	      [],
	      [enable_backend_dbus=no])
if test "x$enable_backend_dbus" = "xyes"; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_dbus], [${PACKAGE_VERSION}])
   AX_ERLANG_DEP([dbus],  [$enable_erlang_deps],
                 [git], [git://github.com/lizenn/erlang-dbus.git])

   PKG_CHECK_MODULES([avahi_compat_libdns_sd], [avahi-compat-libdns_sd])

   append_to_RELXAPPS "occi_backend_dbus"
fi
AM_CONDITIONAL([BACKEND_DBUS], [test "$enable_backend_dbus" = "yes"])


AC_ARG_ENABLE([backend-admin],
	      [AS_HELP_STRING([--disable-backend-admin],
	                      [Disable admin backend @<:@default: enable@:>@])],
	      [],
	      [enable_backend_admin=yes])
if test "x$enable_backend_admin" = "xyes"; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_admin], [${PACKAGE_VERSION}])
   append_to_RELXAPPS "occi_backend_admin"
fi
AM_CONDITIONAL([BACKEND_ADMIN], [test "$enable_backend_admin" = "yes"])

AC_ARG_ENABLE([backend-fs],
	      [AS_HELP_STRING([--disable-backend-fs],
	                      [Disable storage backend @<:@default: enable@:>@])],
	      [],
	      [enable_backend_fs=yes])
if test "x$enable_backend_fs" = "xyes"; then
   AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([occi_backend_fs], [${PACKAGE_VERSION}])
   AX_ERLANG_DEP([statfs], [$enable_erlang_deps],
   	         [git], [https://github.com/lizenn/statfs.git])

   append_to_RELXAPPS "occi_backend_fs"
   append_to_RELXAPPS "statfs"
fi
AM_CONDITIONAL([BACKEND_FS], [test "$enable_backend_fs" = "yes"])

AC_ARG_ENABLE([debug],
	      [AS_HELP_STRING([--enable-debug],
                              [Enable debugging infos @<:@default: disable@:>@])],
              [],
	      [enable_debug=no])
AM_CONDITIONAL([DEBUG], [test "$enable_debug" = "yes"])

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

if test "$check_ei" = "1"; then
     AC_ERLANG_CHECK_LIB([erl_interface])
   AC_SUBST([ERL_CFLAGS], ["-I${ERLANG_ROOT_DIR}/erts-${ERLANG_ERTS_VER}/include -I${ERLANG_LIB_DIR_erl_interface}/include"])
   AC_SUBST([ERL_LDFLAGS], ["-L${ERLANG_LIB_DIR_erl_interface}/lib -lerl_interface -lei"])
fi

AC_SUBST([RELXAPPS])
AC_SUBST([erlang_DEPS])
AC_SUBST([ERLCFLAGS])

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
	rel/Makefile
	rel/relx.config
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
echo "    xml backends  : "$(test "$HAVE_EXPAT" = "yes" && echo expat) " " $(test "$enable_xml2" = "yes" && echo libxml2)
echo ""
echo "Components:"
echo "    listeners     : "$listeners
echo "    backends      : "$backends
echo "    authnz        : "$authnz
echo "    documentation : "$enable_doc
echo ""
