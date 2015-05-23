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

AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

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
dnl AC_PROG_CXX
dnl AC_PROG_LD
dnl AC_PROG_INSTALL
dnl AC_PROG_MAKE_SET
dnl AC_PROG_SED

dnl
dnl Libtool
dnl
dnl Hack to skip C++/Fortran tests (stolen from Beep Media Player)
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

dnl
dnl Convenience functions
dnl
append_to_EXCL_APPS () {
  if test -z "[$]REBAR_EXCL_APPS"; then
    REBAR_EXCL_APPS="[$]1"
  else
    REBAR_EXCL_APPS="[$]{REBAR_EXCL_APPS},[$]1"
  fi
}

dnl
dnl Options
dnl

dnl xml lib and backends
AX_LIB_EXPAT

enable_xml2="no"
AC_ARG_WITH([libxml2],
	    [AS_HELP_STRING([--with-libxml2],
                            [use libxml2 parser from given prefix (ARG=path); check standard prefixes (ARG=yes) or disable (ARG=no)])],
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
	    PKG_CHECK_MODULES([xml2], [libxml-2.0], [enable_xml2="yes"], [enable_xml2="no"])
	    ])

AS_IF([test "$HAVE_EXPAT" != "yes" -a "$enable_xml2" != "yes"], [
   AC_MSG_ERROR([At least one XML parser backend is required (expat or libxml2) !])
])

dnl option --enable-listner-xmpp
AC_ARG_ENABLE([listener-xmpp],
	      [AS_HELP_STRING([--disable-listener-xmpp],
	                      [Disable XMPP listener @<:@default: enable@:>@])],
	      [],
	      [enable_listener_xmpp=yes])
if test "x$enable_listener_xmpp" = "xyes"; then
   PKG_CHECK_MODULES([avahi_compat_libdns_sd], [avahi-compat-libdns_sd])
   PKG_CHECK_MODULES([ssl], [openssl])
else
   append_to_EXCL_APPS listener_xmpp
fi

AC_ARG_ENABLE([authnz-htpasswd],
	      [AS_HELP_STRING([--disable-authnz-htpasswd],
	                      [Disable htpasswd based authentication @<:@default: enable@:>@])],
	      [],
	      [enable_authnz_htpasswd=yes])
if test x$enable_authnz_htpasswd = xyes; then
   AC_MSG_CHECKING([for libaprutil1 (apr_md5.h)])
   if which apu-config > /dev/null; then
      AC_SUBST([APRUTIL_CFLAGS], [`apu-config --includes`])
      AC_SUBST([APRUTIL_LDFLAGS], [`apu-config --link-ld`])
      AC_MSG_RESULT([ok])
   else
      AC_MSG_ERROR([not found])
   fi
else
   append_to_EXCL_APPS authnz_htpasswd
fi

AC_ARG_ENABLE([backend-dbus],
	      [AS_HELP_STRING([--enable-backend-dbus],
	                      [Enable D-BUS backend @<:@default: disable@:>@])],
	      [],
	      [enable_backend_dbus=no])
if test "x$enable_backend_dbus" = "xyes"; then
   PKG_CHECK_MODULES([avahi_compat_libdns_sd], [avahi-compat-libdns_sd])
else
   append_to_EXCL_APPS backend_dbus
fi

AC_ARG_ENABLE([backend-admin],
	      [AS_HELP_STRING([--disable-backend-admin],
	                      [Disable admin backend @<:@default: enable@:>@])],
	      [],
	      [enable_backend_admin=yes])
if ! test "x$enable_backend_admin" = "xyes"; then
   append_to_EXCL_APPS backend_admin
fi

AC_ARG_ENABLE([backend-fs],
	      [AS_HELP_STRING([--disable-backend-fs],
	                      [Disable storage backend @<:@default: enable@:>@])],
	      [],
	      [enable_backend_fs=yes])
if ! test "x$enable_backend_fs" = "xyes"; then
   append_to_EXCL_APPS backend_fs
fi

AC_ARG_ENABLE([debug],
	      [AS_HELP_STRING([--enable-debug],
                              [Enable debugging infos @<:@default: disable@:>@])],
              [],
	      [enable_debug=no])

AC_CHECK_PROGS([MKDOCS], [mkdocs])
if test ! x$MKDOCS = x; then
   enable_doc=yes
else
   enable_doc=no
fi

AC_SUBST([REBAR_EXCL_APPS])

AC_CONFIG_FILES([
	Makefile
	rebar.config
	src/occi.app.src
])

AC_OUTPUT

xml_backends=""
if test "[$]HAVE_EXPAT" = "yes"; then
  xml_backends="expat"
fi
if test "[$]enable_xml2"; then
  if test -z "[$]xml_backends"; then
    xml_backends="libxml2"
  else
    xml_backends="[$]xml_backends libxml2"
  fi
fi

echo "************************************************************************"
echo " erocci configuration is now complete."
echo ""
echo "************************************************************************"
echo "Options:"
echo "    debug         : "$enable_debug
echo "    xml backends  : "$xml_backends
echo ""
