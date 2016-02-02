dnl
dnl Autoconf initialization
dnl
AC_PREREQ([2.69])
AC_INIT([erocci],
	m4_esyscmd([(git describe --always --tags 2>/dev/null || echo 0.6) | sed 's/^v//' |
        sed 's/-\([0-9]\)-/-0\1-/' | tr -d '\n' || echo 1]),
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_SRCDIR([apps/erocci/src/erocci.app.src.in])
AC_CONFIG_SUBDIRS([deps/erocci_core deps/erim_xml])

AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

AC_ARG_ENABLE([dev-mode],
              [AS_HELP_STRING([--enable-dev-mode],
                              [Enable developer mode (add tests, use master versions of dependancies, etc) @<:@default=disable@:>@])],
              [],
              [enable_dev_mode=no])
AC_SUBST([enable_dev_mode])

dnl ------------------------------------------------------------------
dnl Versioning.
dnl ------------------------------------------------------------------

AC_CONFIG_FILES([
	Makefile
])

AC_OUTPUT
