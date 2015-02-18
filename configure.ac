AC_PREREQ([2.64])
AC_INIT([erocci],
	[0.5],
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 -Wall foreign no-define tar-ustar no-dist-gzip dist-xz])

AX_ERLANG_INIT
AX_ERLANG_DEP([jiffy], [git git://github.com/lizenn/erim_xml.git master])
AX_ERLANG_DEP([erim_xml], [git git://github.com/lizenn/erim_xml.git master])

AC_CONFIG_FILES([
	Makefile
	ebin/erocci.app
	apps/Makefile
	apps/core/Makefile
])

AC_OUTPUT
