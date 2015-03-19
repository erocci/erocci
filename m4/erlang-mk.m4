dnl -*- mode: autoconf -*-
dnl erlang-mk.m4

# AX_ERLANG_INIT
# --------------
AC_DEFUN([AX_ERLANG_INIT],
[
	    append_to_ERLCFLAGS () {
	        if test -z "[$]ERLCFLAGS"; then
	              ERLCFLAGS="[$]1"
		else
			ERLCFLAGS="[$]{ERLCFLAGS% } [$]1"
		fi
	    }

    	    append_to_erlang_DEPS () {
	        if test -z "[$]{erlang_DEPS}"; then
	              erlang_DEPS="[$]1"
	        else
                      erlang_DEPS="[$]{erlang_DEPS% } [$]1"
                fi
            }

 	    append_to_RELXAPPS () {
	       if test -z "[$]RELXAPPS"; then
	              RELXAPPS="[$]1"
               else
                      RELXAPPS="[$]{RELXAPPS% }, [$]1"
               fi
           }


	    AC_REQUIRE([AC_ERLANG_NEED_ERL])
            AC_REQUIRE([AC_ERLANG_NEED_ERLC])
	    AC_ERLANG_SUBST_ERTS_VER
	    AC_ERLANG_SUBST_ROOT_DIR

	    append_to_ERLCFLAGS "-pa \$(builddir)/ebin -I\$(srcdir)/src -I\$(srcdir)/include"
	    for inc in include deps apps; do
	      append_to_ERLCFLAGS "-I\$(top_srcdir)/$inc"
	    done
	    for app in $(find $srcdir/apps -maxdepth 1 -mindepth 1 -type d); do
	       appdir=$(echo $app | sed -e 's,'$srcdir',,')
	       append_to_ERLCFLAGS "-I\$(top_srcdir)/$app/include -pa \$(top_builddir)/$appdir/ebin"
	    done

           CLEANFILES="$CLEANFILES erl_crash.dump"
]) dnl AX_ERLANG_MK

# AX_ERLANG_DEP(NAME, BUILD, REP_TYPE, REP_URL, [VERSION = master])
# ----------------------------------------------------------
AC_DEFUN([AX_ERLANG_DEP],
[if test x$2 = xyes; then
    append_to_ERLCFLAGS "-I\$(top_srcdir)/deps/$1/include"
    append_to_erlang_DEPS "$1"

    version=""
    case "$3" in
      "git")
        if test x$5 = x; then
          version=master
 	fi
	;;
      "hg")
	if test x$5 = x; then
	  version=master
	fi
        ;;
      "svn")
	:
        ;;
      *)
	AC_MSG_ERROR([Unknown repository type: $3])
	;;
    esac

    AC_MSG_CHECKING([for Erlang/OTP '$1' library])
    AC_MSG_RESULT([add for fetch and build])
    AC_SUBST([$1_DEP_VCS], [$3])
    AC_SUBST([$1_DEP_URL], [$4])
    AC_SUBST([$1_DEP_VER], [$5])
else
    AC_ERLANG_CHECK_LIB([$1])
fi
]) dnl AX_ERLANG_DEP
