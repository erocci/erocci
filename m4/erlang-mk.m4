dnl -*- mode: autoconf -*-
dnl erlang-mk.m4

# AX_ERLANG_INIT
# --------------
AC_DEFUN([AX_ERLANG_INIT],
[           AC_ERLANG_NEED_ERL
            AC_ERLANG_NEED_ERLC

	    AC_SUBST([erlang_DEPS])

	    ERLCFLAGS="-I\$(top_srcdir)/include -I\$(top_srcdir)/deps"
	    for app in `ls -d apps/*`; do
	      if test -d $app/include; then
	        ERLCFLAGS="${ERLCFLAGS} -I\$(top_srcdir)/$app/include"
	      fi
	    done

	    CLEANFILES="m4_join([ ], [$CLEANFILES], erl_crash.dump)"

	    AC_SUBST([erlangdepsdir], ["\$(top_srcdir)/deps"])
]) dnl AX_ERLANG_MK

# AX_ERLANG_DEP(NAME, BUILD, REP_TYPE, REP_URL, [VERSION = master])
# ----------------------------------------------------------
AC_DEFUN([AX_ERLANG_DEP],
[if test x$2 = xyes; then
    ERLCFLAGS="${ERLCFLAGS} -I\$(top_srcdir)/deps/$1/include"
    erlang_DEPS="${erlang_DEPS} $1"

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
    AC_SUBST([$1_FETCH], ["$3 $4 $5"])
else
    AC_ERLANG_CHECK_LIB([$1])
fi
]) dnl AX_ERLANG_DEP
