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
	        ERLCFLAGS="m4_join([ ], [$ERLCFLAGS], [\"-I\$(top_srcdir)/$app/include\"])"
	      fi
	    done

	    CLEANFILES="m4_join([ ], [$CLEANFILES], erl_crash.dump)"

	    AC_SUBST([erlangdepsdir], ["\$(top_srcdir)/deps"])
]) dnl AX_ERLANG_MK

# AX_ERLANG_DEP(NAME, REP_TYPE, REP_URL, [VERSION = master])
# ----------------------------------------------------------
AC_DEFUN([AX_ERLANG_DEP],
[	    ERLCFLAGS="m4_join([ ], [$ERLCFLAGS], [\"-I\$(top_srcdir)/deps/$1/include\"])"
	    erlang_DEPS="m4_join([ ], [$erlang_DEPS], [$1])"

	    version=""
	    case "$2" in
	      "git")
	        AC_CHECK_PROGS([GIT], [git])
	        if test x$GIT = x; then
	          AC_MSG_ERROR([git tool missing])
		fi
		if test x$4 = x; then
		  version=master
		fi
		;;
	      "hg")
	        AC_CHECK_PROGS([HG], [hg])
	        if test x$HG = x; then
	          AC_MSG_ERROR([mercurial tool (hg) missing])
	        fi
		if test x$4 = x; then
		  version=master
		fi
	        ;;
	      "svn")
	        AC_CHECK_PROGS([SVN], [svn])
	        if test x$SVN = x; then
	          AC_MSG_ERROR([Subversion tool (svn) missing])
	        fi
	        ;;
	      *)
		AC_MSG_ERROR([Unknown repository type: $2])
		;;
	    esac

	    AC_SUBST([$1_FETCH], ["$2 $3 $4"])
]) dnl AX_ERLANG_DEP
