dnl -*- mode: autoconf -*-
dnl erlang-mk.m4

AC_DEFUN([AX_ERLANG_INIT],
[           AC_ERLANG_NEED_ERL
            AC_ERLANG_NEED_ERLC

	    AC_SUBST([ERLANG_MK])
	    ERLANG_MK="include \${top_srcdir}/erlang.mk"

	    ERLCFLAGS="-Iinclude"
	    for app in `ls -d apps/*`; do
	      if test -d $app/include; then
	        ERLCFLAGS="m4_join([ ], [$ERLCFLAGS], [-I$app/include])"
	      fi
	    done

	    AC_ARG_ENABLE([erlang-deps],
			  [AS_HELP_STRING([--disable-erlang-deps],
					  [Disable erlang dependancies fetching @<:@default=enable@:>@])],
			  [],
			  [enable_erlang_deps=yes])

	    CLEANFILES="m4_join([ ], [$CLEANFILES], erl_crash.dump)"

	    AC_SUBST([erlangdepsdir], [\${top_srcdir}/deps])
]) dnl AX_ERLANG_MK

AC_DEFUN([AX_ERLANG_DEP],
[	    ERLCFLAGS="m4_join([ ], [$ERLCFLAGS], [-Ideps/$1/include])"

	    depsrc=($2)
	    if test x$enable_erlang_deps = xyes; then
	       case ${depsrc@<:@0@:>@} in
	       	  git)
		      AC_CHECK_PROGS([GIT], [git])
		      if test x$GIT = x; then
		         AC_MSG_ERROR([git tool missing])
		      fi
		      ;;
		  hg)
		      AC_CHECK_PROGS([HG], [hg])
		      if test x$HG = x; then
		         AC_MSG_ERROR([mercurial tool (hg) missing])
		      fi
		      ;;
		  svn)
		      AC_CHECK_PROGS([SVN], [svn])
		      if test x$SVN = x; then
		         AC_MSG_ERROR([Subversion tool (svn) missing])
		      fi
		      ;;
		  *)
			AC_MSG_ERROR([Unknown repository type: ${depsrc@<:@0@:>@}])
			;;
	       esac
	       AC_MSG_NOTICE([Fetch erlang dependancy: [$1]])
	       make --no-print-directory deps/$1/src
	    else
		AC_MSG_CHECKING([for [$1]])
		if test -d deps/$1/src; then
		   AC_MSG_RESULT([ok])
		else
		   AC_MSG_ERROR([failed])
		fi
	    fi
]) dnl AX_ERLANG_DEP
