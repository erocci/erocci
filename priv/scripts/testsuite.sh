#!/bin/bash -e

basedir=$(dirname $0)

suites=( $(
	       cd $basedir &&
	       for dir in $(find -name 'testsuite_*' -type d); do
		   if [ ! -e $dir/disable ]; then
		       echo ${dir:12}
		   fi
	       done)
       )
suiteslist=$( IFS="|"; echo "{${suites[*]}}" )

usage() {
    echo -e "$(basename $0) [-c] [-s ${suiteslist}]"
    echo -e "\t-c         : continue on fail (default: exit)"
    echo -e "\t-s <suite> : only run <suite>"
}

while getopts :hcs: opt; do
    case $opt in
	c)
	    set +e
	    ;;
	s)
	    suites=( $OPTARG )
	    ;;
	h)
	    usage
	    exit 0
	    ;;
	:)
	    usage
	    exit 1
	    ;;
	*)
	    usage
	    exit 1
	    ;;
    esac
done

for suite in ${suites[*]}; do
    for script in $(find ${basedir}/testsuite_${suite} -name '*.sh' -type f -perm /u=x,g=x,o=x | sort); do
	echo "### "$script
	$script
    done
done
