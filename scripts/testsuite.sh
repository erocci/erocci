#!/bin/bash -e

basedir=$(dirname $0)

ct=$1
case "x$ct" in
    "xjson")
	dir=$basedir/testsuite_json
	;;
    "xxml")
	dir=$basedir/testsuite_xml
	;;
    "x")
	dir="$basedir/testsuite_json $basedir/testsuite_xml"
	;;
    *)
	echo "Usage: "$(basename $0)" [json|xml]"
	exit 1
	;;
esac
	
for script in $(find $dir -name '*.sh' -type f -perm /u=x,g=x,o=x | sort); do
    echo "### "$script
    $script
done
