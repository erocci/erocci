#!/bin/bash
cd `dirname $0`
export MNESIA_DIR="$HOME/.occi/"

tempdir=$(mktemp -d)
config=$tempdir/hello_occi.config

_exit()
{
    rm -rf $tempdir
}

trap _exit EXIT

debug=info
while getopts ":d" opt; do
    case $opt in
	d)
	    debug=debug
	    ;;
	*)
	    ;;
    esac
done

cat <<EOF > $config
[
 {lager, [
          {colored, true},
	  {handlers, 
	   [
	    {lager_console_backend, $debug}
	   ]
	  }
	 ]
 }
].
EOF

exec erl -pa $PWD/ebin \
    $PWD/deps/*/ebin \
    -boot start_sasl \
    -config $config \
    -kernel error_logger silent \
    -s reloader \
    -s hello_occi
