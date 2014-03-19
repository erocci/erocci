#!/bin/bash
cd `dirname $0`
export MNESIA_DIR="$HOME/.occi/"

tempdir=$(mktemp -d)
config=$tempdir/hello_occi.config

ssldir=examples/hello_occi/priv/ssl
cacertfile=$ssldir/cowboy-ca.crt
certfile=$ssldir/server.crt
keyfile=$ssldir/server.key

_exit()
{
    rm -rf $tempdir
}

trap _exit EXIT

debug=info
listener="{http, occi_http, [{port, 8080}]}"
while getopts ":ds" opt; do
    case $opt in
	d)
	    debug=debug
	    ;;
	s)
	    listener="{https, occi_https, [{port, 8443}, {cacertfile, \"$cacertfile\"}, {certfile, \"$certfile\"}, {keyfile, \"$keyfile\"}
       ]}"
	    ;;
	*)
	    ;;
    esac
done

cat <<EOF > $config
[
 {lager, [
   {colored, true},
   {handlers, [
     {lager_console_backend, $debug}
    ]}
  ]},
 {occi, [
    {name, "http://localhost:8080"},
    {listeners, [$listener]}
  ]}
].
EOF

if [ -d $PWD/deps ]; then
    depsbin=$PWD/deps
else
    depsbin=$PWD/..
fi

exec erl -pa $PWD/ebin \
    $depsbin/*/ebin \
    -boot start_sasl \
    -config $config \
    -kernel error_logger silent \
    -s reloader \
    -s hello_occi
