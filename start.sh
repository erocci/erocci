#!/bin/bash -x
cd `dirname $0`

usage() {
    echo "Usage: $0 [-d] [-s] [-x <jid>] [-c <config>] [-h] [-n <name>]"
    echo -e "\t-d           Print debug messages"
    echo -e "\t-s           Start HTTPS listener (default: HTTP)"
    echo -e "\t-x <jid>     Start XMPP listener with given JID"
    echo -e "\t-c <config>  Set alternate config file (default: example.config)"
    echo -e "\t-n <name>    Set system name (e.g.: http://localhost:8080)"
    echo -e "\t-h           Print this help"

}

ssldir=priv/ssl
cacertfile=$ssldir/cowboy-ca.crt
certfile=$ssldir/server.crt
keyfile=$ssldir/server.key

name=
debug=info
config=priv/example.config
listener="{http, occi_http, [{port, 8080}]}"
while getopts ":hdsc:x:n:" opt; do
    case $opt in
	n)
	    name=$OPTARG
	    ;;
	d)
	    debug=debug
	    ;;
	s)
	    listener="{https, occi_https, [{port, 8443}, {cacertfile, \"$cacertfile\"}, {certfile, \"$certfile\"}, {keyfile, \"$keyfile\"}]}"
	    ;;
	x)
	    jid=$OPTARG
	    ;;
	c)
	    config=$OPTARG
	    ;;
	h)
	    usage
	    exit 0
	    ;;
	*)
	    usage
	    exit 1
	    ;;
    esac
done

if [ -n "$jid" ]; then
    read -s -p "Password:" passwd
    listener="{xmppc, occi_xmpp_client, [{jid, \"$jid\"}, {passwd, \"$passwd\"}]}"
fi

if [ -d $PWD/deps ]; then
    depsbin=$PWD/deps
else
    depsbin=$PWD/..
fi

case $debug in
    debug)
	debug_app="-s reloader"
	;;
    *)
	debug_app=
	;;
esac

exec erl -pa $PWD/ebin \
    $depsbin/*/ebin \
    -boot start_sasl \
    -config $config \
    -kernel error_logger silent \
    -lager handlers "[{lager_console_backend, $debug}]" \
    -occi listeners "[$listener]" \
    -occi name "\"$name\"" \
    $debug_app -s occi
