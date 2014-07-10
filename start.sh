#!/bin/bash
basedir=$(cd $(dirname $0) && pwd)

usage() {
    echo "Usage: $0 [-d] [-s] [-x <jid>] [-c <config>] [-h] [-n <name>]"
    echo -e "\t-d           Print debug messages"
    echo -e "\t-s           Start HTTPS listener (default: HTTP)"
    echo -e "\t-x <jid>     Start XMPP listener with given JID"
    echo -e "\t-c <config>  Set alternate config file (default: example.config)"
    echo -e "\t-n <name>    Set system name (e.g.: http://localhost:8080)"
    echo -e "\t-h           Print this help"

}

ssldir=${basedir}/priv/ssl
cacertfile=$ssldir/cowboy-ca.crt
certfile=$ssldir/server.crt
keyfile=$ssldir/server.key
htpasswd=${basedir}/priv/htpasswd

name=
debug=info
config=${basedir}/priv/example.config
listener="{http, occi_http, [{port, 8080}]}"
epasswd="{htpasswd, <<\"priv/htpasswd\">>}"
while getopts ":hdsc:x:n:p:" opt; do
    case $opt in
	n)
	    name=$OPTARG
	    ;;
	d)
	    debug=debug
	    set -x
	    ;;
	s)
	    listener="{https, occi_https, [{port, 8443}, {cacertfile, \"$cacertfile\"}, {certfile, \"$certfile\"}, {keyfile, \"$keyfile\"}]}"
	    ;;
	x)
	    jid=$OPTARG
	    ;;
	c)
	    config=`pwd`/$OPTARG
	    ;;
	p)
	    htpasswd=$OPTARG
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
    epasswd="{xmpp, \"\" }"
fi

if [ -d ${basedir}/deps ]; then
    depsbin=${basedir}/deps
else
    depsbin=${basedir}/..
fi

case $debug in
    debug)
	debug_app="-s reloader"
	;;
    *)
	debug_app=
	;;
esac

cd ${basedir}
exec erl -pa $PWD/ebin \
    $depsbin/*/ebin \
    -boot start_sasl \
    -config $config \
    -kernel error_logger silent \
    -lager handlers "[{lager_console_backend, $debug}]" \
    -epasswd mod $epassd \
    -occi listeners "[$listener]" \
    -occi name "\"$name\"" \
    $debug_app -s occi
