#!/bin/bash
basedir=$(cd $(dirname $0) && pwd)

function usage() {
    echo "Usage: $0 [-d] [-t] [-s] [-x <jid>] [-c <config>] [-h]"
    echo -e "\t-d           Print debug messages"
    echo -e "\t-t           Start HTTP listener"
    echo -e "\t-s           Start HTTPS listener (default: HTTP)"
    echo -e "\t-x <jid>     Start XMPP listener with given JID"
    echo -e "\t-c <config>  Set alternate config file (default: example.config)"
    echo -e "\t-h           Print this help"

}

function join {
    local IFS="$1"; shift; echo "$*"
}

ssldir=${basedir}/priv/ssl
cacertfile=$ssldir/cowboy-ca.crt
certfile=$ssldir/server.crt
keyfile=$ssldir/server.key
htpasswd=${basedir}/priv/htpasswd

name=
debug=info
config=${basedir}/priv/configs/default.config
idx=-1
listeners[0]="{http, occi_http, [{port, 8080}]}"
epasswd="{htpasswd, \"$htpasswd\" }"
while getopts ":hdtsc:x:p:" opt; do
    case $opt in
	d)
	    debug=debug
	    set -x
	    ;;
	t)
	    idx=$(( $idx + 1 ))
	    listeners[$idx]="{http, occi_http, [{port, 8080}]}"
	    ;;
	s)
	    idx=$(( $idx + 1 ))
	    listeners[$idx]="{https, occi_https, [{port, 8443}, {cacertfile, \"$cacertfile\"}, {certfile, \"$certfile\"}, {keyfile, \"$keyfile\"}]}"
	    ;;
	x)
	    jid=$OPTARG
	    case x$jid in
		x)
		    true
		    ;;
		*@local)
		    idx=$(( $idx + 1 ))
		    listeners[$idx]="{xmpplocal, occi_xmpp_client, [{jid, \"$jid\"}]}"
		    epasswd="{xmpp, \"\" }"
		    ;;
		*)
		    read -s -p "Password:" passwd
		    idx=$(( $idx + 1 ))
		    listeners[$idx]="{xmppc, occi_xmpp_client, [{jid, \"$jid\"}, {passwd, \"$passwd\"}]}"
		    epasswd="{xmpp, \"\" }"
		    ;;
	    esac
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

listeners=$(echo "["; join , "${listeners[@]}"; echo "]")
#echo $listeners
#exit 0

cd ${basedir}
exec erl -pa $PWD/ebin \
    $depsbin/*/ebin \
    -boot start_sasl \
    -config $config \
    -kernel error_logger silent \
    -lager handlers "[{lager_console_backend, $debug}]" \
    -epasswd mod "$epasswd" \
    -occi listeners "$listeners" \
    $debug_app -s occi
