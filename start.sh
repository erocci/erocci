#!/bin/bash
basedir=$(cd $(dirname $0) && pwd)

function usage() {
    #echo "Usage: $0 [-d] [-t] [-s] [-x <jid>] [-c <config>] [-h]"
	echo "Usage: $0 [-d] [-t] [-s] [-c <config>] [-n <node_name>][-h]"
    echo -e "\t-d           Print debug messages"
    echo -e "\t-q           Print only error messages"
    echo -e "\t-t           Start HTTP listener"
    echo -e "\t-s           Start HTTPS listener (default: HTTP)"
    #echo -e "\t-x <jid>     Start XMPP listener with given JID"
    echo -e "\t-c <config>  Set alternate config file (default: example.config)"
    echo -e "\t-n <name>    Set node name for distributed mode (default: node1)"
    echo -e "\t-h           Print this help"

}

function join {
    local IFS="$1"; shift; echo -n "$*"
}

appdir=$basedir/apps/erocci
ssldir=$appdir/priv/ssl
cacertfile=$ssldir/cowboy-ca.crt
certfile=$ssldir/server.crt
keyfile=$ssldir/server.key

name=node1
debug=info
configdir=$basedir/config
config=$configdir/sys.config
idx=-1
http_listener="{http, erocci_http, [{port, 8080}]}"
listeners[0]=${http_listener}
#while getopts ":hdqtsc:x:p:" opt; do
while getopts ":hdqtsc:x:n:" opt; do
    case $opt in
	d)
	    debug=debug
	    set -x
	    ;;
	q)
	    debug=error
	    ;;
	t)
	    idx=$(( $idx + 1 ))
	    listeners[$idx]=${http_listener}
	    ;;
	s)
	    idx=$(( $idx + 1 ))
	    listeners[$idx]="{https, erocci_https, [{port, 8443}, {cacertfile, \"$cacertfile\"}, {certfile, \"$certfile\"}, {keyfile, \"$keyfile\"}]}"
	    ;;
	x)
		echo "XMPP listener disabled"
		usage
		exit 1
	    #jid=$OPTARG
	    # case x$jid in
		# x)
		#     true
		#     ;;
		# *@local)
		#     idx=$(( $idx + 1 ))
		#     listeners[$idx]="{xmpplocal, occi_xmpp_client, [{jid, \"$jid\"}, {auth, {xmpp, []}}]}"
		#     ;;
		# *)
		#     read -s -p "Password:" passwd
		#     idx=$(( $idx + 1 ))
		#     listeners[$idx]="{xmppc, occi_xmpp_client, [{jid, \"$jid\"}, {passwd, \"$passwd\"}, {auth, {xmpp, []}}]}"
		#     ;;
	    # esac
	    ;;
	c)
	    if [ -e $OPTARG ]; then
			case $OPTARG in
				/*)
					config=$OPTARG
					;;
				*)
					config=`pwd`/$OPTARG
					;;
			esac
	    elif [ -e ${configdir}/$OPTARG ]; then
			config=${configdir}/$OPTARG
	    else
			echo "Could not find config: "$OPTARG
			exit 1
	    fi
	    ;;
	n)
		name=$OPTARG
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

listeners=$(echo -n "["; join , "${listeners[@]}"; echo -n "]")

cd ${basedir}
exec erl -pa $basedir/ebin -pa $basedir/deps/*/ebin \
     -boot start_sasl \
     -config $config \
     -erocci_core listeners "$listeners" \
	 -sname $name \
	 $ERL_OPTS -s erocci
