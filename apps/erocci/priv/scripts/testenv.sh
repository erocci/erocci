export occi_srv=http://localhost:8080
export occi_jid="user-1@localhost"
export curl_opts="-s -w %{http_code} -f -o /dev/null"

export RED="\033[31;1m"
export GREEN="\033[32;1m"
export NORM="\033[0m"
export COLS=$(tput cols)
#export COLS=$(
#    cols=$(tput cols);
#    if [ $cols -gt 80 ]; then
#	echo 80;
#    else
#	echo $cols
#    fi)

ok() {
    tput hpa $(( $COLS - 4 ))
    echo -e $GREEN "OK" $NORM
    tput hpa 0

    true
}

fail() {
    err=$1
    tput hpa $(( $COLS - 6 ))
    echo -e $RED "FAIL" $NORM
    tput hpa 0

    false
}

norm_url() {
    case ${1} in
	http://*)
	    echo -n ${1}
	    ;;
	/*)
	    echo -n ${occi_srv}${1}
	    ;;
	*)
	    echo -n ${occi_srv}/${1}
	    ;;
    esac	
}

post() {
    expect=$1
    url=$(norm_url $2)
    ct=$3
    content=$4

    echo -n "POST ${url}... "
    
    ret=$(echo -e "${content}" | curl ${curl_opts} ${CURL_OPTS} -X POST --data-binary @- -H "content-type: ${ct}" ${url})
    echo -n ${ret}
    if [ "${ret}" = "${expect}" ]; then
	ok
    else
	fail "${ret}"
    fi
}

put() {
    expect=$1
    url=$(norm_url $2)
    ct=$3
    content=$4

    echo -n "PUT ${url}... "

    ret=$(echo -e "${content}" | curl ${curl_opts} ${CURL_OPTS} -X PUT --data-binary @- -H "content-type: ${ct}" ${url})
    echo -n ${ret}
    if [ "${ret}" = "${expect}" ]; then
	ok
    else
	fail "${ret}"
    fi    
}

put_h() {
    expect=$1
    url=$(norm_url $2)
    ct=$3
    content=$4

    echo -n "PUT ${url}... "
    
    ret=$(echo -e "${content}" \
	  | curl ${curl_opts} ${CURL_OPTS} -X PUT \
		 -H "content-type: ${ct}" \
		 $(for line in "${content[@]}"; do echo '-H' "${line}"; done) \
		 ${url})
    echo -n ${ret}
    if [ "${ret}" = "${expect}" ]; then
    	ok
    else
    	fail "${ret}"
    fi    
}

get() {
    expect=$1
    url=$(norm_url $2)
    ct=$3

    echo -n "GET ${url}... "
    
    ret=$(curl ${curl_opts} ${CURL_OPTS} -H "accept: ${ct}" ${url})
    echo -n ${ret}
    if [ "${ret}" = "${expect}" ]; then
	ok
    else
	fail "${ret}"
    fi        
}

delete() {
    expect=$1
    url=$(norm_url $2)

    echo -n "DELETE ${url}... "
    
    if [ -z "$3" ]; then
       ret=$(curl ${curl_opts} ${CURL_OPTS} -X DELETE ${url})
    else
	ct=$3
	content=$4
	
	ret=$(echo -e "${content}" | curl ${curl_opts} ${CURL_OPTS} -X DELETE --data-binary @- -H "content-type: ${ct}" ${url})
    fi

    echo -n ${ret}
    if [ "${ret}" = "${expect}" ]; then
	ok
    else
	fail "${ret}"
    fi
}

iq_get() {
    node=${1}

    echo "IQ get: ${node}"
}

iq_set() {
    expect=${1}
    node=${2}
    content=${3}
    
    echo "IQ set: ${node}"
}
