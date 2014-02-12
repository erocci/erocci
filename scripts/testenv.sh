export occi_srv=http://localhost:8080
export curl_opts="-s -w %{http_code} -f -o /dev/null "

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

    exit 0
}

fail() {
    err=$1
    tput hpa $(( $COLS - 6 ))
    echo -e $RED "FAIL" $NORM
    tput hpa 0

    exit 1
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
    
    ret=$(echo "${content}" | curl ${curl_opts} -X POST --data @- -H "content-type: ${ct}" ${url})
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
    
    ret=$(echo "${content}" | curl ${curl_opts} -X PUT --data @- -H "content-type: ${ct}" ${url})
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
    
    ret=$(curl ${curl_opts} -H "accept: ${ct}" ${url})
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
       ret=$(curl ${curl_opts} -X DELETE ${url})
    else
	ct=$3
	content=$4
	
	ret=$(echo "${content}" | curl ${curl_opts} -X DELETE --data @- -H "content-type: ${ct}" ${url})
    fi

    echo -n ${ret}
    if [ "${ret}" = "${expect}" ]; then
	ok
    else
	fail "${ret}"
    fi
}
