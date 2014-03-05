#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/
get 200 ${id} "text/plain"
