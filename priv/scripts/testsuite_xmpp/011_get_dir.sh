#!/bin/bash

. $(dirname $0)/../testenv.sh

id=/store/myresources/

iq_get result ${id}
