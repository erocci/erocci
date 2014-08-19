#!/bin/bash

. $(dirname $0)/../testenv.sh

delete 204 '/-/?category=http%3A%2F%2Fschemas.example.org%2Focci%23jsonmixin'
