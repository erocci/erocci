# erocci

erocci is a framework for building OCCI like API, with the following objectives:
* 100% compliance with OCCI specifications (core and rendering)
* scalability
* clear separation between 
  * rendering: text/occi, text/plain, application/occi+json, application/occi+xml, etc.
  * transport: HTTP, XMPP
  * backends: storage (Mnesia, etc.), "procci" to other APIs
* small and heavily tested

[Travis-CI](http://travis-ci.org/erocci/erocci) :: ![Travis-CI](https://secure.travis-ci.org/erocci/erocci.png)

[![Project Stats](https://www.openhub.net/p/erocci/widgets/project_thin_badge.gif)](https://www.openhub.net/p/erocci)

## Architecture

<img src="https://raw.github.com/jeanparpaillon/erocci/master/doc/erocci.png" alt="erocci Architecture diagram" />

## Dependancies

erocci is written in erlang.
It uses autotools for compilation, dependancies checking, etc.

* erlang/OTP, version 17 or greater
* autoconf 2.69 or greater
* automake 1.14 or greater

Optinal features requires the following dependancies:
* htpasswd based authentication: Apache Runtime Library utils headers
* XMPP link-local support: Avahi headers

Debian and Ubuntu: apt-get install erlang rebar libssl-dev libexpat1-dev libxml2-dev libaprutil1-dev libavahi-compat-libdnssd-dev

Fedora: yum install erlang rebar curl gcc openssl openssl-devel  mod_ssl libxml2-devel gcc-c++ expat-devel apr-util-devel avahi-compat-libdns_sd-devel


Fedora: yum install erlang rebar curl gcc openssl openssl-devel mod_ssl libxml2-devel gcc-c++ expat-devel apr-util-devel


## Compiling

### Preparing sources

$ ./autogen.sh
$ ./configure 

### Building

$ make

### Customizing build

$ ./configure --help

## Running

As a framework, erocci is made to build applications and so is not
runnable by itself. Nevertheless, an example application is provided
to let you taste, huh test OCCI.

$ ./start.sh

This application will run the hello_occi application, which:
* load OCCI infrastructure extension
* start the Mnesia backend
* start an HTTP listener on port 8080

## Testing

Run scripts/testsuite.sh to create sample resources and links.

## Mailing lists

* [erocci-info@ow2.org](mailto:erocci-info@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)
* [erocci-dev@ow2.org](mailto:erocci-dev@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)

## CI

Visit CI results on http://travis-ci.org/erocci/erocci
