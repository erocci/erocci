# erocci

erocci is a framework for building OCCI like API, with the following objectives:
* 100% compliance with OCCI specifications (core and rendering)
* scalability
* clear separation between 
  * rendering: text/occi, text/plain, application/occi+json, application/occi+xml, etc.
  * transport: HTTP, XMPP
  * backends: storage (Mnesia, etc.), "procci" to other APIs
* small and heavily tested

[![Build Status](https://travis-ci.org/erocci/erocci.svg?branch=master)](https://travis-ci.org/erocci/erocci)

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

### Ubuntu / Debian

```
# wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && sudo dpkg -i erlang-solutions_1.0_all.deb
# sudo apt-get update
# sudo apt-get install build-essential libssl-dev libexpat1-dev libaprutil1-dev libavahi-compat-libdnssd-dev erlang rebar
```

If you've downloaded the sources from git, you will also need autotools:
```
sudo apt-get install autoconf automake libtool git
```

### Fedora

```
yum install erlang rebar curl gcc openssl openssl-devel  mod_ssl libxml2-devel gcc-c++ expat-devel apr-util-devel avahi-compat-libdns_sd-devel
yum install erlang rebar curl gcc openssl openssl-devel mod_ssl libxml2-devel gcc-c++ expat-devel apr-util-devel
```

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
