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

## Dependencies

erocci is written in erlang. 

* erlang/OTP, version 17 or greater

Optinal features requires the following dependancies:
* htpasswd based authentication: Apache Runtime Library utils headers
* XMPP link-local support: Avahi headers

### Ubuntu / Debian

```
# wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && sudo dpkg -i erlang-solutions_1.0_all.deb
# sudo apt-get update
# sudo apt-get install build-essential libssl-dev libexpat1-dev libaprutil1-dev libavahi-compat-libdnssd-dev erlang 
```

### Fedora

```
yum install erlang curl gcc openssl openssl-devel  mod_ssl libxml2-devel gcc-c++ expat-devel apr-util-devel avahi-compat-libdns_sd-devel
yum install erlang curl gcc openssl openssl-devel mod_ssl libxml2-devel gcc-c++ expat-devel apr-util-devel
```

### Configuring and Building

erocci is made of multiple (optional) components. Selection of
components is achieved with the econfig system (http://github.com/erocci/econfig).

Options can be set:
* either during the build process
* either by filling the _build/default/.econfig file

$ make

## Running

You can start erocci with various predefined configs available in
priv/configs/ dir, for instance:

$ CONFIG=priv/configs/default.config make run

Then:

$ erocci:start().

## Mailing lists

* [erocci-info@ow2.org](mailto:erocci-info@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)
* [erocci-dev@ow2.org](mailto:erocci-dev@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)

## CI

Visit CI results on http://travis-ci.org/erocci/erocci
