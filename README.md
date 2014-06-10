# erocci

erocci is a framework for building OCCI like API, with the following objectives:
* 100% compliance with OCCI specifications (core and rendering)
* scalability
* clear separation between 
  * rendering: text/occi, text/plain, application/occi+json, application/occi+xml, etc.
  * transport: HTTP, XMPP
  * backends: storage (Mnesia, etc.), "procci" to other APIs
* small and heavily tested

[Travis-CI](http://travis-ci.org/jeanparpaillon/erocci) :: ![Travis-CI](https://secure.travis-ci.org/jeanparpaillon/erocci.png)

## Architecture

<img src="https://raw.github.com/jeanparpaillon/erocci/master/doc/erocci.png" alt="erocci Architectrue diagram" />

## Dependancies

erocci is written in erlang. It uses the very good rebar tool for
compiling, getting dependancies, etc.  It uses the erim application
for XML parsing and generation.  So, before running typing 'make',
install the following dependancies (thanks Augusto Ciuffoletti for the
notice):

* erlang/OTP, version 15b1 or greater
* openssl and headers
* libxml2 and headers
* libexpat and headers

Debian and Ubuntu: apt-get install erlang libssl-dev libexpat1-dev libxml2-dev

Fedora: yum install erlang rebar curl gcc openssl openssl-devel  mod_ssl libxml2-devel

(install also development tools if necessary : yum groupinstall "Development tools")


## Compiling

$ make

The Makefile is wrapper around rebar. Learn quickly how to use rebar
for advanced options. The tool is particularly suited for erlang
applications.

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

Visit CI results on http://travis-ci.org/jeanparpaillon/erocci
