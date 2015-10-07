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

erocci is made up of several pluggable components. These components
are implemented as erlang/OTP applications.

* `erocci_core`: OCCI Runtime Kernel, renderers and parsers and plugin APIs
* `erocci_authnz`:  authnz plugins API
* `erocci_listener_http`: HTTP(s) protocol listener
* `erocci_backend_mnesia`: Mnesia database backend
* `erocci_backend_dbus`: D-Bus backends API, allows development of
  backends in any language supported by D-Bus

## Dependencies

erocci is written in erlang. 

* erlang/OTP, version 18 or greater
* make, autoconf (tested with 2.69), gcc
* one of libxml2 or libexpat headers

### Ubuntu / Debian

```
# wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && sudo dpkg -i erlang-solutions_1.0_all.deb
# sudo apt-get update
# sudo apt-get install build-essential erlang libxml2-dev
```

### Fedora

```
yum install erlang curl gcc libxml2-devel gcc-c++ 
```

### MAC OS X

* Requirements:
  * erlang/OTP: https://www.erlang-solutions.com/downloads/download-erlang-otp
  * TO BE COMPLETED

### Configuring and Building

Configure sources and fetch dependencies:
```
$ ./bootstrap
```

(Optional) Run configure again for specific options
```
$ ./configure
```

Build:
```
$ make
```

## Running

erocci is configured with a single config file. Several example config
files are available in: `apps/erocci/priv/configs/`

Start erocci with:
```
$ ./start.sh -c apps/erocci/priv/configs/default.config
```

## Mailing lists

* [erocci-info@ow2.org](mailto:erocci-info@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)
* [erocci-dev@ow2.org](mailto:erocci-dev@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)

## CI

Visit CI results on http://travis-ci.org/erocci/erocci
