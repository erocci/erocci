# Documentation for erocci #

Copyright (c) 2013-2015 Jean Parpaillon <jean.parpaillon@free.fr>
Copyright (c) 2015 Inria

__Version:__ 1.0

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

### Build

The following command will fetch dependancies and build erocci:

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

## Docker

Current version of erocci is packaged into a ready-to-use docker: see
[README](tools/docker/README.md)

## Architecture

![erocci Architecture diagram](https://raw.github.com/erocci/erocci/master/doc/erocci.png)

erocci is made up of several pluggable components. These components
are implemented as erlang/OTP applications.

* `erocci_core`: OCCI Runtime Kernel, renderers and parsers and plugin APIs
* `erocci_listener_http`: HTTP(s) protocol listener
* `erocci_backend_mnesia`: Mnesia database backend
* `erocci_backend_dbus`: D-Bus backends API, allows development of
backends in any language supported by D-Bus

## Dependencies

erocci is written in erlang.

* erlang/OTP, version 18 or greater
* make

### Ubuntu / Debian

```
# wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && sudo dpkg -i erlang-solutions_1.0_all.deb
# sudo apt-get update
# sudo apt-get install build-essential erlang
```

### Fedora

```
yum install erlang curl
```

### MAC OS X

* Requirements:
* erlang/OTP: https://www.erlang-solutions.com/downloads/download-erlang-otp

## Mailing lists

* [erocci-info@ow2.org](mailto:erocci-info@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)
* [erocci-dev@ow2.org](mailto:erocci-dev@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)

## CI

Visit CI results on http://travis-ci.org/erocci/erocci

## Modules ##
