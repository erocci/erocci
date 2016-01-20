

# Documentation for erocci #

Copyright (c) 2013-2014 Jean Parpaillon

Copyright (c) 2015-2016 Inria

__Version:__ 1.0

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

__License:__ Due to recent abuse, the license has changed from APACHE
2.0 to CC BY-NC-SA which is _not_ a free software license. We hope
that an arrangement will be quickly found to come back to previous
licensing.

`erocci` is a framework for building OCCI like API, with the following objectives:
* 100% compliance with OCCI specifications (core and rendering)
* scalability
* clear separation between 
  * rendering: `text/occi`, `text/plain`, `application/occi+json`, `application/occi+xml`, etc.
* transport: HTTP, XMPP
* backends: storage (Mnesia, etc.), "procci" to other APIs
* small and heavily tested

[![Build Status](https://travis-ci.org/erocci/erocci.svg?branch=master)](https://travis-ci.org/erocci/erocci) [![Project Stats](https://www.openhub.net/p/erocci/widgets/project_thin_badge.gif)](https://www.openhub.net/p/erocci)

## Docker

Current version of erocci is packaged into a ready-to-use docker: see
[README](tools/docker/README.md)

## Architecture

![erocci Architecture diagram](https://raw.github.com/erocci/erocci/master/doc/erocci.png)

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
* make, gcc
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


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci.md" class="module">erocci</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_app.md" class="module">erocci_app</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_acl.md" class="module">occi_acl</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_action.md" class="module">occi_action</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_attribute.md" class="module">occi_attribute</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_authnz.md" class="module">occi_authnz</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_authnz_app.md" class="module">occi_authnz_app</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_authnz_sup.md" class="module">occi_authnz_sup</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_backend.md" class="module">occi_backend</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_backend_dummy.md" class="module">occi_backend_dummy</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_backend_mnesia.md" class="module">occi_backend_mnesia</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_capabilities.md" class="module">occi_capabilities</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_category_mgr.md" class="module">occi_category_mgr</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_cid.md" class="module">occi_cid</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_collection.md" class="module">occi_collection</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_config.md" class="module">occi_config</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_core_app.md" class="module">occi_core_app</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_entity.md" class="module">occi_entity</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_extension.md" class="module">occi_extension</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_hook.md" class="module">occi_hook</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_http.md" class="module">occi_http</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_http_common.md" class="module">occi_http_common</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_http_handler.md" class="module">occi_http_handler</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_https.md" class="module">occi_https</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_kind.md" class="module">occi_kind</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_known.md" class="module">occi_known</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_link.md" class="module">occi_link</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_listener.md" class="module">occi_listener</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_log_handler.md" class="module">occi_log_handler</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_mixin.md" class="module">occi_mixin</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_node.md" class="module">occi_node</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser.md" class="module">occi_parser</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_json.md" class="module">occi_parser_json</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_json0.md" class="module">occi_parser_json0</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_occi.md" class="module">occi_parser_occi</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_plain.md" class="module">occi_parser_plain</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_tests.md" class="module">occi_parser_tests</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_text.md" class="module">occi_parser_text</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_xml.md" class="module">occi_parser_xml</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer.md" class="module">occi_renderer</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_json.md" class="module">occi_renderer_json</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_json_tests.md" class="module">occi_renderer_json_tests</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_occi.md" class="module">occi_renderer_occi</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_plain.md" class="module">occi_renderer_plain</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_text.md" class="module">occi_renderer_text</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_uri_list.md" class="module">occi_renderer_uri_list</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_xml.md" class="module">occi_renderer_xml</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_request.md" class="module">occi_request</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_resource.md" class="module">occi_resource</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_scanner_json.md" class="module">occi_scanner_json</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_store.md" class="module">occi_store</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_sup.md" class="module">occi_sup</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_table_mgr.md" class="module">occi_table_mgr</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_type.md" class="module">occi_type</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_uri.md" class="module">occi_uri</a></td></tr></table>

