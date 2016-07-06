

# Documentation for erocci #

Copyright (c) 2015 Inria

__Version:__ 1.0

__Authors:__ Jean Parpaillon ([`jean.parpaillon@inria.fr`](mailto:jean.parpaillon@inria.fr)).

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
[README](../tools/docker/README.md)

## Architecture

[erocci Architecture diagram](https://github.com/erocci/erocci/blob/master/doc/erocci.svg)

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

Build:

```
$ make
```

## Running

You can define your OCCI API with a xml file.
For instance, the following file defines several kinds : blog, author, user, entry.

```
<?xml version="1.0" encoding="UTF-8"?>
<occi:extension xmlns:occi="http://schemas.ogf.org/occi"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"
		xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
		xsi:schemaLocation="http://example.org/occi occi.xsd " name="A REST blog"
		status="experimental" version="1">
  
  <occi:kind scheme="http://example.org/occi/test#" term="blog" title="A Blog">
    <occi:parent scheme="http://schemas.ogf.org/occi/core#" term="resource" />
    <occi:attribute name="example.blog.title" type="xs:string" title="Blog title"
		    use="required" />
  </occi:kind>
  <occi:kind scheme="http://example.org/occi/test#" term="entry" title="entry">
    <occi:parent scheme="http://schemas.ogf.org/occi/core#" term="resource" />
    <occi:attribute name="blog.entry.title" type="xs:string" title="Blog entry title"
		    use="required" />
    <occi:attribute name="blog.entry.date" type="xs:string" title="Creation date"
		    use="optional" />
    <occi:attribute name="blog.entry.content" type="xs:string" title="Entry content"
		    use="required" />
  </occi:kind>

  <occi:kind scheme="http://example.org/occi/test#" term="user" title="Blog user">
    <occi:parent scheme="http://schemas.ogf.org/occi/core#" term="resources" />
    <occi:attribute name="blog.user.name" type="xs:string" title="User name"
		    use="required" />
    <occi:attribute name="blog.user.email" type="xs:string" title="User email" />
    <occi:attribute name="blog.user.role" type="xs:string" title="User role" />
  </occi:kind>

  <occi:kind scheme="http://example.org/occi/test#" term="author" title="Author">
    <occi:parent scheme="http://schemas.ogf.org/occi/core#" term="link" />
  </occi:kind>
</occi:extension>
```
Then, you have to edit the configuring file of erocci at erocci/config/sys.config
Here, you can insert the path to the xml file defining you API in several ways :

- reading from the absolute path :
```
{schema, "/absolute/path/to.xml"}
```
- reading from erocci_backend_mensia/priv/path/to.xml:
```
{schema, {priv_dir, "path/to.xml"}}
```
- reading from a distant repository (for instance [http://github.com/occiware/occi-schemas](http://github.com/occiware/occi-schemas)):
```
{schema, {extension, "http://example.org/occi/schema#"}}
```

Finally, start erocci with:

```
$ ./start.sh
```
(you can use ./start.sh --help to display the help)


## Mailing lists

* [erocci-info@ow2.org](mailto:erocci-info@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)
* [erocci-dev@ow2.org](mailto:erocci-dev@ow2.org) - Subscribe [here](http://forge.ow2.org/mail/?group_id=429)

## CI

Visit CI results on http://travis-ci.org/erocci/erocci

## Troubleshooting ##
If the server always display a 500 error, check you version of erlang (>18).
Then, you need to clean the deps/ folder with :
 
```
$ rm -rf /deps
```
Then you can recompile with 

```
$ make
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci.md" class="module">erocci</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_acl.md" class="module">erocci_acl</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_acls.md" class="module">erocci_acls</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_backend.md" class="module">erocci_backend</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_backends.md" class="module">erocci_backends</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_config.md" class="module">erocci_config</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_core.md" class="module">erocci_core</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_core_sup.md" class="module">erocci_core_sup</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_creds.md" class="module">erocci_creds</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_errors.md" class="module">erocci_errors</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_filter.md" class="module">erocci_filter</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_listener.md" class="module">erocci_listener</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_listeners.md" class="module">erocci_listeners</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_node.md" class="module">erocci_node</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_store.md" class="module">erocci_store</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/erocci_sup.md" class="module">erocci_sup</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi.md" class="module">occi</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_action.md" class="module">occi_action</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_attribute.md" class="module">occi_attribute</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_base_type.md" class="module">occi_base_type</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_category.md" class="module">occi_category</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_collection.md" class="module">occi_collection</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_dl.md" class="module">occi_dl</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_entity.md" class="module">occi_entity</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_extension.md" class="module">occi_extension</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_invoke.md" class="module">occi_invoke</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_kind.md" class="module">occi_kind</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_link.md" class="module">occi_link</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_mixin.md" class="module">occi_mixin</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_models.md" class="module">occi_models</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_http.md" class="module">occi_parser_http</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_json.md" class="module">occi_parser_json</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_text.md" class="module">occi_parser_text</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_parser_xml.md" class="module">occi_parser_xml</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_json.md" class="module">occi_renderer_json</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_text.md" class="module">occi_renderer_text</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_uri.md" class="module">occi_renderer_uri</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_renderer_xml.md" class="module">occi_renderer_xml</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_rendering.md" class="module">occi_rendering</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_resource.md" class="module">occi_resource</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_sup.md" class="module">occi_sup</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_type.md" class="module">occi_type</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_uri.md" class="module">occi_uri</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_utils.md" class="module">occi_utils</a></td></tr>
<tr><td><a href="http://github.com/erocci/erocci/blob/master/doc/occi_xml.md" class="module">occi_xml</a></td></tr></table>

