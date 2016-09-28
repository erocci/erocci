

# Module erocci_http_handler #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Handler REST requests for erocci.

Copyright (c) (c) 2013-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allowed_methods-2">allowed_methods/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_types_accepted-2">content_types_accepted/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_types_provided-2">content_types_provided/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_resource-2">delete_resource/2</a></td><td></td></tr><tr><td valign="top"><a href="#from-2">from/2</a></td><td></td></tr><tr><td valign="top"><a href="#generate_etag-2">generate_etag/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_authorized-2">is_authorized/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_conflict-2">is_conflict/2</a></td><td></td></tr><tr><td valign="top"><a href="#malformed_request-2">malformed_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#resource_exists-2">resource_exists/2</a></td><td></td></tr><tr><td valign="top"><a href="#service_available-2">service_available/2</a></td><td></td></tr><tr><td valign="top"><a href="#to-2">to/2</a></td><td></td></tr><tr><td valign="top"><a href="#trails_all-1">trails_all/1</a></td><td></td></tr><tr><td valign="top"><a href="#trails_collections-1">trails_collections/1</a></td><td></td></tr><tr><td valign="top"><a href="#trails_query-1">trails_query/1</a></td><td>Return trail definitions.</td></tr><tr><td valign="top"><a href="#valid_entity_length-2">valid_entity_length/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allowed_methods-2"></a>

### allowed_methods/2 ###

`allowed_methods(Req, S) -> any()`

<a name="content_types_accepted-2"></a>

### content_types_accepted/2 ###

`content_types_accepted(Req, S) -> any()`

<a name="content_types_provided-2"></a>

### content_types_provided/2 ###

`content_types_provided(Req, S) -> any()`

<a name="delete_resource-2"></a>

### delete_resource/2 ###

`delete_resource(Req, S) -> any()`

<a name="from-2"></a>

### from/2 ###

`from(Req, S) -> any()`

<a name="generate_etag-2"></a>

### generate_etag/2 ###

`generate_etag(Req, S) -> any()`

<a name="init-2"></a>

### init/2 ###

`init(Req, X2) -> any()`

<a name="is_authorized-2"></a>

### is_authorized/2 ###

`is_authorized(Req, S) -> any()`

<a name="is_conflict-2"></a>

### is_conflict/2 ###

`is_conflict(Req, S) -> any()`

<a name="malformed_request-2"></a>

### malformed_request/2 ###

`malformed_request(Req, S) -> any()`

<a name="resource_exists-2"></a>

### resource_exists/2 ###

`resource_exists(Req, S) -> any()`

<a name="service_available-2"></a>

### service_available/2 ###

`service_available(Req, S) -> any()`

<a name="to-2"></a>

### to/2 ###

`to(Req, S) -> any()`

<a name="trails_all-1"></a>

### trails_all/1 ###

`trails_all(Opts) -> any()`

<a name="trails_collections-1"></a>

### trails_collections/1 ###

`trails_collections(Opts) -> any()`

<a name="trails_query-1"></a>

### trails_query/1 ###

`trails_query(Opts) -> any()`

Return trail definitions

<a name="valid_entity_length-2"></a>

### valid_entity_length/2 ###

`valid_entity_length(Req, S) -> any()`

