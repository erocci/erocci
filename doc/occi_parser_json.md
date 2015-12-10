

# Module occi_parser_json #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

An event based JSON parser.

Copyright (c) (C) 2013, Jean Parpaillon

This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.

__Behaviours:__ [`gen_fsm`](gen_fsm.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#action-3">action/3</a></td><td></td></tr><tr><td valign="top"><a href="#action_attribute-3">action_attribute/3</a></td><td></td></tr><tr><td valign="top"><a href="#action_attributes-3">action_attributes/3</a></td><td></td></tr><tr><td valign="top"><a href="#action_req-3">action_req/3</a></td><td></td></tr><tr><td valign="top"><a href="#collection-3">collection/3</a></td><td></td></tr><tr><td valign="top"><a href="#eof-3">eof/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#link-3">link/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_attribute-3">link_attribute/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_attributes-3">link_attributes/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_id-3">link_id/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_kind-3">link_kind/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_mixin-3">link_mixin/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_mixins-3">link_mixins/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_source-3">link_source/3</a></td><td></td></tr><tr><td valign="top"><a href="#link_target-3">link_target/3</a></td><td></td></tr><tr><td valign="top"><a href="#links-3">links/3</a></td><td></td></tr><tr><td valign="top"><a href="#links_req-3">links_req/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin-3">mixin/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_applies-3">mixin_applies/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_apply-3">mixin_apply/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_depend-3">mixin_depend/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_depends-3">mixin_depends/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_location-3">mixin_location/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_scheme-3">mixin_scheme/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_term-3">mixin_term/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixin_title-3">mixin_title/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixins-3">mixins/3</a></td><td></td></tr><tr><td valign="top"><a href="#mixins_req-3">mixins_req/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_action-3">parse_action/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_collection-2">parse_collection/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_entity-3">parse_entity/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_user_mixin-2">parse_user_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource-3">resource/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_attribute-3">resource_attribute/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_attributes-3">resource_attributes/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_id-3">resource_id/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_kind-3">resource_kind/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_link-3">resource_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_links-3">resource_links/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_mixin-3">resource_mixin/3</a></td><td></td></tr><tr><td valign="top"><a href="#resource_mixins-3">resource_mixins/3</a></td><td></td></tr><tr><td valign="top"><a href="#resources-3">resources/3</a></td><td></td></tr><tr><td valign="top"><a href="#resources_req-3">resources_req/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="action-3"></a>

### action/3 ###

`action(Token, From, Ctx) -> any()`

<a name="action_attribute-3"></a>

### action_attribute/3 ###

`action_attribute(Token, From, Parser) -> any()`

<a name="action_attributes-3"></a>

### action_attributes/3 ###

`action_attributes(Token, From, Parser) -> any()`

<a name="action_req-3"></a>

### action_req/3 ###

`action_req(Token, From, Parser) -> any()`

<a name="collection-3"></a>

### collection/3 ###

`collection(Token, From, Parser) -> any()`

<a name="eof-3"></a>

### eof/3 ###

`eof(E, F, Ctx) -> any()`

<a name="init-3"></a>

### init/3 ###

`init(Token, From, Ctx) -> any()`

<a name="link-3"></a>

### link/3 ###

`link(Token, From, Ctx) -> any()`

<a name="link_attribute-3"></a>

### link_attribute/3 ###

`link_attribute(Token, From, Parser) -> any()`

<a name="link_attributes-3"></a>

### link_attributes/3 ###

`link_attributes(Token, From, Parser) -> any()`

<a name="link_id-3"></a>

### link_id/3 ###

`link_id(Token, From, Parser) -> any()`

<a name="link_kind-3"></a>

### link_kind/3 ###

`link_kind(Token, From, Parser) -> any()`

<a name="link_mixin-3"></a>

### link_mixin/3 ###

`link_mixin(Token, From, Parser) -> any()`

<a name="link_mixins-3"></a>

### link_mixins/3 ###

`link_mixins(Token, From, Ctx) -> any()`

<a name="link_source-3"></a>

### link_source/3 ###

`link_source(Token, From, Parser) -> any()`

<a name="link_target-3"></a>

### link_target/3 ###

`link_target(Token, From, Parser) -> any()`

<a name="links-3"></a>

### links/3 ###

`links(Token, From, Parser) -> any()`

<a name="links_req-3"></a>

### links_req/3 ###

`links_req(Token, From, Ctx) -> any()`

<a name="mixin-3"></a>

### mixin/3 ###

`mixin(Token, From, Ctx) -> any()`

<a name="mixin_applies-3"></a>

### mixin_applies/3 ###

`mixin_applies(Token, From, Ctx) -> any()`

<a name="mixin_apply-3"></a>

### mixin_apply/3 ###

`mixin_apply(Token, From, Parser) -> any()`

<a name="mixin_depend-3"></a>

### mixin_depend/3 ###

`mixin_depend(Token, From, Parser) -> any()`

<a name="mixin_depends-3"></a>

### mixin_depends/3 ###

`mixin_depends(Token, From, Ctx) -> any()`

<a name="mixin_location-3"></a>

### mixin_location/3 ###

`mixin_location(Token, From, Parser) -> any()`

<a name="mixin_scheme-3"></a>

### mixin_scheme/3 ###

`mixin_scheme(Token, From, Parser) -> any()`

<a name="mixin_term-3"></a>

### mixin_term/3 ###

`mixin_term(Token, From, Parser) -> any()`

<a name="mixin_title-3"></a>

### mixin_title/3 ###

`mixin_title(Token, From, Parser) -> any()`

<a name="mixins-3"></a>

### mixins/3 ###

`mixins(Token, From, Parser) -> any()`

<a name="mixins_req-3"></a>

### mixins_req/3 ###

`mixins_req(Token, From, Ctx) -> any()`

<a name="parse_action-3"></a>

### parse_action/3 ###

`parse_action(Data, Env, Action) -> any()`

<a name="parse_collection-2"></a>

### parse_collection/2 ###

`parse_collection(Data, Env) -> any()`

<a name="parse_entity-3"></a>

### parse_entity/3 ###

`parse_entity(Data, Env, Occi_resource) -> any()`

<a name="parse_user_mixin-2"></a>

### parse_user_mixin/2 ###

`parse_user_mixin(Data, Env) -> any()`

<a name="request-3"></a>

### request/3 ###

`request(Token, From, Ctx) -> any()`

<a name="resource-3"></a>

### resource/3 ###

`resource(Token, From, Ctx) -> any()`

<a name="resource_attribute-3"></a>

### resource_attribute/3 ###

`resource_attribute(Token, From, Parser) -> any()`

<a name="resource_attributes-3"></a>

### resource_attributes/3 ###

`resource_attributes(Token, From, Parser) -> any()`

<a name="resource_id-3"></a>

### resource_id/3 ###

`resource_id(Token, From, Parser) -> any()`

<a name="resource_kind-3"></a>

### resource_kind/3 ###

`resource_kind(Token, From, Parser) -> any()`

<a name="resource_link-3"></a>

### resource_link/3 ###

`resource_link(Token, From, Parser) -> any()`

<a name="resource_links-3"></a>

### resource_links/3 ###

`resource_links(Token, From, Ctx) -> any()`

<a name="resource_mixin-3"></a>

### resource_mixin/3 ###

`resource_mixin(Token, From, Parser) -> any()`

<a name="resource_mixins-3"></a>

### resource_mixins/3 ###

`resource_mixins(Token, From, Ctx) -> any()`

<a name="resources-3"></a>

### resources/3 ###

`resources(Token, From, Parser) -> any()`

<a name="resources_req-3"></a>

### resources_req/3 ###

`resources_req(Token, From, Ctx) -> any()`

