% -*- mode: erlang -*-
Nonterminals
request filter_req mixin_req action_req resources_req res_repr_req
 category_headers category_header attribute_headers attribute_header
link_headers link_header location_headers location_header
category_values category_value category_opt_attrs
category_opt_attr link_values link_value link_opt_attrs
link_opt_attr target_attr scheme_attr class_attr
title_attr rel_attr location_attr c_attributes_attr c_attributes c_attribute
attribute_name_props actions_attr self_attr category_attr attributes_attr
attribute_kv_attr attribute_value_attr attribute_attr.

Terminals
'__filter' '__mixin' '__action' '__resources' '__res_repr'
'category' 'link' 'x-occi-attribute' 'x-occi-location' '?action='
'scheme' 'class' 'title' 'rel' 'location' 'attributes' 'actions' 'self'
quoted_value url path quote term string integer float attribute_name_attr
':' ',' '<' '>' ';' '=' '{' '}'.

Rootsymbol request.

Expect 2.

request -> filter_req : '$1'.
request -> mixin_req : '$1'.
request -> action_req : '$1'.
request -> resources_req : '$1'.
request -> res_repr_req : '$1'.

filter_req -> '__filter' : '$1'.
filter_req -> '__filter' category_headers : '$1'.
filter_req -> '__filter' category_headers attribute_headers : '$1'.

mixin_req -> '__mixin' category_header : '$1'.

action_req -> '__action' category_header : '$1'.
action_req -> '__action' category_header attribute_headers : '$1'.

resources_req -> '__resources' location_headers : '$1'.

res_repr_req -> '__res_repr' category_headers link_headers:
		    build_res(lists:flatten('$2'), lists:flatten('$3'), []).
res_repr_req -> '__res_repr' category_headers              attribute_headers:
		    build_res(lists:flatten('$2'), [],                  lists:flatten('$3')).
res_repr_req -> '__res_repr' category_headers link_headers attribute_headers: 
		    build_res(lists:flatten('$2'), lists:flatten('$3'), lists:flatten('$4')).

category_headers -> category_header : ['$1'].
category_headers -> category_header category_headers : ['$1'|'$2'].

category_header -> 'category' ':' category_values : ['$3'].

attribute_headers -> attribute_header : ['$1'].
attribute_headers -> attribute_header attribute_headers : ['$1'|'$2'].

attribute_header -> 'x-occi-attribute' ':' attributes_attr : ['$3'].

link_headers -> link_header : ['$1'].
link_headers -> link_header link_headers : ['$1'|'$2'].
    
link_header -> 'link' ':' link_values : ['$3'].

location_headers -> location_header : ['$1'].
location_headers -> location_header location_headers : ['$1'|'$2'].

location_header -> 'x-occi-location' ':' url : '$3'.

category_values -> category_value : ['$1'].
category_values -> category_value ',' category_values : ['$1'|'$3'].

category_value -> term scheme_attr class_attr : {occi_cid, '$2', unwrap('$1'), '$3'}.
category_value -> term scheme_attr class_attr category_opt_attrs : [{occi_cid, '$2', unwrap('$1'), '$3'},'$4'].

category_opt_attrs -> category_opt_attr : ['$1'].
category_opt_attrs -> category_opt_attr category_opt_attrs : ['$1'|'$2'].

category_opt_attr -> title_attr : '$1'.
category_opt_attr -> rel_attr : '$1'.
category_opt_attr -> location_attr : '$1'.
category_opt_attr -> c_attributes_attr : '$1'.
category_opt_attr -> actions_attr : '$1'.

link_values -> link_value : ['$1'].
link_values -> link_value ',' link_value : ['$1'|'$3'].

link_value -> target_attr rel_attr : {link, ['$1','$2']}.
link_value -> target_attr rel_attr link_opt_attrs : {link, ['$1','$2'|'$3']}.

link_opt_attrs -> link_opt_attr : ['$1'].
link_opt_attrs -> link_opt_attr link_opt_attrs : ['$1'|'$2'].

link_opt_attr -> self_attr : '$1'.
link_opt_attr -> category_attr : '$1'.
link_opt_attr -> attribute_attr : '$1'.

target_attr -> '<' path '>' : {target, unwrap('$2')}.
target_attr -> '<' path '?action=' term '>' : {target, unwrap('$2'), unwrap('$4')}.

scheme_attr -> ';' 'scheme' '=' quoted_value : to_atom(unwrap('$4')).

class_attr -> ';' 'class' '=' quoted_value : to_atom(unwrap('$4')).

title_attr -> ';' 'title' '=' quoted_value : {title, unwrap('$4')}.

rel_attr -> ';' 'rel' '=' quoted_value : {rel, unwrap('$4')}.

location_attr -> ';' 'location' '=' path : {location, unwrap('$4')}.

c_attributes_attr -> ';' 'attributes' '=' quoted_value : {attributes, unwrap('$4')}.

c_attributes -> c_attribute : [{attribute, '$1'}].
c_attributes -> c_attribute ',' c_attributes : [{attribute, '$1'}|'$3'].

c_attribute -> term : 
	    [{name, unwrap('$1')}].
c_attribute -> term '{' attribute_name_props '}' : 
	    [{name, unwrap('$1')},
	     {properties, '$3'}].

attribute_name_props -> term : [unwrap('$1')].
attribute_name_props -> term ',' attribute_name_props : [unwrap('$1')|'$3'].

actions_attr -> ';' 'actions' '=' quoted_value : {actions, unwrap('$4')}.

self_attr -> ';' 'self' '=' quoted_value : {self, unwrap('$4')}.

category_attr -> ';' 'category' '=' quoted_value : {category, unwrap('$4')}.

attribute_attr -> ';' attributes_attr : '$2'.

attributes_attr -> attribute_kv_attr : ['$1'].
attributes_attr -> attribute_kv_attr ',' attributes_attr : ['$1'|'$3'].

attribute_kv_attr -> attribute_name_attr '=' attribute_value_attr : {unwrap('$1'), '$3'}.

attribute_value_attr -> quoted_value : unwrap('$1').
attribute_value_attr -> integer : unwrap('$1').
attribute_value_attr -> float : unwrap('$1').

Erlang code.

-include("occi.hrl").

-export([parse_filter/1,
	 parse_mixin/1,
	 parse_action/1,
	 parse_resources/1,
	 parse_resources_repr/1]).

%%%
%%% Grammar is context (request) sensitive.
%%% Prefix Tokens with a magic to give context
%%%
parse_filter(Tokens) ->
    parse([{token, {'__filter', 0}}|Tokens]).

parse_mixin(Tokens) ->
    parse([{token, {'__mixin', 0}}|Tokens]).

parse_action(Tokens) ->
    parse([{token, {'__action', 0}}|Tokens]).

parse_resources(Tokens) ->
    parse([{token, {'__resources', 0}}|Tokens]).

parse_resources_repr(Tokens) ->
    parse([{token, {'__res_repr', 0}}|Tokens]).

unwrap({_Cat,_Pos,Val}) ->
    Val.

to_atom(Val) ->
    list_to_atom(Val).

build_res([Category | Mixins],
	  Links,
	  Attributes) ->
    ResRef = get_resource(Category, Mixins),
    LinkRefs = get_links(ResRef, Links),
    case catch occi_resource:set_attributes(ResRef, Attributes) of
	{error, {einval, Name, Value}} ->
	    return_error(1, io_lib:format("Invalid value: ~s=~s", [Name, Value]));
	ok -> ok
    end,
    [ResRef | LinkRefs].

get_resource(#occi_cid{scheme=Scheme, term=Term, class=kind}=Category, Mixins) ->
    case occi_category_mgr:get_ref(Category) of
	undefined ->
	    return_error(1, io_lib:format("Invalid category: ~s~s", [Category#occi_cid.scheme, Category#occi_cid.term]));
	CategoryRef ->
	    MixinRefs = lists:map(fun(Id) ->
					  case occi_category_mgr:get_ref(Id) of
					      undefined ->
						  return_error(1, io_lib:format("Invalid category: ~s~s", 
										[Id#occi_cid.scheme, Id#occi_cid.term]));
					      MixinRef -> MixinRef
					  end
				  end, Mixins),
	    occi_resource:new(CategoryRef, MixinRefs)
    end;
get_resource(#occi_cid{}=Category, _Mixins) ->
    return_error(1, io_lib:format("Invalid resource class: ~s", [Category#occi_cid.class])).

get_links(_ResRef, _Links) ->
    %% TODO
    [].
