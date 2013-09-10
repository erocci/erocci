% -*- mode: erlang -*-
Nonterminals
header category_values category_value category_opt_attrs
category_opt_attr link_values link_value link_opt_attrs
link_opt_attr location_values target_attr scheme_attr class_attr
title_attr rel_attr location_attr c_attributes_attr c_attributes c_attribute
attribute_name_props actions_attr self_attr category_attr attributes_attr
attribute_kv_attr attribute_value_attr attribute_attr.

Terminals
'category' 'link' 'x-occi-attribute' 'x-occi-location' '?action='
'scheme' 'class' 'title' 'rel' 'location' 'attributes' 'actions' 'self'
url path quote term string digits float attribute_name_attr
':' ',' '<' '>' ';' '=' '{' '}'.

Rootsymbol header.

Expect 2.

header -> 'category' ':' category_values : {categories, '$3'}.
header -> 'link' ':' link_values : {links, '$3'}.
header -> 'x-occi-attribute' ':' attributes_attr : {attributes, '$3'}.
header -> 'x-occi-location' ':' location_values : {locations, '$3'}.

category_values -> category_value : ['$1'].
category_values -> category_value ',' category_values : ['$1'|'$3'].

category_value -> term scheme_attr class_attr category_opt_attrs : {category, ['$1','$2','$3'|'$4']}.

category_opt_attrs -> category_opt_attr : ['$1'].
category_opt_attrs -> category_opt_attr category_opt_attrs : ['$1'|'$2'].

category_opt_attr -> title_attr : '$1'.
category_opt_attr -> rel_attr : '$1'.
category_opt_attr -> location_attr : '$1'.
category_opt_attr -> c_attributes_attr : '$1'.
category_opt_attr -> actions_attr : '$1'.

link_values -> link_value : ['$1'].
link_values -> link_value ',' link_value : ['$1'|'$3'].

link_value -> target_attr rel_attr link_opt_attrs : {link, ['$1','$2'|'$3']}.

link_opt_attrs -> link_opt_attr : ['$1'].
link_opt_attrs -> link_opt_attr link_opt_attrs : ['$1'|'$2'].

link_opt_attr -> self_attr : '$1'.
link_opt_attr -> category_attr : '$1'.
link_opt_attr -> attribute_attr : '$1'.

location_values -> url : ['$1'].
location_values -> url ',' location_values : ['$1','$3'].

target_attr -> '<' path '>' : {target, '$2'}.
target_attr -> '<' path '?action=' term '>' : {target, '$2', '$4'}.

scheme_attr -> ';' 'scheme' '=' quote url quote : {scheme, '$5'}.

class_attr -> ';' 'class' '=' quote term quote : {class, '$5'}.

title_attr -> ';' 'title' '=' string : {title, '$4'}.

rel_attr -> ';' 'rel' '=' quote url quote : {rel, '$5'}.

location_attr -> ';' 'location' '=' quote url quote : {location, '$5'}.

c_attributes_attr -> ';' 'attributes' '=' quote c_attributes quote : {attributes, '$5'}.

c_attributes -> c_attribute : [{attribute, '$1'}].
c_attributes -> c_attribute ',' c_attributes : [{attribute, '$1'}|'$3'].

c_attribute -> term : 
	    [{name, '$1'}].
c_attribute -> term '{' attribute_name_props '}' : 
	    [{name, '$1'},
	     {properties, '$3'}].

attribute_name_props -> term : ['$1'].
attribute_name_props -> term ',' attribute_name_props : ['$1'|'$3'].

actions_attr -> ';' 'actions' ',' quote location_values quote : {actions, '$5'}.

self_attr -> ';' 'self' ',' quote path quote : {self, '$5'}.

category_attr -> ';' 'category' ',' quote url quote : {category, '$5'}.

attributes_attr -> attribute_kv_attr : ['$1'].
attributes_attr -> attribute_kv_attr ',' attributes_attr : ['$1'|'$3'].

attribute_kv_attr -> attribute_name_attr '=' attribute_value_attr : {'$1', '$2'}.

attribute_value_attr -> string : '$1'.
attribute_value_attr -> digits : '$1'.
attribute_value_attr -> float : '$1'.

attribute_attr -> attributes_attr : '$1'.

Erlang code.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%
%%% Tests
%%% 
-ifdef(TEST).

parse1_test() ->
    ?assert(true).

-endif.
