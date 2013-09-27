% -*- mode: erlang -*-
Nonterminals
headers header category_values category_value category_opt_attrs
category_opt_attr link_values link_value link_opt_attrs
link_opt_attr location_values target_attr scheme_attr class_attr
title_attr rel_attr location_attr c_attributes_attr c_attributes c_attribute
attribute_name_props actions_attr self_attr category_attr attributes_attr
attribute_kv_attr attribute_value_attr attribute_attr.

Terminals
'category' 'link' 'x-occi-attribute' 'x-occi-location' '?action='
'scheme' 'class' 'title' 'rel' 'location' 'attributes' 'actions' 'self'
quoted_value url path quote term string integer float attribute_name_attr
':' ',' '<' '>' ';' '=' '{' '}'.

Rootsymbol headers.

Expect 2.

headers -> header : ['$1'].
headers -> header headers : ['$1'|'$2'].

header -> 'category' ':' category_values : {categories, '$3'}.
header -> 'link' ':' link_values : {links, '$3'}.
header -> 'x-occi-attribute' ':' attributes_attr : {attributes, '$3'}.
header -> 'x-occi-location' ':' location_values : {locations, '$3'}.

category_values -> category_value : ['$1'].
category_values -> category_value ',' category_values : ['$1'|'$3'].

category_value -> term scheme_attr class_attr : {category, [unwrap('$1'),'$2','$3']}.
category_value -> term scheme_attr class_attr category_opt_attrs : {category, [unwrap('$1'),'$2','$3'|'$4']}.

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

location_values -> url : [unwrap('$1')].
location_values -> url ',' location_values : [unwrap('$1')|'$3'].

target_attr -> '<' path '>' : {target, unwrap('$2')}.
target_attr -> '<' path '?action=' term '>' : {target, unwrap('$2'), unwrap('$4')}.

scheme_attr -> ';' 'scheme' '=' quoted_value : {scheme, unwrap('$4')}.

class_attr -> ';' 'class' '=' quoted_value : {class, unwrap('$4')}.

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
attribute_value_attr -> integer : list_to_integer(binary_to_list(unwrap('$1'))).
attribute_value_attr -> float : list_to_integer(binary_to_list(unwrap('$1'))).

Erlang code.

unwrap({_Cat,_Pos,Val}) ->
    Val.
