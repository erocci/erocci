-ifndef(occi_xml_hrl).
-define(occi_xml_hrl, true).

-include("occi.hrl").

-define(occi_ns, 'http://schemas.ogf.org/occi').
-define(xlink_ns, 'http://www.w3.org/2008/06/xlink').

-define(extension, #xmlel{ns=?occi_ns, name=extension}).
-define(extensionEnd, #xmlendtag{ns=?occi_ns, name=extension}).

-define(component, #xmlel{ns=?occi_ns, name=component}).
-define(componentEnd, #xmlendtag{ns=?occi_ns, name=component}).

-define(collection, #xmlel{ns=?occi_ns, name=collection}).
-define(collectionEnd, #xmlendtag{ns=?occi_ns, name=collection}).

-define(entity, #xmlel{ns=?occi_ns, name=entity}).
-define(entityEnd, #xmlendtag{ns=?occi_ns, name=entity}).

-define(resource, #xmlel{ns=?occi_ns, name=resource}).
-define(resourceEnd, #xmlendtag{ns=?occi_ns, name=resource}).

-define(link, #xmlel{ns=?occi_ns, name=link}).
-define(linkEnd, #xmlendtag{ns=?occi_ns, name=link}).

-define(categories, #xmlel{ns=?occi_ns, name=categories}).
-define(categoriesEnd, #xmlendtag{ns=?occi_ns, name=categories}).

-define(kind, #xmlel{ns=?occi_ns, name=kind}).
-define(kindEnd, #xmlendtag{ns=?occi_ns, name=kind}).

-define(mixin, #xmlel{ns=?occi_ns, name=mixin}).
-define(mixinEnd, #xmlendtag{ns=?occi_ns, name=mixin}).

-define(action, #xmlel{ns=?occi_ns, name=action}).
-define(actionEnd, #xmlendtag{ns=?occi_ns, name=action}).

-define(parent, #xmlel{ns=?occi_ns, name=parent}).
-define(parentEnd, #xmlendtag{ns=?occi_ns, name=parent}).

-define(depends, #xmlel{ns=?occi_ns, name=depends}).
-define(dependsEnd, #xmlendtag{ns=?occi_ns, name=depends}).

-define(applies, #xmlel{ns=?occi_ns, name=applies}).
-define(appliesEnd, #xmlendtag{ns=?occi_ns, name=applies}).

-define(attribute, #xmlel{ns=?occi_ns, name=attribute}).
-define(attributeEnd, #xmlendtag{ns=?occi_ns, name=attribute}).

-define(target, #xmlel{ns=?occi_ns, name=target}).
-define(targetEnd, #xmlendtag{ns=?occi_ns, name=target}).

-define(source, #xmlel{ns=?occi_ns, name=source}).
-define(sourceEnd, #xmlendtag{ns=?occi_ns, name=source}).

-define(simpleType, #xmlel{ns=?xmlschema_ns, name=simpleType}).
-define(simpleTypeEnd, #xmlendtag{ns=?xmlschema_ns, name=simpleType}).

-define(restriction, #xmlel{ns=?xmlschema_ns, name=restriction}).
-define(restrictionEnd, #xmlendtag{ns=?xmlschema_ns, name=restriction}).

-define(list, #xmlel{ns=?xmlschema_ns, name=list}).
-define(listEnd, #xmlendtag{ns=?xmlschema_ns, name=list}).

-define(union, #xmlel{ns=?xmlschema_ns, name=union}).
-define(unionEnd, #xmlendtag{ns=?xmlschema_ns, name=union}).

-define(minInclusive, #xmlel{ns=?xmlschema_ns, name=minInclusive}).
-define(minInclusiveEnd, #xmlendtag{ns=?xmlschema_ns, name=minInclusive}).

-define(minExclusive, #xmlel{ns=?xmlschema_ns, name=minExclusive}).
-define(minExclusiveEnd, #xmlendtag{ns=?xmlschema_ns, name=minExclusive}).

-define(maxInclusive, #xmlel{ns=?xmlschema_ns, name=maxInclusive}).
-define(maxInclusiveEnd, #xmlendtag{ns=?xmlschema_ns, name=maxInclusive}).

-define(maxExclusive, #xmlel{ns=?xmlschema_ns, name=maxExclusive}).
-define(maxExclusiveEnd, #xmlendtag{ns=?xmlschema_ns, name=maxExclusive}).

-define(totalDigits, #xmlel{ns=?xmlschema_ns, name=totalDigits}).
-define(totalDigitsEnd, #xmlendtag{ns=?xmlschema_ns, name=totalDigits}).

-define(fractionDigits, #xmlel{ns=?xmlschema_ns, name=fractionDigits}).
-define(fractionDigitsEnd, #xmlendtag{ns=?xmlschema_ns, name=fractionDigits}).

-define(length, #xmlel{ns=?xmlschema_ns, name=length}).
-define(lengthEnd, #xmlendtag{ns=?xmlschema_ns, name=length}).

-define(minLength, #xmlel{ns=?xmlschema_ns, name=minLength}).
-define(minLengthEnd, #xmlendtag{ns=?xmlschema_ns, name=minLength}).

-define(maxLength, #xmlel{ns=?xmlschema_ns, name=maxLength}).
-define(maxLengthEnd, #xmlendtag{ns=?xmlschema_ns, name=maxLength}).

-define(enumeration, #xmlel{ns=?xmlschema_ns, name=enumeration}).
-define(enumerationEnd, #xmlendtag{ns=?xmlschema_ns, name=enumeration}).

-define(whiteSpace, #xmlel{ns=?xmlschema_ns, name=whiteSpace}).
-define(whiteEnd, #xmlendtag{ns=?xmlschema_ns, name=whiteSpace}).

-define(pattern, #xmlel{ns=?xmlschema_ns, name=pattern}).
-define(patternEnd, #xmlendtag{ns=?xmlschema_ns, name=pattern}).

-endif.
