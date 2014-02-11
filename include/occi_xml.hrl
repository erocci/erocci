-define(occi_ns, 'http://schemas.ogf.org/occi').
-define(xmlschema_ns, 'http://www.w3.org/2001/XMLSchema').
-define(xlink_ns, 'http://www.w3.org/2008/06/xlink').

-define(extension, #xmlel{ns=?occi_ns, name=extension}).
-define(extensionEnd, #xmlendtag{ns=?occi_ns, name=extension}).

-define(component, #xmlel{ns=?occi_ns, name=component}).
-define(componentEnd, #xmlendtag{ns=?occi_ns, name=component}).

-define(collection, #xmlel{ns=?occi_ns, name=collection}).
-define(collectionEnd, #xmlendtag{ns=?occi_ns, name=collection}).

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

-define(simpleDef, #xmlel{ns=?xmlschema_ns}).
-define(simpleDefEnd, #xmlendtag{ns=?xmlschema_ns}).
