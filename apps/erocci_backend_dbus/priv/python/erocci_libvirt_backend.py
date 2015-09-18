#!/usr/bin/env python
#
# @copyright (c) 2014 Jean Parpaillon
# @author Jean Parpaillon <jean.parpaillon@free.fr>
#
import os
import sys
import signal
import libvirt

from gi.repository import Gtk
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop

import erocci

class LibvirtService(dbus.service.Object):

    @staticmethod
    def get_schema():
        dirname = os.path.dirname(os.path.abspath(__file__))
        basedir = os.path.join(dirname, "..", "..", "..", "..")
        path = os.path.abspath(os.path.join(basedir, "priv", "schemas", "occi-infrastructure.xml"))
        content = ''
        with open(path, 'r') as fh:
            for line in fh:
                content = content + line
        print "Load schema from %s\n" % (path,)
        return content

    def __init__(self):
        bus_name = dbus.service.BusName('org.ow2.erocci.backend.LibvirtBackend', bus=dbus.SessionBus())
        dbus.service.Object.__init__(self, bus_name, '/')

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature='a{sv}', out_signature='av')
    def init(self, opts):
        return dbus.types.Array([dbus.types.UTF8String(LibvirtService.get_schema(), variant_level=1)], 'v')

    @dbus.service.method(erocci.BACKEND_IFACE)
    def terminate(self):
        Gtk.main_quit()
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature=erocci.SIG_NODE)
    def save(self, node_tuple):
        print "save(%s)\n" % (node_tuple,)
        node = erocci.OcciNode.from_dbus(node_tuple)
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature=erocci.SIG_NODE)
    def update(self, node_tuple):
        print "update(%s)\n" % (node_tuple,)
        node = erocci.OcciNode.from_dbus(node_tuple)
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature='s')
    def delete(self, url):
        print "delete(%s)\n" % (url)
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature='s', out_signature='a' + erocci.SIG_NODE)
    def find(self, url):
        print "find(%s)\n" % (url)
        if url == 'resources/resource1':
            node = erocci.OcciNode(url, 0, '', '', erocci.TYPE_RESOURCE, None)
            return dbus.types.Array([node.to_dbus()], erocci.SIG_NODE)

        elif url == '/-/':
            node = erocci.OcciNode.capabilities([])
            return dbus.types.Array([node.to_dbus()], erocci.SIG_NODE)

        elif url == '':
            node = erocci.OcciNode(url, url, '', '', erocci.TYPE_UNBOUNDED_COLL, None)
            return dbus.types.Array([node.to_dbus()], erocci.SIG_NODE)

        return dbus.types.Array([], erocci.SIG_NODE)

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature=erocci.SIG_NODE, out_signature=erocci.SIG_NODE)
    def load(self, node_tuple):
        print "load(%s)\n" % (node_tuple,)
        node = erocci.OcciNode.from_dbus(node_tuple)
        if node.has_data():
            return node.to_dbus()
        else:
            data = None
            if node._id == 'resources/resource1':
                kind = erocci.OcciCategory('http://schemas.ogf.org/occi/infrastructure#', 'compute')
                attrs = {'occi.compute.cores': 4}
                data = erocci.OcciResource(0, kind, [], attrs, [])
            elif node._id == '':
                data = erocci.OcciCollection(1, ['resources/resource1'])

            if data is None:
                raise erocci.OcciException()
            else:
                node.set_data(data)
                return node.to_dbus()

    @dbus.service.method(dbus.PROPERTIES_IFACE, in_signature='ss', out_signature='v')
    def Get(self, interface_name, property_name):
        if interface_name == erocci.BACKEND_IFACE:
            if property_name == 'schema':
                return LibvirtService.get_schema()
            else:
                raise dbus.exception.DBusException(
                    'org.ow2.erocci.UnknownProperty',
                    'The %s interface does not have %s property' % (erocci.BACKEND_IFACE, property_name))
        else:
            raise dbus.exception.DBusException(
                'org.ow2.erocci.UnknownInterface',
                'The / object does not implement the %s interface' % interface_name)

    @dbus.service.method(dbus.PROPERTIES_IFACE, in_signature='s', out_signature='a{sv}')
    def GetAll(self, interface_name):
        if interface_name == erocci.BACKEND_IFACE:
            return { 'schema': LibvirtService.get_schema() }
        else:
            raise dbus.exception.DBusException(
                'org.ow2.erocci.UnknownInterface',
                'The / object does not implement the %s interface' % interface_name)

    @dbus.service.signal(dbus.PROPERTIES_IFACE, signature='sa{sv}as')
    def PropertiesChanged(self, interface_name, changed_properties, invalidated_properties):
        pass

signal.signal(signal.SIGINT, signal.SIG_DFL)
DBusGMainLoop(set_as_default=True)
service = LibvirtService()
print "Starting Livirt erocci backend service..."
Gtk.main()
