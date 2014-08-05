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
        basedir = os.path.join(dirname, "..", "..")
        path = os.path.join(basedir, "priv", "schemas", "occi-infrastructure.xml")
        content = ''
        with open(path, 'r') as fh:
            for line in fh:
                content = content + line
        return content

    def __init__(self):
        bus_name = dbus.service.BusName('org.ow2.erocci.backend.LibvirtBackend', bus=dbus.SessionBus())
        dbus.service.Object.__init__(self, bus_name, '/')

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature='a{sv}', out_signature='av')
    def init(self, opts):
        return dbus.types.Array(
            dbus.types.String(LibvirtService.get_schema(), variant_level=1),
            'v')

    @dbus.service.method(erocci.BACKEND_IFACE)
    def terminate(self):
        Gtk.main_quit()
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature=erocci.SIG_NODE)
    def save(self, node_tuple):
        node = erocci.OcciNode.from_dbus(node_tuple)
        print "Save node: %s\n" % (node)
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature=erocci.SIG_NODE)
    def update(self, node_tuple):
        node = erocci.OcciNode.from_dbus(node_tuple)
        print "Update node: %s\n" % (node)
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature='s')
    def delete(self, url):
        print "Delete node: %s\n" % url
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature='s', out_signature=erocci.SIG_NODE)
    def find(self, url):
        if url == 'resources/resource1':
            return erocci.OcciNode(url, 'jean', erocci.TYPE_RESOURCE).to_dbus()
        elif url == '':
            return erocci.OcciNode(url, '', erocci.TYPE_UNBOUNDED_COLL).to_dbus()
        return

    @dbus.service.method(erocci.BACKEND_IFACE, in_signature='s', out_signature=erocci.SIG_NODE)
    def load(self, url):
        if url == 'resources/resource1':
            kind = erocci.OcciCategory('http://schemas.ogf.org/occi/infrastructure#', 'compute')
            attrs = {'occi.compute.cores', 4}
            return erocci.OcciResource(url, kind, attributes=attrs).to_dbus()
        elif url == '':
            return erocci.OcciCollection(url, ['resources/resource1']).to_dbus()
        return
        
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
