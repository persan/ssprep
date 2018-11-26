#!/usr/bin/env python

# example helloworld2.py

import pygtk
pygtk.require('2.0')
import gtk
from os.path import *
class HelloWorld2(gtk.Window):

    # Our new improved callback.  The data passed to this method
    # is printed to stdout.
    def callback(self, widget, data):

        print ("Hello again - was pressed")

    def On_Browse_Clicked(self, widget, data):
        fc=gtk.FileChooserDialog(title=data.prompt,
                                 parent=self,
                                 action=gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,
                                 buttons=(gtk.STOCK_CANCEL,gtk.RESPONSE_CANCEL,gtk.STOCK_OPEN,gtk.RESPONSE_OK),
                                 backend=None)
        fc.set_default_response(gtk.RESPONSE_OK)
        response = fc.run()
        if response == gtk.RESPONSE_OK:
            data.set_text(fc.get_filename())
	fc.destroy()
    def validatePath(self,widget,data):
        if exists(data.get_text()):
            pass
        else:
           pass

# another callback
    def delete_event(self, widget, event, data=None):
        self.destroy()
        return False

    def __init__(self, config):
        gtk.Window.__init__(self,gtk.WINDOW_TOPLEVEL)

        # This is a new call, which just sets the title of our
        # new window to "Hello Buttons!"
        self.set_title("Title")

        # Here we just set a handler for delete_event that immediately
        # exits GTK.
        self.connect("delete_event", self.delete_event)

        # Sets the border width of the window.
        self.set_border_width(10)

        # We create a box to pack widgets into.  This is described in detail
        # in the "packing" section. The box is not really visible, it
        # is just used as a tool to arrange widgets.
        box1 = gtk.VBox(False, 0)
        box1.show()
        self.add(box1)
        table =  gtk.Table(rows=2, columns=3, homogeneous=False)
        table.set_row_spacings(5)
        table.set_row_spacings(5)
        table.show()
        box1.pack_start(table,True, True, 0)
        table.attach(gtk.Label("Root Directory (workspace):"),
                                0,1,
                                0,1)

        self.RootDir=gtk.Entry(max=0)
        table.attach(self.RootDir,
                     1,2,
                     0,1)
        button = gtk.Button("Browse")
        self.RootDir.prompt="Root Directory"
        button.connect("clicked", self.On_Browse_Clicked, self.RootDir)
        table.attach(button,
                     2,3,
                     0,1)
        line=1
        for i in config:
           table.attach(i[0],0,1,line,line+1)
           table.attach(i[1],1,2,line,line+1)
           if len(i) == 3:
              button = gtk.Button("Browse")
              i[1].prompt=i[2]
              table.attach(button,2,3,line,line+1)
              button.connect("clicked", self.On_Browse_Clicked, i[1])
           line=line +1

        ##
        ## Add the Ok,Cancel and Help buttons
        ##

        Hbox = gtk.HBox(True, 6)
        Hbox.set_border_width(6)

        button=gtk.Button(label=None,stock=gtk.STOCK_OK)
        button.connect("clicked", self.On_OK)
        Hbox.pack_start(button,expand=True, fill=True, padding=0)

        button=gtk.Button(label=None,stock=gtk.STOCK_CANCEL)
        button.connect("clicked", self.On_Cancel)
        Hbox.pack_start(button,expand=True, fill=True, padding=0)

        button=gtk.Button(label=None,stock=gtk.STOCK_HELP)
        button.connect("clicked", self.On_Help)
        Hbox.pack_start(button,expand=True, fill=True, padding=0)
        box1.pack_start(Hbox)

        self.show_all()
    def On_OK(self,widget,data=None):
       print "On_OK"
       self.destroy()
    def On_Cancel(self,widget,data=None):
       print "On_Cancel"
       self.destroy()
    def On_Help(self,widget,data=None):
       print "On_Help"
       self.destroy()

config=[[gtk.Label("Text1"),gtk.Entry(max=0),"Textar"],
        [gtk.Label("Foo"),  gtk.CheckButton("Enabled")],
        [gtk.Label("File"), gtk.Entry(max=0)]]

class Configurations:
    def __init__(self):
        pass
    def __iter__(self):
        self.cursor = 1
        return self
    def __next__(self):
        if self.cursor == self.size:
            raise StopIteration
        ret = self.cursor
        self.cursor= self.cursor +1
        return (ret,1,"fwl")

hello = HelloWorld2(config)

