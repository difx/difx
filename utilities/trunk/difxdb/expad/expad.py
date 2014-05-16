#! /usr/bin/python
# coding: latin-1

#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================
__author__="Helge Rottmann"

import os
import sys
import tkMessageBox
import datetime
from Tkinter import *
from tkinter.multilistbox import *
from string import  upper

from difxdb.model import model
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.experimentaction import *
from difxdb.business.experimenttypeaction import *
from difxdb.business.versionhistoryaction import *
from difxdb.business.useraction import *
from difxutil.dbutil import *

# minimum database schema version required by this program
minSchemaMajor = 1
minSchemaMinor = 2

class GenericWindow(object):
    def __init__(self, parent=None,rootWidget=None):
        
        self.rootWidget = rootWidget
        self.parent = parent
        self.config = None
        self.editColor = "wheat"
        self.defaultBgColor = self.rootWidget["background"]
        
class MainWindow(GenericWindow):
    
    def __init__(self, parent=None, rootWidget=None):
        
        # call super class constructor
        super( MainWindow, self ).__init__(parent, rootWidget)

        self.addExperimentDlg = AddExperimentWindow(self, rootWidget)
        
        self.rootWidget.title("expad: Experiment Administration")
        
        self.expEdit = 0
        self.selectedExperimentId = None
        self.selectedExpIndex = -1
        self.cboStatusVar = StringVar()
        self.cboTypeVar = StringVar()
        self.cboUserVar = StringVar()
        self.cboReleasedByVar = StringVar()
        
        session = dbConn.session()
        # obtain all experiment stati from database
        self.expStati = []
        for status in  session.query(model.ExperimentStatus).order_by("statuscode").all():
            self.expStati.append(status.experimentstatus)
        
         # obtain all experiment types from database
        self.expTypes = []
        for type in getActiveTypes(session):
            self.expTypes.append(type.type)
            
        # obtain all enbaled users from the database
        self.users = []
        for user in getEnabledUsers(session):
            self.users.append(user.name)
            
        session.close()
            
        
            
    def show(self):
        
        self._setupWidgets()
        
        self.updateExpListbox()
        
        
       
        
    def _setupWidgets(self):
        
        self.rootWidget.rowconfigure(0, weight=1) 
        self.rootWidget.columnconfigure(0, weight=1)     
        
        Button(self.rootWidget, text="Exit", command=self.rootWidget.destroy).grid(row=10,column=10,sticky=E)
        Button(self.rootWidget, text="Refresh", command=self.show).grid(row=10,column=0,sticky=E)
        
        # frames
        frmExps = LabelFrame(self.rootWidget, text="Experiments")     
        frmDetail = LabelFrame(self.rootWidget, text="Detail", padx=5)
        frmExps.columnconfigure(0,weight=1)
        frmDetail.columnconfigure(0,weight=1)
        
        #frmExps
        col1 = ListboxColumn("experiment", 9)
        col2 = ListboxColumn("number", 4)
        col3 = ListboxColumn("status", 10)
        col4 = ListboxColumn("type", 8)
        col5 = ListboxColumn("user", 12)
        col6 = ListboxColumn("created", 14) 
        col7 = ListboxColumn("archived", 14)
        col8 = ListboxColumn("released", 14) 
        self.grdExps = MultiListbox(frmExps, col1, col2, col3, col4, col5, col6, col7, col8)
        self.grdExps.bindEvent("<ButtonRelease-1>", self.selectExpEvent)
        
        btnAddExp = Button(frmExps, text="Add experiment", command=self.addExperimentDlg.show)
             
        #frmDetail
        Label(frmDetail, text="code: ").grid(row=0,column=0, sticky=W)
        Label(frmDetail, text="number: ").grid(row=1,column=0, sticky=W)
        Label(frmDetail, text="status: ").grid(row=10,column=0, sticky=W)
        Label(frmDetail, text="type: ").grid(row=11,column=0, sticky=W)
        Label(frmDetail, text="analyst: ").grid(row=12,column=0, sticky=W)
        Label(frmDetail, text="released by: ").grid(row=13,column=0, sticky=W)
        Label(frmDetail, text="date archived: ").grid(row=15,column=0, sticky=W)
        Label(frmDetail, text="archived by: ").grid(row=20,column=0, sticky=W)
        Label(frmDetail, text = "comments: ").grid(row=25, column=0, sticky=W) 
        self.txtCode = Entry(frmDetail, text = "")
        self.txtNumber = Entry(frmDetail, text = "")
        self.cboStatus = OptionMenu (frmDetail, self.cboStatusVar, *self.expStati ,command=self.onExpDetailChange)
        self.cboType= Listbox(frmDetail, selectmode=MULTIPLE, height=4, selectforeground="white", selectbackground="dodger blue", fg="grey" )
        self.cboUser = OptionMenu(frmDetail, self.cboUserVar,  *self.users, command=self.onExpDetailChange)
        self.cboReleasedBy = OptionMenu(frmDetail, self.cboReleasedByVar,  *self.users, command=self.onExpDetailChange)
        self.txtDateArchived = Entry(frmDetail, text = "")
        self.txtArchivedBy = Entry(frmDetail, text = "")
        self.txtComment = Text(frmDetail, height=3, width=25)
        self.btnUpdate = Button(frmDetail, text="update experiment", command=self.updateExpEvent)
        self.btnDelete = Button(frmDetail, text="delete experiment", command=self.deleteExpEvent)
        
        self.cboType.insert(END, *self.expTypes)
        
        
        #arrange widgets on root widget
        frmExps.grid(row=0, column=0, sticky=E+W+N+S)
        frmDetail.grid(row=0, column=10, sticky=E+W+N+S)
        
        #arrange widgets on frmExps 
        self.grdExps.grid(row=0, column=0, sticky=E+W+N+S)
        btnAddExp.grid(row=10,column=0, sticky=E+W)
        
        #arrange widgets on frmDetail
        self.txtCode.grid(row=0, column=1, sticky=E+W)
        self.txtNumber.grid(row=1, column=1, sticky=E+W)
        self.cboStatus.grid(row=10, column=1, sticky=E+W)
        self.cboType.grid(row=11, column=1, sticky=E+W)
        self.cboUser.grid(row=12, column=1, sticky=E+W)
        self.cboReleasedBy.grid(row=13, column=1, sticky=E+W)
        self.txtDateArchived.grid(row=15, column=1, sticky=E+W)
        self.txtArchivedBy.grid(row=20, column=1, sticky=E+W)
        self.txtComment.grid(row=25, column=1, sticky=E+W)
        self.btnUpdate.grid(row=100, column=0, columnspan=2, sticky=E+W)
        self.btnDelete.grid(row=110, column=0, columnspan=2, sticky=E+W)
        
        # bind events
        self.txtNumber.bind("<KeyRelease>", self.onExpDetailChange)
        self.txtComment.bind("<KeyRelease>", self.onExpDetailChange)
        self.cboType.bind('<ButtonRelease-1>', self.onExpDetailChange)
        self.btnUpdate["state"] = DISABLED
        self.btnDelete["state"] = DISABLED
        

  
    def onExpDetailChange(self, Event):
        
        self.expEdit = 0
        currentTypes = []
        origTypes= []
        selectedExperiment = None
         
        if (self.selectedExperimentId is None):
            return
        session = dbConn.session()
        selectedExperiment = session.query(model.Experiment).filter_by(id=self.selectedExperimentId).one()
        

        # get currently selected types
        for sel in self.cboType.curselection():
		currentTypes.append(self.cboType.get(sel))
        #get original types
        for type in selectedExperiment.types:
		origTypes.append(type.type)
        
        self.expEdit += self.setChangeColor(self.txtNumber, self.txtNumber.get(), str(selectedExperiment.number).zfill(4))
        self.expEdit += self.setChangeColor(self.cboStatus, self.cboStatusVar.get(), selectedExperiment.status.experimentstatus)
        
        if  selectedExperiment.user is not None:
            self.expEdit += self.setChangeColor(self.cboUser, self.cboUserVar.get(), selectedExperiment.user.name)
        else:
            self.expEdit += self.setChangeColor(self.cboUser, self.cboUserVar.get(), "")
            
        if  selectedExperiment.releasedByUser is not None:
            self.expEdit += self.setChangeColor(self.cboReleasedBy, self.cboReleasedByVar.get(), selectedExperiment.releasedByUser.name)
        else:
            self.expEdit += self.setChangeColor(self.cboReleasedBy, self.cboReleasedByVar.get(), "")
        
        self.expEdit += self.setChangeColor(self.txtComment, self.txtComment.get(1.0, END), selectedExperiment.comment)
        
        
        if currentTypes != origTypes:
            self.expEdit += self.setChangeColor(self.cboType, "1", "0")
        else:
            self.expEdit += self.setChangeColor(self.cboType, "0", "0")
        
        if self.cboStatusVar.get() == "released" and selectedExperiment.status.experimentstatus != "released":
            self.cboReleasedBy["state"] = NORMAL
        else:
            self.cboReleasedBy["state"] = DISABLED
            
               
        if self.expEdit > 0:
            self.btnUpdate["state"] = NORMAL
            self.btnUpdate["bg"] = self.editColor
            self.btnUpdate["activebackground"] = self.editColor
        else:
            
            self.btnUpdate["state"] = DISABLED
            self.btnUpdate["bg"] = self.defaultBgColor
            self.btnUpdate["activebackground"] = self.defaultBgColor
            
        session.close()
                 
  
    def setChangeColor(self, component, componentValue, compareValue):
        
        isChange = 0
        color = self.defaultBgColor
        editColor = self.editColor
        
        if (str(none2String(compareValue)) != strip(componentValue)):
            
            component.config(bg = editColor)
            if component.__class__.__name__ == OptionMenu:
                component.config(bg=editColor, activebackground=editColor)
            isChange = 1
        else:   
            component.config(bg = color)
            if component.__class__.__name__ == OptionMenu:
                component.config(bg=color, activebackground=color)

        return isChange
    
    def updateExpListbox(self):
                
        session = dbConn.session()
        exps = session.query(model.Experiment).order_by(desc(model.Experiment.number)).all()

        self.grdExps.clearData()
               
        for exp in exps:
            
            # retrieve types
            expTypes = [] 
            for type in exp.types:
                expTypes.append(type.type)
            
            username = ""
            if exp.user is not None:
                username = exp.user.name
                
            self.grdExps.appendData((exp.code, "%04d" % exp.number, exp.status.experimentstatus, " ".join(expTypes), username, exp.dateCreated,  exp.dateArchived, exp.dateReleased))
     
        session.close()
        
        self.grdExps.update()
        self.grdExps.selection_set(self.selectedExpIndex)
             
        self.getExpDetails()
        
        
        
        
 
    def getExpDetails(self):
        '''
        Retrieve the  experiment details from the database
        for the currently selected listbox item.
        Update the detail  fields accordingly
        '''
        
        
        self.txtCode["state"] = NORMAL
        self.txtNumber["state"] = NORMAL
        self.txtDateArchived["state"] = NORMAL
        self.cboReleasedBy["state"] = NORMAL
        self.txtArchivedBy["state"] = NORMAL
        self.txtComment["state"] = NORMAL
        
        
        self.txtCode.delete(0,END)
        self.txtNumber.delete(0,END)
        self.txtDateArchived.delete(0,END)
        self.txtArchivedBy.delete(0,END)     
        self.txtComment.delete(1.0,END)
        
        if self.selectedExpIndex == -1:
            self.btnUpdate["state"] = DISABLED
            self.btnDelete["state"] = DISABLED
            self.cboStatus["state"] = DISABLED
            self.cboUser["state"] = DISABLED
            self.cboType["state"] = DISABLED
            self.cboReleasedBy["state"] = DISABLED
            self.txtCode["state"] = DISABLED
            self.txtNumber["state"] = DISABLED
            self.txtArchivedBy["state"] = DISABLED
            self.txtComment["state"] = DISABLED
            self.txtDateArchived["state"] = DISABLED
            
            self.selectedExperimentId = None
            return
        
        self.btnDelete["state"] = NORMAL
        self.cboStatus["state"] = NORMAL
        self.cboUser["state"] = NORMAL
        self.cboType["state"] = NORMAL
        self.cboReleasedBy["state"] = NORMAL


        selectedCode = self.grdExps.get(self.selectedExpIndex)[0]
        
        session = dbConn.session()
        exp = getExperimentByCode(session, selectedCode)
        
        # get associated experiment types
        expTypes = []
        for type in exp.types:
            expTypes.append(type.type)
    
        # select tpyes in Listbox
        self.cboType.selection_clear(0,END)
        for type in expTypes:
            for index in range(0,len(self.expTypes)):
                if (self.expTypes[index] == type):
                    self.cboType.selection_set(index)
                    break
             
        if (exp != None):
            self.cboStatusVar.set(exp.status.experimentstatus)
            if exp.user is not None:
                self.cboUserVar.set(exp.user.name)
            else:
                self.cboUserVar.set("")

            if exp.releasedByUser is not None:
                self.cboReleasedByVar.set(none2String(exp.releasedByUser.name))
            else:
                self.cboReleasedByVar.set("")         
            
            self.txtCode.insert(0, exp.code)
            self.txtNumber.insert(0, "%04d" % none2String(exp.number))
            self.txtDateArchived.insert(0, none2String(exp.dateArchived))
            self.txtArchivedBy.insert(0, none2String(exp.archivedBy))
            self.txtComment.insert(1.0, none2String(exp.comment))
            
            #remember original state of the selected experiment record
            self.selectedExperimentId = exp.id
        
        self.txtCode["state"] = DISABLED
        self.txtDateArchived["state"] = DISABLED
        self.txtArchivedBy["state"] = DISABLED
        self.cboReleasedBy["state"] = DISABLED
        
        self.onExpDetailChange(None)
        
        session.close()
        
    def selectExpEvent(self, Event):
        
        if (len(self.grdExps.curselection()) > 0):
            self.selectedExpIndex =  self.grdExps.curselection()
        else:
            self.selectedExpIndex =  -1
        
        self.getExpDetails()
    
    def updateExpEvent(self):
        
        if self.selectedExpIndex == -1:
            return
        
        session = dbConn.session()
        
        selectedStatus = self.cboStatusVar
        status = session.query(model.ExperimentStatus).filter_by(experimentstatus=selectedStatus.get()).one()
        
        selectedUsername = self.cboUserVar.get()
        user = getUserByName(session, selectedUsername)
        
        selectedCode = self.grdExps.get(self.selectedExpIndex)[0]
        exp = getExperimentByCode(session, selectedCode)
        
        # validate that this experiment can be set to released
        if (status.experimentstatus == "released"):
            # check that required releasedBy has been set for released experiments
            
            if exp.status.experimentstatus != "released" and self.cboReleasedByVar.get() == "":
                tkMessageBox.showerror("Error", "Released by field needs to be set.")
                return
            
            # check if the experiment has already been archived
            if not isExperimentArchived(session, selectedCode):
                tkMessageBox.showerror("Error", "Experiments must be archived before they can be released.")
                return
            
            # insert dateReleased
            exp.dateReleased = datetime.datetime.now()
          
        # get currently selected types
        types = []
        for sel in self.cboType.curselection():
            type = session.query(model.ExperimentType).filter_by(type=self.cboType.get(sel)).one()
            types.append(type)
        
      
        
        exp.status = status
        exp.number = self.txtNumber.get()
        exp.releasedByUser = getUserByName(session, self.cboReleasedByVar.get());
        exp.comment = strip(self.txtComment.get(1.0, END))
        exp.types = types
        exp.user = user
        
        self.selectedExperimentId = exp.id
        
        session.commit()
        session.flush()
        session.close()
        
        session.close()
        self.editExp = 0
        self.onExpDetailChange(None)
        self.updateExpListbox()
        
        
    def deleteExpEvent(self):
        
        if self.selectedExpIndex == -1:
            return
        
        session = dbConn.session()
        
        code = self.grdExps.get(self.selectedExpIndex)[0]
        
        if (tkMessageBox.askokcancel("Confirm experiment deletion", "Do you really want to remove experiment " + code)):
            try:
                deleteExperimentByCode(session, code)
            except Exception as e:
                tkMessageBox.showerror("Error", e)
            
        session.close()
        self.updateExpListbox()
        
class AddExperimentWindow(GenericWindow):
     
    def __init__(self, parent, rootWidget=None):
        super( AddExperimentWindow, self ).__init__(parent, rootWidget)
        
        self.cboTypeVar = StringVar()
        self.cboUserVar = StringVar()
        self.expTypes=[]
        self.users=[]
        
        session = dbConn.session()
        
        for type in  getActiveTypes(session):
            self.expTypes.append(type.type)
    
        for user in getEnabledUsers(session):
            self.users.append(user.name)
        
        session.close()
        
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.parent.rootWidget, takefocus=True)
        self.dlg.title("Add experiment")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()
    
        
        self._setupWidgets()
    
    def close(self):
        
        self.rootWidget.grab_set()
        self.parent.updateExpListbox()
        self.dlg.destroy()
        
    def _setupWidgets(self):
                     
        Label(self.dlg, text="Code").grid(row=0, sticky=W)
        self.txtExpCode = Entry(self.dlg)
        Label(self.dlg, text="Analyst").grid(row=1, sticky=W)
        Label(self.dlg, text="Type").grid(row=2, sticky=W)
        
        self.cboType= Listbox(self.dlg, listvariable=self.cboTypeVar, selectmode=MULTIPLE, height=5, selectforeground="white", selectbackground="dodger blue", fg="grey" )
        self.cboType.delete(0,END)
	self.cboType.insert(0, *self.expTypes)
        
        self.cboUser = OptionMenu(self.dlg, self.cboUserVar,  *self.users)
        
        btnOK = Button(self.dlg, text="OK", command=self._persistExperiment)
        btnCancel = Button(self.dlg, text="Cancel", command=self.dlg.destroy)
        
        self.txtExpCode.grid(row=0, column=1, sticky=E+W)
        self.cboUser.grid(row=1, column=1, sticky=E+W)
        self.cboType.grid(row=2, column=1, sticky=E+W)
        btnOK.grid(row=10, column=0)
        btnCancel.grid(row=10, column=1, sticky=E)
        
        self.txtExpCode.focus_set()
        
    def _persistExperiment(self):
         
        selectedTypes = []
        analyst = None
        
        code = upper(self.txtExpCode.get())
       
        # check that Code has been set
        if (code == ""):
            return
        
        session = dbConn.session()
        
        # get selected types
	for sel in self.cboType.curselection():
		selectedTypes.append(self.cboType.get(sel))
        
        # get selected analyst
        if self.cboUserVar.get() != "":
            analyst = getUserByName(session, self.cboUserVar.get())
      
        try:
            # add experiment with state "scheduled"
            addExperiment(session, code, analyst=analyst, types=selectedTypes,statuscode=10)
        except Exception as e:
            tkMessageBox.showerror("Error", e)
        
        session.close()
            
        self.close()        
             
if __name__ == "__main__":
    
    root = Tk()
    
    try:
        if (os.getenv("DIFXROOT") == None):
            sys.exit("Error: DIFXROOT environment must be defined.")

        configPath = os.getenv("DIFXROOT") + "/conf/difxdb.ini"


        config = DifxDbConfig(configPath, create=True)

        # try to open the database connection
        connection = Connection()
        connection.type = config.get("Database", "type")
        connection.server = config.get("Database", "server")
        connection.port = config.get("Database", "port")
        connection.user = config.get("Database", "user")
        connection.password = config.get("Database", "password")
        connection.database = config.get("Database", "database")
        connection.echo = False

        dbConn = Schema(connection)
        session = dbConn.session()
        
        if not isSchemaVersion(session, minSchemaMajor, minSchemaMinor):
            major, minor = getCurrentSchemaVersionNumber(session)
            session.close()
            print "Current difxdb database schema is %s.%s but %s.%s is minimum requirement." % (major, minor, minSchemaMajor, minSchemaMinor)
            sys.exit(1)
            
        session.close()
        mainDlg = MainWindow(None, rootWidget=root)

        mainDlg.show()

        root.mainloop()
        
        
        
    except Exception as e:   
        sys.exit(e)
   
   
   
