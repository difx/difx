#! /usr/bin/env python3
# coding: latin-1

#===========================================================================
# Copyright (C) 2016  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================

import os
import sys
import tkinter.messagebox
import datetime

import tkinter.ttk
from optparse import OptionParser
from tkinter import *
from tkinter_difx.multilistbox import *
from tkinter_difx.DatePicker import DatePicker

from difxdb.model import model
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.experimentaction import *
from difxdb.business.experimenttypeaction import *
from difxdb.business.versionhistoryaction import *
from difxdb.business.moduleaction import *
from difxdb.business.filedataaction import *
from difxdb.business.exportfileaction import *
from difxdb.business.useraction import *
from difxutil.dbutil import *

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)

# minimum database schema version required by this program
minSchemaMajor = 1
minSchemaMinor = 7


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
        self.selectDateDlg = SelectDateWindow(self, rootWidget)
        
        self.rootWidget.title("expad: Experiment Administration")
        
        self.expEdit = 0
        self.selectedExperimentId = None
        self.selectedExpIndex = -1
        self.cboStatusVar = StringVar()
        self.cboTypeVar = StringVar()
        self.cboUserVar = StringVar()
        self.cboReleasedByVar = StringVar()
        self.obsDate = None
        
        session = dbConn.session()

        # obtain all experiment stati from database
        self.expStati = ["select"]
        for status in  session.query(model.ExperimentStatus).order_by("statuscode").all():
            self.expStati.append(status.experimentstatus)
        
         # obtain all experiment types from database
        self.expTypes = []
        for type in getActiveTypes(session):
            self.expTypes.append(type.type)
            
        # obtain all enaibled users from the database
        self.users = ["select"]
        for user in getEnabledUsers(session):
            self.users.append(user.name)
            
        session.close()
            
    def show(self):
        
        self._setupWidgets()
        self.updateExpListbox()

    def update(self):
        '''
        update
        '''
        self.txtObsDate["state"] = NORMAL
        self.txtObsDate.delete(0,END)
        self.txtObsDate.insert(0, self.obsDate)
        self.rootWidget.event_generate("<<Test>>")

        
        
    def _setupWidgets(self):
        
        self.rootWidget.rowconfigure(0, weight=1) 
        self.rootWidget.rowconfigure(10, weight=1) 
        self.rootWidget.columnconfigure(0, weight=1)     
        

        # top level components
        frmExps = LabelFrame(self.rootWidget, text="Experiments")     
        frmControl = LabelFrame(self.rootWidget, padx=5, pady=15)
        frmBottomControl = LabelFrame(self.rootWidget)
        self.frmTabs = tkinter.ttk.Notebook(self.rootWidget)
        btnExit = Button(self.rootWidget, text="Exit", command=self.rootWidget.destroy)

        frmExps.grid(row=0, column=0, columnspan=2, sticky=E+W+N+S)
        self.frmTabs.grid(row=10,column=0, rowspan=1,sticky=EW)
        frmControl.grid(row=10, column=1, sticky=E+W+N+S)
        frmBottomControl.grid(row=20, column=0, sticky=E+W+N+S)     
        btnExit.grid(row=20,column=1,sticky=E+W+S)
        
        # frmTabs
        self.detailTab = tkinter.ttk.Frame(self.frmTabs)
        self.mediaTab = tkinter.ttk.Frame(self.frmTabs)
        self.exportTab = tkinter.ttk.Frame(self.frmTabs)
        self.historyTab = tkinter.ttk.Frame(self.frmTabs)
        self.frmTabs.add(self.detailTab, text='Details', sticky="news")
        self.frmTabs.add(self.mediaTab, text='Media', sticky="news", state="disabled")
        self.frmTabs.add(self.exportTab, text='Export', sticky="news", state="disabled")
        self.frmTabs.add(self.historyTab, text='Status history', sticky="news", state="disabled")

        self.detailTab.columnconfigure(0,weight=1)
        self.detailTab.rowconfigure(0,weight=1)
        self.mediaTab.columnconfigure(0,weight=1)
        self.exportTab.columnconfigure(0,weight=1)
        self.historyTab.columnconfigure(0,weight=1)
        
        # frmDetail
        frmDetail = LabelFrame(self.detailTab, text="Detail", padx=5)
        frmDetail.grid(row=0, column=0, sticky=E+W+N+S)

        #frmMedia
        frmMedia = LabelFrame(self.mediaTab, text="Media", padx=5)
        frmMedia.grid(row=0, column=0, sticky=E+W+N+S)

        #frmExport
        frmExport = LabelFrame(self.exportTab, text="Export", padx=5)
        frmExport.grid(row=0, column=0, sticky="nesw")

        #frmStatus
        frmStatus = LabelFrame(self.historyTab,  padx=5, pady=5)
        frmStatus.grid(row=0, column=0, sticky="nesw")

        # frmControl
        btnAddExp = Button(frmControl, text="Add experiment", command=self.addExperimentDlg.show)
        self.btnDelete = Button(frmControl, text="Delete experiment", command=self.deleteExpEvent)
        Button(frmControl, text="Refresh", command=self.show).grid(row=20,column=0,sticky=E+W)

        btnAddExp.grid(row=0,column=0, sticky=E+W)
        self.btnDelete.grid(row=10, column=0, sticky=E+W)

        #frmBottomControl
        self.btnUpdate = Button(frmBottomControl, text="update experiment", command=self.updateExpEvent)
        self.btnUpdate.grid(row=0, column=0, sticky=E+W)

        #frmExps
        col1 = ListboxColumn("experiment", 9)
        col2 = ListboxColumn("number", 4, numeric=True)
        col3 = ListboxColumn("status", 15)
        col4 = ListboxColumn("type", 6)
        col5 = ListboxColumn("datasets", 4, numeric=True)
        col6 = ListboxColumn("analyst", 12)
        col7 = ListboxColumn("observed", 10) 
        col8 = ListboxColumn("created", 10) 
        col9 = ListboxColumn("archived", 10)
        col10 = ListboxColumn("released", 10) 
        self.grdExps = MultiListbox(frmExps, 16, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10)
        self.grdExps.bindEvent("<ButtonRelease-1>", self.selectExpEvent)
             
        # frmMedia
        colList = []
        colList.append(ListboxColumn("station",3, sortable=True))
        colList.append(ListboxColumn("vsn",8, searchable=True))
        colList.append(ListboxColumn("location",20, searchable=True))
        colList.append(ListboxColumn("scans",3, numeric=True))
        colList.append(ListboxColumn("received",15))
        colList.append(ListboxColumn("comment",35))
        columns = tuple(colList)
        self.grdModules = MultiListbox(frmMedia, 19, *columns)
        self.grdModules.grid(row=0, column=0, sticky="news")

        # frmStatus
        Label(frmStatus, text="Current status: ").grid(row=0,column=0, sticky=W)
        self.lblCurrentStatus = Label(frmStatus, text = "None")
        Label(frmStatus, text="New status: ").grid(row=5,column=0, sticky=W)
        self.lblReleasedBy = Label(frmStatus, text = "Released by")
        self.lblStatusDate = Label(frmStatus, text = "Date")
        self.txtStatusDate = Entry(frmStatus, text = "")
        self.btnStatusDate = Button(frmStatus, text="Select date", command=self._selectStatusDate, state="disabled")
        self.cboStatus = OptionMenu (frmStatus, self.cboStatusVar, *self.expStati ,command=self.onExpDetailChange)
        self.cboReleasedBy = OptionMenu(frmStatus, self.cboReleasedByVar,  *self.users, command=self.onExpDetailChange)

        colList = []
        colList.append(ListboxColumn("Status",20,sortable=False, searchable=False))
        colList.append(ListboxColumn("When",20 ,sortable=False, searchable=False))
        columns = tuple(colList)
        self.lblCurrentStatus.grid(row=0,column=1, sticky=W)
        self.cboStatus.grid(row=5, column=1, sticky=E+W)
        self.lblStatusDate.grid(row=6,column=0, sticky=W)
        self.txtStatusDate.grid(row=6,column=1, sticky=W)
        self.btnStatusDate.grid(row=6,column=2, sticky=W)
        self.lblReleasedBy.grid(row=7,column=0, sticky=W)
        self.cboReleasedBy.grid(row=7, column=1, sticky=E+W)

        frmHistory = LabelFrame(self.historyTab, text="Status",  padx=5)
        frmHistory.grid(row=10,column=0, columnspan=2, sticky="news")
        self.grdStatusHistory = MultiListbox(frmHistory, 15, *columns)
        self.grdStatusHistory.grid(row=0, column=0, sticky="news")

        #frmExport
        colList = []
        colList.append(ListboxColumn("path",30, sortable=True))
        colList.append(ListboxColumn("file",15, sortable=True))
        colList.append(ListboxColumn("checksum",28, sortable=True))
        colList.append(ListboxColumn("created",16, sortable=True))
        columns = tuple(colList)
        self.grdExports = MultiListbox(frmExport, 19, *columns)
        self.grdExports.grid(row=0, column=0, sticky="news")

        #frmDetail
        Label(frmDetail, text="code: ").grid(row=0,column=0, sticky=W)
        Label(frmDetail, text="number: ").grid(row=1,column=0, sticky=W)
        Label(frmDetail, text="observed: (yyyy-mm-dd) ").grid(row=10,column=0, sticky=W)
        Label(frmDetail, text="type: ").grid(row=11,column=0, sticky=W)
        Label(frmDetail, text="analyst: ").grid(row=12,column=0, sticky=W)
        Label(frmDetail, text="email on module arrival: ").grid(row=14,column=0, sticky=W)
        Label(frmDetail, text="date archived: ").grid(row=15,column=0, sticky=W)
        Label(frmDetail, text="archived by: ").grid(row=20,column=0, sticky=W)
        Label(frmDetail, text = "comments: ").grid(row=25, column=0, sticky=W) 
        self.txtCode = Entry(frmDetail, text = "")
        self.txtNumber = Entry(frmDetail, text = "")
        self.txtObsDate = Entry(frmDetail, text = "")
        self.cboType= Listbox(frmDetail, selectmode=MULTIPLE, height=4, selectforeground="white", selectbackground="dodger blue", fg="grey", exportselection=0)
        self.cboUser = OptionMenu(frmDetail, self.cboUserVar,  *self.users, command=self.onExpDetailChange)
        self.txtNotify = Entry(frmDetail, text = "")
        self.txtNumber = Entry(frmDetail, text = "")
        self.txtDateArchived = Entry(frmDetail, text = "")
        self.txtArchivedBy = Entry(frmDetail, text = "")
        self.txtComment = Text(frmDetail, height=5, width=25)
        self.btnObsDate = Button(frmDetail, text="select", command=self.selectDateDlg.show)
        
        self.cboType.insert(END, *self.expTypes)
        
        #arrange widgets on frmExps 
        self.grdExps.grid(row=0, column=0, sticky=E+W+N+S)
        
        #arrange widgets on frmDetail
        self.txtCode.grid(row=0, column=1, columnspan=2, sticky=E+W)
        self.txtNumber.grid(row=1, column=1, columnspan=2, sticky=E+W)
        self.txtObsDate.grid(row=10, column=1, sticky=E+W)
        self.btnObsDate.grid(row=10,column=2,sticky=E+W)
        self.cboType.grid(row=11, column=1, columnspan=2, sticky=E+W)
        self.cboUser.grid(row=12, column=1, columnspan=2, sticky=E+W)
        self.txtNotify.grid(row=14, column=1, columnspan=2, sticky=E+W)
        self.txtDateArchived.grid(row=15, columnspan=2, column=1, sticky=E+W)
        self.txtArchivedBy.grid(row=20, column=1, columnspan=2, sticky=E+W)
        self.txtComment.grid(row=25, column=1, columnspan=2, sticky=E+W)
        
        # bind events
        self.txtNotify.bind("<KeyRelease>", self.onExpDetailChange)
        self.txtObsDate.bind("<KeyRelease>", self.onExpDetailChange)
        self.txtObsDate.bind("<<Changed>>", self.onExpDetailChange)
        self.txtNumber.bind("<KeyRelease>", self.onExpDetailChange)
        self.txtComment.bind("<KeyRelease>", self.onExpDetailChange)
        self.cboType.bind('<ButtonRelease-1>', self.onExpDetailChange)
        self.rootWidget.bind("<<Test>>", self.onExpDetailChange)
        self.btnObsDate["state"] = DISABLED
        self.btnUpdate["state"] = DISABLED
        self.btnDelete["state"] = DISABLED
        
        
    def _selectStatusDate(self):
        
        self.selectDateDlg.show()
  
    def updateStatusHistory(self, exp):

        session = dbConn.session()
        if (self.selectedExperimentId is None):
            return
        if (exp is None):
            return

        self.lblCurrentStatus["text"] = exp.status.experimentstatus

        # update grdStatusHistory
        self.grdStatusHistory.delete(0, END)
        self.grdStatusHistory.clearData()

        history = getStatusHistory(session, self.selectedExperimentId)
        for item in history:
                self.grdStatusHistory.appendData((item.status, item.dateCreated))

        self.grdStatusHistory.update()

        session.close()

    def updateExport(self):

        session = dbConn.session()

        if (self.selectedExperimentId is None):
            return

        self.grdExports.delete(0, END)
        self.grdExports.clearData()

        files = getExportFilesByExperimentId(session, self.selectedExperimentId)


        for f in files:
                self.grdExports.appendData((f.exportPath, f.filename, f.checksum, f.dateCreated))

        session.close()
        self.grdExports.update()


    def updateMedia(self):
        session = dbConn.session()

        if (self.selectedExperimentId is None):
            return

        self.grdModules.delete(0, END)
        self.grdModules.clearData()

        modules = getModulesByExperimentId(session, self.selectedExperimentId)
        files = getFilesByExperimentId(session, self.selectedExperimentId)
        for module in modules:
                self.grdModules.appendData((module.stationCode, module.slot.module.vsn, module.slot.location, module.numScans, module.received, module.comment))        

        for f in files:
                self.grdModules.appendData((f.stationCode, "", f.location, f.numScans, f.received, f.comment))  

        session.close()
        self.grdModules.update()


    def onExpDetailChange(self, Event):
        ' bind event to be called whenever experiment details have been changed by the user'
        
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
        
        selectedExperiment.status.experimentstatus = ""
        
        self.expEdit += self.setChangeColor(self.txtNotify, self.txtNotify.get(), selectedExperiment.emailnotification)
        self.expEdit += self.setChangeColor(self.txtNumber, self.txtNumber.get(), str(selectedExperiment.number).zfill(4))
        self.expEdit += self.setChangeColor(self.txtObsDate, self.txtObsDate.get(), selectedExperiment.dateObserved)

        if self.cboStatusVar.get() != "select":
            self.expEdit += self.setChangeColor(self.cboStatus, self.cboStatusVar.get(), selectedExperiment.status.experimentstatus)
            self.lblStatusDate.grid()
            self.txtStatusDate.grid()
            self.btnStatusDate.grid()
            self.txtStatusDate.delete(0,END)
            self.txtStatusDate.insert(0,datetime.datetime.now())
        
        else:
            self.cboStatus.config(bg=self.defaultBgColor)
            self.lblStatusDate.grid_remove()
            self.txtStatusDate.grid_remove()
            self.btnStatusDate.grid_remove()
        
        if  selectedExperiment.user is not None:
            self.expEdit += self.setChangeColor(self.cboUser, self.cboUserVar.get(), selectedExperiment.user.name)
        else:
            self.expEdit += self.setChangeColor(self.cboUser, self.cboUserVar.get(), "select")
            
        if  selectedExperiment.releasedByUser is not None:
            self.expEdit += self.setChangeColor(self.cboReleasedBy, self.cboReleasedByVar.get(), selectedExperiment.releasedByUser.name)
        else:
            self.expEdit += self.setChangeColor(self.cboReleasedBy, self.cboReleasedByVar.get(), "select")
        
        self.expEdit += self.setChangeColor(self.txtComment, self.txtComment.get(1.0, END), selectedExperiment.comment)
        
        
        if currentTypes != origTypes:
            self.expEdit += self.setChangeColor(self.cboType, "1", "0")
        else:
            self.expEdit += self.setChangeColor(self.cboType, "0", "0")
        
        # enable released by entry
        #if self.cboStatusVar.get() == "released" and selectedExperiment.status.experimentstatus != "released":
        if self.cboStatusVar.get() == "released":
            self.cboReleasedBy.grid()
            self.lblReleasedBy.grid()
            self.cboReleasedBy["state"] = NORMAL
            
        else:
            self.cboReleasedBy["state"] = DISABLED
            self.lblReleasedBy.grid_remove()
            self.cboReleasedBy.grid_remove()
            
               
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
        
        if (str(none2String(compareValue)) != componentValue.strip()):
            
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
        '''
        retrieves all experiments from the database and fills the grdExps accordingly. Calls getExpDetails() to populate
        all detail tabs/widgets.
        '''
                
        session = dbConn.session()
        exps = session.query(model.Experiment).order_by(desc(model.Experiment.number)).all()

        self.grdExps.clearData()
               
        for exp in exps:

            # show only experiments given on command line
            if len(defaultExps) > 0:
                if exp.code not in  defaultExps:
                        continue
                        
            
            # retrieve types
            expTypes = [] 
            for type in exp.types:
                expTypes.append(type.type)

            username = ""
            if exp.user is not None:
                username = exp.user.name

            # for the table shorten datetimes to only dates
            dateCreated = ""
            dateArchived = ""
            dateReleased = ""
            
            if exp.dateCreated:
                    dateCreated = exp.dateCreated.date()
            if exp.dateArchived:
                    dateArchived = exp.dateArchived.date()
            if exp.dateReleased:
                    dateReleased = exp.dateReleased.date()
                
            # number of datasets (modules and files)
            numDataset = len(exp.modules) + len(exp.fileData)
            self.grdExps.appendData((exp.code, "%04d" % exp.number, exp.status.experimentstatus, " ".join(expTypes), numDataset, username, exp.dateObserved, dateCreated,  dateArchived, dateReleased))
     
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
        self.txtNotify["state"] = NORMAL
        self.txtNumber["state"] = NORMAL
        self.txtDateArchived["state"] = NORMAL
        self.cboReleasedBy["state"] = NORMAL
        self.txtObsDate["state"] = NORMAL
        self.txtArchivedBy["state"] = NORMAL
        self.txtComment["state"] = NORMAL
        
        
        self.txtCode.delete(0,END)
        self.txtNotify.delete(0,END)
        self.txtObsDate.delete(0,END)
        self.txtNumber.delete(0,END)
        self.txtDateArchived.delete(0,END)
        self.txtArchivedBy.delete(0,END)     
        self.txtComment.delete(1.0,END)
        
        if self.selectedExpIndex == -1:
            self.btnUpdate["state"] = DISABLED
            self.btnDelete["state"] = DISABLED
            self.btnObsDate["state"] = DISABLED
            self.txtObsDate["state"] = DISABLED
            self.cboStatus["state"] = DISABLED
            self.cboUser["state"] = DISABLED
            self.cboType["state"] = DISABLED
            self.cboReleasedBy["state"] = DISABLED
            self.txtCode["state"] = DISABLED
            self.txtNotify["state"] = DISABLED
            self.txtNumber["state"] = DISABLED
            self.txtArchivedBy["state"] = DISABLED
            self.txtComment["state"] = DISABLED
            self.txtDateArchived["state"] = DISABLED
            #self.mediaTab["state"] = DISABLED
            self.frmTabs.tab(self.mediaTab, state="disabled")
            self.frmTabs.tab(self.historyTab, state="disabled")
            self.frmTabs.tab(self.exportTab, state="disabled")
            
            
            self.selectedExperimentId = None
            return
        
        self.btnDelete["state"] = NORMAL
        self.btnObsDate["state"] = NORMAL
        self.cboStatus["state"] = NORMAL
        self.cboUser["state"] = NORMAL
        self.cboType["state"] = NORMAL
        self.cboReleasedBy["state"] = NORMAL


        selectedCode = self.grdExps.get(self.selectedExpIndex)[0]
        
        session = dbConn.session()
        exp = getExperimentByCode(session, selectedCode)

        if not exp:
                return
        
        self.cboStatusVar.set("select")

        expTypes = []
                    
        if (exp != None):
            # get associated experiment types

            if exp.types:
                for type in exp.types:
                    expTypes.append(type.type)
     
            # select types in Listbox
            self.cboType.selection_clear(0,END)
            for type in expTypes:
                for index in range(0,len(self.expTypes)):
                    if (self.expTypes[index] == type):
                        self.cboType.selection_set(index)
                        break

            
            if exp.user is not None:
                self.cboUserVar.set(exp.user.name)
            else:
                self.cboUserVar.set("select")

            if exp.releasedByUser is not None:
                self.cboReleasedByVar.set(none2String(exp.releasedByUser.name))
            else:
                self.cboReleasedByVar.set("select")         
            
            self.txtCode.insert(0, exp.code)
            self.txtNotify.insert(0, none2String(exp.emailnotification))
            self.txtNumber.insert(0, "%04d" % none2String(exp.number))
            self.txtDateArchived.insert(0, none2String(exp.dateArchived))
            self.txtObsDate.insert(0, none2String(exp.dateObserved))
            self.txtArchivedBy.insert(0, none2String(exp.archivedBy))
            self.txtComment.insert(1.0, none2String(exp.comment))
            
            #remember original state of the selected experiment record
            self.selectedExperimentId = exp.id

            #disable media tab if no media 
            if len(exp.modules) > 0 or len(exp.fileData) > 0:
                self.frmTabs.tab(self.mediaTab, state="normal")
            else:
                self.frmTabs.tab(self.mediaTab, state="disabled")

            #disable export tab if no exported files
            if exp.exportFiles:
                self.frmTabs.tab(self.exportTab, state="normal")
            else:
                self.frmTabs.tab(self.exportTab, state="disabled")

            self.frmTabs.tab(self.historyTab, state="normal")
                
        
        self.txtCode["state"] = DISABLED
        self.txtDateArchived["state"] = DISABLED
        self.txtArchivedBy["state"] = DISABLED
        self.cboReleasedBy["state"] = DISABLED

        # update the additional detail tabs
        self.updateMedia()
        self.updateExport()
        self.updateStatusHistory(exp)
        
        # call event to process any changes made in any detail fields
        self.onExpDetailChange(None)
        
        session.close()
        
    def selectExpEvent(self, Event):
        '''
        Event triggered when an experiment is selected in grdExps
        Results in all experiment details tabs to be updated
        '''
        
        if (self.grdExps.curselection() >= 0):
            self.selectedExpIndex =  self.grdExps.curselection()
        else:
            self.selectedExpIndex =  -1

        
        # update all detail experiment information
        self.getExpDetails()
    
    def updateExpEvent(self):
        '''
        This event will execute any updates of the experiment details done by the user.
        '''
        
        if self.selectedExpIndex == -1:
            return

        session = dbConn.session()
        
        selectedUsername = self.cboUserVar.get()
        if selectedUsername != "select":
            user = getUserByName(session, selectedUsername)
        else:
            user = None
        
        selectedCode = self.grdExps.get(self.selectedExpIndex)[0]
        exp = getExperimentByCode(session, selectedCode)

        #check for change in status
        selectedStatus = self.cboStatusVar.get()

        if selectedStatus != "select":
            # experiment status was changed
            status = session.query(model.ExperimentStatus).filter_by(experimentstatus=selectedStatus).one()
            # make a new entry in the ExperimentStatusHistory
            history = model.ExperimentStatusHistory()
            history.expID = exp.id
            history.status = selectedStatus
            history.dateCreated = self.txtStatusDate.get()
            exp.status = status
            session.add(history)

        # validate that this experiment can be set to released
        if (selectedStatus == "released"):
            # check that required releasedBy has been set for released experiments
            
            #if exp.status.experimentstatus != "released" and self.cboReleasedByVar.get() == "":
            if self.cboReleasedByVar.get() == "select":
                tkinter.messagebox.showerror("Error", "Released by field needs to be set.")
                return
            
            # check if the experiment has already been archived
            if not isExperimentArchived(session, selectedCode):
                tkinter.messagebox.showerror("Error", "Experiments must be archived before they can be released.")
                return
            
            # insert dateReleased
            exp.dateReleased = datetime.datetime.now()
          
        # get currently selected types
        types = []
        for sel in self.cboType.curselection():
            type = session.query(model.ExperimentType).filter_by(type=self.cboType.get(sel)).one()
            types.append(type)
        
      
        
        exp.number = self.txtNumber.get()
        obsDate = None
        if (self.txtObsDate.get() != ""):
                try:
                        obsDate = datetime.datetime.strptime(self.txtObsDate.get(), "%Y-%m-%d")
                except Exception as e:
                        tkinter.messagebox.showerror("Error", "Misformed observation date. Must be yyyy-mm-dd")
                        session.close()
                        return

        exp.dateObserved = obsDate
        if self.cboReleasedByVar.get() != "select":
                exp.releasedByUser = getUserByName(session, self.cboReleasedByVar.get());

        exp.comment = self.txtComment.get(1.0, END).strip()
        exp.types = types
        exp.user = user
        
        # replace white spaces by comma in email recipient list
        reclist = ""
        recipients = self.txtNotify.get().split()
        for recipient in recipients:
            reclist += recipient + ","
        
        exp.emailnotification = reclist[:-1]
       
        
        self.selectedExperimentId = exp.id
        
        session.commit()
        session.flush()
        self.editExp = 0
        
        session.close()
        self.onExpDetailChange(None)
        self.updateExpListbox()
        
        
        
    def deleteExpEvent(self):
        
        if self.selectedExpIndex == -1:
            return
        
        session = dbConn.session()
        
        code = self.grdExps.get(self.selectedExpIndex)[0]
        
        if (tkinter.messagebox.askokcancel("Confirm experiment deletion", "Do you really want to remove experiment " + code)):
            try:
                deleteExperimentByCode(session, code)
            except Exception as e:
                tkinter.messagebox.showerror("Error", e)
            
        session.close()
        self.updateExpListbox()
        
class SelectDateWindow(GenericWindow):
    def __init__(self, parent, rootWidget=None):
        super( SelectDateWindow, self ).__init__(parent, rootWidget)

    def show(self):

        # create modal dialog
        self.dlg = Toplevel(self.parent.rootWidget, takefocus=True)
        self.dlg.title("Select observation date")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()

        self._setupWidgets()

    def close(self):
        
        self.parent.obsDate = self.cal.selectedDate
        self.parent.update()
        self.rootWidget.grab_set()
        self.dlg.destroy()

    def _setupWidgets(self):
                     
        self.cal = DatePicker(self.dlg)
        btnOK = Button(self.dlg, text="OK", command=self.close)
        btnOK.grid(row=10,column=10)


class AddExperimentWindow(GenericWindow):
     
    def __init__(self, parent, rootWidget=None):
        super( AddExperimentWindow, self ).__init__(parent, rootWidget)
        
        self.cboTypeVar = StringVar()
        self.cboUserVar = StringVar()
        self.expTypes=[]
        self.users=[]
        self.obsDate = None

        self.selectDateDlg = SelectDateWindow(self, rootWidget)
        
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
                     
        
        self.txtExpCode = Entry(self.dlg) 
        self.txtObsDate =  Entry(self.dlg)   
        self.btnObsDate = Button(self.dlg, text="Select date", command=self._selectObsDate)
        self.cboType= Listbox(self.dlg, listvariable=self.cboTypeVar, selectmode=MULTIPLE, height=5, selectforeground="white", selectbackground="dodger blue", fg="grey", exportselection=0)
        self.cboType.delete(0,END)
        self.cboType.insert(0, *self.expTypes)
    
        self.cboUser = OptionMenu(self.dlg, self.cboUserVar,  *self.users)
        
        btnOK = Button(self.dlg, text="OK", command=self._persistExperiment)
        btnCancel = Button(self.dlg, text="Cancel", command=self.dlg.destroy)
        
        Label(self.dlg, text="Code").grid(row=0, sticky=W)
        Label(self.dlg, text="Analyst").grid(row=1, sticky=W)
        Label(self.dlg, text="Type").grid(row=2, sticky=W)
        Label(self.dlg, text="Observation date").grid(row=5, sticky=W)
        
        Label(self.dlg, text="*").grid(row=0, column=3,sticky=W)
        Label(self.dlg, text="(yyyy-mm-dd)").grid(row=10, column=3, sticky=W)
        
        self.txtExpCode.grid(row=0, column=1, columnspan=2, sticky=E+W)
        self.cboUser.grid(row=1, column=1,columnspan=2, sticky=E+W)
        self.cboType.grid(row=2, column=1,columnspan=2, sticky=E+W)
        self.txtObsDate.grid(row=5, column=1, sticky=E+W)
        self.btnObsDate.grid(row=5, column=2, sticky=E+W)
        
        btnOK.grid(row=100, column=0)
        btnCancel.grid(row=100, column=1, sticky=E)
        
        self.txtExpCode.focus_set()
        
    def update(self):
        self.txtObsDate.delete(0,END)
        self.txtObsDate.insert(0, self.obsDate)

    def _selectObsDate(self):
        
        self.selectDateDlg.show()

    def _persistExperiment(self):
         
        selectedTypes = []
        analyst = None
        
        code = self.txtExpCode.get().upper()
        self.obsDate = self.txtObsDate.get()
       
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
            addExperiment(session, code, analyst=analyst, obsDate=self.obsDate, types=selectedTypes,statuscode=10)
        except Exception as e:
            tkinter.messagebox.showerror("Error", e)
        
        session.close()
            
        self.close()        

def getUsage():

        usage = "%prog [options] [<experiment>]\n\n"
        usage += '\nA GUI program for administration of difx experiments stored in a database. \n\n'
        usage += 'If the optional <experiment> is given, information will be listed for this experiment only.\n'
        usage += 'This program is part of the difxdb tools (for information consult the difx wiki pages).\n\n'
        usage += 'NOTE: The program requires the DIFXROOT environment to be defined.\n'
        usage += "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf.\n"
        return usage
             
if __name__ == "__main__":
    
    root = Tk()
    defaultExps = []
    
    try:
        if (os.getenv("DIFXROOT") == None):
            sys.exit("Error: DIFXROOT environment must be defined.")

        usage = getUsage()
        version = ""
        parser = OptionParser(version=version, usage=usage)
        (options, args) = parser.parse_args()

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
            print("Error: current difxdb database schema is %s.%s but %s.%s is the minimum requirement." % (major, minor, minSchemaMajor, minSchemaMinor))
            sys.exit(1)

        # check for experiment(s) passed on the command line
        for arg in args:
                exp = arg.upper()
                if not experimentExists(session, exp):
                        print("Error: experiment %s not found in database." % exp)
                        sys.exit(1)
                defaultExps.append(exp)

        session.close()
        mainDlg = MainWindow(None, rootWidget=root)

        mainDlg.show()

        root.mainloop()
        
        
        
    except Exception as e:   
        sys.exit(e)
   
   
   
