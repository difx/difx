#!/usr/bin/python
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

import re
import os
import time
import tkMessageBox
import PIL
import barcode

from difxdb.business.versionhistoryaction import *
from difxdb.business.experimentaction import * 
from difxdb.business.moduleaction import *
from difxdb.business.slotaction import *
from difxdb.model.dbConnection import Schema, Connection
from difxdb.model import model
from difxutil.dbutil import *
from difxdb.difxdbconfig import DifxDbConfig
from difxfile.difxdir import *

from barcode.writer import ImageWriter, FONT
from string import strip, upper
from collections import deque

from sqlalchemy import *
from Tkinter import *
from tkinter.multilistbox import *

# minimum database schema version required by comedia
minSchemaMajor = 1
minSchemaMinor = 0

class GenericWindow(object):
    def __init__(self, parent=None,rootWidget=None):
        
        self.rootWidget = rootWidget
        self.parent = parent
        self.config = None
        
class MainWindow(GenericWindow):
    
    def __init__(self, parent=None, rootWidget=None):
        
        # call super class constructor
        super( MainWindow, self ).__init__(parent, rootWidget)

        self.rootWidget.title("comedia: Correlator Media Archive")
        
        # sub dialogs
        self.checkinDlg = CheckinWindow(self, rootWidget)  
        self.checkoutDlg = CheckoutWindow(self, rootWidget)  
        self.changeSlotDlg = ChangeSlotWindow(self, rootWidget)  
        self.labelOptionsDlg= LabelOptionsWindow(self,rootWidget)
        self.databaseOptionsDlg= DatabaseOptionsWindow(self,rootWidget)
        self.scanModulesDlg = ScanModulesWindow(self, rootWidget)
        
        self.defaultBgColor = rootWidget["background"]
        self.isConnected = False
        
        self.selectedSlotIndex = -1 
        self.moduleEdit = 0
        self.filterReleaseList = IntVar()
        self.filterDirLess = IntVar()
        self.filterUnscanned = IntVar()
        self.filterExpVar = StringVar()
        
        self.expFilterItems = []
        self.labelSizeX = 320
        self.labelSizeY = 130
        self.moduleFilter = ""
        self.slotFilter = ""
        self.forceCheckout = False
                
    def show(self):
        
        self._setupWidgets()
        self.updateSlotListbox()
        self.refreshStatusEvent()
        
        
    def _setupWidgets(self):
        
     
        # menubar setup
        menubar = Menu(self.parent)
        
        optionmenu = Menu(menubar, tearoff=0)
        optionmenu.add_command(label="Label options", command=self.showLabelOptions)
        optionmenu.add_command(label="Database options", command=self.showDatabaseOptions)

        menubar.add_cascade(label="Options", menu=optionmenu)

        self.rootWidget.config(menu=menubar)
        
        # frames
        self.frmMain = LabelFrame(self.rootWidget, text="Filter")
        self.frmDetail = LabelFrame(self.rootWidget, text="Detail")
        self.frmEditExperiment = Frame(self.frmDetail)
        frmStatus = LabelFrame(self.rootWidget, text="Status")
        
        self.btnQuit = Button(self.rootWidget, text="Exit", command=self.rootWidget.destroy)
        
        #widgets on frmMain       
        self.chkRelease = Checkbutton(self.frmMain, text = "releasable modules", variable = self.filterReleaseList, command=self.updateSlotListbox)
        self.chkDirLess = Checkbutton(self.frmMain, text = "modules w/o .dir", variable = self.filterDirLess, command=self.updateSlotListbox)
        self.chkUnscanned = Checkbutton(self.frmMain, text = "unscanned .dir", variable = self.filterUnscanned, command=self.updateSlotListbox)
        
        col1 = ListboxColumn("slot",10, searchable=True)
        col2 = ListboxColumn("vsn",10, sortable=True)
        col3 = ListboxColumn("station",4, sortable=True)
        col4 = ListboxColumn("experiments",30)
        col5 = ListboxColumn("scans",4)
        col6 = ListboxColumn("capacity",5) 
        col7 = ListboxColumn("datarate",5)
        col8 = ListboxColumn("received",15)
        self.grdSlot = MultiListbox(self.frmMain, col1, col2, col3, col4, col5, col6, col7, col8)
        self.grdSlot.bindEvent("<ButtonRelease-1>", self.selectSlotEvent)
        self.btnNewModule = Button (self.frmMain, text="Check-in module", command=self.checkinModule)
          
        
        # widgets on frmStatus
        Label(frmStatus, text="Number of modules without .dir files: ").grid(row=0,column=0, sticky=W)
        Label(frmStatus, text="Number of unscanned .dir files: ").grid(row=1,column=0, sticky=W)
        self.lblNumDirLess = Label(frmStatus, text = "")
        self.lblNumUnscanned = Label(frmStatus, text = "")
        self.btnRefresh = Button(frmStatus, text="Refresh status", command=self.refreshStatusEvent)
        self.btnModuleScan = Button(frmStatus, text="Scan", command=self.scanModuleEvent)
        
        # widgets on frmDetail
        Label(self.frmDetail, text = "slot: ").grid(row=0, column=0, sticky=W)
        Label(self.frmDetail, text = "vsn: ").grid(row=1, column=0, sticky=W)
        Label(self.frmDetail, text = "station: ").grid(row=2, column=0, sticky=W)
        Label(self.frmDetail, text = "capacity: ").grid(row=3, column=0, sticky=W)
        Label(self.frmDetail, text = "datarate: ").grid(row=4, column=0, sticky=W)
        Label(self.frmDetail, text = "received: ").grid(row=5, column=0, sticky=W)
        Label(self.frmDetail, text = "experiment(s): ").grid(row=6, column=0, sticky=W) 
        Label(self.frmDetail, text = "comments: ").grid(row=8, column=0, sticky=W) 
        self.txtLocationContent = Entry(self.frmDetail, text = "", state=DISABLED)
        self.btnChangeSlot = Button(self.frmDetail, text = "Change slot", state=DISABLED, command=self.showChangeSlotWindow)
        self.lblVSNContent = Entry(self.frmDetail, text = "", state=DISABLED)
        self.lblStationContent = Entry(self.frmDetail, text = "", state=DISABLED)
        self.lblCapacityContent = Entry(self.frmDetail, text = "", state=DISABLED)
        self.lblDatarateContent = Entry(self.frmDetail, text = "", state=DISABLED)
        self.lblReceivedContent = Entry(self.frmDetail, text = "", state=DISABLED)
        scrollCboExperiments = Scrollbar(self.frmDetail)
        self.cboExperiments =  Listbox(self.frmDetail, height=3, yscrollcommand=scrollCboExperiments.set, selectmode=MULTIPLE, state=DISABLED)
        scrollCboExperiments.config(command=self.cboExperiments.yview)
        self.txtComment = Text(self.frmDetail, height=3, width=25)
        self.btnDeleteModule = Button(self.frmDetail, text="Check-out module", command=self.checkOutModule, state=DISABLED)
        self.btnEditModule = Button(self.frmDetail, text="Update module", command=self.updateModule, state=DISABLED)
        self.btnPrintLibraryLabel = Button (self.frmDetail, text="Print library label", command=self.printLibraryLabel,state=DISABLED)
        self.btnPrintVSNLabel = Button (self.frmDetail, text="Print VSN label", command=self.printVSNLabel,state=DISABLED)
        self.btnRescan = Button (self.frmDetail, text="Rescan directory", command=self.rescanModuleEvent,state=DISABLED)
        
        # widgets on frmEditExperiment
        scrollCboFreeExperiments = Scrollbar(self.frmEditExperiment)
        self.cboFreeExperiments = Listbox(self.frmEditExperiment, height=3, yscrollcommand=scrollCboFreeExperiments.set, selectmode=MULTIPLE)
        scrollCboFreeExperiments.config(command=self.cboFreeExperiments.yview)
        self.btnAddExperiments = Button(self.frmEditExperiment, text="<<", command=self.addExperimentEvent)
        self.btnRemoveExperiments = Button(self.frmEditExperiment, text=">>", command=self.removeExperimentEvent)
         
        
        # arrange objects on grid       
        self.frmMain.grid(row=1,rowspan=2,column=0, sticky=E+W+N+S)   
        self.frmDetail.grid(row=1, column=3, sticky=E+W+N+S )
        frmStatus.grid(row=2,column=3,sticky=N+S+E+W)
        self.frmEditExperiment.grid(row=6, column=3, sticky=N+W )
        self.btnQuit.grid(row=10,columnspan=5, pady=5, padx=10, sticky=E)
        
        # arrange objects on frmMain
        self.chkRelease.grid(row=1, column=0, columnspan=3, sticky=W)
        self.chkDirLess.grid(row=2, column=0, columnspan=3, sticky=W)
        self.chkUnscanned.grid(row=3, column=0, columnspan=3, sticky=W)
        self.grdSlot.grid(row=10, column=0, sticky=N+S+E+W)
        self.btnNewModule.grid(row=20, columnspan=2, sticky=E+W, pady=5, padx=5)        
        
        # arrage objects on frmStatus
        self.lblNumDirLess.grid(row=0, column=1, sticky=W)
        self.lblNumUnscanned.grid(row=1, column=1, sticky=W)
        self.btnModuleScan.grid(row=1,  column=2, sticky=W, padx=5)
        self.btnRefresh.grid(row=2,  column=0, columnspan=3, sticky=E+W, padx=5)
        
        #arrange objects on frmDetail
        self.txtLocationContent.grid(row=0, column=1, sticky=E+W)
        self.btnChangeSlot.grid(row=0, column=2, sticky=E+W)
        self.lblVSNContent.grid(row=1, column=1, columnspan=2, sticky=E+W)
        self.lblStationContent.grid(row=2, column=1, columnspan=2, sticky=E+W)
        self.lblCapacityContent.grid(row=3, column=1, columnspan=2, sticky=E+W)
        self.lblDatarateContent.grid(row=4, column=1, columnspan=2, sticky=E+W)
        self.lblReceivedContent.grid(row=5, column=1, columnspan=2, sticky=E+W)
        self.cboExperiments.grid(row=6, column=1, columnspan=2, sticky=E+W+N+S)
        scrollCboExperiments.grid(row=6,column=3, rowspan=2, sticky=W+N+S)
        self.txtComment.grid(row=8, column=1, columnspan=2, sticky=E+W)
        self.btnEditModule.grid(row=20, column=0, sticky=E+W)
        self.btnDeleteModule.grid(row=20, column=1, sticky=E+W)
        self.btnPrintLibraryLabel.grid(row=21,column=0, sticky=E+W)
        self.btnPrintVSNLabel.grid(row=21,column=1, sticky=E+W)
        self.btnRescan.grid(row=20,column=2, sticky=E+W)
        
        
        # arrange objects on frmEditExperiment
        self.cboFreeExperiments.grid(row=0, column=1, rowspan=2, sticky=W+N+S)
        scrollCboFreeExperiments.grid(row=0,column=2, rowspan=2, sticky=W+N+S)
        self.btnAddExperiments.grid(row=0, column=0, sticky=W)
        self.btnRemoveExperiments.grid(row=1, column=0, sticky=W)
        self.frmEditExperiment.grid_remove()
        
        # bind events to widgets
        self.txtLocationContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblVSNContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblStationContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblCapacityContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblDatarateContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblReceivedContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.cboExperiments.bind("<ButtonRelease-1>", self.selectExperimentEvent)
        self.txtComment.bind("<KeyRelease>", self.editModuleDetailsEvent)
    
    def printVSNLabel(self):
        
        if (self.selectedSlotIndex < 0):
            return
        
        slot = model.Slot()
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
        
        if (slot is not None):
            
            os.system('rm -f /tmp/comedia_vsn.png')
            vsnString = "%s/%s/%s" % (slot.module.vsn, slot.module.capacity, slot.module.datarate)
            
            options = dict(font_size=62, dpi=300, text_distance=0, quiet_zone=1, module_height=10) 
            
            ean = barcode.get_barcode('code39', vsnString, writer=MyImageWriter())
            ean.save('/tmp/comedia_vsn', options )
            
            os.system( self.config.get("Comedia", "printCommand") + ' -o ppi=300 /tmp/comedia_vsn.png')
            os.system('rm -f /tmp/comedia_vsn.png')
    
    def printLibraryLabel(self, slotName=None):
        
        if (slotName == None):
            if (self.selectedSlotIndex == -1):
                return
            else:
                slotName = self.grdSlot.get(self.selectedSlotIndex)[0]
        
        slot = model.Slot()
        slot = session.query(model.Slot).filter_by(location=slotName).one()
        
        
        if (slot > 0):
            
            im = PIL.Image.new("L", (self.labelSizeX,self.labelSizeY),255)

            try:
                font = PIL.ImageFont.truetype(self.config.get("Comedia","fontFile"), int(self.config.get("Comedia", "fontSize")))
            except:
                font = PIL.ImageFont.load_default()
            
            os.system('rm -f /tmp/comedia_tmp.png')
            draw = PIL.ImageDraw.Draw(im)
            draw.text((10,10), self.config.get("Comedia","headerLine"), font=font, fill=1)
            draw.text((10,40),"%s" % slot.location, font=font, fill=1)
            draw.text((10,70),"%s / %s / %s" % (slot.module.vsn, slot.module.capacity, slot.module.datarate) , font=font, fill=1)

            im.save("/tmp/comedia_tmp.png")
            
            os.system( self.config.get("Comedia", "printCommand") + ' /tmp/comedia_tmp.png')
            os.system('rm -f /tmp/comedia_tmp.png')
            
    def updateModule(self):
        
        if (self.selectedSlotIndex == -1):
            return
        
        if (self.isConnected == False):
            return
            
        slot = model.Slot()
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
        
        if (slot > 0):
         
            slot.module.vsn = self.lblVSNContent.get()
            slot.module.stationCode = self.lblStationContent.get()
            slot.module.capacity = self.lblCapacityContent.get()
            slot.module.datarate = self.lblDatarateContent.get()
            slot.module.comment = self.txtComment.get(1.0,END)
            
            # remove all experiment assigments of this module
            slot.module.experiments = []
            for expItem in self.cboExperiments.get(0, END):
                exp = getExperimentByCode(session, expItem)
                
                if (exp != None):
                    slot.module.experiments.append(exp)
            
            #session.update(slot)
            session.commit()
            session.flush()
        
        self.frmEditExperiment.grid_remove()
        self.moduleEdit = 0
        self._saveModuleDetails()
        self.editModuleDetailsEvent(None)
        self.updateSlotListbox()
  
    def callbackExpFilter(self, item):
        
        self.cboExpFilter.configure(text=item)
        self.filterExpVar.set(item)
      
        self.updateSlotListbox()
        
           
    def updateSlotListbox(self):
    
        # deselect active slot
        self.selectedSlotIndex = -1
        
        if (self.isConnected == False):
            return
       
        slots = getOccupiedSlots(session)

        self.grdSlot.delete(0, END)
      
        releaseList = []
        
        self.grdSlot.clearData()
        
        for slot in slots:
            
            expList = []
            for exp in slot.module.experiments:
                    expList.append(exp.code)
                    
                    
            #check if "released" checkbox is activated
            if (self.filterReleaseList.get()):
                if (not isCheckOutAllowed(session, slot.module.vsn)):
                    continue
                else:
                    releaseList.append(slot.module.vsn)
                    
            
            # check if "dirLess" checkbox is activated
            if (self.filterDirLess.get()):
                if (hasDir(slot.module.vsn)):
                    continue
                    
            # check if "uscanned" checkbox is activated
            if (self.filterUnscanned.get()):
                if (slot.module.numScans != None):
                    continue
         
            self.grdSlot.appendData((slot.location, slot.module.vsn, slot.module.stationCode, " ".join(expList), slot.module.numScans, slot.module.capacity, slot.module.datarate, slot.module.received))
            
       
        self.grdSlot.update()
        self.updateSlotDetails()
   
    
    def _saveModuleDetails(self):
        
        self.lastLocationContent = self.txtLocationContent.get()
        self.lastVSNContent = self.lblVSNContent.get()
        self.lastStationContent = self.lblStationContent.get()
        self.lastCapacityContent = self.lblCapacityContent.get()
        self.lastDatarateContent = self.lblDatarateContent.get()
        self.lastReceivedContent = self.lblReceivedContent.get()
        
        self.lastExperiments = self.cboExperiments.get(0,END)
        self.lastComment = self.txtComment.get(1.0, END)
      
         
    def _updateExperimentListboxes(self):
        pass
    
    def updateSlotDetails(self):
         
        
        self.btnPrintVSNLabel["state"] = DISABLED
        self.btnRescan["state"] = DISABLED
        self.btnPrintLibraryLabel["state"] = DISABLED
        self.btnDeleteModule["state"] = DISABLED
        self.txtLocationContent["state"] = NORMAL
        self.lblVSNContent["state"] = NORMAL
        self.lblStationContent["state"] = NORMAL
        self.lblCapacityContent["state"] = NORMAL
        self.lblDatarateContent["state"] = NORMAL
        self.lblReceivedContent["state"] = NORMAL
        self.cboExperiments["state"] = NORMAL
        self.txtComment["state"] = NORMAL
        self.btnChangeSlot["state"] = NORMAL
        
        self.txtLocationContent.delete(0,END)
        self.lblVSNContent.delete(0,END)
        self.lblStationContent.delete(0,END)
        self.lblCapacityContent.delete(0,END)
        self.lblDatarateContent.delete(0,END)
        self.lblReceivedContent.delete(0,END)
        self.cboExperiments.delete(0, END)
        self.txtComment.delete(1.0, END)
        
        self.frmEditExperiment.grid_remove()
        
        if self.selectedSlotIndex == -1:
            self.clearSlotSelection()
            return
        
        if (self.isConnected == False):
            return
        
        self.btnPrintVSNLabel["state"] = NORMAL
        self.btnRescan["state"] = NORMAL
        self.btnPrintLibraryLabel["state"] = NORMAL
        self.btnDeleteModule["state"] = NORMAL
        
        slot = model.Slot()  
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
    
        if (slot != None):
            assignedCodes = []
            
            self.txtLocationContent.insert(0, slot.location)
            self.lblVSNContent.insert(0, slot.module.vsn)
            self.lblStationContent.insert(0, none2String(slot.module.stationCode))
            self.lblCapacityContent.insert(0, slot.module.capacity)
            self.lblDatarateContent.insert(0, slot.module.datarate)
            self.lblReceivedContent.insert(0, slot.module.received)
            self.txtComment.insert(1.0, unicode(none2String(slot.module.comment)))
            
            # update experiment listbox
            for experiment in slot.module.experiments:
                assignedCodes.append(experiment.code)
                self.cboExperiments.insert(END, experiment.code)
                
            # update listbox containing unassigned experiments
            freeExps = getActiveExperimentCodes(session)
            self.cboFreeExperiments.delete(0,END)
            for code in freeExps:
                if code in assignedCodes:
                    continue
                self.cboFreeExperiments.insert(END, code)
                
            self._saveModuleDetails()
            
        self.txtLocationContent["state"] = DISABLED
        self.lblReceivedContent["state"] = DISABLED
        self.cboExperiments["state"] = NORMAL
     
    def clearSlotSelection(self):
        
        self.selectedSlotIndex = -1
        self.moduleEdit = 0
        
        # clear fields in the Details form
        self.txtLocationContent.delete(0,END)
        self.lblVSNContent.delete(0,END)
        self.lblStationContent.delete(0,END)
        self.lblCapacityContent.delete(0,END)
        self.lblDatarateContent.delete(0,END)
        self.lblReceivedContent.delete(0,END)
        self.cboExperiments.delete(0,END)
        self.txtComment.delete(1.0,END)
        
        # disable fields/buttons in the Details form
        self.txtLocationContent["state"] = DISABLED
        self.lblVSNContent["state"] = DISABLED
        self.lblStationContent["state"] = DISABLED
        self.lblCapacityContent["state"] = DISABLED
        self.lblDatarateContent["state"] = DISABLED
        self.lblReceivedContent["state"] = DISABLED
        self.txtComment["state"] = DISABLED
        self.cboExperiments["state"] = DISABLED
        self.btnEditModule["state"] = DISABLED
        self.btnPrintVSNLabel["state"] = DISABLED
        self.btnRescan["state"] = DISABLED
        self.btnPrintLibraryLabel["state"] = DISABLED
        self.btnDeleteModule["state"] = DISABLED
        self.btnChangeSlot["state"] = DISABLED
        
        # reset colors
        self.txtLocationContent["bg"] = self.defaultBgColor
        self.lblVSNContent["bg"] = self.defaultBgColor
        self.lblStationContent["bg"] = self.defaultBgColor
        self.lblCapacityContent["bg"] = self.defaultBgColor
        self.lblDatarateContent["bg"] = self.defaultBgColor
        self.lblReceivedContent["bg"] = self.defaultBgColor
        self.cboExperiments["bg"] = self.defaultBgColor
        self.txtComment["bg"] = self.defaultBgColor
        
        self.frmEditExperiment.grid_remove()
        
        # save contents of the Detail form fields
        self._saveModuleDetails
        
    def checkOutModule(self):
            
        if (self.selectedSlotIndex == -1):
            return
        
        slot = model.Slot()    
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])

        if (slot == None):
            return
        
        # delete module
        module = model.Module()
        
        try:
            module = session.query(model.Module).filter_by(id = slot.module.id).one()
        except:
            return
        
        
        if (isCheckOutAllowed(session,module.vsn) == False):
            self.checkoutDlg.show(module)
            #tkMessageBox.showerror("Error", "Module cannot be checked-out.\nIt  contains unreleased experiments\nor\nIt hasn't been scanned yet.")
            
            return
        elif (module.numScans == 0):
            if (tkMessageBox.askyesno("Empty module", "This module seems to be empty\nDo you really want to check-out this module") == False):
                return
            

        if (tkMessageBox.askokcancel("Confirm module check-out", "Do you really want to remove module " + slot.module.vsn + " from the library? ")):
            self.doCheckout(module)
        
        return
    
    def doCheckout(self, module):
        
        session.delete(module) 
        session.commit()
        session.flush()

        self.clearSlotSelection()

        self.updateSlotListbox()
        self.refreshStatusEvent()

        # delete .dir file            
        dirFile = buildDirFilename(settings["dirPath"], module.vsn)
        if os.path.isfile(dirFile):
            os.remove(dirFile)
        else:
            print "file %s does not exists" % dirFile
            
        
    def clearSearchEvent(self, Event):
        
        self.moduleFilter = ""
        self.slotFilter = ""
        
        self.updateSlotListbox()
        
    def selectExperimentEvent(self, Event):
        
        if (self.selectedSlotIndex == -1):
            return
        
        self.frmEditExperiment.grid()
        
    def removeExperimentEvent(self):
        
        if (len(self.cboExperiments.curselection()) == 0):
            return
        selection = list(self.cboExperiments.curselection())
        selection.reverse()
        for exp in selection:
            code = self.cboExperiments.get(exp)
            self.cboExperiments.delete(exp)
            self.cboFreeExperiments.insert(END, code)
        
        self.editModuleDetailsEvent(None)
        
    def addExperimentEvent(self):
        
        if (len(self.cboFreeExperiments.curselection()) == 0):
            return
        
        selection = list(self.cboFreeExperiments.curselection())
        selection.reverse()
        
        for exp in selection:
            code = self.cboFreeExperiments.get(exp)
            self.cboFreeExperiments.delete(exp)
            self.cboExperiments.insert(END, code)   
        
        self.editModuleDetailsEvent(None)
 
    def rescanModuleEvent(self):
        
        if self.selectedSlotIndex == -1:
            return
       
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
        
        self.scanModulesDlg.scanModules(slot.module)

      
    def scanModuleEvent(self):
       
        self.scanModulesDlg.scanModules()
      #  self.refreshStatusEvent()
      #  self.updateSlotListbox()
            
            
    def refreshStatusEvent(self):
        
        dirLessCount = 0
        unvalidatedCount = 0
        
        slots = getOccupiedSlots(session)
             
        for slot in slots:
            
            # find modules without .dir file
            if (not hasDir(slot.module.vsn)):
                dirLessCount += 1
        
            # find unvalidated modules
            if (slot.module.numScans == None):
                unvalidatedCount += 1
        
        
        self.lblNumDirLess["text"] = dirLessCount
        self.lblNumUnscanned["text"] = unvalidatedCount - dirLessCount
        
        if (unvalidatedCount - dirLessCount > 0):
            self.btnModuleScan["state"] = NORMAL
        else:
            self.btnModuleScan["state"] = DISABLED
                
        
        
    def selectSlotEvent(self, Event):
    
        # check for unsaved module edits
        if (self.moduleEdit > 0):
            if (tkMessageBox.askyesno("Cancel unsaved changes", "There are unsaved changes in the module details\nAre you sure you want to abandon these?") == False):
                self.grdSlot.selection_clear(self.grdSlot.curselection())
                self.grdSlot.selection_set(self.selectedSlotIndex)
                self.frmEditExperiment.grid_remove()
                return
            else:
                self._saveModuleDetails()
                self.editModuleDetailsEvent(None)
     
        if (len(self.grdSlot.curselection()) > 0):
            self.selectedSlotIndex =  self.grdSlot.curselection()
        else:
            self.selectedSlotIndex =  -1
        
        self.updateSlotDetails()

    def searchSlotEvent(self, Event):
        #self.slotFilter = upper(strip(self.txtSearchSlot.get()))
        self.updateSlotListbox()
    
    def searchModuleEvent(self, Event):
         
        self.moduleFilter = upper(strip(self.txtSearch.get()))   
        self.updateSlotListbox()
        
        
    def editModuleDetailsEvent(self, Event):
        
        self.moduleEdit = 0
        
        color = self.defaultBgColor
        editColor = "wheat"
        
        if (self.lastLocationContent != self.txtLocationContent.get()):
            self.txtLocationContent["background"] = editColor
            self.moduleEdit += 1
        else:
            self.txtLocationContent["background"] = color
           
            
        if (self.lastVSNContent != self.lblVSNContent.get()):
            self.lblVSNContent["background"] = editColor
            self.moduleEdit += 1
            
        else:
            self.lblVSNContent["background"] = color
            
        if (self.lastStationContent != self.lblStationContent.get()):
            self.lblStationContent["background"] = editColor
            self.moduleEdit += 1
            
        else:
            self.lblStationContent["background"] = color
            
            
        if (self.lastCapacityContent != self.lblCapacityContent.get()):
            self.lblCapacityContent["background"] = editColor
            self.moduleEdit += 1

        else:
            self.lblCapacityContent["background"] = color
            
            
        if (self.lastDatarateContent != self.lblDatarateContent.get()):
            self.lblDatarateContent["background"] = editColor
            self.moduleEdit += 1
        else:
            self.lblDatarateContent["background"] = color
          
            
        if (self.lastReceivedContent != self.lblReceivedContent.get()):
            self.lblReceivedContent["background"] = editColor
            self.moduleEdit += 1
        else:
            self.lblReceivedContent["background"] = color
        
        if (sorted(self.cboExperiments.get(0, END)) != sorted(self.lastExperiments)):
            self.cboExperiments["background"] = editColor
            self.moduleEdit +=1
        else:
            self.cboExperiments["background"] = color
            
        if (self.lastComment != self.txtComment.get(1.0,END)):
            self.txtComment["background"] = editColor
            self.moduleEdit += 1
            
        else:
            self.txtComment["background"] = color
      
        if self.moduleEdit > 0:
            self.btnEditModule["state"] = NORMAL
            self.btnEditModule["bg"] = editColor
            self.btnEditModule["activebackground"] = editColor
        else:
            self.btnEditModule["bg"] = self.defaultBgColor 
            self.btnEditModule["activebackground"] =  self.defaultBgColor 
            self.btnEditModule["state"] = DISABLED
            
        
    def checkinModule(self):
        
        self.checkinDlg.show()
        
    def showLabelOptions(self):
             
        self.labelOptionsDlg.config = self.config
        self.labelOptionsDlg.show()
        
    def showDatabaseOptions(self):
             
        self.databaseOptionsDlg.config = self.config
        self.databaseOptionsDlg.show()
    
    def showChangeSlotWindow(self):
        
        if (self.selectedSlotIndex < 0):
            return
        
        slot = model.Slot()
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
        
        self.changeSlotDlg.selectedSlot = slot
        self.changeSlotDlg.show()
        
class CheckoutWindow(GenericWindow):
    
    def __init__(self, parent=None, rootWidget=None):
        
        # call super class constructor
        super( CheckoutWindow, self ).__init__(parent, rootWidget)
        
        
    def show(self, module):
        
        self.module = module
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title("Check-out module")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.focus_set()
        self.dlg.grab_set()
        
        self._setupWidgets()
        #self.updateExperimentListbox()
        
    def _setupWidgets(self):
        
        Label(self.dlg, text="Module cannot be checked out.\n", font="Helvetica 10 bold", fg="red").grid(row=0, sticky=W)    
        Label(self.dlg, text="It  contains unreleased experiments or hasn't been scanned yet.").grid(row=1, sticky=W)
        
        btnOK = Button(self.dlg, text="OK", command=self.dlg.destroy)
        btnForce = Button(self.dlg, text="Force check-out", command=self._onButtonForce)
        
        btnOK.grid(row=10, column=1, sticky=W,pady=7)
        btnForce.grid(row=10, column=3, sticky=E+W)
        
    def _onButtonForce(self):
        if  tkMessageBox.askyesno("Force module check out?", "Do you really want to check out this module?") == True:
            self.parent.doCheckout(self.module)
            self.dlg.destroy()
            
        self.dlg.destroy()
        return 
        
        
class CheckinWindow(GenericWindow):
     
    def __init__(self, parent=None, rootWidget=None):
        
        # call super class constructor
        super( CheckinWindow, self ).__init__(parent, rootWidget)
        
        self.addExperimentDlg = AddExperimentWindow(self, rootWidget)
        
            
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title("Check-in module")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.focus_set()
        self.dlg.grab_set()
        
        self.chkPrintLibLabelVar = IntVar()
        self.chkPrintLibLabelVar.set(1)
        
        self._setupWidgets()
        self.updateExperimentListbox()
     
    def updateExperimentListbox(self):
         
        self.lstExp.delete(0,END)
        
        # obtain listbox items from database
        experiments = getActiveExperimentCodes(session)
        for code in experiments:
            self.lstExp.insert(END, code)
        
    def _setupWidgets(self):
        
        # create dialog elements
        yScroll = Scrollbar ( self.dlg, orient=VERTICAL )
        yScroll2 = Scrollbar ( self.dlg, orient=VERTICAL )

        Label(self.dlg, text="VSN").grid(row=0)
        Label(self.dlg, text="Slot").grid(row=1)
        Label(self.dlg, text="Experiment(s)\n(optional)").grid(row=3)

        self.txtVSN = Entry(self.dlg)

        self.lstSlot = Listbox(self.dlg, yscrollcommand=yScroll.set, height=5, exportselection = False )
        self.lstExp = Listbox(self.dlg, yscrollcommand=yScroll2.set, height=5 , selectmode=MULTIPLE, exportselection = False)
        chkPrintLibLabel = Checkbutton(self.dlg, text = "print library label", variable = self.chkPrintLibLabelVar)
        yScroll.config(command=self.lstSlot.yview)
        yScroll2.config(command=self.lstExp.yview)

        # populate slot list
        ciSlotItems = getEmptySlots(session)
        for instance in ciSlotItems:
            self.lstSlot.insert(END, instance.location)

        #frame = LabelFrame(self.dlg)
        btnOK = Button(self.dlg, text="OK", command=self._persistSlot)
        btnCancel = Button(self.dlg, text="Cancel", command=self.dlg.destroy)
        btnAddExp = Button(self.dlg, text="Add exp.", command=self._addExperiment)

        # arrange elements on grid
        self.txtVSN.grid(row=0, column=1)
        self.lstSlot.grid(row=1, column=1)
        self.lstExp.grid(row=3, column=1)
        chkPrintLibLabel.grid(row=4,column=1, sticky=W)

        #frame.grid(row=10, column=0, columnspan=4, sticky=E+W)
        btnOK.grid(row=10, column=1, sticky=W,pady=7)
        btnCancel.grid(row=10, column=3, sticky=E+W)
        btnAddExp.grid(row=3, column=3, sticky=E+W)
        yScroll.grid ( row=1, column=2, sticky=W+N+S )
        yScroll2.grid ( row=3, column=2, sticky=W+N+S )
        
        self.txtVSN.focus_set()
        
        
    def _splitVSNLabelScan(self):
        
        m = re.match('([a-zA-Z]+[\+-]\d+)/(\d+)/(.+)', self.txtVSN.get().lstrip())
     
        if (m != None):
            vsn = upper(m.group(1))

            
            if (len(vsn) != 8):
                raise Exception("Illegal VSN")

            capacity = m.group(2)
            datarate = m.group(3)
            # drop CRC code at end of datarate (appears when VSN label is scanned)
            datarate = datarate[:-1]
            
            return(vsn, capacity, datarate)
        else:
            raise Exception("Illegal VSN label")
    
    def _addExperiment(self):
        self.addExperimentDlg.show()
    
    def _persistSlot(self):
     
        error = ""
        
        # check that VSN has been set
        if (self.txtVSN.get() == ""):
            error += "Empty VSN\n"

        # check that start slot has been set
        if (len(self.lstSlot.curselection()) == 0):
            error += "Empty slot\n"
                
            
        if (error != ""):
            tkMessageBox.showerror("Error", error)
            return
        
        try:
            vsn, capacity, datarate = self._splitVSNLabelScan()

        except:
            tkMessageBox.showerror("Error", "Illegal VSN label content. Must be VSN/capacity/datarate.")
            return
        
        if (moduleExists(session, vsn)):
            tkMessageBox.showerror("Error","Module\n%s\nalready checked-in" % vsn)
            return

        # retrieve currently selected item from slot select box
        selectedSlot = model.Slot() 
        selectedSlot = session.query(model.Slot).filter_by(location=self.lstSlot.get(self.lstSlot.curselection()[0])).one()
        
        # create new Module object
        if (selectedSlot != None):

            newModule = model.Module()

            newModule.vsn = vsn
            newModule.capacity = capacity
            newModule.datarate = datarate
            newModule.slot = selectedSlot

            session.add(newModule)

            # append selected experiments
            for expItem in self.lstExp.curselection():

                exp = model.Experiment()
                exp = session.query(model.Experiment).filter_by(code=self.lstExp.get(expItem)).one()

                newModule.experiments.append(exp)

            session.commit()
            session.flush()

            if (self.chkPrintLibLabelVar.get() == 1):
                self.parent.printLibraryLabel(slotName = selectedSlot.location)
            self.parent.updateSlotListbox()
            
            self.dlg.destroy()
         

        return
    
class ChangeSlotWindow(GenericWindow):
    
    def __init__(self, parent, rootWidget=None):
        
        # call super class constructor
        super( ChangeSlotWindow, self ).__init__(parent, rootWidget) 
        
        self.selectedSlot = None
        self.chkPrintLibLabelVar = IntVar()
        self.chkPrintLibLabelVar.set(1)
        
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title ("Change module slot")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()
        
        self._setupWidgets()
    
    
    def _setupWidgets(self):
        
        yScroll = Scrollbar ( self.dlg, orient=VERTICAL )
        
        Label(self.dlg, text="Change slot for module " ).grid(row=1, column=0, sticky=W)
        Label(self.dlg, text=self.selectedSlot.module.vsn).grid(row=1, column=1, sticky=W)
        Label(self.dlg, text="Current slot: " ).grid(row=2, column=0, sticky=W)
        Label(self.dlg, text=self.selectedSlot.location).grid(row=2, column=1, sticky=W)
        Label(self.dlg, text="New slot: " ).grid(row=3, sticky=W)
        self.lstSlot = Listbox(self.dlg, yscrollcommand=yScroll.set, height=5, exportselection = False)   
        yScroll.config(command=self.lstSlot.yview)
        chkPrintLibLabel = Checkbutton(self.dlg, text = "Print new library label", variable = self.chkPrintLibLabelVar).grid(row=5, column=0, sticky=W)
        
        Button(self.dlg, text="OK", command=self._persistSlot).grid(row=10, column=0, sticky=E+W)
        Button(self.dlg, text="Cancel", command=self.dlg.destroy).grid(row=10, column=1, sticky=E+W) 
        
        # populate slot list
        ciSlotItems = getEmptySlots(session)
        for instance in ciSlotItems:
            self.lstSlot.insert(END, instance.location)
      
        self.lstSlot.grid(row=3, column=1)
        yScroll.grid ( row=3, column=2, sticky=W+N+S )
        
    def _persistSlot(self):
        
        if self.selectedSlot == None:
            return
        if self.lstSlot.get(self.lstSlot.curselection()[0]) == "":
            return
        
        module = self.selectedSlot.module
        
        newLocation =  self.lstSlot.get(self.lstSlot.curselection()[0])
        
        newSlot = getSlotByLocation(session, newLocation )
        
        self.selectedSlot.module = None
        session.commit()
        
        module.slot = newSlot
            
        session.commit()
        session.flush()
        
        if (self.chkPrintLibLabelVar.get() == 1):
            self.parent.printLibraryLabel(slotName = newSlot.location)
        
        self.parent.updateSlotListbox()
        
        self.dlg.destroy()
        
class DatabaseOptionsWindow(GenericWindow):
     
    def __init__(self, parent, rootWidget=None):
        
        # call super class constructor
        super( DatabaseOptionsWindow, self ).__init__(parent, rootWidget)
             
        
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title ("Database Options")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()
        
        self._setupWidgets()
    
    
    def _setupWidgets(self):
              
        Label(self.dlg, text="Type").grid(row=0, sticky=W)
        Label(self.dlg, text="Server").grid(row=1, sticky=W)
        Label(self.dlg, text="Port").grid(row=2, sticky=W)
        Label(self.dlg, text="Database").grid(row=3, sticky=W)
        Label(self.dlg, text="Username").grid(row=4, sticky=W)
        Label(self.dlg, text="Password").grid(row=5, sticky=W)
        
        optionList = ("mysql", "postgresql", "oracle")
        self.cboDBTypeVar = StringVar()
        self.cboDBTypeVar.set(self.config.get("Database", "type"))
        self.cboDBType = OptionMenu ( self.dlg, self.cboDBTypeVar, *optionList )

        
        self.txtServer = Entry(self.dlg)
        self.txtPort = Entry(self.dlg)
        self.txtDatabase = Entry(self.dlg)
        self.txtUsername= Entry(self.dlg)
        self.txtPassword = Entry(self.dlg)
       
        Button(self.dlg, text="OK", command=self.saveConfig).grid(row=10, column=0, sticky=E+W)
        Button(self.dlg, text="Cancel", command=self.dlg.destroy).grid(row=10, column=1, sticky=E+W) 
        Button(self.dlg, text="Test Connection", command=self._checkDatabaseConnection).grid(row=9, column=0, sticky=E+W) 
        
        self.cboDBType.grid(row=0, column=1,sticky=E+W)
        self.txtServer.grid(row=1, column=1,sticky=E+W)
        self.txtPort.grid(row=2, column=1, sticky=E+W)
        self.txtDatabase.grid(row=3, column=1, sticky=E+W)
        self.txtUsername.grid(row=4, column=1, sticky=E+W)
        self.txtPassword.grid(row=5, column=1, sticky=E+W)
    
        self.txtServer.insert(0, self.config.get("Database", "server"))
        self.txtPort.insert(0, self.config.get("Database", "port"))
        self.txtDatabase.insert(0, self.config.get("Database", "database"))
        self.txtUsername.insert(0, self.config.get("Database", "user"))
        self.txtPassword.insert(0, self.config.get("Database", "password"))
    
        
    def _checkDatabaseConnection(self):
                
        connection = Connection()
        connection.type = self.cboDBTypeVar.get()
        connection.server = self.txtServer.get()
        connection.port = self.txtPort.get()
        connection.user = self.txtUsername.get()
        connection.password = self.txtPassword.get()
        connection.database = self.txtDatabase.get()

        try:
            Schema(connection)

            tkMessageBox.showinfo("Check database connection", "Connection to database successful")
        except Exception as e:
            tkMessageBox.showerror("Check database connection", "Connection to database failed %s" % e)
    
    def saveConfig(self):
        
        self.config.set("Database", "type", self.cboDBTypeVar.get())
        self.config.set("Database", "server", self.txtServer.get())
        self.config.set("Database", "port", self.txtPort.get())
        self.config.set("Database", "database", self.txtDatabase.get())
        self.config.set("Database", "user", self.txtUsername.get())
        self.config.set("Database", "password", self.txtPassword.get())
        
        self.config.writeConfig()
            
        self.dlg.destroy()
 
class ScanModulesWindow(GenericWindow):
 
    def __init__(self, parent, rootWidget=None):
        
        # call super class constructor
        super( ScanModulesWindow, self ).__init__(parent, rootWidget)
        
        self.checkList = deque()
        self.manualList = deque()
        
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title("Scan Module Directories")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()

        
        self._setupWidgets()
    
    
    def _setupWidgets(self):
        
        rowCount = 1
       
        canvas = Canvas(self.dlg, width=800)
        canvas.rowconfigure(0, weight=1)
        canvas.columnconfigure(0, weight=1)
        yBar = Scrollbar(self.dlg)
        xBar = Scrollbar(self.dlg)
        xBar.config(command=canvas.xview, orient=HORIZONTAL)
        yBar.config(command=canvas.yview)                   
        canvas.config(xscrollcommand=xBar.set, yscrollcommand=yBar.set)
        
        # make the canvas expandable
        root.grid_rowconfigure(0, weight=1)
        root.grid_columnconfigure(0, weight=1)
 
        frmModules = Frame(canvas)
        frmModules.rowconfigure(1, weight=1)
        frmModules.columnconfigure(1, weight=1)
        
        if (len(self.checkList) > 0):
            
            Label(frmModules, text="module", relief="flat").grid(row=0,column=0, sticky=E+W)
            Label(frmModules, text="assigned exp.", relief="flat").grid(row=0,column=1, sticky=E+W)
            Label(frmModules, text="parsed exp.",relief="flat").grid(row=0,column=2, sticky=E+W)
            Label(frmModules, text="parse errors.",relief="flat").grid(row=0,column=3, sticky=E+W)

            for module in self.checkList:
                if module.parseErrors > 0:
                    color = "red"
                else:
                    color = "black"
                errorString = "%s / %s" % (module.parseErrors, module.numScans)
                Label(frmModules, text=module.vsn, relief="sunken", justify="left",padx=10).grid(row=rowCount,column=0, sticky=E+W)
                Label(frmModules, text=list(module.assignedExps), relief="sunken", justify="left", padx=10).grid(row=rowCount,column=1, sticky=E+W)
                Label(frmModules, text=list(module.scannedExps),relief="sunken", justify="left", padx=10).grid(row=rowCount,column=2, sticky=E+W)
                Label(frmModules, text=errorString,fg = color,relief="sunken", justify="left", padx=10).grid(row=rowCount,column=3, sticky=E+W)
                
                # if no experiments have been scanned or assigned don't allow to close this case
                if len(module.scannedExps) > 0 or len(module.assignedExps) > 0:
                    Radiobutton(frmModules, text="fix", variable=module.action, value=0, state=NORMAL).grid(row=rowCount, column=5)
                    Radiobutton(frmModules, text="don't fix, don't ask again", variable=module.action, value=2).grid(row=rowCount, column=7)
                Radiobutton(frmModules, text="resolve later", variable=module.action, value=1, state=NORMAL).grid(row=rowCount, column=6)
                

                rowCount += 1
        
        if (len(self.manualList) > 0):
            Label(frmModules, text="The following modules contain non-standard scan names that cannot be decoded to obtain the experiment code", relief="flat").grid(row=rowCount,column=0, columnspan=5, sticky=E)
            Label(frmModules, text="module", relief="flat").grid(row=rowCount+1,column=0, sticky=E+W)
            
            rowCount += 2
            
            for module in self.manualList: 
                Label(frmModules, text=module.vsn, relief="sunken", justify="left",padx=10).grid(row=rowCount,column=0, sticky=E+W)
                Label(frmModules, text="Please assign the experiment(s) manually", relief="sunken", justify="left", padx=10).grid(row=rowCount,column=1, sticky=E+W)
                rowCount += 1
            
        btnOK = Button(self.dlg, text="OK", command=self.updateModuleEvent)
        btnCancel = Button(self.dlg, text="Cancel", command=self.dlg.destroy)
        
        canvas.grid(row=0,column=0,sticky=N+S+E+W)
        xBar.grid(row=1,column=0, sticky=E+W)
        yBar.grid(row=0,column=1, sticky=N+S)
        btnOK.grid(row=10, column=0, sticky=E)
        btnCancel.grid(row=10, column=1, sticky=E+W)
        
        canvas.create_window(0, 0, anchor=NW, window=frmModules)
        frmModules.update_idletasks()

        canvas.config(scrollregion=canvas.bbox("all"))
        
    def scanModules(self, module=None):
        
        outdatedDir = []
        self.checkList.clear()
        self.manualList.clear()
        modules= []
        
        if module == None:
            modules = getUnscannedModules(session)
        else:
            # delete information from previous scan
            module.numScans = None
            module.experiments= []
            module.stationCode = None
            
            session.commit()        
            modules.append(module)
            
            
        for module in modules:
            
            assignedExps = deque()
            
            # check if .dir file exists
            if (not hasDir(module.vsn)):
                continue
                     
            try:
                difxdir = DifxDir(settings["dirPath"], module.vsn)
            except Exception as e:
                tkMessageBox.showerror("Error", e)
                continue
                
            scannedExps = difxdir.getExperiments()
            
            if (difxdir.getFileDate() < time.mktime(module.received.timetuple())):
                outdatedDir.append(difxdir.getFilename())
                continue
            
            # compare associated experiments
            for exp in module.experiments:
                assignedExps.append(exp.code)
              
            if (sorted(scannedExps) != sorted(assignedExps)) or (difxdir.getParseErrorCount() > 0):
              
                checkModule = self.CheckModuleItem()
                checkModule.vsn = module.vsn
                checkModule.assignedExps = assignedExps
                checkModule.scannedExps = scannedExps
                checkModule.numScans = difxdir.getScanCount()
                checkModule.stationCode = difxdir.getStationCode()
                checkModule.parseErrors = difxdir.getParseErrorCount() 

                self.checkList.append(checkModule)
                
          #  elif (difxdir.getParseErrorCount() > 0):
          #      warnModule = self.CheckModuleItem()
          #      warnModule.vsn = module.vsn
          #      self.manualList.append(warnModule)
                
            else:
                #print "scanned ", module.vsn
                # update module information
                module.numScans = difxdir.getScanCount()
                module.stationCode = difxdir.getStationCode()
           
        session.commit()
        session.flush()
        
        if (len(outdatedDir) > 0):
            errStr = ""
            for file in outdatedDir:
                errStr +=  os.path.basename(file) + "\n"
            tkMessageBox.showerror("Error", "The following files have creation dates earlier than their module check-in date.\nProbably these files should be deleted manually.\n%s" % errStr)
            
        if (len(self.checkList) > 0) or (len(self.manualList) > 0):
            self.show()
            
       # self.parent.refreshStatusEvent()
      #  self.parent.updateSlotListbox()
            
    
     
    def updateModuleEvent(self):
        
        for checkModule in self.checkList:
            
            module = getModuleByVSN(session,checkModule.vsn)
            
            if (module == None):
                continue
            
            # action was 'fix' 
            if (checkModule.action.get() == 0):
                # remove all experiment assignments of this module
                module.experiments = []
                
                # loop over all scanned experiment codes
                for expCode in checkModule.scannedExps:
                    
                    if (not experimentExists(session, expCode)):
                       
                        # add new experiment
                        addExperiment(session, expCode)
                        
                        
                    exp = getExperimentByCode(session, expCode)
                    module.experiments.append(exp)
                    module.numScans = checkModule.numScans
                    module.stationCode = checkModule.stationCode
                        
                session.commit()
                session.flush()
                
                continue
            # action was 'remind again'
            elif (checkModule.action.get() == 1):
                continue
            #action was 'don't remind again'
            elif (checkModule.action.get() == 2):
                module.numScans = checkModule.numScans
                session.commit()
                session.flush()
                
        session.commit()
        session.flush()
        
        self.parent.refreshStatusEvent()
        self.parent.updateSlotListbox()
        self.dlg.destroy()
        
    class CheckModuleItem(object):
            
        def __init__(self):

            self.vsn = ""
            self.assignedExps = deque()
            self.scannedExps = deque()
            self.action = IntVar()
            self.numScans = 0
            self.parseErrors = 0
            
            self.action.set(0)
        
    
class LabelOptionsWindow(GenericWindow):
     
    def __init__(self, parent, rootWidget=None):
        
        # call super class constructor
        super( LabelOptionsWindow, self ).__init__(parent, rootWidget)
        
        
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title("Label Options")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        
        self.dlg.grab_set()
        
        self._setupWidgets()
    
    
    def _setupWidgets(self):
              
        Label(self.dlg, text="Label header").grid(row=0, sticky=W)
        Label(self.dlg, text="Label font ttf file").grid(row=1, sticky=W)
        Label(self.dlg, text="Label font size").grid(row=2, sticky=W)
        Label(self.dlg, text="Label print command").grid(row=3, sticky=W)
        
        
        self.txtLabelHeader = Entry(self.dlg)
        self.txtFontFile = Entry(self.dlg)
        self.txtFontSize = Entry(self.dlg)
        self.txtPrintCommand = Entry(self.dlg)
       
        Button(self.dlg, text="OK", command=self.saveConfig).grid(row=10, column=0, sticky=E+W)
        Button(self.dlg, text="Cancel", command=self.dlg.destroy).grid(row=10, column=1, sticky=E+W) 
        
       
        self.txtLabelHeader.grid(row=0, column=1,sticky=E+W)
        self.txtFontFile.grid(row=1, column=1,sticky=E+W)
        self.txtFontSize.grid(row=2, column=1,sticky=E+W)
        self.txtPrintCommand.grid(row=3, column=1, sticky=E+W)
    
        self.txtLabelHeader.insert(0, self.config.get("Comedia", "headerLine"))
        self.txtFontFile.insert(0, self.config.get("Comedia", "fontFile"))
        self.txtFontSize.insert(0, self.config.get("Comedia", "fontSize"))
        self.txtPrintCommand.insert(0, self.config.get("Comedia", "printCommand"))
        
    
    def saveConfig(self):
        self.config.set("Comedia", "headerLine", self.txtLabelHeader.get())
        self.config.set("Comedia", "fontFile", self.txtFontFile.get())
        self.config.set("Comedia", "fontSize", self.txtFontSize.get())
        self.config.set("Comedia", "printCommand", self.txtPrintCommand.get())
        
        
        self.config.writeConfig()

        self.dlg.destroy()
        
class AddExperimentWindow(GenericWindow):
     
    def __init__(self, parent, rootWidget=None):
        super( AddExperimentWindow, self ).__init__(parent, rootWidget)
      
    
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.parent.rootWidget, takefocus=True)
        self.dlg.title("Add experiment")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()
    
        
        self._setupWidgets()
    
    def close(self):
        
        self.parent.dlg.grab_set()
        self.parent.updateExperimentListbox()
       # self.parent.parent.updateExpFilter()
        self.parent.txtVSN.focus_set()
        self.dlg.destroy()
        
    def _setupWidgets(self):
              
        Label(self.dlg, text="Code").grid(row=0, sticky=W)
        self.txtExpCode = Entry(self.dlg)
        
        
        btnOK = Button(self.dlg, text="OK", command=self._persistExperiment)
        btnCancel = Button(self.dlg, text="Cancel", command=self.close)
        
        self.txtExpCode.grid(row=0, column=1, sticky=E+W)
        btnOK.grid(row=10, column=0)
        btnCancel.grid(row=10, column=1, sticky=E)
        
        self.txtExpCode.focus_set()
        
    def _persistExperiment(self):
        
        code = upper(self.txtExpCode.get())
        # check that Code has been set
        if (code == ""):
            return
        
        try:
            # add experiment with state "scheduled"
            addExperimentWithState(session, code, 10)
        except Exception as e:
            tkMessageBox.showerror("Error", e)
        
     
        self.close()
        
class MyImageWriter(ImageWriter):
    
    def _mm2px(self, mm, dpi=300):
        return (mm * dpi) / 25.4

    def calculate_size(self, modules_per_line, number_of_lines, dpi=300):
        
        width = 2 * self.quiet_zone + modules_per_line * self.module_width
        height = 1.0 + self.module_height * number_of_lines
        if self.text:
            height += (self.font_size + self.text_distance) / 3

        return int(self._mm2px(width, dpi)), int(self._mm2px(height, dpi))

    def _paint_text(self, xpos, ypos):
        # align font to the left side of the bar code
        xpos = self.quiet_zone
        pos = (self._mm2px(xpos, self.dpi), self._mm2px(ypos, self.dpi))
        font = PIL.ImageFont.truetype(FONT, self.font_size)
        self._draw.text(pos, self.text, font=font, fill=self.foreground)
        
 
class ComediaConfig(DifxDbConfig):

    def makeDefaultConfig(self):
        
        super(ComediaConfig, self).makeDefaultConfig()
        
        self.config.add_section('Comedia')
        self.config.set('Comedia', 'headerLine', 'Correlator Media Library')
        self.config.set('Comedia', 'fontFile', '/usr/share/fonts/truetype/arial.ttf')
        self.config.set('Comedia', 'fontSize', '24')
        self.config.set('Comedia', 'printCommand', 'lpr -P')   
          
#def getEmptySlots():   
    
#    result =  session.query(model.Slot).order_by(model.Slot.location).filter_by(isActive = 1).order_by(model.Slot.location).filter(model.Slot.moduleID == None)
    
#    return(result)

if __name__ == "__main__":
    
    dbConn = None
    session = None
    settings = {}
    
    configName = 'difxdb.ini'
    
    root = Tk()
    
    mainDlg = MainWindow(None, rootWidget=root)
    
    #  check for DIFXROOT environment
    if (os.getenv("DIFXROOT") == None):
        sys.exit("Error: environment variable DIFXROOT must be defined.")
    settings["difxRoot"] = os.getenv("DIFXROOT")
    settings["configFile"] = settings["difxRoot"] + "/conf/" + configName
    
    
    if not os.path.isfile(settings["configFile"]):
        createConfig = True
        print "Created initial configuration file ( %s ) for you.\n" % settings["configFile"]
        print "Please edit this file and restart comedia\n"
        
    else:
        createConfig = False
            
    # read the configuration file
    config = ComediaConfig(settings["configFile"], create=createConfig)
    
    if (createConfig):
        sys.exit()
        
    if (os.getenv("MARK5_DIR_PATH") == None):
        sys.exit("Error: environment variable MARK5_DIR_PATH must be defined.")
    else:
        settings["dirPath"] = os.getenv("MARK5_DIR_PATH")
        
    
    # try to open the database connection
    connection = Connection()
    connection.type = config.get("Database", "type")
    connection.server = config.get("Database", "server")
    connection.port = config.get("Database", "port")
    connection.user = config.get("Database", "user")
    connection.password = config.get("Database", "password")
    connection.database = config.get("Database", "database")
    
    connection.echo = False
    
    try:
        dbConn = Schema(connection)
        session = dbConn.session()

        mainDlg.isConnected = True
        
        if not isSchemaVersion(session, minSchemaMajor, minSchemaMinor):
            major, minor = getCurrentSchemaVersionNumber(session)
            print "Current difxdb database schema is %s.%s but %s.%s is minimum requirement." % (major, minor, minSchemaMajor, minSchemaMinor)
            sys.exit(1)
        
    except Exception as e:
        print "Error: ",  e, "\nPlease check your database settings in %s " % settings["configFile"] 
        mainDlg.isConnected = False
        sys.exit()
        
        

    mainDlg.config = config
    
    mainDlg.show()
    
    root.mainloop()


    
    
    
  
