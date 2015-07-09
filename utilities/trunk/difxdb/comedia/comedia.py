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
import subprocess

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
	self.editExpDlg = EditExperimentsWindow(self, rootWidget)
        
        self.defaultBgColor = rootWidget["background"]
        self.isConnected = False
        
        self.selectedSlotIndex = -1 
        self.moduleEdit = 0
        self.filterReleaseList = IntVar()
        self.filterDirLess = IntVar()
        self.filterUnscanned = IntVar()
        self.filterExpVar = StringVar()
        self.filterModuleTypeVar = StringVar()
        
        self.expFilterItems = []
        self.labelSizeX = 320
        self.labelSizeY = 130
        self.moduleFilter = ""
        self.slotFilter = ""
        self.forceCheckout = False
        self.moduleTypes = ['all','SATA','PATA','Mark6']
        
        self.filterModuleTypeVar.set(self.moduleTypes[0])
        
        # regular expressions for matching various VSN types
        self.patternSataVSN = re.compile('([a-zA-Z]+[-]\d+)')
        self.patternPataVSN = re.compile('([a-zA-Z]+[\+]\d+)')
        self.patternMark6VSN = re.compile('([a-zA-Z]+[%]\d+)')
                
    def show(self):
        
        self._setupWidgets()
        self.refreshStatusEvent()
        self.updateSlotListbox()
        
        
    def _setupWidgets(self):
        
     
        # menubar setup
        menubar = Menu(self.parent)
        
        optionmenu = Menu(menubar, tearoff=0)
        optionmenu.add_command(label="Label options", command=self.showLabelOptions)
        optionmenu.add_command(label="Database options", command=self.showDatabaseOptions)

        menubar.add_cascade(label="Options", menu=optionmenu)

        self.rootWidget.config(menu=menubar)
        
        # frames
        self.frmFilter = LabelFrame(self.rootWidget, text="Filter")
        self.frmMain = LabelFrame(self.rootWidget, text="Modules")
        self.frmDetail = LabelFrame(self.rootWidget, text="Detail")
        frmAction = LabelFrame(self.rootWidget, text="Actions")
        frmStatus = LabelFrame(self.rootWidget, text="Status")
        
        
        self.btnQuit = Button(self.rootWidget, text="Exit", command=self.rootWidget.destroy)
        
        # widgets on frmFilter       
        self.chkRelease = Checkbutton(self.frmFilter, text = "show releasable modules", variable = self.filterReleaseList, command=self.updateSlotListbox)
        self.chkDirLess = Checkbutton(self.frmFilter, text = "show modules without .dir file", variable = self.filterDirLess, command=self.updateSlotListbox)
        self.chkUnscanned = Checkbutton(self.frmFilter, text = "show modules with unscanned .dir files", variable = self.filterUnscanned, command=self.updateSlotListbox)
        self.cboModuleType = OptionMenu(self.frmFilter, self.filterModuleTypeVar, *self.moduleTypes, command=self.applyModuleFilter)
        
        # widgets on frmMain
	colList = []
	colList.append(ListboxColumn("slot",4, searchable=True))
	colList.append(ListboxColumn("vsn",6, searchable=True))
        colList.append(ListboxColumn("station",3, sortable=True))
        colList.append(ListboxColumn("experiments",30))
        colList.append(ListboxColumn("scans",3))
        colList.append(ListboxColumn("capacity",4) )
        colList.append(ListboxColumn("datarate",4))
        colList.append(ListboxColumn("received",15))
	columns = tuple(colList)
        self.grdSlot = MultiListbox(self.frmMain, 16, *columns)
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
        self.btnEditExp = Button (self.frmDetail, text="Change experiments", command=self.showEditExperimentsWindow,state=DISABLED)
        self.txtComment = Text(self.frmDetail, height=3, width=25)
        
        # widgets on frmAction
        self.btnDeleteModule = Button(frmAction, text="Check-out module", command=self.checkOutModule, state=DISABLED)
        self.btnEditModule = Button(frmAction, text="Update details", command=self.updateModule, state=DISABLED)
        self.btnPrintLibraryLabel = Button (frmAction, text="Print library label", command=self.printLibraryLabel,state=DISABLED)
        self.btnPrintVSNLabel = Button (frmAction, text="Print VSN label", command=self.printVSNLabel,state=DISABLED)
        self.btnRescan = Button (frmAction, text="Rescan directory", command=self.rescanModuleEvent,state=DISABLED)
        self.btnExpad = Button (frmAction, text="Show exp. details", command=self.showExpDetailEvent,state=DISABLED) 
	
        # arrange objects on grid     
        self.frmFilter.grid(row=3,column=0, sticky=E+W+N+S,padx=2,pady=2) 
        self.frmMain.grid(row=1,rowspan=2,column=0, sticky=E+W+N+S, padx=2,pady=2)   
        self.frmDetail.grid(row=1, column=3, sticky=E+W+N+S, padx=2,pady=2 )
        frmAction.grid(row=2,column=3,sticky=N+S+E+W, padx=2,pady=2)
        frmStatus.grid(row=3,column=3,sticky=N+S+E+W, padx=2,pady=2)
        self.btnQuit.grid(row=10,column=0, columnspan=4, sticky=E, padx=2,pady=2)
        
        # arrange objects on frmFilter
        Label(self.frmFilter, text = "state: ").grid(row=1, column=0, sticky=W)
        self.chkRelease.grid(row=1, column=1, sticky=W)
        self.chkDirLess.grid(row=2, column=1, sticky=W)
        self.chkUnscanned.grid(row=3, column=1, sticky=W)
        Label(self.frmFilter, text = "module type: ").grid(row=10, column=0, sticky=W)
        self.cboModuleType.grid(row=10,column=1, sticky=W)
        
        # arrange objects on frmMain
        self.grdSlot.grid(row=10, column=0, sticky=N+S+E+W)
        self.btnNewModule.grid(row=20, columnspan=2, sticky=E+W+S, pady=5, padx=5)        
        
        # arrage objects on frmStatus
        self.lblNumDirLess.grid(row=0, column=1, sticky=W)
        self.lblNumUnscanned.grid(row=1, column=1, sticky=W)
        self.btnModuleScan.grid(row=1,  column=2, sticky=W, padx=5)
        self.btnRefresh.grid(row=2,  column=0, columnspan=3, sticky=E+W, padx=5)
        
        #arrange objects on frmDetail
        self.txtLocationContent.grid(row=0, column=1,columnspan=2, sticky=E+W)
        self.btnChangeSlot.grid(row=0, column=3, sticky=E+W)
        self.lblVSNContent.grid(row=1, column=1, columnspan=3, sticky=E+W)
        self.lblStationContent.grid(row=2, column=1, columnspan=3, sticky=E+W)
        self.lblCapacityContent.grid(row=3, column=1, columnspan=3, sticky=E+W)
        self.lblDatarateContent.grid(row=4, column=1, columnspan=3, sticky=E+W)
        self.lblReceivedContent.grid(row=5, column=1, columnspan=3, sticky=E+W)
        self.cboExperiments.grid(row=6, column=1, sticky=E+W+N+S)
        scrollCboExperiments.grid(row=6,column=2, rowspan=2, sticky=W+N+S)
        self.btnEditExp.grid(row=6, column=3, sticky=E)
        self.txtComment.grid(row=8, column=1, columnspan=3, sticky=E+W)
        self.btnEditModule.grid(row=20, column=0, sticky=E+W)
        self.btnDeleteModule.grid(row=20, column=1, sticky=E+W)
        self.btnRescan.grid(row=20,column=2, sticky=E+W)
        self.btnPrintLibraryLabel.grid(row=21,column=0, sticky=E+W)
        self.btnPrintVSNLabel.grid(row=21,column=1, sticky=E+W)
        self.btnExpad.grid(row=21,column=2, sticky=E+W)
	
        # bind events to widgets
        self.txtLocationContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblVSNContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblStationContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblCapacityContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblDatarateContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.lblReceivedContent.bind("<KeyRelease>", self.editModuleDetailsEvent)
        self.txtComment.bind("<KeyRelease>", self.editModuleDetailsEvent)
    
    def printVSNLabel(self):
        
        if (self.selectedSlotIndex < 0):
            return
        
        session = dbConn.session()
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
        
        session.close()
    
    def printLibraryLabel(self, slotName=None):
        
        if (slotName == None):
            if (self.selectedSlotIndex == -1):
                return
            else:
                slotName = self.grdSlot.get(self.selectedSlotIndex)[0]
        
        session = dbConn.session()
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
        
        session.close()
            
    def updateModule(self):
        
        
        
        if (self.selectedSlotIndex == -1):
            return
        
        if (self.isConnected == False):
            return
        
        session = dbConn.session()
        
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
            
            session.commit()
            session.flush()
        
        session.close()
        
        self.moduleEdit = 0
        self._saveModuleDetails()
        self.editModuleDetailsEvent(None)
        self.updateSlotListbox()
  
    def callbackExpFilter(self, item):
        
        self.cboExpFilter.configure(text=item)
        self.filterExpVar.set(item)
      
        self.updateSlotListox()
        

    def applyModuleFilter(self, value):
        
        self.updateSlotListbox()
        
    def updateSlotListbox(self):
        
        session = dbConn.session()
    
        # deselect active slot
        self.selectedSlotIndex = -1
        
        if (self.isConnected == False):
            return
       
        slots = getOccupiedSlots(session)

        self.grdSlot.delete(0, END)
      
        releaseList = []
        
        self.grdSlot.clearData()
        
        # check setting of moduleType filter
        if (self.filterModuleTypeVar.get() == "SATA"):
            pattern = self.patternSataVSN
        elif (self.filterModuleTypeVar.get() == "PATA"):
            pattern = self.patternPataVSN
        elif (self.filterModuleTypeVar.get() == "Mark6"):
            pattern = self.patternMark6VSN
        else:
            pattern = None
        
        for slot in slots:
                             
            # check if module matches the filtered module type
            if (pattern != None):
                 
                if not pattern.match(slot.module.vsn):
                    continue
                    
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
            
            expList = []
            for exp in slot.module.experiments:
                    expList.append(exp.code)
            
         
            self.grdSlot.appendData((slot.location, slot.module.vsn, slot.module.stationCode, " ".join(expList), slot.module.numScans, slot.module.capacity, slot.module.datarate, slot.module.received))
            
       
        self.grdSlot.update()
        self.updateSlotDetails()
        
        session.close()
   
    
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
        self.btnExpad["state"] = DISABLED
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
	self.btnEditExp["state"] = DISABLED
        
        self.txtLocationContent.delete(0,END)
        self.lblVSNContent.delete(0,END)
        self.lblStationContent.delete(0,END)
        self.lblCapacityContent.delete(0,END)
        self.lblDatarateContent.delete(0,END)
        self.lblReceivedContent.delete(0,END)
        self.cboExperiments.delete(0, END)
        self.txtComment.delete(1.0, END)
        
        if self.selectedSlotIndex == -1:
            self.clearSlotSelection()
            return
        
        if (self.isConnected == False):
            return
        
        self.btnPrintVSNLabel["state"] = NORMAL
        self.btnRescan["state"] = NORMAL
        self.btnPrintLibraryLabel["state"] = NORMAL
        self.btnDeleteModule["state"] = NORMAL
	self.btnEditExp["state"] = NORMAL
        
        
        session = dbConn.session()
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
            self.txtComment.insert(1.0, unicode(none2String(slot.module.comment),errors='ignore'))
            
            # update experiment listbox
            for experiment in slot.module.experiments:
                assignedCodes.append(experiment.code)
                self.cboExperiments.insert(END, experiment.code)
                
	    if len(assignedCodes) > 0:
		self.btnExpad["state"] = NORMAL
                
            self._saveModuleDetails()
            
        self.txtLocationContent["state"] = DISABLED
        self.lblReceivedContent["state"] = DISABLED
        self.cboExperiments["state"] = NORMAL
        
        session.close()
     
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
        
        # save contents of the Detail form fields
        self._saveModuleDetails
        
    def checkOutModule(self):
            
        if (self.selectedSlotIndex == -1):
            return
        
        session = dbConn.session()
        
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
            self.checkoutDlg.show(module.id)
            #tkMessageBox.showerror("Error", "Module cannot be checked-out.\nIt  contains unreleased experiments\nor\nIt hasn't been scanned yet.")
            
            return
        elif (module.numScans == 0):
            if (tkMessageBox.askyesno("Empty module", "This module seems to be empty\nDo you really want to check-out this module") == False):
                return
            

        if (tkMessageBox.askokcancel("Confirm module check-out", "Do you really want to remove module " + slot.module.vsn + " from the library? ")):
	    session.close()
            self.doCheckout(module.id)
        
        
        session.close()
        return
    
    def doCheckout(self, moduleId):
        
        session = dbConn.session()
        
        module = getModuleById (session, moduleId)
        
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
            print "Warning: file %s does not exists" % dirFile
            
        session.close()
            
        
    def clearSearchEvent(self, Event):
        
        self.moduleFilter = ""
        self.slotFilter = ""
        
        self.updateSlotListbox()
        
    def showExpDetailEvent(self):

	if self.selectedSlotIndex == -1:
            return

	session = dbConn.session()
        slot = model.Slot()
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])

	if len(slot.module.experiments) == 0:
		return
	expadArgs = ["expad"]

	for exp in slot.module.experiments:
		expadArgs.append(exp.code)

	subprocess.call(expadArgs)

	session.close()
	
    def rescanModuleEvent(self):
        
        if self.selectedSlotIndex == -1:
            return
        
        session = dbConn.session()
        
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
        
        self.scanModulesDlg.scanModules(slot.module)
        
        session.close()
        
    def assignExperimentEvent(self):
        
        if self.selectedSlotIndex == -1:
            return
        
        session = dbConn.session()
        
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
        
        self.scanModulesDlg.scanModules(slot.module)
        
        session.close()

      
    def scanModuleEvent(self):
       
        self.scanModulesDlg.scanModules()
            
            
    def refreshStatusEvent(self):
        
        dirLessCount = 0
        unvalidatedCount = 0
        
        session = dbConn.session()
        
        slots = getOccupiedSlots(session)
             
        for slot in slots:
            
	    isDir = False
	    isScan = False
            # find modules without .dir file
            if (not hasDir(slot.module.vsn)):
                dirLessCount += 1
	    else:
		isDir = True
        
            # find unvalidated modules
            if (slot.module.numScans == None):
                unvalidatedCount += 1
	    else:
		isScan = True

	    # find modules that have been previously scanned but no .dir file
	    # exists anymore (e.g. when manually deleted from disk)
	    if isScan and not isDir:
		self.scanModulesDlg.scanModules(slot.module)
		print "rescan module: ", slot.module.vsn
		
        
        self.lblNumDirLess["text"] = dirLessCount
        self.lblNumUnscanned["text"] = unvalidatedCount - dirLessCount
        
        if (unvalidatedCount - dirLessCount > 0):
            self.btnModuleScan["state"] = NORMAL
        else:
            self.btnModuleScan["state"] = DISABLED
                
        session.close()
        
    def selectSlotEvent(self, Event):
    
        # check for unsaved module edits
        if (self.moduleEdit > 0):
            if (tkMessageBox.askyesno("Cancel unsaved changes", "There are unsaved changes in the module details\nAre you sure you want to abandon these?") == False):
                self.grdSlot.selection_clear(self.grdSlot.curselection())
                self.grdSlot.selection_set(self.selectedSlotIndex)
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
    
    def showEditExperimentsWindow(self):

        if (self.selectedSlotIndex < 0):
            return
        
        session = dbConn.session()
        slot = model.Slot()
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
                
        self.editExpDlg.selectedSlotId = slot.id
        self.editExpDlg.show()
        
        session.close()

    def showChangeSlotWindow(self):
        
        if (self.selectedSlotIndex < 0):
            return
        
        session = dbConn.session()
        slot = model.Slot()
        slot = getSlotByLocation(session, self.grdSlot.get(self.selectedSlotIndex)[0])
                
        self.changeSlotDlg.selectedSlotId = slot.id
        self.changeSlotDlg.show()
        
        session.close()
        
class CheckoutWindow(GenericWindow):
    
    def __init__(self, parent=None, rootWidget=None):
        
        # call super class constructor
        super( CheckoutWindow, self ).__init__(parent, rootWidget)
        
        
    def show(self, moduleId):
        
        self.moduleId = moduleId
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title("Check-out module")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.focus_set()
        self.dlg.grab_set()
        
        self._setupWidgets()
        
    def _setupWidgets(self):
        
        Label(self.dlg, text="Module cannot be checked out.\n", font="Helvetica 10 bold", fg="red").grid(row=0, sticky=W)    
        Label(self.dlg, text="It  contains unreleased experiments or hasn't been scanned yet.").grid(row=1, sticky=W)
        
        btnOK = Button(self.dlg, text="OK", command=self.dlg.destroy)
        btnForce = Button(self.dlg, text="Force check-out", command=self._onButtonForce)
        
        btnOK.grid(row=10, column=1, sticky=W,pady=7)
        btnForce.grid(row=10, column=3, sticky=E+W)
        
    def _onButtonForce(self):
        if  tkMessageBox.askyesno("Force module check out?", "Do you really want to check out this module?") == True:
            
            self.parent.doCheckout(self.moduleId)
            
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
        
        session = dbConn.session()
        self.lstExp.delete(0,END)
        
        # obtain listbox items from database
        experiments = getActiveExperimentCodes(session)
        for code in experiments:
            self.lstExp.insert(END, code)
            
        session.close()
        
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

        session = dbConn.session()
        # populate slot list
        ciSlotItems = getEmptySlots(session)
        for instance in ciSlotItems:
            self.lstSlot.insert(END, instance.location)
            
        session.close()

        btnOK = Button(self.dlg, text="OK", command=self._persistSlot)
        btnCancel = Button(self.dlg, text="Cancel", command=self.dlg.destroy)
        btnAddExp = Button(self.dlg, text="Add exp.", command=self._addExperiment)

        # arrange elements on grid
        self.txtVSN.grid(row=0, column=1)
        self.lstSlot.grid(row=1, column=1)
        self.lstExp.grid(row=3, column=1)
        chkPrintLibLabel.grid(row=4,column=1, sticky=W)

        btnOK.grid(row=10, column=1, sticky=W,pady=7)
        btnCancel.grid(row=10, column=3, sticky=E+W)
        btnAddExp.grid(row=3, column=3, sticky=E+W)
        yScroll.grid ( row=1, column=2, sticky=W+N+S )
        yScroll2.grid ( row=3, column=2, sticky=W+N+S )
        
        self.txtVSN.focus_set()
        
        
    def _splitVSNLabelScan(self):
        
        m = re.match('([a-zA-Z]+[%\+-]\d+)/(\d+)/(.+)', self.txtVSN.get().lstrip())
     
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
        
        session = dbConn.session()
        
        if (moduleExists(session, vsn)):
            tkMessageBox.showerror("Error","Module\n%s\nalready checked-in" % vsn)
            session.close()
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
         
        
        session.close()
        return
    
class ChangeSlotWindow(GenericWindow):
    
    def __init__(self, parent, rootWidget=None):
        
        # call super class constructor
        super( ChangeSlotWindow, self ).__init__(parent, rootWidget) 
        
        self.selectedSlotId = -1
        self.chkPrintLibLabelVar = IntVar()
        self.chkPrintLibLabelVar.set(1)
        
        self.selectedSlot = None
        
    def show(self):
        
        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title ("Change module slot")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()
        
        self._setupWidgets()
    
    
    def _setupWidgets(self):
        
        if self.selectedSlotId == -1:
            return
            
        session = dbConn.session()
        
        self.selectedSlot = getSlotById(session, self.selectedSlotId)
        
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
        
        session.close()
        
    def _persistSlot(self):
        
        if self.selectedSlotId == -1:
            return
        

        if self.lstSlot.get(self.lstSlot.curselection()[0]) == "":
            return
        
        session = dbConn.session()
        
        self.selectedSlot = getSlotById(session, self.selectedSlotId)
        
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
        
        session.close()
        
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
 
class EditExperimentsWindow(GenericWindow):

    def __init__(self, parent, rootWidget=None):

        # call super class constructor
        super( EditExperimentsWindow, self ).__init__(parent, rootWidget)
        
        self.selectedSlotId = -1
        self.selectedSlot = None
        self.initialCodes = []
        
    def show(self):

        # create modal dialog
        self.dlg = Toplevel(self.rootWidget, takefocus=True)
        self.dlg.title("Edit experiment list")
        self.dlg.transient(self.rootWidget)
        self.dlg.state("normal")
        self.dlg.grab_set()


        self._setupWidgets()

    def _setupWidgets(self):
        
        if self.selectedSlotId == -1:
            return
         
        session = dbConn.session()     
        self.selectedSlot = getSlotById(session, self.selectedSlotId)
        
        # frmModule
        frmModule = LabelFrame(self.dlg, text="Module")
        frmModule.grid(row=1, column=1, sticky=W)
        Label(frmModule, text=self.selectedSlot.module.vsn).grid(row=1, column=1, sticky=W)
        
        # frmAssigned
        frmAssigned = LabelFrame(self.dlg, text="Assigned")
        frmAssigned.grid(row=2, column=1,sticky=W)     
        scrollCboExperiments = Scrollbar(frmAssigned)
        self.cboExperiments =  Listbox(frmAssigned, height=15, yscrollcommand=scrollCboExperiments.set, selectmode=MULTIPLE)
	
        #frmButtons
        frmButtons = Frame(self.dlg)
        frmButtons.grid(row=2, column=2,sticky=W)   
        self.btnAddExperiments = Button(frmButtons, text="<<", command=self.addExperimentEvent)
        self.btnRemoveExperiments = Button(frmButtons, text=">>", command=self.removeExperimentEvent)
        
        # frmFree
        frmFree = LabelFrame(self.dlg, text="Available")
        frmFree.grid(row=2, column=3,sticky=W)     
        scrollCboFreeExperiments = Scrollbar(frmFree)       
        self.cboFreeExperiments = Listbox(frmFree, height=15, yscrollcommand=scrollCboFreeExperiments.set, selectmode=MULTIPLE)
        
        #frmBottom
        frmBottom = Frame(self.dlg)
        frmBottom.grid(row=3, column=1,columnspan=3, sticky=E)
        btnOK = Button(frmBottom, text="OK", command=self._persistExperiments)
        btnCancel = Button(frmBottom, text="Cancel", command=self.dlg.destroy)
        
        scrollCboExperiments.config(command=self.cboExperiments.yview)
        scrollCboFreeExperiments.config(command=self.cboFreeExperiments.yview)
        
	self.cboExperiments.grid(row=5, column=1, rowspan=2, sticky=E+W+N+S)
        self.cboFreeExperiments.grid(row=5, column=5, rowspan=2, sticky=W+N+S)
        scrollCboExperiments.grid(row=5,column=3, rowspan=2, sticky=W+N+S)
        scrollCboFreeExperiments.grid(row=5,column=7, rowspan=2, sticky=W+N+S)
        self.btnAddExperiments.grid(row=5, column=4, sticky=N)
        self.btnRemoveExperiments.grid(row=6, column=4, sticky=N)
        
        btnCancel.grid(row=1, column=1, sticky=W)
        btnOK.grid(row=1, column=2, sticky=E)
        
        
        # obtain assigned experiments from DB
        self.initialCodes = self.loadExperimentCodes()
        selectedCodes = self.initialCodes

        self.updateExperiments(selectedCodes)
        
        
        session.close()
        
    def _persistExperiments(self):
        """
        Stores the currently selected experiments for the module into the database
        """
        
        if self.selectedSlotId == -1:
            return
        
        # get all currently selected experiments 
        codes = sorted(self.cboExperiments.get(0, END))
        
        if codes != sorted(self.initialCodes):
            session = dbConn.session()
            
            slot = model.Slot()
            slot = getSlotById(session, self.selectedSlotId)
        
            if (slot > 0):
                # remove all experiment assigments of this module
                slot.module.experiments = []
                for expItem in codes:
                    exp = getExperimentByCode(session, expItem)

                    if (exp != None):
                        slot.module.experiments.append(exp)

                session.commit()
                session.flush()
            
            session.close()
            
        self.parent.updateSlotListbox()
        
        self.dlg.destroy()
         
       
    def loadExperimentCodes(self):
        """
        Loads the experiments assigned to the current module from database.
        Returns an array of strings containing the experiment codes
        """
        assignedCodes = []
        
        if self.selectedSlot == None:
            return assignedCodes
        
        for experiment in self.selectedSlot.module.experiments:
                assignedCodes.append(experiment.code)
        
        return assignedCodes

    def updateExperiments(self, selectedCodes):
        """
        Updates the contents of the listboxes holding the assigned and available
        experiment codes.
        
        selectedCodes: array of experiment codes to be filled in the listbox holding the assigned experiments
        """
        
        if self.selectedSlot == None:
            return
                 
        # update experiment listbox
        self.cboExperiments.delete(0,END)
        for experiment in selectedCodes:
            self.cboExperiments.insert(END, experiment)
                
        # update listbox containing unassigned experiments
        session = dbConn.session() 
        freeExps = getActiveExperimentCodes(session)
        self.cboFreeExperiments.delete(0,END)
        for code in freeExps:
            if code in selectedCodes:
                continue
            self.cboFreeExperiments.insert(END, code)
        
        session.close()
        
    def removeExperimentEvent(self):
        """
        Removes an experiment code from the listbox of the assigned epxeriments
        """
        
        if (len(self.cboExperiments.curselection()) == 0):
            return
        
        selection = list(self.cboExperiments.curselection())
        selection.reverse()
        
        for exp in selection:
            self.cboExperiments.delete(exp)
        
        # get all assigned experiments
        exps = self.cboExperiments.get(0, END)
        
        self.updateExperiments(exps)
        
        
    def addExperimentEvent(self):
        """
        Adds an experiment code to the listbox of the assigned epxeriments
        """
        
        if (len(self.cboFreeExperiments.curselection()) == 0):
            return
        
        selection = list(self.cboFreeExperiments.curselection())
        selection.reverse()
        
        
        for exp in selection:
            code = self.cboFreeExperiments.get(exp)
            self.cboExperiments.insert(END, code)   
            
        # get all assigned experiments
        exps = self.cboExperiments.get(0, END)
        
        self.updateExperiments(exps)
	

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
        
        session = dbConn.session()
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
                
            else:
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
            
        session.close()
            
    def updateModuleEvent(self):
        
        session = dbConn.session()
        
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
        
        session.close()
        
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
            session = dbConn.session()
            # add experiment with state "scheduled"
            addExperimentWithState(session, code, 10)
        except Exception as e:
            tkMessageBox.showerror("Error", e)
        finally:
            session.close()
        
     
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
          
if __name__ == "__main__":
    
    dbConn = None
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
        sess = dbConn.session()

        mainDlg.isConnected = True
        
        if not isSchemaVersion(sess, minSchemaMajor, minSchemaMinor):
            major, minor = getCurrentSchemaVersionNumber(sess)
            print "Current difxdb database schema is %s.%s but %s.%s is minimum requirement." % (major, minor, minSchemaMajor, minSchemaMinor)
            sys.exit(1)
        
    except Exception as e:
        print "Error: ",  e, "\nPlease check your database settings in %s " % settings["configFile"] 
        mainDlg.isConnected = False
        sys.exit()
    finally:
        sess.close()
        
        

    mainDlg.config = config
    
    mainDlg.show()
    
    root.mainloop()


    
    
    
  
