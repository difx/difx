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
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================
from Tkinter import *
import copy
from string import strip

class ListboxColumn(object):
    
    def __init__(self, header="", width=10, sortable=True, searchable=True, numeric=False):
        
        self.header = header
        self.width = width
        self.sortable = sortable
        self.searchable = searchable
        self.numeric = numeric
        
    
class MultiListbox(Frame):
  
   def __init__(self, master, height, *columns):
      
      Frame.__init__(self, master)
      
      self.backgroundColor = self["bg"]
      self.headerColor = self["bg"]
      
      self.data = []
      self.height = height
      self.columns = columns
      self.lists = []
      #self.columnFilters={}
      
      self.colmapping={}
      self.searchEntries={}
      self.origData = None
      enableSearch = False
  
      m = PanedWindow(self)
      m.config(handlesize=0, sashrelief=RAISED, sashwidth=2, borderwidth=5)
      m.grid(row=0, column=0, sticky=W+N+E+S)
     
      # check if any of the columns are searchable
      for column in self.columns:
          if column.searchable:
            enableSearch = True
            break

      colNum = 0
      
      for column in self.columns:
         
         frame = Frame(m, width=column.width)
         
         frame.config(padx=0, pady=0, relief=FLAT, bd=0)
         frame.grid(row=0,column=colNum, sticky=E+W)
         frame.columnconfigure(colNum, weight=2)
         
         if ( enableSearch ):
             if (column.searchable):
                 txtSearch = Entry ( frame, width=column.width)
                 txtSearch.grid(row=2, column=colNum, sticky=E+W)
                 txtSearch.bind("<KeyRelease>", self._searchColumnEvent)
                 self.colmapping[txtSearch]=(colNum, 1)
                 self.searchEntries[colNum] = txtSearch 
             else:
                 Entry (frame, text="", relief=FLAT, state=DISABLED, width=column.width).grid(row=2, column=colNum, sticky=E+W)
                 
         btnHeader = Button(frame, text=column.header, borderwidth=1, relief=RAISED, height=1, width=column.width, bg=self.headerColor, disabledforeground="black")
         btnHeader.grid(row=1, column=colNum, sticky=E+W)
         btnHeader.bind('<Button-1>', self._sort)
         btnHeader.grid_propagate(0)
         
         self.colmapping[btnHeader]=(len(self.lists),1)
         
         lb = Listbox(frame, width=column.width, height=self.height, borderwidth=1, selectborderwidth=0,
                        relief=FLAT, exportselection=FALSE)
         
         lb.grid(row=3, column=colNum, sticky=E+W)
         self.colmapping[lb]=(colNum, 1)
         self.lists.append(lb)

         m.add(frame)

         lb.bind('<B1-Motion>', lambda e, s=self: s._select(e.y))
         lb.bind('<Button-1>', lambda e, s=self: s._select(e.y))
         lb.bind('<Leave>', lambda e: 'break')
         lb.bind('<B2-Motion>', lambda e, s=self: s._b2motion(e.x, e.y))
         lb.bind('<Button-2>', lambda e, s=self: s._button2(e.x, e.y))
         lb.bind('<Button-4>', lambda e, s=self: s._scroll(SCROLL, -1, UNITS))
         lb.bind('<Button-5>', lambda e, s=self: s._scroll(SCROLL, 1, UNITS))
         
         if column.sortable == True:
             btnHeader["state"] = NORMAL
         else:
             btnHeader["state"] = DISABLED
             
         colNum += 1
     
      if (colNum > 0):
        btnClearSearch = Button (frame, bitmap="error")
        sb = Scrollbar(frame, orient=VERTICAL, command=self._scroll)
        sb.grid(row=3, column=colNum, sticky=W+N+S)
        self.lists[0]['yscrollcommand']=sb.set
	if enableSearch:
		btnClearSearch.grid(row=2, column=colNum, sticky=W+N+S)

        sb.bind('<Button-4>', lambda e, s=self: s._scroll(SCROLL, -1, UNITS))
        sb.bind('<Button-5>', lambda e, s=self: s._scroll(SCROLL, 1, UNITS))
        btnClearSearch.bind("<ButtonRelease-1>", self._clearSearchEvent)

       
   def update(self):
        
        #clear widget
        self.delete(0,END)
        
        # insert data into listboxes
        for dataRow in self.data:
            
            match = 0
            for col in range(0, len(dataRow)):  
                if dataRow[col] is None:
                    colValue = ""
                else:
                    colValue = str(dataRow[col])
                    
                # check column search filters
                if col in self.searchEntries:
                    filter = self.searchEntries[col].get()
                    if (filter in strip(colValue)):
                        match += 1
                
                    
            # if all column filters match, insert this line into the listbox         
            if (match == len(self.searchEntries)):
                for col in range(0, len(dataRow)):
                    if dataRow[col] is None:
                        colValue = ""
                    else:
                        colValue = str(dataRow[col])
                    self.lists[col].insert(END, colValue)
            
	self.origData = copy.deepcopy(self.get(0,END))
            
            
        
   def _clearSearchEvent(self, Event):
        
        for widget in self.colmapping.keys():
            if widget.__class__.__name__ == "Entry":        
                widget.delete(0,END)
                
        self.update()
        
   def _searchColumnEvent(self, Event):
        
        self.update()
                
        
   def _sort(self, e):
      
      # get the listbox to sort by (mapped by the header button)
      button = e.widget
      
      # don't sort if column has disabled sorting
      if (button["state"] == DISABLED):
          return
      
      col, direction = self.colmapping[button]


      if direction == 1:
	  reverse = False
      else: 
          reverse = True

      # get the entire table data into mem
      tableData = self.get(0,END)
      
      rowcount = len(tableData)

      #remove old sort indicators if it exists
      for widget in self.colmapping.keys():
          if widget.__class__.__name__ == "Button":
              
            lab = widget.cget('text')

            if lab[0]==u"\u2191": widget.config(text=lab[1:])
            if lab[0]==u"\u2193": widget.config(text=lab[1:])

      btnLabel = button.cget('text')
      
      #sort data based on direction
      if direction==0:
         tableData = self.origData
      else:
         
         if direction==1: button.config(text=u"\u2191" + btnLabel)
         else: button.config(text=u"\u2193" + btnLabel)

	 if self.columns[col].numeric:
		 tableData = sorted (tableData, reverse=reverse, key=lambda x: 0.0 if x[col]=="" else float(x[col]))
         else:
		tableData = sorted (tableData, reverse=reverse, key=lambda x:(x[col]))

      #clear widget
      self.delete(0,END)

      # refill widget
      for row in range(rowcount):
         self.insert(END, tableData[row])

      # toggle direction flag
      if(direction==1): direction=-1
      else: direction += 1
      self.colmapping[button] = (col, direction)


   def _select(self, y):
      row = self.lists[0].nearest(y)
      self.selection_clear(0, END)
      self.selection_set(row)
      return 'break'

   def _button2(self, x, y):
      for l in self.lists: l.scan_mark(x, y)
      return 'break'

   def _b2motion(self, x, y):
      for l in self.lists: l.scan_dragto(x, y)
      return 'break'

   def _scroll(self, *args):
      for l in self.lists:
         apply(l.yview, args)
      return 'break'

   def curselection(self):
      if (len(self.lists[0].curselection()) > 0):
          return self.lists[0].curselection()[0]
      else:
          return(None)

   def delete(self, first, last=None):
      for l in self.lists:
         l.delete(first, last)

   def get(self, first, last=None):
      result = []
      for l in self.lists:
          result.append(l.get(first,last))
      if last: return apply(map, [None] + result)
      return result

   def index(self, index):
      self.lists[0].index(index)


   def clearData(self):
       self.data = []
       
   def appendData(self, *elements):
       
       self.origData = None
       
       self.data.append(*elements)
       
   def insert(self, index, *elements):
      
      for e in elements:
         i = 0
         for l in self.lists:
           
            l.insert(index, e[i])
            i = i + 1
      
   def size(self):
      return self.lists[0].size()

   def see(self, index):
      for l in self.lists:
         l.see(index)

   def selection_anchor(self, index):
      for l in self.lists:
         l.selection_anchor(index)

   def selection_clear(self, first, last=None):
      for l in self.lists:
         l.selection_clear(first, last)

   def selection_includes(self, index):
      return self.lists[0].selection_includes(index)

   def selection_set(self, first, last=None):
      for l in self.lists:
         l.selection_set(first, last)
         
   def bindEvent(self, sequence, function):
       
       for widget in self.colmapping.keys():
          if widget.__class__.__name__ == "Listbox":
              widget.bind(sequence, function)
              
         
   def setHeaderColor (self, color):
       self.headerColor = color

if __name__ == '__main__':

   tk = Tk()
   
   col1 = ListboxColumn("column 1",5, searchable=False)
   col2 = ListboxColumn("column 2",10, sortable=False)
   col3 = ListboxColumn("column 3",20)
   
   Label(tk, text='MultiListbox').grid(row=0, column=0)
   mlb = MultiListbox(tk, col1, col2, col3)
   

   for zeile in xrange(100):
      mlb.append((zeile,  zeile,  'row %s' % zeile))
   
   mlb.update()
   mlb.grid(row=1)
   tk.mainloop()
