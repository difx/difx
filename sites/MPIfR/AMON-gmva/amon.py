#!usr/bin/python
# Copyright (c) Yurii Pidopryhora & Max Planck Institute for Radio Astronomy (2018). 
#
# Please address all inquiries to Yurii Pidopryhora at yurii@mpifr-bonn.mpg.de or yuretzius@gmail.com
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>,
# or write to the Free Software Foundation, Inc., 
# 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# a. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# b. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
# c. Neither the name of the author nor the names of contributors may 
#    be used to endorse or promote products derived from this software 
#    without specific prior written permission.
#
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# standard Python libs
from __future__ import division # division behaves like in Python 3
import sys
import numpy as np
import subprocess
import os
from shutil import copyfile
import Tkinter as tk
#from PyPDF2 import PdfFileMerger
# local libs
import amon_lib.amon_lib as aml
import amon_lib.vex_parser_lib as vex
import amon_lib.plt_lbls_lib as lbl

AMON_VERSION = '1.41b'

help_info = """
####################################################################################################
                                      RUNNING THE SCRIPT:

python amon.py <.alist file name>  <.vex file name>

python amon.py <.vex file name> (will display only inactive elements based on .vex)  

python amon.py (displays only this part of the help)   

Python 2.7 or later is assumed, this version WON'T run with Python 3. 
Standard Python libraries required for this script are: sys, os, shutil, subprocess, 
                                                        numpy, matplotlib, Tkinter.
They are typically included in most Python distributions.  

Many important features of this program will work only if hops fplot and gs are available.

To run, this version of AMON must include the following files (all files except amon.py
are placed in two subdirectories, amon_lib and amon_config):

amon.py                                                                  main script
amon_lib: amon_lib.py, vex_parser_lib.py, plt_lbls_lib.py, __init__.py   libraries
amon_config/amon_config.txt                                              the configuration file
amon_config/amon_help.txt (this name can be changed in the config file)  this help file
amon_config/gmva_codes (this name can be changed in the config file)     one letter - two letter
                                                                         antenna code
                                                                         correspondence file        
      
####################################################################################################

"""

global mode_set, ref_ant, start_row, pol_choice, source_choice, print_size_choice, do_PRF, do_FR # variables, set by GUI tools
global frame_buttons, Can1 # redrawable canvas and frame in it
global labels, baselines, fringes, errors # displayed main data
global labels_main, baselines_main, fringes_main, errors_main # immutable main data
global no_rows, no_columns # changing size of the displayed main data
global status_label1, status_var1, status_label2, status_var2  # status labels and strings at the bottom
global need_entry, max_disp_rows, max_no_rows # if we need to display only partical filed and if yes, how large
global entry_tkvar, check_tkvar # PDF and FR check variables
global fplots_viewed # numbber of fplots viewed
global tempfilenamelist # all temp files and all images opened
global help_win #Toplevel window for the separate window of help viewing
global gsprocesses # all gs processes started

#####################################################################################################
################################## FUNCTION DEFINITIONS #############################################
# clear the bottom status strings
def clear_status_lines():
  global status_label1, status_var1, status_label2, status_var2 
  status_var1 = ''
  status_var2 = ''
  status_label1.configure(text=status_var1)
  status_label2.configure(text=status_var2)
# returns the list of keys for the given value in the given dict
def keys_by_value(dict, value):
  return [k for k, v in dict.items() if v == value]
# carefully removing a file (can be a relative path too, e. g. foo/blah.txt)
def rm_list_files_in_cur_dir(filelist):
  for filename in filelist:
    rm_file_in_cur_dir(filename)
def rm_file_in_cur_dir(filename):
  #full_path = os.getcwd()+"/"+filename
  if os.path.isfile(filename):
    os.remove(filename)
    return True
  else:
    return False
# checking if file already exists
def file_in_cur_dir(filename):
  #full_path = os.getcwd()+"/"+filename
  if os.path.isfile(filename):
    return True
  else:
    return False
# determining button color based on value changing from 1 to 9
def button_color(val):
  if np.isnan(val):
    return 'magenta', 'black'
  if val == 0:
    bg = 'red'
  else:
    bg = "#%02x%02x%02x" % (int((1.-val/9.)*200), int((val/9.)*255), 0)
  if val in (7,8,9):
    fg = 'black'
  else:
    fg = 'white' # better looks for brighter colors
  return bg, fg
# Create the main canvas with scrollbars
def main_canvas(frame, r, c):
  canvas = tk.Canvas(frame)
  canvas.grid(row=r, column=c)
  # Link a vertical scrollbar to the canvas in row 0, column 1 of frame_canvas
  vsbar = tk.Scrollbar(frame, orient="vertical", command=canvas.yview)
  vsbar.grid(row=r, column=c+1, sticky='news')
  canvas.configure(yscrollcommand=vsbar.set)
  # Link a horizontal scrollbar to the canvas in row 1, column 0 of frame_canvas
  hsbar = tk.Scrollbar(frame, orient="horizontal", command=canvas.xview)
  hsbar.grid(row=r+1, column=c, sticky='news')
  canvas.configure(xscrollcommand=hsbar.set)
  return canvas
# Create the frame to contain the buttons
def frame_for_buttons(canvas, size):
  frame = tk.Frame(canvas,
                  #bg="Blue",
                   bd=2,
                   relief=tk.GROOVE # making a nice border
                   )
  canvas.create_window((0,0), window=frame,anchor='nw')
  # Bind the buttons frame to a function that fixes the Canvas size
  def resize(event):
    canvas.configure(scrollregion=Can1.bbox("all"), width=size[0], height=size[1])
  frame.bind("<Configure>", resize) # catching attempt to change the frame size
  return frame
########################################################################################
############### MAIN ELEMENT POPULATION FOR DISPLAY ####################################
# fill frame with buttons
def fill_w_buttons(frame, labels, baselines, fringes, errors, pol_choice, codesdict):
  global status_label1, status_var1, status_label2, status_var2
  global pids_to_kill, procs_to_kill
  
  element_counter = 0
  
  ####### populating field borders
  # horizontal labels
  # column 1 to no_columns+1, rows 0 and no_rows+2 of frame_buttons
  
  # init status line
  clear_status_lines()
  
  #top and bottom row
  for col in range(no_columns//2):
    label = tk.Label(frame, text=baselines[col], bg = 'white', relief=tk.GROOVE, padx = 0, pady = 0,  font=("Courier", 8))
    label.grid(row=0, column=col*2+1, columnspan=2, sticky="nsew")
    label = tk.Label(frame, text=baselines[col], bg = 'white', relief=tk.GROOVE, padx = 0, pady = 0,  font=("Courier", 8))
    label.grid(row=no_rows+3, column=col*2+1, columnspan=2, sticky="nsew")
    element_counter+=2
  
  #empty horizontal rows with wide text to keep uniform width
  for col in range(1,no_columns+1):
    label = tk.Label(frame, text='HHH', font=("Helvetica", 4), bg = 'white', fg = 'white', relief=tk.GROOVE)
    label.grid(row=1, column=col, columnspan=1, sticky="nsew")
    label = tk.Label(frame, text='HHH', font=("Helvetica", 4), bg = 'white', fg = 'white', relief=tk.GROOVE)
    label.grid(row=no_rows+2, column=col, columnspan=1, sticky="nsew")
    element_counter+=2
  
  # vertical labels
  # row 1 to no_rows, columns 0 and no_columns+2 of frame_buttons

  # leftmost and rightmost labels
  for row in range(2,no_rows+2):
    # anchor in this case is the equivalent of justify, which for labels works only for multiple lines of text
    label = tk.Label(frame, text=labels[row-2], bg = 'white', relief=tk.GROOVE, padx = 2, pady = 0, font=("Helvetica", 8), anchor="w")
    label.grid(row=row, column=0, sticky="news")
    label = tk.Label(frame, text=labels[row-2], bg = 'white', relief=tk.GROOVE, padx = 2, pady = 0, font=("Helvetica", 8), anchor="w")
    label.grid(row=row, column=no_columns+2, sticky="news")
    element_counter+=2
  ##### end of populating the borders
    
  ### element-related functions
  ###### clicking an element: running fplot and gs
  def click(row, col, pol):
    global fplots_viewed, tempfilenamelist, gsprocesses
    file = errors[row, col, pol].split()[0][1:]
    print "viewing:", pathadd+file
    fplots_viewed += 1
    tempfilenamePS = 'amon_click_temp'+str(fplots_viewed)+'.ps'
    tempfilenamelist.append(tempfilenamePS)
    process = subprocess.Popen([hopspath+"fplot", "-d", tempfilenamePS, pathadd+file, "&"], close_fds=True) #close_fds = True ensures that this process is detached from the current script
    while process.poll() == None: # it is None while the process is still active
      pass # waiting for fplot to finish
    process = subprocess.Popen(["gs", "-dBATCH", "-dNOPROMPT", \
                                "-q", "-sDEVICE=x11alpha", tempfilenamePS], \
                                 stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE) # PIPE allows to communicate with the process
                                                                                                        # x11alpha is better than the default x11, because it adds antialiasing
    gsprocesses.append(process)
    ### attempt to use tkinter, failed -- too slow, to bad display
    #process = subprocess.Popen(["gs", "-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=ppm",\
    #                              "-r150", "-sOutputFile="+tempfilenamePPM, tempfilenamePS]) #tkinter does not understand this ppm
    #process = subprocess.Popen(["convert", "-density", "200", tempfilenamePS, "-scale", "50%", tempfilenameGIF]) # density sets dpi of the original file
    #click_win = tk.Toplevel(root)
    #click_win.title(file)
    #click_win.geometry("800x1000")
    #click_win.update()
    #click_canvas = tk.Canvas(click_win, bg = "white")
    #click_photo = tk.PhotoImage(file = tempfilenameGIF)
    #click_canvas.photo = click_photo # keeping a reference to PhotoImage, otherwise python clears it!
    #click_img = click_canvas.create_image(0, 0, anchor = tk.NW, image = click_photo)
    #click_canvas.pack(fill=tk.BOTH, expand = 1)# tk.BOTH means expand in both x and y
    # the following opens fplot in gs viewer, but this has issues with closing 
    #process = subprocess.Popen([hopspath+"fplot", pathadd+file, "&"], close_fds=True)
    if do_PDF:
      source = labels[row][5:]
      if col%2 == 0:
        if not pol:
          pl = 'LL'
        else:
          pl = 'LR'
        bas = baselines[col//2]
      else:
        if not pol:
          pl = 'RR'
        else:
          pl = 'RL'
        bas = baselines[(col-1)//2]
      pdffile = vex_exp_name+'_'+file[:6]+'_'+source+'_'+bas+'_'+pl+'.pdf'
      if not file_in_cur_dir(pdffile): #if this particular plot was not created yet
        print "Saving a pdf copy of this fplot view in", pdffile
        process = subprocess.Popen(["ps2pdf", tempfilenamePS, pdffile])
        while process.poll() == None: # it is None while the process is still active
          pass # waiting for ps2pdf to finish
  #### end of click function
  
  #### cosmetic display functions
  def flt_str_truncate(str_to_tr):
    return '{:1.1f}'.format(float(str_to_tr))
  def flt_str_truncateINT(str_to_tr):
    val = np.around(float(str_to_tr))
    if np.isfinite(val): return '{}'.format(int(val))
    else: return 'NaN'
  def flt_str_truncateS(str_to_tr):
    return '{:+1.1f}'.format(1000.*float(str_to_tr))
  def flt_str_truncateINTS(str_to_tr):
    val = np.around(1000.*float(str_to_tr))
    if np.isfinite(val): return '{}'.format(int(val))
    else: return 'NaN'

  #### hovering over an element, displaying its info
  def status_change(row, col, pol_choice, codesdict):
    global status_label1, status_var1, status_label2, status_var2
    if col%2 == 0:
      if not pol_choice:
        pol = 'LL'
      else:
        pol = 'LR'
      bas = baselines[col//2]
    else:
      if not pol_choice:
        pol = 'RR'
      else:
        pol = 'RL'
      bas = baselines[(col-1)//2]
    scan_no = labels[row][:4]
    source = labels[row][5:]
    bas = codesdict[bas[0]]+codesdict[bas[1]] # translate into two-letter codes
    fri = int(fringes[row, col, pol_choice, 0])
    if fri < 0 : # not observed or not correlated
      timestamp = errors[row, col, pol_choice]
      #status_var1 = "{} scn: {}, src: {}, {}:{}, time: {}.".format(vex_exp_name, scan_no, source, bas, pol, timestamp)
      status_var1 = "scn: {}, src: {}, {}:{}, t: {}.".format(scan_no, source, bas, pol, timestamp)
      status_var2 = ""
    else:  # observed and correlated
      err = errors[row, col, pol_choice][0]
      if err == '0': err = 'none'
      secr = errors[row, col, pol_choice].split()[0][1:]
      SNR = errors[row, col, pol_choice].split()[1] # not displaying, but keep here for reference
      SBD = flt_str_truncateINTS(errors[row, col, pol_choice].split()[2])
      DRATE = errors[row, col, pol_choice].split()[3] # need for FRATE
      RefFreq = errors[row, col, pol_choice].split()[4]
      FRATE = '{:1.1f}'.format(float(DRATE)*float(RefFreq)*1e-3)
      DRATE = flt_str_truncateINTS(errors[row, col, pol_choice].split()[3]) # no longer need it exact
      timestamp = errors[row, col, pol_choice].split()[5]
      if timestamp[0] != '2': print errors[row, col, pol_choice] # diagnostic feature in case of incorrect parsing, just leave it
      ALLSNR1 = flt_str_truncateINT(errors[row, col, pol_choice].split()[6])
      ALLSNR2 = flt_str_truncateINT(errors[row, col, pol_choice].split()[7])
      ALLSNR3 = flt_str_truncateINT(errors[row, col, pol_choice].split()[8])
      ALLSNR4 = flt_str_truncateINT(errors[row, col, pol_choice].split()[9])
      #status_var1 = "{} scn: {}, src: {}, {}:{}, time: {}, fringe: {}, err. code: {}, fplot file: {}".format(vex_exp_name, scan_no, source, bas, pol, timestamp, fri, err, secr)
      status_var1 = "scn: {}, src: {}, {}:{}, t: {}, fr: {} err: {}".format(scan_no, source, bas, pol, timestamp, fri, err)
      status_var2 = "SNR (LL|RR LR|RL): {} | {}  {} | {}, SBD = {} ns, DRATE = {} fs/s, FRATE = {} mHz.".format(ALLSNR1, ALLSNR2, ALLSNR3, ALLSNR4, SBD, DRATE, FRATE)    
    status_label1.configure(text=status_var1)
    status_label2.configure(text=status_var2)
  #### end of status_change function
  
  ###### THE MAIN BODY OF BUTTONS #################
  ## main body of buttons, rows/column 1 through no_rows, no_columns of frame_buttons

  for row in range(2,no_rows+2):
    for col in range(1,no_columns+1):
      if fringes[row-2, col-1,pol_choice,0] == -10: # not scheduled
        pass
      elif fringes[row-2, col-1,pol_choice,0] == -5: # scheduled, but not correlated
        bg_color = 'white'
        fg_color = 'white'
        abg_color = 'yellow'
        afg_color = 'yellow'
        button = tk.Button( frame, 
                            text="N", 
                            bg = bg_color,
                            fg = fg_color,
                            activebackground = abg_color,
                            activeforeground = afg_color,
                            borderwidth=1,
                            relief=tk.SUNKEN,
                            justify = tk.CENTER,
                            padx = 1,
                            pady = 1
                            )
        button.bind("<Enter>", lambda event, row=row, col=col: status_change(row-2, col-1, pol_choice, codesdict))
        #label.bind("<Leave>", something)
        button.grid(row=row, column=col, sticky="nsew")
        element_counter+=1
      elif fringes[row-2, col-1,pol_choice,0] == 0: # no fringe 
        if not do_FR: # require fringe turned off, display all elements
          if mode_set: # any mode but FRINGE
            colors = button_color(fringes[row-2, col-1, pol_choice, mode_set])
            bg_color = colors[0]
            fg_color = colors[1]
            abg_color = 'yellow'
            afg_color = 'black'
            fill = 'N'
            if errors[row-2,col-1,pol_choice][0] != '0':
              fill = errors[row-2,col-1,pol_choice][0]
            else:
              fg_color = bg_color
              afg_color = abg_color
            button = tk.Button( frame,
                                text=fill,
                                bg=bg_color,
                                fg=fg_color,
                                activebackground = abg_color,
                                activeforeground = afg_color,
                                command=lambda row=row, col=col: click(row-2, col-1, pol_choice),
                                relief=tk.GROOVE,
                                padx = 1,
                                pady = 1,
                                justify = tk.CENTER,
                                font=("Helvetica", 8)
                                )
            button.bind("<Enter>", lambda event, row=row, col=col: status_change(row-2, col-1, pol_choice, codesdict))
            #button.bind("<Leave>", something)
            button.grid(row=row, column=col, sticky="nsew")
            element_counter+=1
          else: # FRINGE mode
            bg_color = 'red'
            fg_color = 'red'
            abg_color = 'yellow'
            afg_color = 'yellow'
            fill = 'N'
            if errors[row-2,col-1,pol_choice][0] != '0':
              fg_color = 'black'
              afg_color = 'black'
              fill = errors[row-2,col-1,pol_choice][0]
            button = tk.Button( frame,
                                text=fill,
                                bg = bg_color,
                                fg = fg_color,
                                activebackground = abg_color,
                                activeforeground = afg_color,
                                command=lambda row=row, col=col: click(row-2, col-1, pol_choice),
                                relief=tk.GROOVE,
                                padx = 1,
                                pady = 1,
                                justify = tk.CENTER,
                                font=("Helvetica", 8)
                                )
            button.bind("<Enter>", lambda event, row=row, col=col: status_change(row-2, col-1, pol_choice, codesdict))
            #button.bind("<Leave>", something)
            button.grid(row=row, column=col, sticky="nsew")
            element_counter+=1
        else: # require fringe turned on, fringe == 0 elements marked cyan and deactivated
          bg_color = 'cyan'
          fg_color = 'cyan'
          abg_color = 'yellow'
          afg_color = 'yellow'
          fill = 'N'
          if errors[row-2,col-1,pol_choice][0] != '0':
            fg_color = 'black'
            afg_color = 'black'
            fill = errors[row-2,col-1,pol_choice][0]
          button = tk.Button( frame, 
                              text=fill, 
                              bg = bg_color,
                              fg = fg_color,
                              activebackground = abg_color,
                              activeforeground = afg_color,
                              borderwidth=1,
                              relief=tk.SUNKEN,
                              justify = tk.CENTER,
                              padx = 1,
                              pady = 1,
                              font=("Helvetica", 8)
                            )
          button.bind("<Enter>", lambda event, row=row, col=col: status_change(row-2, col-1, pol_choice, codesdict))
          #label.bind("<Leave>", something)
          button.grid(row=row, column=col, sticky="nsew")
          element_counter+=1
      else: # value 1-9
        colors = button_color(fringes[row-2, col-1, pol_choice, mode_set])
        bg_color = colors[0]
        fg_color = colors[1]
        abg_color = 'yellow'
        afg_color = 'black'
        fill = 'N'
        if errors[row-2,col-1,pol_choice][0] != '0':
          fill = errors[row-2,col-1,pol_choice][0]
        else:
          fg_color = bg_color
          afg_color = abg_color
        button = tk.Button( frame,
                            text=fill,
                            bg=bg_color,
                            fg=fg_color,
                            activebackground = abg_color,
                            activeforeground = afg_color,
                            command=lambda row=row, col=col: click(row-2, col-1, pol_choice),
                            relief=tk.GROOVE,
                            padx = 1,
                            pady = 1,
                            justify = tk.CENTER,
                            font=("Helvetica", 8)
                            )
        button.bind("<Enter>", lambda event, row=row, col=col: status_change(row-2, col-1, pol_choice, codesdict))
        #button.bind("<Leave>", something)
        button.grid(row=row, column=col, sticky="nsew")
        element_counter+=1
  return element_counter # total number of elements created in this run
########### END OF fill_w_buttons: MAIN ELEMENT POPULATION FOR DISPLAY ###############
######################################################################################
     
# general replotting of the whole canvas, initiated by pressing the replot button
def redraw(frame, size):
  global frame_buttons, Can1
  global need_entry, start_row, max_disp_rows
  global labels, baselines, fringes, errors
  global no_rows, no_columns
  # new display cut
  frame_buttons.destroy()
  Can1.destroy()
  Can1 = main_canvas(frame, 0, 0)
  frame_buttons = frame_for_buttons(Can1, size)
  need_entry, start_row, max_disp_rows, labels, baselines, fringes, errors = aml.keep_only_some_elements(ref_ant, source_choice, start_row, labels_main, baselines_main, fringes_main, errors_main)
  no_rows = len(labels)
  no_columns = len(baselines)*2
  config_entry()
  no_elem = fill_w_buttons(frame_buttons, labels, baselines, fringes, errors, pol_choice, codesdict)
  #print('\nNumber of elements created in this display:', no_elem,'\n')

###################### PDF PRINT PART ####################################################
##########################################################################################
############### MAIN ELEMENT POPULATION FOR PDF PRINT ####################################
#### main "button"-printing function
def print_buttons(cnv, fr, err):
  for rw in range(fr.shape[0]):
    for cl in range(fr.shape[1]):
      if fr[rw, cl, pol_choice, 0] == -10: # not scheduled
        pass
      elif fr[rw, cl, pol_choice, 0] == -5: # scheduled, but not correlated
        bg_color = 'white'
        fg_color = 'white'
        elem = lbl.GrLabel(fg_color, bg_color, '', cnv.fontsize)
        lbl.AddCellToCanvas(cnv, elem, rw, cl)
      elif fr[rw, cl, pol_choice, 0] == 0: # no fringe 
        if not do_FR:
          if mode_set:
            colors = button_color(fr[rw, cl, pol_choice, mode_set])
            bg_color = colors[0]
            fg_color = colors[1]
            if err[rw, cl, pol_choice][0] != '0':
              txt = err[rw, cl, pol_choice][0]
            else:
              txt = ''
            elem = lbl.GrLabel(fg_color, bg_color, txt, cnv.fontsize)
            lbl.AddCellToCanvas(cnv, elem, rw, cl)
          else:
            bg_color = 'red'
            fg_color = 'red'
            if err[rw, cl, pol_choice][0] != '0':
              fg_color = 'black'
              txt = err[rw, cl, pol_choice][0]
            else:
              txt = ''
            elem = lbl.GrLabel(fg_color, bg_color, txt, cnv.fontsize)
            lbl.AddCellToCanvas(cnv, elem, rw, cl)
        else: # display no fringe cells turned off
          bg_color = 'cyan'
          fg_color = 'cyan'
          if err[rw, cl, pol_choice][0] != '0':
            fg_color = 'black'
            txt = err[rw, cl, pol_choice][0]
          else:
            txt = ''
          elem = lbl.GrLabel(fg_color, bg_color, txt, cnv.fontsize)
          lbl.AddCellToCanvas(cnv, elem, rw, cl)
      else: # fringe 1-9
        colors = button_color(fr[rw, cl, pol_choice, mode_set])
        bg_color = colors[0]
        fg_color = colors[1]
        if err[rw, cl, pol_choice][0] != '0':
            txt = err[rw, cl, pol_choice][0]
        else:
          txt = ''
        elem = lbl.GrLabel(fg_color, bg_color, txt, cnv.fontsize)
        lbl.AddCellToCanvas(cnv, elem, rw, cl)
#### end of the "button"-printing function 
###################### END OF ELEMENT POPULATION FOR PRINT
 
###### HANDLING THE PDF PRINT: file name, title, breaking into pages etc.
# gs version of merge pdfs
def merge_pdfs(pdffilelist, mergedfilename):
  global status_label1, status_var1, status_label2, status_var2
  #gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -sOutputFile=merged.pdf 1.pdf 2.pdf
  if len(pdffilelist) <= 1:
    raise Exception('pdf file list should include at least two existing files')
  else:
    if not file_in_cur_dir(pdffilelist[0]):
      raise Exception('Unable to open', pdffilelist[0])
    elif not file_in_cur_dir(pdffilelist[1]):
      raise Exception('Unable to open', pdffilelist[1])
    else:
      print "...merging "+pdffilelist[0]+"\nwith "+pdffilelist[1]+"..."
      process = subprocess.Popen(["gs", "-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=pdfwrite", \
                                  "-dPDFSETTINGS=/prepress", "-sOutputFile="+mergedfilename, \
                                  pdffilelist[0], pdffilelist[1]])
      while process.poll() == None: # it is None while the process is still active
        pass # waiting for process to finish
    if len(pdffilelist) > 2:
      copyfile(mergedfilename, 'amon_merge_temp.pdf')
      for ii in range(2,len(pdffilelist)):
        if not file_in_cur_dir(pdffilelist[ii]):
          raise Exception('Unable to open', pdffilelist[ii])
        else:
          print "...adding " + pdffilelist[ii] + "..."
          process = subprocess.Popen(["gs", "-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=pdfwrite", \
                                      "-dPDFSETTINGS=/prepress", "-sOutputFile="+mergedfilename, \
                                      "amon_merge_temp.pdf", pdffilelist[ii]])
          while process.poll() == None: # it is None while the process is still active
            pass # waiting for process to finish
          copyfile(mergedfilename, 'amon_merge_temp.pdf')
      rm_file_in_cur_dir('amon_merge_temp.pdf')
      print "Finished merging!"
#### maybe later include both and run PyPDF2 version if PyPDF2 is available
#############################################
# PyPDF2 version
#def merge_pdfs(pdffilelist, mergedfilename):
#  merger = PdfFileMerger()
#  openedfiles = []
#  for pdf in pdffilelist:
#    fin = open(pdf, 'rb')
#    merger.append(fin)
#    openedfiles.append(fin)
#  with open(mergedfilename, 'wb') as fout:
#    merger.write(fout) 
#  for fin in openedfiles:
#    fin.close()
#  del merger, openedfiles  
############################################# 
### creating a pdf copy
def print_canvas():
  #### print file name construction
  prt_file_name = vex_exp_name+'_'
  prt_file_name += keys_by_value(menuMODE_translate, mode_set)[0]+'_'
  if not do_FR:
    prt_file_name += 'NF_'
  if not ref_ant:
    prt_file_name += 'AllAnt_'
  else:
    prt_file_name += 'RfAnt_' + codesdict[ref_ant] +'_'
  if not pol_choice:
    prt_file_name += 'LLRR_'
  else:
    prt_file_name += 'LRRL_'
  if not source_choice:
    prt_file_name += 'AllSrc'
  else:
    prt_file_name += source_choice
  #### end of print file name construction
  #### constructing the plot title
  title_main = 'AMON v.' + AMON_VERSION+': '
  title_main+='[' + keys_by_value(menuMODE_translate, mode_set)[0]
  if not do_FR: title_main+=' NF'
  title_main+='] '
  title_main+=vex_exp_name
  if ref_ant:
    title_main+=' Rf ant: ' + codesdict[ref_ant]
  else:
    title_main+=' ALL ant'
  if source_choice:
    title_main+=', src: ' + source_choice
  else:
    title_main+=', ALL src'
  if pol_choice:
    title_main+=', XX. '
  else:
    title_main+=', ||. '
  ##### end of the plot title contruction
  ##### erray preparation for print
  labels_pr, baselines_pr, fringes_pr, errors_pr = \
    aml.keep_only_some_elements_PRINT(ref_ant, source_choice, labels_main, baselines_main, fringes_main, errors_main)
  ### how many pages needed?
  lblcanv = lbl.LabelCanvas(print_size_choice, 'dummy')
  numb_vert_pages = int(np.ceil(len(labels_pr)/lblcanv.size[0]))
  numb_horiz_pages = int(np.ceil(len(baselines_pr)/lblcanv.size[1]))
  del lblcanv
  #### one-page case
  if numb_vert_pages == 1 and numb_horiz_pages == 1:
    flnm = prt_file_name+'.pdf'
    title = title_main
    print '\nLuckily the whole view fits into just one page!'
    print 'Creating '+flnm+'...'
    lblcanv = lbl.LabelCanvas(print_size_choice, title)
    lblcanv.AddAxes(labels_pr, baselines_pr)
    lblcanv.StartPlot()
    lbl.PopulateAxesInLabelCanvas(lblcanv)
    print_buttons(lblcanv, fringes_pr, errors_pr)
    lblcanv.Print(flnm)
    print 'Done!\n'
    del lblcanv
  #### multi-page case
  else:
    allpdfs = []
    pagecounter = 1
    print '\nHave to split the result into two or more pages...'
    for page_v in range(numb_vert_pages):
      for page_h in range(numb_horiz_pages):
        flnm = prt_file_name+'_'+str(page_v+1)+'_'+str(page_h+1)+'.pdf'
        title = title_main + 'Pg#'+ str(pagecounter) + ': rw='+str(page_v+1)+', cl='+str(page_h+1)+'.' 
        allpdfs.append(flnm)
        print '...creating '+flnm+'...'
        lblcanv = lbl.LabelCanvas(print_size_choice, title)
        lblcanv.AddAxes(labels_pr[lblcanv.size[0]*page_v:lblcanv.size[0]*(page_v+1)],\
                        baselines_pr[lblcanv.size[1]*page_h:lblcanv.size[1]*(page_h+1)])
        lblcanv.StartPlot()
        lbl.PopulateAxesInLabelCanvas(lblcanv)
        print_buttons(lblcanv,\
                      fringes_pr[lblcanv.size[0]*page_v:lblcanv.size[0]*(page_v+1), lblcanv.size[1]*page_h*2:lblcanv.size[1]*(page_h+1)*2,:,:],\
                      errors_pr[lblcanv.size[0]*page_v:lblcanv.size[0]*(page_v+1), lblcanv.size[1]*page_h*2:lblcanv.size[1]*(page_h+1)*2,:])
        lblcanv.Print(flnm)
        del lblcanv
        pagecounter+=1
    print 'Created ' + str(numb_vert_pages * numb_horiz_pages) + ' pages!'
    print 'Merging all pages into a single multipage file: ' + prt_file_name +'.pdf'
    merge_pdfs(allpdfs, prt_file_name+'.pdf')
    print 'Deleting the one-page pdf files...'
    rm_list_files_in_cur_dir(allpdfs)
    print 'Done!\n'
    del allpdfs, labels_pr, baselines_pr, fringes_pr, errors_pr
############ end pdf print part

######################## END FUNCTION DEFS ##############################################

#####################################################################################  
##################### MAIN START HERE ###############################################
########### start of main code, have to keep it linear because of the GUI  

try:
   _ = sys.argv[1]
except IndexError: # script is launched without any arguments
  print(help_info)
  sys.exit()
  
alistfilename, vexfilename = aml.get_filenames()  

#### alistfilename may be '' if running in vex-only mode 
    

CURRENT_PATH = os.getcwd() # path to the launch dir

#### getting the data from .alist

( vex_exp_name, labels_main, sources, baselines_main, antennas,
  fringes_main, errors_main, root_size, canvas_size, help_size,
  configfilename, codesfilename, codesdict, helpfilename,
  pathadd, hopspath, fplot_files ) = aml.load_data_alist_vex(alistfilename, vexfilename, CURRENT_PATH)

#### initial variable set
labels = labels_main
baselines = baselines_main
fringes = fringes_main
errors = errors_main
max_no_rows = len(labels)
max_no_columns = len(baselines)*2
mode_set = 0
ref_ant = ''
pol_choice = 0
start_row = 0
status_var1 = ''
status_var2 = ''
source_choice = ''
print_size_choice = 'small A4L'
do_PDF = 0
do_FR = 1
fplots_viewed = 0
tempfilenamelist=[]
gsprocesses = []

#### a fancier way to display antennas in the menu: both one- and two-letter codes
antennas_disp = [_+' '+codesdict[_] for _ in antennas]
antennas_dict = dict(zip(antennas_disp, antennas))

### initiating HOPS in case it is not initiated, not really working, luckily fplot runs without it
#subprocess.run("...", shell = True) # Python 3 
#hopspath = "/cluster/hops/x86_64-3.17/bin/"
#subprocess.call(". " + hopspath + "hops.bash", shell=True) 

### making the first display cut

need_entry, start_row, max_disp_rows, labels, baselines, fringes, errors = aml.keep_only_some_elements(ref_ant, source_choice, start_row, labels_main, baselines_main, fringes_main, errors_main)
no_rows = len(labels)
no_columns = len(baselines)*2

###### GUI prep
root = tk.Tk()
root.geometry("%sx%s" % root_size)
root.title('AMON v.'+AMON_VERSION)
FMas = tk.Frame(root)
FMas.grid(sticky='news')

#####################################
############ GUI controls ###########
#####################################

# GUI grid consists of 4 rows and 16 cols
# row 0: upper controls
# row 1: the canvas
# row 2 and 3: lower controls


#################### UPPER CONTROLS ##################
##################### MENU MODE ##############
# create menu -- row 0, col 0-1 of FMas
label = tk.Label(FMas, text="MODE:", font = ("Helvetica", 10))
label.grid(row=0, column=0, sticky="e")
menuMODE_tkvar = tk.StringVar(root)
menuMODE_items = ['FRINGE']
menuMODE_items.extend(['XPOL', 'SBD', 'DRATE'])
menuMODE_translate = {'FRINGE':0, 'XPOL':1, 'SBD':2, 'DRATE':3}
menuMODE_tkvar.set('FRINGE') # set the default option
popupMenuMODE = tk.OptionMenu(FMas, menuMODE_tkvar, *menuMODE_items)
popupMenuMODE.configure(font = ("Courier", 11)) 
popupMenuMODE.grid(row = 0, column = 1, sticky = 'w')
menuMODE_choices = popupMenuMODE.nametowidget(popupMenuMODE.menuname)
menuMODE_choices.configure(font=("Courier", 11)) 
# on change dropdown value
def change_dropdownMODE(*args):
  global mode_set 
  mode_set=menuMODE_translate[menuMODE_tkvar.get()]
  # link function to change dropdown
menuMODE_tkvar.trace('w', change_dropdownMODE) # watcher
################## END MENU MODE ##############

##################### MENU REFANT ##############
# create menu -- row 0, col 2-3 of FMas
label = tk.Label(FMas, text="Ref. Ant.:", font = ("Helvetica", 10))
label.grid(row=0, column=2, sticky="e")
menuRFA_tkvar = tk.StringVar(root)
menuRFA_items = ['ALL']
menuRFA_items.extend(antennas_disp)
menuRFA_tkvar.set('ALL') # set the default option
popupMenuRFA = tk.OptionMenu(FMas, menuRFA_tkvar, *menuRFA_items)
popupMenuRFA.configure(font = ("Courier", 11)) 
popupMenuRFA.grid(row = 0, column = 3, sticky = 'w')
menuRFA_choices = popupMenuRFA.nametowidget(popupMenuRFA.menuname)
menuRFA_choices.configure(font=("Courier", 11)) 
# on change dropdown value
def change_dropdownRFA(*args):
  global ref_ant 
  ref_ant=menuRFA_tkvar.get()
  if ref_ant == 'ALL':
    ref_ant = ''
  else:
    ref_ant = antennas_dict[ref_ant]
# link function to change dropdown
menuRFA_tkvar.trace('w', change_dropdownRFA) # watcher
################## END MENU REFANT ##############

##################### MENU POL ##############
# create menu -- row 0, col 4-5 of FMas
label = tk.Label(FMas, text="Pol.:", font = ("Helvetica", 10))
label.grid(row=0, column=4, sticky="e")
menuPOL_tkvar = tk.StringVar(root)
menuPOL_items = ['LL RR']
menuPOL_items.append('LR RL')
menuPOL_tkvar.set('LL RR') # set the default option
popupMenuPOL = tk.OptionMenu(FMas, menuPOL_tkvar, *menuPOL_items)
popupMenuPOL.configure(font = ("Courier", 11)) 
popupMenuPOL.grid(row = 0, column = 5, sticky = 'w')
menuPOL_choices = popupMenuPOL.nametowidget(popupMenuPOL.menuname)
menuPOL_choices.configure(font=("Courier", 11)) 
# on change dropdown value
def change_dropdownPOL(*args):
  global pol_choice
  pol_choice = menuPOL_tkvar.get()
  if pol_choice == 'LL RR':
    pol_choice = 0
  else:
    pol_choice = 1
# link function to change dropdown
menuPOL_tkvar.trace('w', change_dropdownPOL) # watcher
################## END MENU POL ##############

##################### MENU SRC ##############
# create menu -- row 0, col 6-7 of FMas
label = tk.Label(FMas, text="Src.:", font = ("Helvetica", 10))
label.grid(row=0, column=6, sticky="e")
menuSRC_tkvar = tk.StringVar(root)
menuSRC_items = ['ALL']
menuSRC_items.extend(sources)
menuSRC_tkvar.set('ALL') # set the default option
popupMenuSRC = tk.OptionMenu(FMas, menuSRC_tkvar, *menuSRC_items)
popupMenuSRC.configure(font = ("Courier", 11)) 
popupMenuSRC.grid(row = 0, column = 7, sticky = 'w')
menuSRC_choices = popupMenuSRC.nametowidget(popupMenuSRC.menuname)
menuSRC_choices.configure(font=("Courier", 11)) 
# on change dropdown value
def change_dropdownSRC(*args):
  global source_choice
  source_choice = menuSRC_tkvar.get()
  if source_choice == 'ALL': source_choice = ''
# link function to change dropdown
menuSRC_tkvar.trace('w', change_dropdownSRC) # watcher
################## END MENU SRC ##############

##################### MENU PRINT ##############
# create menu -- row 0, col 8-9 of FMas
label = tk.Label(FMas, text="Print Size:", font = ("Helvetica", 10))
label.grid(row=0, column=8, sticky="e")
menuPRT_tkvar = tk.StringVar(root)
menuPRT_items = ['small A4L', 'normal A4L', 'large A4L', 'small A4P', 'normal A4P', 'large A4P']
menuPRT_tkvar.set('small A4L') # set the default option
popupMenuPRT = tk.OptionMenu(FMas, menuPRT_tkvar, *menuPRT_items)
popupMenuPRT.configure(font = ("Courier", 11)) 
popupMenuPRT.grid(row = 0, column = 9, sticky = 'w')
menuPRT_choices = popupMenuPRT.nametowidget(popupMenuPRT.menuname)
menuPRT_choices.configure(font=("Courier", 11)) 
# on change dropdown value
def change_dropdownPRT(*args):
  global print_size_choice
  print_size_choice = menuPRT_tkvar.get()
# link function to change dropdown
menuPRT_tkvar.trace('w', change_dropdownPRT) # watcher
################## END MENU PRINT ##############

#################### TOP ROW ENTRY ##############################
# create entry -- row 0, column 10-11 of FMas
entr_label = tk.Label(FMas, text='')
entr_label.grid(row=0, column=10, sticky="e")
entry_tkvar = tk.StringVar(root)
entry_tkvar.set(start_row) # set the default option
rowEntry = tk.Entry(FMas, textvariable=entry_tkvar, width=5, justify=tk.RIGHT)
rowEntry.grid(row = 0, column = 11, sticky = "w")
# on change entry value
def change_entry(*args):
  global start_row
  try:
    ent_val = int(entry_tkvar.get())
    if ent_val < 0:
      start_row = 0
      entry_tkvar.set(str(start_row))
    elif ent_val > (max_no_rows - max_disp_rows):
      start_row = max_no_rows - max_disp_rows
      entry_tkvar.set(str(start_row))
    else:
      start_row = ent_val
  except ValueError:
    entry_tkvar.set(str(start_row))
# link function to change entry
entry_tkvar.trace('w', change_entry) # watcher
def config_entry():
  if need_entry:
    entr_txt = "Top row (0 - {}):".format(max_no_rows - max_disp_rows)
    entr_label.config(text = entr_txt)
    rowEntry.config(state='normal')
  else:
    entr_txt = "Top row (disabled):"
    entr_label.config(text = entr_txt)
    rowEntry.config(state='disabled')
    entry_tkvar.set(0)
########### END ENTRY ###################################
##### then follows REDRAW BUTTON in row 0, column 12-15 of FMas
##### but for technical reasons it has to be defined after the canvas
##### END OF UPPER CONTROLS

################# LOWER CONTROLS

################# PRINT BUTTON #####################
# create print button, row 2, col 0 of FMas
##### help display function
  
###### end of the print function
button_print = tk.Button(FMas,
                          text='PRINT PDF',
                          font = ("Helvetica", 12),
                          fg = 'white',
                          bg = 'blue',
                          command=print_canvas,
                          relief=tk.RAISED
                           )
button_print.grid(row=2, column=0, sticky="news")
################# END PRINT BUTTON ###################

################# HELP BUTTON #####################
# create help button, row 3, col 0 of FMas
##### help display function
def button_help():
  global help_win
  try:
    test = help_win.winfo_exists()
  except NameError: # it was not initiated yet
    test = False
  if not test:
    """
        just reading the content of helfilename
        and showing it in a separate window
    """
    with open(helpfilename, 'r') as helpf:
      help_text = helpf.read()
    help_text += "\n\n"
    # add antenna codes to the main help file
    with open(codesfilename, 'r') as antf:
      help_text += antf.read()
    help_text += """
####################################################################################################
                                 CURRENT CONFIGURATION FILE:

(linked directly from the included configuration file)

"""
    with open(configfilename, 'r') as cfgf:
      help_text += cfgf.read()
    help_text += """
####################################################################################################
"""
    help_win = tk.Toplevel(root)
    help_win.title('AMON '+ AMON_VERSION + ' HELP')
    helpScroll = tk.Scrollbar(help_win)
    helpText = tk.Text(help_win, height=help_size[1], width=help_size[0], font = ("Courier", 12), bg = 'white', fg = 'black')
    helpScroll.pack(side=tk.RIGHT, fill=tk.Y)
    helpText.pack(side=tk.LEFT, fill=tk.Y)
    helpScroll.config(command=helpText.yview)
    helpText.config(yscrollcommand=helpScroll.set)
    helpText.insert(tk.END, help_text)
  else:
    help_win.lift() #it is already opened, bring it up front
###### end of the help display function
button_help = tk.Button(FMas,
                        text='HELP',
                        font = ("Helvetica", 12),
                        fg = 'white',
                        bg = 'darkgreen',
                        command=button_help,
                        relief=tk.RAISED
                        )
button_help.grid(row=3, column=0, sticky="news")
################# END HELP BUTTON ###################

################ BOTTOM LABELs #######################
# bottom, row 2-3 of FMas, cols 1-13            
status_label1 = tk.Label(FMas, text=status_var1, font = ("Courier", 11))
status_label1.grid(row=2, column=1, columnspan = 13, sticky="news")
status_label2 = tk.Label(FMas, text=status_var2, font = ("Courier", 11))
status_label2.grid(row=3, column=1, columnspan = 13, sticky="news")
#######################################################

################PDF CHECK##############
#bottom, row 2 of FMas, cols 14-15
checkPDF_tkvar = tk.IntVar(root)
checkPDF_tkvar.set(0) # set the default option
PDF_check = tk.Checkbutton(FMas,
                           text="PDF copy",
                           variable=checkPDF_tkvar,
                           font = ("Helvetica", 12),
                           fg = 'red',
                           relief=tk.GROOVE
                           )
PDF_check.grid(row=2, column=14, columnspan = 2, sticky="news")
def change_PDF_check(*args):
  global do_PDF
  do_PDF = checkPDF_tkvar.get()
# link function to change entry
checkPDF_tkvar.trace('w', change_PDF_check) # watcher
########## end PDF check

################INCLUDE FRINGE check##############
#bottom, row 3 of FMas, cols 14-15
checkFR_tkvar = tk.IntVar(root)
checkFR_tkvar.set(1) # set the default option
FR_check = tk.Checkbutton(FMas,
                          text="require fringe",
                          variable=checkFR_tkvar,
                          font = ("Helvetica", 12),
                          fg = 'red',
                          relief=tk.GROOVE
                          )
FR_check.grid(row=3, column=14, columnspan = 2, sticky="news")
def change_FR_check(*args):
  global do_FR
  do_FR = checkFR_tkvar.get()
# link function to change entry
checkFR_tkvar.trace('w', change_FR_check) # watcher
########## end INCLUDE FRINGE check

########## END OF LOER CONTROLS

#################### MAIN CANVAS ###################
########### canvas prep
###### Main table -- row 1 of FMas
# Create a frame for the canvas
frame_canvas = tk.Frame(FMas,
                        bd=2,
                        relief=tk.GROOVE # making a nice border
                        )
frame_canvas.grid(row=1, column=0, columnspan=16, sticky='news')

# Add a canvas with scroll bars in that frame for all the action
# canvas is in (0,0) of frame_canvas

Can1 = main_canvas(frame_canvas, 0, 0)
frame_buttons = frame_for_buttons(Can1, canvas_size)

#################### END MAIN CANVAS ###################

# redraw button must be defined AFTER the main canvas
################# REDRAW BUTTON #####################
# create redraw button, row 0, col 12-15 of FMas
button_redraw = tk.Button(FMas,
                          text='REPLOT',
                          font = ("Helvetica", 12),
                          fg = 'red',
                          command=lambda frame = frame_canvas, size=canvas_size: redraw(frame, size),
                          relief=tk.RAISED
                           )
button_redraw.grid(row=0, column=12, columnspan = 4, sticky="news")
################# END REDRAW BUTTON ###################

#### plot the first time

redraw(frame_canvas, canvas_size)
print "Done!\n"
print "Opening GUI...\n"


### launch
root.mainloop()

######################### FINALIZING AFTER GUI CLOSES ###########################

#### cleaning temp files
rm_list_files_in_cur_dir(tempfilenamelist)

#### stopping all gsprocesses

### stopping the annoying x11 messages
old_stderr = sys.stderr
old_stdout = sys.stdout
f = open(os.devnull, 'w') # equivalent to /dev/bull in Linux and nul in Win
sys.stderr = f 
sys.stdout = f
# send 'spacebar' to all opened gs
for process in gsprocesses:
  try:
    process.communicate(input = " ") # into stdin
  except OSError:
    pass #process already gone
sys.stdout.flush()
sys.stderr.flush()
sys.stderr = old_stderr
sys.stdout = old_stdout
f.close()
