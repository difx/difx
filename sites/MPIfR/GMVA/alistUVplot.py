#!/usr/bin/python 
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

import sys
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator # needed for minor tick marks
from mpl_toolkits.axes_grid1.parasite_axes import SubplotHost # needed for double axes

#### load command line parameters

params = sys.argv

help_info = """
##################################################################################################
alistUVplot V.1.0b for Python3
This is a script to plot UVcoverage based on a given alist file. The script can be run with no
parameters with the default settings: alist file = alist_v6.out, all antennas and all sources,
both good fringes and no-fringe points, both RR and LL (if a lin pol antenna is present,
ALL correlations with either X or Y present are included). Autocorrelations are always ignored.
Both generic and v6 alist format are supported.

!!!!!!  No foolproofing is added at the moment, if it crashes -- you crashed it  !!!!!!

Parameters:
-h                 -- prints this help, then exits.
-has               -- special mode, prints all antennas and sources present in the alist file,
                      then exits, no plotting is done.
-f new_alist.file  -- changes the alist file, must be followed by a valid file name.
                      One can run more than one file by separating them with ; with no spaces:
                      e. g. -f file1.alist;file2.alist;file3.alist
-a B               -- plot only for this antenna, must be followed by a valid single-letter code.
                      One can include any number of antennas with no spaces and any separators,
                      e. g. -a BXY or -a k:l,m
-s BLLAC           -- plot only for this source, must be followed by a valid source name string.
                      One can include more than one source with any separators, but no spaces,
                      e. g. -s 3C279,BLLAC:SGR_A or even with no separators -s 3C279BLLAC.
-g                 -- include good data (with fringes) only
-b                 -- include bad data (with no fringes) only  

using both -g and -b flags will plot both good and bad data points, but this is not necessary,
since plotting all points is default

-rr                -- include only RR pol (if lin pol is present, it is NOT included)

-ll                -- include only LL pol (if lin pol is present, it is NOT included)

using both -ll and -rr flags will effectively ignore any linear polarizations present

-t                 -- uv tick scale in Mlambda, default is 1000 Mlambda,
                      may be necessary to change to 100 for nicer viewing of compact arrays
-p                 -- percentile of snr included into the color scale
                      (otherwise a few very high SNR outliers can dominate the scale)
                      default is 75, i. e. 3d quartile.
-c                 -- angular scale unit in uas, for the circles, default is 25 uas.

More parameters can be changed directly in the script file, most important parameters are set
in the very beginning. A pdf copy of the latest plot is saved in alistUVplot.pdf, but this file
is overwritten during subsequent runs.
##################################################################################################
"""

if '-h' in params:
  print(help_info) # help requested
  sys.exit(1)

#### input parameters  

filenames=['alist_v6.out'] # DEFAULT
mult_files = False
if '-f' in params:
  filename = params[params.index('-f')+1] # another filename requested
  if ';' in filename: # more than one filename included
    mult_files = True
    filenames = filename.split(';')
  else: # no, this is just a single file
    filenames = [filename]

source = '' # DEFAULT, all sources
if '-s' in params: source = params[params.index('-s')+1] # a particular source requested

ant = '' # DEFAULT, all antennas
if '-a' in params: ant = params[params.index('-a')+1] # a particular antenna is requested

# DEFAULT: show both good and bad
showGood = '-g' in params
showBad = '-b' in params
if not showGood and not showBad:
  showGood = True
  showBad = True

# DEFAULT: show RR, LL and any combinations with X or Y
pol_str = ''
showRR = '-rr' in params
if showRR: pol_str = pol_str + ' RR'
showLL = '-ll' in params
if showLL: pol_str = pol_str + ' LL'
showXY = False
pol_str = pol_str + '.'
if not showRR and not showLL:
  showRR = True
  showLL = True
  showXY = True
  pol_str = ' all pols.'

#### for most global VLBI 1000 Mlabda is a good scale, but for small arrays,
#### e.g. EVN or VLBA 100 Mlabda would be better
UVtickscale = 1000.
if '-t' in params: UVtickscale = float(params[params.index('-t')+1])

#### typically the highest snr points sould be expluded from the colormap
#### because otherwise the whole colorscale is dominated by them
#### exluding the top quartile seems good 
zplot_percentile = 75.
if '-p' in params: zplot_percentile = float(params[params.index('-p')+1])
#### this parameter determines the precision of angular scale visualization
#### 25. uas works well in most cases 
ang_step = 25. # in uas
if '-c' in params: ang_step = float(params[params.index('-c')+1])

####### finished with basic parameters

########## graphical parameter block ################
bg_color = 'lightgray'
grid_color = 'white'
add_line_color = 'darkblue' ### the "cross-hairs"
color_map = 'Spectral_r' # _r reverses the colors of the map
badpointcolor = 'white'
# some other good color maps:
# (for full info go to https://matplotlib.org/users/colormaps.html )
# 'viridis', 'plasma', 'inferno', 'magma'
# 'gray', 'bone', 'pink', 'greys', 'binary'
# 'spring', 'summer', 'autumn', 'winter', 'cool'
# 'hot', 'gist_heat', 'copper'
# 'Spectral', 'coolwarm', 'gnuplot', 'gnuplot2'
# 'gist_rainbow', 'rainbow', 'jet'
marker_style = 'o'
# * star, o circle, s square, . dot, + , x as they are,  1-4 mill, 8 - octagon, h, H - hexagons
# d long diamond, D - symmetric diamond, _ | bars, p pentagon, > < ^ v triangles
marker_size = 25 # in weird squared units, i.e. 25 is actually 5
grid_on = True
number_xticks = 5
number_yticks = 5
xminorticknumber = 5
yminorticknumber = 5
xtickformat = '%1.0f'
ytickformat = '%1.0f'
ztickformat = '%1.0f' # for the color bar
titlefontsize = 24 # main title
axisfontsize = 24 # axis titles
labelfontsize = 22 # for all UV ticks
labelfontcolor='black' # changing the color DESPITE the global attribute
labelboxfacecolor='None' # box around each label
labelboxedgecolor='None'
labelboxtransparency=1.0 # 0 is nothing, 1 is everything
aspect_value = 1.0 # useless for this symmetric plot, but keep for consistency
#xaxis_label = r'$U$, M$\lambda$'
xaxis_label = r'U, Mlambda'
#yaxis_label = r'$V$, M$\lambda$'
yaxis_label = r'V, Mlambda'
### most LaTeX symbols work, here's a more complex examples
#xaxis_label = r'$V_{\mathrm{LSR}}$ $[$ km s$^{-1}\,]$'
#yaxis_label = r'$\left < \, T_{\mathrm{B}} \,\right >$ $[$ K $]$'
### colorbar fonts
cbartitlefontsize = 24
cbarlabelfontsize = 20
cbartitlefontcolor = 'k'
cbarlabelfontcolor = 'k'
####### cross-hairs
angfontsize = 14 # the tiny uas labels

##### plotting and printing parameters
figwidth=11 # the width in inches
figheight=8 # the height in inches
plot_resolution=72 # dpi value
print_resolution = 600 # dpi value
savefilename='alistUVplot.pdf'
print_format='pdf' # can be 'png', 'pdf', 'eps', 'ps' and 'svg'
####################################

############################################################
#### loading data from the alist file into plot arrays #####
############################################################
# arrays for points with non-zero fringes 
xplot = [] # U
yplot = [] # V
zplot = [] # this collects log10(SNR) later used for coloring the points
# arrays for points with zero fringes
xBADplot = [] # U
yBADplot = [] # V
# for -has mode
allants = []
allsources = []
# to determine max uv radius
Rmax = 0.
RBADmax = 0.

for filename in filenames:
  with open(filename, 'r') as f:
    line = '*'
    while line[0] == '*': # skip the header
      line = f.readline()
    #freq = 1e6*float(line.split()[36] # in Hz -- can get ref freq for informational purposes
    while (line != "\n" and line != ""):
      if not ('-has' in params): # normall plotting mode
        bs = line.split()[14] # baseline, e. g. BX
        pol = line.split()[17] # polarization, e. g. RR
        qual = line.split()[15] # fringe code, e. g. 9 or 7D
        src = line.split()[13] # source name, e. g. BLLAC
        U = float(line.split()[32])
        V = float(line.split()[33])
        R = np.sqrt(U*U+V*V)
        if ((pol[0] == 'R' and pol[1] == 'R' and showRR)\
        or (pol[0] == 'L' and pol[1] == 'L' and showLL)\
        or (('X' in pol or 'Y' in pol) and showXY))\
        and (not source or src in source)\
        and ((bs[0] != bs[1]) and (not ant or bs[0] in ant or bs[1] in ant)):
          if int(qual[0]) > 0:
            xplot.append(U)
            yplot.append(V)
            xplot.append(-U)
            yplot.append(-V)
            zplot.append(np.log10(float(line.split()[20]))) #snr, immediately translate to the logarithmic scale
            zplot.append(np.log10(float(line.split()[20]))) #snr, immediately translate to the logarithmic scale
            if R > Rmax: Rmax = R # looking for max UV radius
          else:
            xBADplot.append(U)
            yBADplot.append(V)
            xBADplot.append(-U)
            yBADplot.append(-V)
            if R > RBADmax: RBADmax = R # looking for max UV radius
      else: # -has mode
        bs = line.split()[14] # baseline, e. g. BX
        src = line.split()[13] # source, e. g. BLLAC
        if bs[0] not in allants: allants.append(bs[0])
        if bs[1] not in allants: allants.append(bs[1])
        if src not in allsources: allsources.append(src)      
      line = f.readline()
####################################################
####### done with loading the data #################
####################################################

if '-has' in params:
  print('\nAntennas present in this alist file:')
  atp = ''
  for _ in sorted(allants):
    atp = atp+_+', '
  atp=atp[:-2]+'.\n'
  print(atp)
  print('\nSources present in this alist file:')
  atp = ''
  for _ in sorted(allsources):
    atp = atp + _ + ', '
  atp=atp[:-2]+'.'
  print(atp)
  sys.exit(2)

#### constructing the title (can do it only now to modify the empty strings)
if not source: source = 'all sources'
if not ant:
  ant = 'all antennas'
else:
  ant = 'ant: ' + ant
label1 = r'$UV$ cov.: ' + source + ', ' + ant + ',' + pol_str
############ end graphic parameters ###################


### if not a single points for the given set of parameters
### e. g. if a source is unknown or this antenna did not observe
### or if user asked to exclude both good and bad fringes
if (not xplot and not xBADplot) or (not showGood and not showBad):
  print('\nNo data corresponds to the given criteria, exiting...')
  sys.exit(0) # stop the script

#wavelength = 299792458./freq # in meters -- can calculate wavelength for informational purposes

#### analyzing the loaded data to determine the size of the plot field
#### and the color scale
xmin=np.min(xplot+xBADplot) # both good and bad points, + in this case concatenates the lists
xmax=np.max(xplot+xBADplot)
ymin=np.min(yplot+yBADplot)
ymax=np.max(yplot+yBADplot)
if zplot: # zplot stays [] if all points are bad
  zmin=np.min(zplot) # for convenient colorbar labeling
  zmax=np.max(zplot)
else: # not really needed, but to have the variables defined just in case
  zmin = 0
  zmax = 0
# now find the max of all of them
# the biggest of all determines the whole field, because we want it to be symmetric
xmax = np.max([np.abs(xmin), np.abs(xmax), np.abs(ymin), np.abs(ymax)])
xmin = -xmax
ymin = -xmax
ymax = xmax

#### now we determine the good tick positions, rounding them to the following
xtickscale = UVtickscale # determines the order to which the ticks are rounded
ytickscale = UVtickscale

### roll it up or down to the nearest nice number
xtickmin = xtickscale*np.floor(xmin/xtickscale)
xtickmax = xtickscale*np.ceil(xmax/xtickscale)
ytickmin = ytickscale*np.floor(ymin/ytickscale)
ytickmax = ytickscale*np.ceil(ymax/ytickscale)

#### minimum is kept, but some of the highest points are better to be expluded from
#### the color map in order not to dominate the dynamic range
if zplot:
  ztickmin = zmin
  ztickmax = np.percentile(zplot, zplot_percentile) # typically 3d quartile
else: ### just to keep these defined
  ztickmin = 0
  ztickmax = 0
   
# finally add some offsets to keep the field slightly bigger for easier viewing
offset = (0.1*xtickmax,0.1*ytickmax) # added to max and min of the data to make a gap between data and axes

### these are the final sizes of the plot
xplmax = xtickmax+offset[0]
xplmin = xtickmin-offset[0]
yplmax = ytickmax+offset[1]
yplmin = ytickmin-offset[1]

label1_position = (0, yplmax+offset[1]) # the title position in plot coordinates

######################################
####### end of min/max analysis ######
######################################

#### calculating the angular size equivalent (lambda/D)
#### for the longest baseline and 2 angular steps higher than the largest
if showGood and showBad: Rmax = max(Rmax, RBADmax)
elif showBad: Rmax = RBADmax
min_ang = 648000./(np.pi*Rmax) # in uas -- 648000 = 180*3600, number of as in pi radians
min_pl_ang = ang_step*np.around(min_ang/ang_step)
uvradpl1 = 648000./(np.pi*min_pl_ang)
uvpllabel1 = str(int(min_pl_ang))+ ' uas'
uvradpl2 = 648000./(np.pi*(min_pl_ang+ang_step*2))
uvpllabel2 = str(int(min_pl_ang+ang_step*2))+ ' uas'
#### finished with angular scale

##### setting the arrays for ticks
xticksarray=np.linspace(xtickmin, xtickmax, number_xticks, endpoint=True)
yticksarray=np.linspace(ytickmin, ytickmax, number_yticks, endpoint=True)
xtickminlabel = xtickmin
xtickmaxlabel = xtickmax
ytickminlabel = ytickmin
ytickmaxlabel = ytickmax

xticklabnum=np.linspace(xtickminlabel, xtickmaxlabel, number_xticks, endpoint=True)
xticklabels=[]
for numb in xticklabnum:
  xticklabels.append(xtickformat % numb) # simply collecting the strings of the numbers in the given format
yticklabnum=np.linspace(ytickminlabel, ytickmaxlabel, number_yticks, endpoint=True)
yticklabels=[]
for numb in yticklabnum:
  yticklabels.append(ytickformat % numb)
##### finished with the UV ticks

##### preparing the log10 space for the color bar
### this is a dedicated function
def NiceLogScale(log_scale_min, log_scale_max): # assuming min to be positive
  """
    given two points on the log10 scale
    this function fills the gap between them with
    log10 points corresponding to whole steps
    in the highest order of 10,
    i. e. for log10(7.1) and log10(26.7)
    it will add between them 8, 9, 10, 20    
  """
  scale = [log_scale_min]
  unit = np.around(10.**np.floor(log_scale_min)) # e. g. for 7 this will be 1
  new_item = unit*np.ceil((10.**log_scale_min)/unit) # next whole number after the given
  # if new_item is almost identical with the first, go to the next one
  if np.abs(new_item - 10**log_scale_min) < 1e-3: new_item = new_item + unit # to account for small calculation errors
  while np.log10(new_item) < log_scale_max:
    scale.append(np.log10(new_item))
    if new_item == unit*10.: unit = unit*10 # going to the next order
    new_item = new_item+unit
  scale.append(log_scale_max)
  return scale 
##### end of function
#### actual filling in of the colorbar scale and ticks
cbartickarray= NiceLogScale(ztickmin, ztickmax)
cbarticksize = len(cbartickarray)
cbarlabels=[]
# we take care to rarefy the labels if there are too many of them
for numb in cbartickarray:
  ## we only plot labels if 1. total number of them is small or 2. they are even times power of 10 or 3. they are 1,10,100,1000 etc. 
  if cbarticksize < 10 or np.around(10.**numb/10.**np.floor(numb)) % 2 == 0 or np.around(10.**numb/10.**np.floor(numb)) == 1.:
    cbarlabels.append(ztickformat % np.around(10.**numb))
  else:
    cbarlabels.append('')
cbarlabels[0] = '' # the min label is not necessary
cbarlabels[-1] = '' # the max label will be plotted separately, set in the next line
toplabel = '>' + (ztickformat % np.around(10.**cbartickarray[-1]))
##### finished with the colorbar array setup

#################################################################################################  
# set the GRAPHIC GLOBALS, for the plot lines to be massive and suitable for significant shrinking
# TICKS
mpl.rcParams['xtick.major.size'] = 13
mpl.rcParams['xtick.major.width'] = 3
mpl.rcParams['xtick.minor.size'] = 8
mpl.rcParams['xtick.minor.width'] = 2
mpl.rcParams['xtick.color'] = 'black' # the color of both tick and label,
                                      # but the label color is tweaked later
mpl.rcParams['xtick.direction'] = 'in' # can be 'in', 'out' or 'inout'
  
mpl.rcParams['ytick.major.size'] = 13
mpl.rcParams['ytick.major.width'] = 3
mpl.rcParams['ytick.minor.size'] = 8
mpl.rcParams['ytick.minor.width'] = 2
mpl.rcParams['ytick.color'] = 'black'
mpl.rcParams['ytick.direction'] = 'in' 
  
mpl.rcParams['xtick.major.pad']=10
mpl.rcParams['ytick.major.pad']=10
  
#### keep this block for reference, it is not needed, will be set up later
#GRID 
#mpl.rcParams['grid.color'] = 'gray'   # grid color
#mpl.rcParams['grid.linestyle'] = ':'  # dotted
#mpl.rcParams['grid.linewidth'] = 0.5  # in points
#mpl.rcParams['grid.alpha'] = 0.3      # transparency, between 0.0 and 1.0

# for more global parameters see in Python directory ...site-packages/matplotlib/mpl-data/matplotlibrc.
############## finished with setting up globals

##############################
#### the PLOTTING block ######
##############################

fig=plt.figure(figsize=(figwidth,figheight),dpi=plot_resolution)
fig.patch.set_alpha(1.0) # figure background transparency control, makes only figure background transparent
plt.gcf().subplots_adjust(bottom=0.11, top=0.89, left=0.15, right=0.85, wspace=0.1, hspace=0.1) # add more space for axis labels and borders
# gcf() means 'get current figure', a handle for accessing its properties

# normal axes
ax = SubplotHost(fig, 1,1,1, aspect=aspect_value*abs((xplmax-xplmin)/(yplmax-yplmin)), axisbg=bg_color)
# 1,1,1 mean nrows, ncols and plot_number
ax.patch.set_alpha(1.0)
# X AXIS
ax.axis["bottom"].set_label(xaxis_label)  
ax.spines['bottom'].set_linewidth(4)
#ax.spines['bottom'].set_position(('data',yplmin))
ax.axis['bottom'].label.set_horizontalalignment('center') # 'left' is in fact 'right', and 'right' --> 'left'! 
ax.axis['bottom'].label.set_verticalalignment('top')
ax.axis['bottom'].label.set_fontsize(axisfontsize)
ax.axis['bottom'].label.set_family('sans-serif')
ax.axis['bottom'].label.set_style('normal') # 'normal', 'italic' or 'oblique'
ax.axis['bottom'].label.set_color('black')

# Y AXIS
ax.axis["left"].set_label(yaxis_label)
ax.spines['left'].set_linewidth(4)
#ax.spines['left'].set_position(('data',xplmin))
ax.axis['left'].label.set_horizontalalignment('center') # 'left' is in fact 'right', and 'right' --> 'left'! 
ax.axis['left'].label.set_verticalalignment('center')
ax.axis['left'].label.set_fontsize(axisfontsize)
ax.axis['left'].label.set_family('sans-serif')
ax.axis['left'].label.set_style('normal') # 'normal', 'italic' or 'oblique'
ax.axis['left'].label.set_color('black')

# setting ticks on all axes
ax.xaxis.set_ticks_position('both') # can be also 'top', 'bottom' or 'none'
ax.yaxis.set_ticks_position('both') # can be also 'left', 'right' or 'none'

# counterpart X AXIS
ax.spines['top'].set_color('black') # set to 'none' to make axis invisible
ax.spines['top'].set_linewidth(4)
#ax.spines['top'].set_position(('data',yplmax))
  
# counterpart Y AXIS
ax.spines['right'].set_color('black')  
ax.spines['right'].set_linewidth(4)
#ax.spines['right'].set_position(('data',xplmax)) # there is +1 offset in the number

# TICKS
ax.set_xlim(xplmin, xplmax)
ax.set_ylim(yplmin, yplmax)
ax.set_xticks(xticksarray) 
ax.set_yticks(yticksarray)
ax.set_xticklabels(xticklabels)
ax.set_yticklabels(yticklabels)
  
for label in ax.get_xticklabels():
  label.set_fontsize(labelfontsize)
  label.set_color(labelfontcolor)
  label.set_bbox(dict(facecolor=labelboxfacecolor, edgecolor=labelboxedgecolor, alpha=labelboxtransparency))
  label.set_horizontalalignment('center') # 'left' is in fact 'right', and 'right' --> 'left'! 
  label.set_verticalalignment('top')
    
for label in ax.get_yticklabels():
  label.set_fontsize(labelfontsize)
  label.set_color(labelfontcolor)
  label.set_bbox(dict(facecolor=labelboxfacecolor, edgecolor=labelboxedgecolor, alpha=labelboxtransparency))
  label.set_horizontalalignment('right') # 'left' is in fact 'right', and 'right' --> 'left'! 
  label.set_verticalalignment('center')
 
# minor tickmarks

xminorLocator   = AutoMinorLocator(xminorticknumber) 
ax.xaxis.set_minor_locator(xminorLocator)
yminorLocator   = AutoMinorLocator(yminorticknumber)
ax.yaxis.set_minor_locator(yminorLocator)

#### this is the main grid setup
if grid_on:
  ax.grid(which='major', axis='x', linewidth=1.5, linestyle='--', color=grid_color, alpha=0.7, zorder = 1) # alpha sets transparency
  ax.grid(which='major', axis='y', linewidth=1.5, linestyle='--', color=grid_color, alpha=0.7, zorder = 1)
  ax.grid(which='minor', axis='x', linewidth=0.75, linestyle=':', color=grid_color, alpha=0.5, zorder = 1)
  ax.grid(which='minor', axis='y', linewidth=0.75, linestyle=':', color=grid_color, alpha=0.5, zorder = 1)
  
# NOTE: zorder controls the order of layers, 1 is the lowest, larger numbers go closer to top
# here zorder is set for all graphic elements to keep the overlapping neat

#### we will define norm and cmap withing the corresponding functions, but this is how
#### they can be defined separately
#norm = mpl.colors.Normalize(vmin=ztickmin, vmax=ztickmax) # now every color in the range ztickmin, ztickmax is mapped to [0,1] with norm()
#cmap = mpl.cm.get_cmap(color_map) # set the color map
#zcolors = [cmap(norm(_)) for _ in zplot]

##### the main scatter plot of positive fringe points
if zplot and showGood:
  ax.scatter(xplot, yplot, c = zplot, ### 3 arrays of the same length, zplot controls the color based of the color map
             s = marker_size,
             vmin = ztickmin, vmax=ztickmax, cmap = color_map, # defining the colormap
             marker = marker_style,
             linewidths = None, # edge line of the marker, None means default
             edgecolors = 'face', # same as facecolor
             zorder = 3,
             rasterized=True # to avoid each point represented as a separate vector graphic object
             ) # take care to present errors as a DOUBLE list, otherwise it throws undocumented error
  
  ###### colorbar block ##############################
  ###### only needed if good points are plotted
  ##### these numbers control the size and position of the color bar, which is squeezed within this additional small plot field
  cbar_ax = fig.add_axes([0.81,0.15,0.05,0.7]) # dedicated colorbar axes, cannot plot this in the ax without triggering massive mpl glitches
  ##### ColorbarBase is a generic mpl colorbar, it is not connected to our scatter plot, we simply define it with
  ##### exactly the same color space
  cbar = mpl.colorbar.ColorbarBase(cbar_ax,                          
                                   cmap = mpl.cm.get_cmap(color_map),
                                   ticks = cbartickarray,
                                   # alpha=0.5,
                                   norm=mpl.colors.Normalize(vmin=min(cbartickarray),vmax=max(cbartickarray),clip=False),
                                   drawedges=False, # separators between colors, useful for a few colors only
                                   orientation='vertical')
             
  cbar.set_label('SNR', fontsize = cbartitlefontsize, color = cbartitlefontcolor)
  #cbar.set_ticks(cbartickarray, update_ticks=True) -- not needed, defined in the cbar setup above
  cbar.set_ticklabels(cbarlabels, update_ticks=True)
  #cbar.update_ticks() -- may be necessary in some cases to avoid glitches
  cbar.outline.set_color('black')
  cbar.outline.set_linewidth(3)

  # tweak cbar labels to override the tiny globals
  # labelboxfacecolor='None' # box around each label
  # labelboxedgecolor='None'
  # labelboxtransparency=1.0 # 0 is nothing, 1 is everything
  for label in cbar.ax.get_yticklabels():
    label.set_fontsize(cbarlabelfontsize)
    label.set_color(cbarlabelfontcolor)
    # label.set_bbox(dict(facecolor=labelboxfacecolor, edgecolor=labelboxedgecolor, alpha=labelboxtransparency)) 
  
  #### the separate label for the top of the colorbar
  cbar_ax.text(1.02, 1.02, toplabel, # vertical position has to be tweaked!
               horizontalalignment='left', 
               verticalalignment='bottom', # 'bottom' is in fact 'top', designation is inversed
               fontsize=cbarlabelfontsize, # can also be 'small' or 'large' etc.
               family='sans-serif', # can be 'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
               weight='normal', #  'normal', 'bold', 'heavy', 'light', 'ultrabold', 'ultralight'
               color='k')   
  
  ######## end colorbar block ###############          
  ###########################################
  
#### the no-fringe plotting block, no cbar here is needed
if xBADplot and showBad:
  ### points with no fringe plot in single color
  ax.scatter(xBADplot, yBADplot, c = badpointcolor,
             s = marker_size,
             marker = marker_style,
             linewidths = None, # edge line of the marker, None means default
             edgecolors = 'face', # same as facecolor
             zorder = 2,
             rasterized=True # to avoid each point represented as a separate vector graphic object
             ) # take care to present errors as a DOUBLE list, otherwise it throws undocumented error
  
  #### 'legend' for bad points
  ### a single point in upper left corner
  ax.scatter(0.85*xplmin, 0.85*yplmax, c = badpointcolor,
             s = marker_size,
             marker = marker_style,
             linewidths = None, # edge line of the marker, None means default
             edgecolors = 'face', # same as facecolor
             zorder = 4,
             #rasterized=True # to avoid each point represented as a separate vector graphic object
             ) # take care to present errors as a DOUBLE list, otherwise it throws undocumented error
  ### its annotation
  ax.annotate(' = no fringe',
            xy=(0.85*xplmin, 0.85*yplmax),
            fontsize=18,
            color = badpointcolor,
            horizontalalignment='left',
            verticalalignment='center',
            #rotation = -40,
            zorder = 4)
           
##### finished with bad point plotting

##########################################
##### the 'cross-hairs' block ############
##########################################
            
ax.plot([xplmin, xplmax], [0,0], linewidth=1.5, linestyle='--', color=add_line_color, zorder = 4) # zero line
ax.plot([0,0], [yplmin, yplmax], linewidth=1.5, linestyle='--', color=add_line_color, zorder = 4) # zero line

#can use add_artist for the same
#ax.add_artist(plt.Circle((0,0),uvradpl1,color='k',fill=False, linestyle = '--', linewidth = 1.5, label = 'blah'))

ax.annotate(uvpllabel1,
            xy=(uvradpl1*np.cos(np.pi*50./180.), uvradpl1*np.sin(np.pi*50./180.)), # 50 degrees is visually better than 45
            fontsize=angfontsize,
            color = add_line_color,
            horizontalalignment='center',
            verticalalignment='top',
            rotation = -40,
            zorder = 4)
ax.add_patch(plt.Circle((0,0),uvradpl1,color=add_line_color,linestyle = 'dashed', fill=False, linewidth = 1.5, zorder = 4))
ax.annotate(uvpllabel2,
            xy=(uvradpl2*np.cos(np.pi*50./180.), uvradpl2*np.sin(np.pi*50./180.)),
            fontsize=angfontsize,
            color = add_line_color,
            horizontalalignment='center',
            verticalalignment='top',
            rotation = -40,
            zorder = 4)
ax.add_patch(plt.Circle((0,0),uvradpl2,color=add_line_color,linestyle = 'dashed', fill=False, linewidth = 1.5, zorder = 4))
################# finished with cross-hairs

# add the title
ax.text(label1_position[0], label1_position[1], label1, # vertical position has to be tweaked!
        horizontalalignment='center', 
        verticalalignment='bottom', # 'bottom' is in fact 'top', designation is inversed
        fontsize=titlefontsize, # can also be 'small' or 'large' etc.
        family='sans-serif', # can be 'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
        weight='normal', #  'normal', 'bold', 'heavy', 'light', 'ultrabold', 'ultralight'
        color='k')   

######## create, save and plot the figure
fig.add_subplot(ax)
    
# print the plot
plt.gcf()
plt.savefig(savefilename, dpi=print_resolution, format=print_format)
# show the plot
fig = plt.gcf()
fig.set_dpi(plot_resolution)
plt.show()
