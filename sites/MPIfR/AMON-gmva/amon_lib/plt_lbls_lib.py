from __future__ import division # division behaves like in Python 3
from matplotlib import pyplot as plt

class LabelCanvas:
  """
      a plotting environment for text labels GrLabel,
      takes care or various definitions like plotting
      limits and tools to translate plotting into
      simple numbers or rows and columns like in a table
      A number of sizes is predefined.
  """
  def __init__(self, kind, title): # kind = 'small', 'normal' etc., title -- any (reasonable in size) string
    self.title = title
    kinds = {'small A4L':  (6, (46, 42), (0.06,0.95,0.02,0.95), (0.958, 0.007, 0.958, -0.0215),\
                           (0.98, -0.031, 0.075, 0.021), ((0.958, -0.0215),(0.0745, 0.0105))),\
             'normal A4L': (8, (36, 32), (0.06,0.95,0.02,0.95), (0.953, 0.007, 0.949, -0.027),\
                           (0.98, -0.021, 0.096, 0.0266), ((0.953, -0.027),(0.0955, 0.0133))),\
             'large A4L':  (9, (31, 27), (0.06,0.95,0.02,0.95), (0.945, 0.005, 0.943, -0.031),\
                           (0.977, -0.019, 0.105, 0.031), ((0.945, -0.031),(0.1045, 0.0155))),\
             'small A4P':  (6, (69, 26), (0.06,0.955,0.02,0.95), (0.968, 0.035, 0.911, -0.0145),\
                           (0.983, -0.033, 0.13, 0.03), ((0.968, -0.0145),(0.1295, 0.015))),   \
             'normal A4P': (8, (52, 19), (0.06,0.955,0.02,0.95), (0.963, 0.035, 0.883, -0.019),\
                           (0.982, -0.025, 0.16, 0.038), ((0.963, -0.019),(0.1595, 0.019))), \
             'large A4P':  (9, (47, 17), (0.06,0.955,0.02,0.95), (0.958, 0.02, 0.8915, -0.021),\
                           (0.98, -0.03, 0.16, 0.043), ((0.958, -0.021),(0.1595, 0.0215)))           
            }
    if kind in kinds.keys():
      self.kind          = kind
      self.fontsize      = kinds[kind][0]
      self.size          = kinds[kind][1] # (rows, cols//2) i. e. (scans, baselines)
      self.botmargin     = kinds[kind][2][0] # bottom margin
      self.topmargin     = kinds[kind][2][1] # top margin
      self.lftmargin     = kinds[kind][2][2] # left margin
      self.rgtmargin     = kinds[kind][2][3] # right margin
      self.scntoprow     = kinds[kind][3][0] # top row to start plotting scans
      self.scnlftcol     = kinds[kind][3][1] # left column to plot scans
      self.scnrgtcol     = kinds[kind][3][2] # right column to plot scans
      self.scnrowstep    = kinds[kind][3][3] # step in rows
      self.bsltoprow     = kinds[kind][4][0] # top row to plot baselines
      self.bslbotrow     = kinds[kind][4][1] # bottom row to plot baselines
      self.bsllftcol     = kinds[kind][4][2] # left column to start plotting baselines
      self.bslcolstep    = kinds[kind][4][3] # step in columns
      self.cellstartrow  = kinds[kind][5][0][0] # start row for cells
      self.cellrowstep   = kinds[kind][5][0][1] # step in rows for cells
      self.cellstartcol  = kinds[kind][5][1][0] # start col for cells
      self.cellcolstep   = kinds[kind][5][1][1] # step in cols for cells
      if kind[-3:] == "A4L": # A4 landscape
        self.figwidth=11.69
        self.figheight=8.27
      elif kind[-3:] == "A4P": # A4 portrait
        self.figwidth=8.27
        self.figheight=11.69
    else:
      raise Exception('LabelCanvas only supports', kinds.keys())
      
  def StartPlot(self):
    """
        This is a continuation of __init__
        separated only for reading convenience.
        It takes care of starting the PyPlot plotting
        environment.        
    """
    ### plot prep
    resolution = 72 # in case plt.show() invoked, will change before saving pdf
    self.fig=plt.figure(figsize=(self.figwidth, self.figheight), dpi=resolution)
    # gcf = get current figure, wspace and hspace -- space between subplots, others are essentially margins
    # 0 corresponds to left edge and bottom edge, 1 -- to right and top 
    plt.gcf().subplots_adjust(bottom=self.botmargin, top=self.topmargin, left=self.lftmargin, right=self.rgtmargin, wspace=0, hspace=0) # add more space for axis labels and borders
    # create the actual plotting field
    nrows=1
    ncols=1
    plot_number=1
    #bgcolor='#dfdfdf' # html hex string, can be seen in  Photoshop
    # or
    bgcolor='white' # any html X11 text name 
    # can also be an RGB tuple
    plt.subplot(nrows,ncols,plot_number,
                axisbg = bgcolor
                #facecolor=bgcolor # in newer versions
                )
    self.ax=plt.gca() # handle to access the axes and their labels, means 'get current axes'
    #plt.xlim(0, 100) # may define convenient coordinates
    #plt.ylim(0, 100)
    ### making sure we see no axis or ticks or labels
    self.ax.spines['top'].set_color('none') # set to 'none' to make axis invisible 
    self.ax.spines['bottom'].set_color('none')  
    self.ax.spines['left'].set_color('none') # set to 'none' to make axis invisible
    self.ax.spines['right'].set_color('none')  
    self.ax.xaxis.set_ticks_position('none') # can be also 'top', 'bottom' or 'none'
    self.ax.yaxis.set_ticks_position('none') # can be also 'left', 'right' or 'none'
    self.ax.set_xticklabels([])
    self.ax.set_yticklabels([])
    #### end plot prep
    plt.title(self.title,
              horizontalalignment='center',
              verticalalignment='bottom', # 'bottom' is in fact 'top', designation is inversed
              fontsize=11, # can also be 'small' or 'large' etc.
              family='sans-serif', # can be 'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
              weight='normal', #  'normal', 'bold', 'heavy', 'light', 'ultrabold', 'ultralight'
              color='black')
              
  def AddAxes(self, labels, baselines):  
    """
        Also separated from __init__ just fro convenience.
        This defines vertical and horizontal labels to be
        plot, checks them for size etc., but does not actually
        plot them yet.
    """    
    if len(labels) > self.size[0]:
      raise Exception('Only no more than', self.size[0], 'row labels acceptable!')
    else:
      # trim the labels
      newlabels = []
      for label in labels:
        if len(label) < 13 or len(label) > 13:
          label = '{:<13}'.format(label[:13])
        newlabels.append(label)
      self.labels = newlabels
    if len(baselines) > self.size[1]:
      raise Exception('Only no more than', self.size[1], 'column labels acceptable!')
    elif not all(len(_) == 2 for _ in baselines): # we must have 2 and only 2 letters in baseline labels
      raise Exception('Only 2-letter baseline labels are permitted!')
    else:
      self.baselines = baselines
  
  def Show(self):
    """
        view the plot
    """
    fig = plt.gcf()
    fig.set_dpi(72)
    plt.show()
  
  def Print(self, filename):
    """
        makes a pdf file
    """
    fig = plt.gcf()
    fileformat='pdf' # can be 'png', 'pdf', 'eps', 'ps' and 'svg'
    resolution=600 # dpi
    plt.savefig(filename, dpi=resolution, format=fileformat)
    plt.clf() # close current figure
    plt.close()
    
class GrLabel:
  """
      a class of text labels to be plot in a PyPlot environment
      Only foreground color, background color, text and font size
      are defined. If text is '', the label is made the size
      of a single letter.
  """
  def __init__(self, fgcolor, bgcolor, text, fontsize):
    self.fgcolor = fgcolor
    self.bgcolor = bgcolor
    self.text = text
    self.fontsize = fontsize
    
  def put(self, axis, row, col):
    """
        needs defined PyPlot axes to be run
        row and col are in the "figure" coordinates,
        i. e. 1 at the top, 0 at the bottom,
        0 left, and 1 right, and any fraction
        (not necessarily even within these bounds,
        if there are margins) can define a position        
    """
    self.row = row
    self.col = col
    self.axis = axis
    bg_color = self.bgcolor
    fg_color = self.fgcolor
    edge_color = 'black'
    fill = self.text
    if self.text == '':
      fill = 'N'
      fg_color = bg_color
    bbox_props = dict(boxstyle="square,pad=0.3", fc=bg_color, ec=edge_color, lw=0.3)
    # box around the text
    self.instance = axis.text(col,
                              row,
                              fill,
                              ha="left",
                              va="bottom",
                              #rotation=180,
                              fontsize=self.fontsize,
                              color=fg_color,
                              #backgroundcolor = "green",
                              family='monospace',
                              #fontname = 'Helvetica',
                              #linespacing = 1.,
                              weight = 'normal', #  ['normal' | 'bold' | 'heavy' | 'light' | 'ultrabold' | 'ultralight']
                              bbox=bbox_props
                              )

def PopulateAxesInLabelCanvas(canvas):
  """
      Now that GrLabel class is defined this function
      uses the label and baselines values assigned 
      to a LabelCanvas (StartPlot and AddAxes assumed
      to have been run beforehand) and physically
      plots them as labels.
  """
  if len(canvas.labels) == canvas.size[0]:
    counter = 0
    for baseline in canvas.baselines:
      elem = GrLabel('black','white', baseline[0]+' '+baseline[1], canvas.fontsize)
      elem.put(canvas.ax, canvas.bsltoprow, canvas.bsllftcol+canvas.bslcolstep*counter)
      elem.put(canvas.ax, canvas.bslbotrow, canvas.bsllftcol+canvas.bslcolstep*counter)
      counter += 1
  else: # less elements than the maximum size
    counter = 0
    for baseline in canvas.baselines:
      elem = GrLabel('black','white', baseline[0]+' '+baseline[1], canvas.fontsize)
      elem.put(canvas.ax, canvas.bsltoprow, canvas.bsllftcol+canvas.bslcolstep*counter)
      elem.put(canvas.ax, canvas.scntoprow + canvas.scnrowstep*len(canvas.labels), canvas.bsllftcol+canvas.bslcolstep*counter)
      counter += 1
  if len(canvas.baselines) == canvas.size[1]:
    counter = 0
    for label in canvas.labels:
      elem = GrLabel('black','white', label, canvas.fontsize)
      elem.put(canvas.ax, canvas.scntoprow + canvas.scnrowstep*counter, canvas.scnlftcol)
      elem.put(canvas.ax, canvas.scntoprow + canvas.scnrowstep*counter, canvas.scnrgtcol)
      counter += 1
  else: # less elements than the maximum size
    counter = 0
    for label in canvas.labels:
      elem = GrLabel('black','white', label, canvas.fontsize)
      elem.put(canvas.ax, canvas.scntoprow + canvas.scnrowstep*counter, canvas.scnlftcol)
      elem.put(canvas.ax, canvas.scntoprow + canvas.scnrowstep*counter, canvas.bsllftcol+canvas.bslcolstep*len(canvas.baselines))
      counter += 1
    
    
def AddCellToCanvas(canvas, cell, row, col): 
  """
      Unlike GrLabel.put (which operates in canvas coordinates 0-1)
      here row and col are in "countable" units.
      Note that they can actually be fractional, only fit
      within min - max canvas bounds.
  """
  if row > canvas.size[0]-1 or row < 0:
    raise Exception('row value can only be within 0 and', canvas.size[0]-1)
  if col > canvas.size[1]*2-1 or col < 0: # factor 2, because baseline cells have double pol positions
    raise Exception('col value can only be within 0 and', canvas.size[1]*2-1)
  cell.put(canvas.ax, canvas.cellstartrow+canvas.cellrowstep*row, canvas.cellstartcol+canvas.cellcolstep*col)
