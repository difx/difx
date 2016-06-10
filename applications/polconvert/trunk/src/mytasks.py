#
# User defined tasks setup.
# Generated from buildmytask.
#

if sys.path[1] != '/home/gbc/CASA/PolConvert':
  sys.path.insert(1, '/home/gbc/CASA/PolConvert')
from odict import odict
if not globals().has_key('mytasks') :
  mytasks = odict()

mytasks['polconvert'] = '\n\nVersion 1.2-r6\n\nConverts VLBI visibilities from mixed-polarization basis (i.e.,\nlinear-to-circular) into circular basis. Works with single VLBI stations \nas well as with phased arrays (i.e., phased ALMA).\n\n'

if not globals().has_key('task_location') :
  task_location = odict()

task_location['polconvert'] = '/home/gbc/CASA/PolConvert'
import inspect
myglobals = sys._getframe(len(inspect.stack())-1).f_globals
tasksum = myglobals['tasksum'] 
for key in mytasks.keys() :
  tasksum[key] = mytasks[key]

from polconvert_cli import  polconvert_cli as polconvert
