##############################################################################
# A version of this file should be placed in the directory from which the
# Correlator and other tools are run.
# 
# The following should ONLY appear in the copy of observation.py in the root
# directory of pydifx
##############################################################################
print 'Warning USING THE DEFAULT observation.py FILE!'
##############################################################################

##############################################################################
# EVERY copy of observation should contain this line.
# Most of these parameters will be the same for all correlations however they
# can be overridden by overwriting them below
##############################################################################
from correlator_defaults import *


logname = 'log'
iat_path = eop_path

#calc file parameters
