# Various parameters which are default for *all* observation

# NONE of these variables are inherited directly by any of the scripts
# They are all inherited via observation.py
# Therefore if you need to make many experiment-specific changes, they
# they can be made in the experiment-specific observation.py
from logging import DEBUG

#######################################################################
import os
#executables
#######################################################################
# mpirun
# mpi = '/irasoft/difx/mpich/mpich-1.2.7p1/util/mpirun'
difxroot= os.environ['DIFXROOT']
mpi = os.environ['MPICXX']

#mpifxcorr
#mpifxcorr = '/irasoft/difx/tools/bin/mpifxcorr'
#mpifxcorr = '/irasoft/difx/mpifxcorr2hz/src/mpifxcorr'
#mpifxcorr = '/irasoft/difx/mpifxcorr2hz_hacked/src/mpifxcorr'
mpifxcorr = '/'.join((difxroot,'bin','mpifxcorr'))
# difx2fits
difx2fits = '/'.join((difxroot, '/bin/difx2fits'))
#remote shell used to connect to machines (used by killdifx)
rsh = 'ssh'
# checkCalcServer host
checkCalc = 'checkCalcServer localhost'
# startCalcServer
startCalc = 'startCalcServer'

#######################################################################
# Log file locations
#######################################################################
logpath = '/'.join((difxroot, 'log'))
logname = 'log'
logfile_level = DEBUG

#######################################################################
# calcif
#######################################################################
calcif_options = '-v'
calcif_timeout = 240

#######################################################################
# difx2fits
#######################################################################
difx2fits_options = '-v -v -s 1'
difx2fits_delete = False

#######################################################################
# eop.py
#######################################################################
eop_path = '/'.join((difxroot, "share"))
iat_path = eop_path
# can get TAI-UTC from here but they seem to be out of date
# http://gemini.gsfc.nasa.gov/500/oper/solve_apriori_files/ut1ls.dat
iat_url = "http://maia.usno.navy.mil/ser7/tai-utc.dat"
eop_url = "http://gemini.gsfc.nasa.gov/solve_save/usno_finals.erp"

eop_extra = 2
eop_download = False
eop_force = False

#######################################################################
# killdifx.py
#######################################################################
killdifx_options = ''

#######################################################################
# Cluster path (used by machinegen)
#######################################################################
cluster_path = "/irasoft/difx/tools/share/grid.cluster"

#######################################################################
# calc file defaults
#######################################################################
job_id = 1
spectral_average = 1
taper_function = 'UNIFORM'
offset = 0
increment = 1.0

tail = 30
download = False
force = False

#######################################################################
# log2clock / log2input defaults
#######################################################################
starttime = None

#######################################################################
# mpifxcorr.py defaults
#######################################################################
mpifxcorr_timeout = 86400

#######################################################################
# spawn.py defaults
#######################################################################
spawn_timeout = 30

#######################################################################
# vex2flag defaults
#######################################################################
flag_shrink = 0
flag_printuv = False
flag_flagfilename = 'flag'
