#!/usr/bin/python
# Simple wrapper for ls and chk_vlbi.py to create and check the data files for
# the correlator
# Cormac Reynolds: 2010 June 
import os, subprocess, time, re, tempfile, optparse, time, shutil
import espressolib

#global threads_per_machine
#threads_per_machine = 6

def sortbyfilename(x):
    # ignore path, just sort on filename
    sortstring = x.split('/')[-1]
    return sortstring

def makefilelists(telescope, data_area, machine, dir_patterns, globpatterns, expname):
    #outfile = telescope + '.filelist'
    filepattern = str()
    for pattern in dir_patterns:
        for globpattern in globpatterns:
            filepattern += pattern + globpattern + ' '

    #TEMPFILE = tempfile.NamedTemporaryFile()
    tempfilename = expname + '_' + telescope+'tempfilenamexxx.txt'
    TEMPFILE = open(tempfilename,'w')
    command = "ssh " + machine + " 'ls " + filepattern + "'"
    print command
    filelist, error1 = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    #filelist = pexpect.run(command)
    #filelist = filelist.split('\r\n')
    filelist = filelist.split('\n')
    filelist.pop()
    filelist = sorted(filelist, key=sortbyfilename)
    for file in filelist:
        print>>TEMPFILE, file

    TEMPFILE.flush()
    TEMPFILE.close()

    TEMPFILE = open(tempfilename,'r')

    outfilename = expname + '_' + telescope + '.filelist'
    OUTFILE = open(outfilename, 'w')
    chk_vlbi = espressolib.which('chk_vlbi.py')
    #chk_vlbi = '/nfs/apps/corr/DiFX-trunk/applications/espresso/trunk/chk_vlbi.py'
    if not chk_vlbi:
        ERRORFILE = espressolib.openlock('file_errors.txt')
        print>>ERRORFILE, 'chk_vlbi.py not found in $PATH'
        ERRORFILE.close()
        raise Exception('chk_vlbi.py not found in $PATH')
    command = "ssh " + machine + " '" + chk_vlbi + " " + os.getcwd() + os.sep + TEMPFILE.name + "'"
    #filelist = pexpect.run(command)
    #print filelist
    filelist, error2 = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    #filelist = filelist.split('\r\n')
    filelist = filelist.split('\n')
    filelist.pop()
    nbad = 0
    for file in filelist:
        print>>OUTFILE, file
        if re.match('#', file):
            nbad += 1

    if error1 or error2:
        ERRORFILE = espressolib.openlock('file_errors.txt')
        print>>ERRORFILE, error1, error2
        ERRORFILE.close()

    TEMPFILE.close()
    OUTFILE.close()
    os.remove(TEMPFILE.name)
    return len(filelist), nbad

def write_machines(expname, machines):
    MACHINESFILE = open(expname + ".machines", 'w')
    for machine in machines:
        print>>MACHINESFILE, machine

    MACHINESFILE.close()

def check_machines(machines):
    # check that if a machine appears more than once, all appearances are
    # consecutive in the list
    machine_error = str()
    while len(machines) > 1:
        machine = machines.pop(0)
        while machines and machines[0] == machine:
            machines.pop()
        if machine in machines:
            machine_error += ' ' + machine 


    if machine_error and options.no_rmaps_seq:
       print '\n' * 2, '!' * 20
       print "Warning:", machine_error, "appear(s) multiple times in the machines.list file, but not in consecutive order. This machine file will not work unless you have openmpi v1.4 or greater!"


def write_threads(expname, hosts, computemachines):

    THREADFILE = open(expname + ".threads", 'w')

    print>>THREADFILE, 'NUMBER OF CORES:    ', len(computemachines)

    #for i in range(len(computemachines)):
    #    print>>THREADFILE, threads_per_machine

    for machine in computemachines:
        print>>THREADFILE, hosts[machine][0]

    THREADFILE.close()

def write_run(expname, np, options):

    # prototype for the runfile which we will fill later
    difx_runfile = os.environ.get('DIFX_RUNFILE')
    runfilename = "run"

    if difx_runfile:
        shutil.copy(difx_runfile, runfilename)
    else:
        # in case no prototype run file, this basic run file will work for
        # many sites
        RUNFILE = open(runfilename, 'w')
        print>>RUNFILE, "mpirun -np ", np, options, "-machinefile {JOBNAME}.machines $DIFXROOT/bin/mpifxcorr {JOBNAME}.input"
        RUNFILE.close()
    try:
        os.chmod(runfilename, 0775)
    except:
        print "could not change permissions of", runfilename


def kill_children(pids):
    for pid in pids:
        try:
            os.kill(pid,9)
        except:
            pass
    for pid in pids:
        os.waitpid(pid,0)


# the program starts here

# parse the options
usage = '''%prog [options] <datafiles.dat>
will produce file lists for the data areas described in <datafiles.dat>
It also creates a "run", .machines, and .threads files based on the
available nodes in the file given by $CORR_HOST'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
#parser.add_option( "--njobs", "-n",
#        type='int', dest="njobs", default='1',
#        help='Number of jobs you want to create threads files for' )
parser.add_option( "--nofilelist", "-F",
        action="store_true", dest="nofilelist", default=False,
        help='No new filelists. Just create machines, thread and run file' )
parser.add_option( "--nocheck", "-C",
        action="store_true", dest="nocheck", default=False,
        help="Don't check files for valid headers" )
parser.add_option( "--station", "-s",
        type='str', dest="station", default=None,
        help="Comma separated list of stations to include (default=all)" )
parser.add_option( "--computehead", "-H",
        action='store_true', dest="allcompute", default=False,
        help="Allow head and data nodes to be used as compute nodes" )
parser.add_option( "--no_rmaps_seq", "-M",
        action='store_true', dest="no_rmaps_seq", default=False,
        help="Do not pass the '--mca rmaps seq' instruction to mpirun (requires openmpi v1.4 or greater)" )

(options, args) = parser.parse_args()

#njobs = options.njobs

if len(args) != 1:
    parser.print_help()
    parser.error("no data areas file given")



telescopedirs = open(args[0]).readlines()
telescopedirs = [ telescopedirs[i].strip('\n') for i in range(len(telescopedirs)) ]
telescopedirs = [ telescopedirs[i].strip() for i in range(len(telescopedirs)) ]

# the experiment name is always the first line
firstline = telescopedirs.pop(0)

globpatterns = firstline.split()
expname = globpatterns.pop(0)
if not globpatterns:
    globpatterns = ['']

# use only selected stations (if given)
if options.station:
    station_list = options.station.upper().split(',')
    new_telescopedirs = []
    for telescope in telescopedirs :
        telescope_name = telescope.split()[0].upper()
        if telescope_name in station_list:
            new_telescopedirs.append(telescope)
    telescopedirs = new_telescopedirs

#print telescopedirs


try:
    os.remove('file_errors.txt')
except:
    pass

pids = []
datamachines = []

if not options.nofilelist:
    print "Wait for process to finish. Do not press ^C."
# first create the filelists
for line in sorted(telescopedirs):
    try:
        #line = line.strip('\n')
        #line = line.strip()
        line = re.compile(r'#.*').sub(r'', line)
        if not re.match(r'\w', line):
            continue

        telescope, data_area = re.split(r'\s*=\s*', line)
        machine, dir_patterns = re.split('\s*:\s*', data_area)
        dir_patterns = dir_patterns.split()
        datamachines.append(machine)

        # might as well do this in parallel in the background to speed things
        # up, even if the control is a bit sloppy
        if not options.nofilelist:
            pid = os.fork()
            pids.append(pid)
            if pid == 0:
                nfiles, nbad = makefilelists(telescope, data_area, machine, dir_patterns, globpatterns, expname)
                print "got", nfiles, "files for", telescope, 
                if nbad: 
                    print '(', nbad, 'corrupt )',
                print 
                os._exit(0)
    except:
        # clean up jobs if something goes awry
        kill_children(pids)
        print "\n\nERROR: killing ssh processes due to error making file lists"
        print "ERROR: Check formatting of", args[0] + ":" 
        print line
        raise Exception("could not make file list")
        

# and wait for the filelist creation processes to finish
#time.sleep(1)
for pid in pids:
    #print 'sysWaiting for the child process ( pid = ', pid, ') to finish...\n'
    os.waitpid(pid, 0)


# now make a default machine, threads and run file
try:
    hosts = espressolib.get_corrhosts(os.environ.get('DIFX_MACHINES'))
except:
    raise Exception('You must set $DIFX_MACHINES. No machines file created')

headmachine = os.uname()[1].lower()

# nodes that are also used as head or datastream nodes should get fewer
# threads, if they are requested to be used as compute nodes at all.
for machine in datamachines + [headmachine]:
    if hosts[machine][0] > 1:
        hosts[machine][0] -= 1

if len(hosts) == 0:
    raise Exception('Did not find any hosts in your $CORR_HOSTS file')
elif len(hosts) <= len(set(datamachines + [headmachine])):
   # all nodes already occupied by master and datastreams
   options.allcompute = True

#print 'head=', headmachine 

computemachines = []
for host in sorted(hosts.keys()):
    # usually avoid using head node and datastream nodes as compute nodes
    # (overridden with -H switch).
    if options.allcompute or (not host in [headmachine] + datamachines):
        # also only use nodes with more than 0 threads available.
        if hosts[host][0]:
            computemachines.append(host)

if not computemachines:
    raise Exception('You have no compute nodes left after the master node and datastream nodes have been allocated! Check your hosts in $CORR_HOSTS.')

machines = [headmachine] + datamachines + computemachines
check_machines(machines[:])
write_machines(expname, machines)
write_threads(expname, hosts, computemachines)
mpirun_options = ''
if not options.no_rmaps_seq:
    mpirun_options = '--mca rmaps seq'
write_run(expname, len(computemachines + datamachines) +1, mpirun_options)


if not options.nofilelist:
    print "All done!"
