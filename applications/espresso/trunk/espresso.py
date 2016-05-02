#!/usr/bin/env python
# =======================================================================
# Copyright (C) 2016 Cormac Reynolds
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
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# =======================================================================

# take care of a few logistics before launching the correlator
# Cormac Reynolds: June 2010

import subprocess
import optparse
import re
import shutil
import os
import sys
import time
import fileinput
import pprint
import espressolib
import getpass
import psutil


def run_vex2difx(v2dfilename, vex2difx_options):
    # run vex2difx, and wait for completion
    command = " ".join(["vex2difx", vex2difx_options, v2dfilename])
    print command
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def change_path(inputfilename, changeparm, oldpath, newpath):
    # modify paths in the input file
    for line in fileinput.FileInput(inputfilename, inplace=1, backup=".org"):
        if re.match(changeparm, line):
            line = line.replace(oldpath, newpath)
        print line,

    fileinput.close()


def run_calcif2(jobname, calcfilename):
    # tidy up old calcif2 files and run calcif2 again
    calcoutputfiles = [".uvw", ".rate", ".im", ".delay"]
    for file in calcoutputfiles:
        # clear up old calc files so we are sure it completed
        file = jobname + file
        if os.path.exists(file):
            os.remove(file)
    command = " ".join(["calcif2", calcfilename])
    print command
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def backup_oldrun(jobname, outdir, backupdir):
    # back up previous correlator job to subdirectory
    if os.path.exists(outdir):
        print "\nwill move old jobs to", backupdir, "\n"
        dirlist = os.listdir(outdir)
        for file in dirlist:
            if jobname in file and not re.match("\.", file):
                os.renames(outdir + file, backupdir + file)


def copy_models(jobname, indir, outdir):
    # copy all files with the jobname in their name to the output directory.
    # This will include all the files needed to run difx2fits, and other
    # interesting logs.
    dirlist = os.listdir(indir)
    for file in dirlist:
        if jobname + "." in file and not re.match("\.", file):
            if os.path.isfile(file):
                shutil.copy2(indir + file, outdir)


def copy_jobcontrol(expname, jobname, indir, outdir, extension):
    # copy files to the output directory for archiving purposes. Rename to
    # match the jobname.
    infile = indir + expname + extension
    outfile = outdir + jobname + extension
    if os.path.isfile(infile):
        shutil.copy2(infile, outfile)
    else:
        sys.stderr.write(infile + " not found!")


def make_new_runfiles(
        jobname, expname, jobtime, difx_message_port,
        ntasks_per_node=1):
    # make copies of the prototype run and .thread files
    runfile = "run_" + jobname
    shutil.copy("run", runfile)
    for line in fileinput.FileInput(runfile, inplace=1):
        # update placeholders in prototype
        line = re.sub(r"{JOBNAME}", jobname, line)
        line = re.sub(r"{TIME}", jobtime, line)
        line = re.sub(r"{DIFX_MESSAGE_PORT}", difx_message_port, line)
        line = re.sub(r"{NTASKS-PER-NODE}", ntasks_per_node, line)
        print line,
    fileinput.close()
    os.chmod(runfile, 0775)

    threadfilename = jobname + ".threads"
    shutil.copy(expname + ".threads", threadfilename)

    machinesfilename = jobname + ".machines"
    shutil.copy(expname + ".machines", machinesfilename)


def parse_joblistfile(joblistfilename, speedup=1.0):
    # get the full list of jobs from the joblist file
    # Return a dictionary. Keys are the job names. Values are the stations in
    # the job
    joblistfile = open(joblistfilename).readlines()

    joblistfile.pop(0)
    joblist = dict()
    for line in joblistfile:
        jobname, jobstart, jobend = line.split()[0:3]
        joblist[jobname] = dict()
        stations = re.search(r"#\s+(.*)", line).group(1)
        joblist[jobname]["stations"] = stations

        joblen = (float(jobend) - float(jobstart))/speedup
        days, hours, minutes, seconds = espressolib.daysToDhms(joblen)
        joblist[jobname]["joblen"] = (
                "{0:02d}:{1:02d}:{2:02d}".format(hours, minutes, seconds))

    return joblist


def parse_v2dfile(v2dfilename):
    """extract file names from the v2d file"""
    v2dfile = open(v2dfilename).readlines()
    vexfilename = str()
    binconfigfilename = None
    for line in v2dfile:
        # remove comments
        line = re.sub(r"#.*", "", line)
        vexmatch = re.search(r"vex\s*=\s*(\S*)", line)
        if vexmatch:
            vexfilename = vexmatch.group(1)
        binconfigmatch = re.search(r"binConfig\s*=\s*(\S*)", line)
        if binconfigmatch:
            binconfigfilename = binconfigmatch.group(1)

    return vexfilename, binconfigfilename


def parse_binconfig(binconfigfilename):
    """extract file names from the binconfig file"""
    binconfigfile = open(binconfigfilename).readlines()
    polycofilename = str()
    for line in binconfigfile:
        # remove comments
        line = re.sub(r"@.*", "", line)
        polycomatch = re.search(r"POLYCO FILE \d+\s*:\s*(\S*)", line)
        if polycomatch:
            polycofilename = polycomatch.group(1)

    return polycofilename


def run_lbafilecheck(
        datafilename, stations, computehead, no_rmaps_seq, interactive):
    # run lbafilecheck creating machines and .threads files for this job
    stations = stations.strip()
    stations = re.sub(r"\s+", ",", stations)
    stations = "'" + stations + "'"
    options = ""
    if computehead:
        options += " -H "
    if no_rmaps_seq:
        options += " -M "
    if not interactive:
        options += " -n "
    command = " ".join(
            ["lbafilecheck.py -F", options, "-s", stations, datafilename])
    print command
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def fill_operator_log(logfile):
    # Fire up an editor for operator comments
    editor = os.environ.get("EDITOR", "vim")
    command = " ".join([editor, logfile])
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def plot_speedup(logfiles, outdir, expname):
    # plot the speedup factor from the difx log file
    speedup_plot = outdir + expname + "_speedup.pdf"
    speedup_files = " ".join(logfiles)
    plot_opt = " "
    if len(logfiles) < 10:
        plot_opt = " -l "
    command = " ".join(
            ["cd", outdir, "; plot_logtime.py", plot_opt, "-o", speedup_plot,
                speedup_files])
    speedup = subprocess.Popen(
            command, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            shell=True)
    return speedup


def run_interactive(corrjoblist, outdir):
    """Run jobs in an interactive enviroment"""
    difxwatch = None
    for jobname in sorted(corrjoblist.keys()):
        if options.difxwatch:
            print "starting difxwatch in the background"
            difxwatch = subprocess.Popen(["difxwatch", "-i 600"])
        else:
            try:
                command = ["pkill", "-2", "-f", "difxwatch"]
                code = subprocess.call(
                        command, stdout=sys.stdout, stderr=sys.stderr)
            except:
                pass

        # start the correlator log
        print "starting errormon2 in the background"
        errormon2 = subprocess.Popen("errormon2")
        #errormon2 = subprocess.Popen('errormon2 2>&1| grep ' + jobname, shell=True)

        try:
            runfile = jobname
            runfile = "./run_" + runfile
            print "starting the correlator running", runfile
            subprocess.check_call(runfile, shell=True, stdout=sys.stdout)
        except subprocess.CalledProcessError:
            # would probably prefer just a 'finally' here so everything falls
            # over in case of failure, but if difxwatch kills the jobs we want
            # to continue
            pass
        except:
            # a ^C is likely to be the only thing that gets us here.
            #raise
            pass
        finally:
            # we're finished with the log...
            os.kill(errormon2.pid, 9)
            time.sleep(1)
            logfilename = outdir + jobname + ".difxlog"
            logfiles.append(jobname + ".difxlog")
            logfile = open(logfilename, "w")
            print "\nfiltering the log file and copying to", logfile.name
            #shutil.copy2('log', logfile)
            for line in open("log"):
                if jobname in line[58:73]:
                    print>>logfile, line,
            logfile.close()


def wait_for_file(filename):
    while not os.path.exists(filename):
        print "Waiting for", filename
        time.sleep(1)

    print filename, "found!"


def run_batch(corrjoblist, outdir):
    """Run jobs in a batch enviroment"""

    # start the correlator log
    errormon_log = "./log"
    print "starting errormon2 in the background"
    try:
        os.remove(errormon_log)
        print "removed old", errormon_log
    except:
        pass
    errormon2 = subprocess.Popen("errormon2", env=espresso_env)
    #errormon2 = subprocess.Popen('errormon2 2>&1| grep ' + jobname, shell=True)

    # make the log file have a unique name by *moving* the 'log' created by
    # errormon2 to a file name derived from this passname.
    # This log file will receive all log messages from jobs spawned by this
    # espresso session. Will divide into separate jobs at end.
    pass_logfilename = passname + ".difxlog"
    try:
        wait_for_file(errormon_log)
        print "renaming log to", pass_logfilename
        shutil.move("./log", pass_logfilename)
    except:
        print errormon_log, "not found! No difxlog will be produced"

    # submit all the jobs to the batch scheduler
    for jobname in sorted(corrjoblist.keys()):
        try:
            runfile = jobname
            runfile = "sbatch ./run_" + runfile
            print "starting the correlator with:", runfile
            subprocess.check_call(
                    runfile, shell=True, stdout=sys.stdout, env=espresso_env)
        except:
            pass

    # Just wait until the jobs have completed. Operator hits ^C to progress.
    # Enter will provide queue report.
    queue_command = (
            " ".join(["squeue", "-n "]) + ",".join(sorted(corrjoblist.keys())))
    while True:
        try:
            #command = ['squeue', '-u', '$USER']
            print queue_command
            subprocess.check_call(queue_command, stdout=sys.stdout, shell=True)

            raw_input(
                    "Jobs submitted - hit ^C when all jobs have completed. Hit return to see list of running jobs.")
        except KeyboardInterrupt:
            for jobname in sorted(corrjoblist.keys()):
                command = " ".join(["scancel", "-n", jobname])
                print command
                subprocess.check_call(command, stdout=sys.stdout, shell=True)
            break
        except:
            print queue_command + "failed!"
            #pass
            #raise Exception(command + 'failed!')
            #raise

    # tidy up log files for each job
    for jobname in sorted(corrjoblist.keys()):
        # we're finished with the logs..
        os.kill(errormon2.pid, 9)
        #time.sleep(1)
        logfilename = outdir + jobname + ".difxlog"
        logfiles.append(jobname + ".difxlog")
        logfile = open(logfilename, "w")
        print "\nfiltering the log file and copying to", logfile.name
        #shutil.copy2("log", logfile)
        for line in open(pass_logfilename):
            if jobname in line[58:73]:
                print>>logfile, line,
        logfile.close()


def set_difx_message_port(start_port=50201):
    """set unique difx message port so unicast environment can have parallel
    jobs."""
    difx_message_port = start_port
    # get active connections
    connections = psutil.net_connections()
    ports_used = []
    for connection in connections:
        ports_used.append(connection[3][1])

    while True:
        if difx_message_port in ports_used:
            print "DIFX_MESSAGE_PORT", difx_message_port, "in use"
            difx_message_port += 1
        else:
            print "DIFX_MESSAGE_PORT=", difx_message_port
            break

    return difx_message_port


# Main program start.
# parse the options
usage = """%prog <jobname>
    will:
    run vex2difx
    correct the output file name
    run calcif2
    move previous correlator job to backup directory
    copy model information to correlator data area
    start errormon2
    start the correlator!
    quit errormon2 and copy the log file to the output directory
    accept an operator comment for storing with the output
    send an email to the operator (if authorised)

<jobname> may be a space separated list.
<jobname> may also include a python regular expression after the '_' in the job
name to match multiple jobs. (The job name up to the '_' must be given
explicitly)"""


parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--clock", "-c",
        dest="clockjob", action="store_true", default=False,
        help="Store output in a clock subdirectory. Also passes -f to vex2difx")
parser.add_option(
        "--test", "-t", dest="testjob", action="store_true", default=False,
        help="Store output in a test subdirectory. Also passes -f to vex2difx")
parser.add_option(
        "--nocalc", "-C",
        dest="nocalc", action="store_true", default=False,
        help="Do not re-run calc")
parser.add_option(
        "--novex", "-n",
        dest="novex", action="store_true", default=False,
        help="Do not re-run vex2difx")
parser.add_option(
        "--nopause", "-p",
        dest="nopause", action="store_true", default=False,
        help="Do not pause after running calc - proceed straight to correlation")
parser.add_option(
        "--alljobs", "-a",
        type="str", dest="expt_all", default=None,
        help="Correlate all jobs produced by vex2difx for the experiment specified (no other arguments required)")
parser.add_option(
        "--computehead", "-H",
        dest="computehead", action="store_false", default=True,
        help="Don't Use head and datastream nodes as compute nodes")
parser.add_option(
        "--no_rmaps_seq", "-M",
        dest="no_rmaps_seq", action="store_true", default=False,
        help="Don't Pass the '--mca rmaps seq' instruction to mpirun")
parser.add_option(
        "--no_email", "-E",
        dest="noemail", action="store_true", default=False,
        help="Don't prompt for notification email")
parser.add_option(
        "--difxwatch", "-d",
        dest="difxwatch", action="store_true", default=False,
        help="Run difxwatch")
parser.add_option(
        "--interactive", "-i",
        dest="interactive", action="store_true", default=False,
        help="Run interactively, else assume slurm batch jobs")
parser.add_option(
        "--jobtime", "-j",
        type="str", dest="jobtime", default=None,
        help="""Max. job time for batch jobs, default is runtime * speedup
        format = hh:mm:ss""")
parser.add_option(
        "--speedup", "-s",
        type="float", dest="predicted_speedup", default=1.0,
        help="""Predicted speedup factor to determine job run time""")

(options, args) = parser.parse_args()

if options.testjob and options.clockjob:
    raise Exception("Don't use both -t and -c together!")

if len(args) < 1 and not options.expt_all:
    parser.print_help()
    parser.error("Give job name(s)")

# set max jobtime for batch jobs
#if options.jobtime:
#    jobtime = options.jobtime
#elif options.clockjob:
#    jobtime = '00:10:00'
#elif options.testjob:
#    jobtime = '00:10:00'
#else:
#    jobtime = '02:00:00'

# Determine the name of the correlator pass from the first jobname or the -a
# switch
if options.expt_all:
    passname = options.expt_all
else:
    passname = re.match(r"(.*)_", args[0]).group(1)

# a '-' is used to distinguish different correlator passes. If only one pass,
# then the expname and passname are the same
passid = str()
expname = passname
if "-" in passname:
    expname, passid = expname.split("-")

operator_log = passname + "_comment.txt"
try:
    raw_input(
            "\nHit return, then enter an operator comment, minimally: PROD/CLOCK/TEST/FAIL")
    fill_operator_log(operator_log)
except:
    print "Operator comment not saved!"

# get email and password so we can notify. does not make sense for batch jobs.
get_email = False
if options.interactive:
    if not options.noemail and not options.clockjob and not options.testjob:
        get_email = True
user = str()
emailserver = ()
while get_email:
    try:
        user = raw_input(
                "Enter *gmail* address and password for notifications (or return to ignore):\n")
        if user:
            emailserver = espressolib.Email(user, getpass.getpass())
            emailserver.connect()
            emailserver.disconnect()
            break
        else:
            get_email = False
    except:
        print "Connection failed - try again"


# preliminaries: clear old input files out of the way
if not options.novex:
    if options.expt_all:
        for filename in os.listdir(os.getcwd()):
            if re.match(options.expt_all + r"_\d+\.input$", filename):
                os.rename(filename, filename + ".bak")

    for jobname in args:
        inputfilename = jobname + ".input"
        if os.path.exists(inputfilename):
            # clear old input file out of the way first
            # That way later parts will crash if vex2difx fails
            os.rename(inputfilename, inputfilename + ".bak")


vex2difx_options = ""
if options.clockjob or options.testjob:
    vex2difx_options = " -f "


# run vex2difx.
v2dfilename = passname + ".v2d"
vexfilename, binconfigfilename = parse_v2dfile(v2dfilename)
if not vexfilename:
    raise Exception("Could not find VEX file in " + v2dfilename)

if binconfigfilename:
    polycofilename = parse_binconfig(binconfigfilename)
    if not polycofilename:
        raise Exception("Could not find polycofile in " + binconfigfilename)

if not options.novex:
    run_vex2difx(v2dfilename, vex2difx_options)


joblistfilename = passname + ".joblist"
(fulljoblist) = parse_joblistfile(joblistfilename, options.predicted_speedup)

# figure out the list of jobs to run this time
corrjoblist = dict()
# if the -a switch was used, then do all jobs
if options.expt_all:
    corrjoblist = fulljoblist
else:
    # if no -a, then match any patterns given on the command line
    for jobpattern in args:
        for jobname in fulljoblist.keys():
            if re.search(jobpattern + "$", jobname):
                corrjoblist[jobname] = fulljoblist[jobname]

print "job list to correlate = ", pprint.pformat(corrjoblist), "\n"


# get the paths of our input and output directories
indir = os.getcwd() + os.sep
try:
    outdirbase = os.environ.get("CORR_DATA") + os.sep
except:
    raise Exception("You must set $CORR_DATA to an output data directory!")

outdir = outdirbase + expname + os.sep
if options.clockjob:
    outdir += "clocks/"
if options.testjob:
    outdir += "test/"

# get a unique name for a backup directory based on the current time.
backupdir = outdir + time.strftime("%Y-%m-%d-%H-%M-%S") + os.sep

if not os.path.exists(outdir):
    print "making the output directory", outdir
    os.makedirs(outdir)


# do the prep work for each job.
for jobname in sorted(corrjoblist.keys()):
    # figure out filenames, directories, etc. using normal difx/cuppa
    # conventions

    inputfilename = jobname + ".input"
    calcfilename = jobname + ".calc"

    # fix the output filename to point at the cuppa data disk
    if not options.novex:
        print "\nrenaming the 'OUTPUT FILENAME' in", inputfilename, "from", \
                indir, "to", outdir, "\n"
        change_path(inputfilename, 'OUTPUT FILENAME:', indir, outdir)

    # run calcif2
    if not options.nocalc:
        run_calcif2(jobname, calcfilename)

    # copy any old jobs to a backupdir
    backup_oldrun(jobname, outdir, backupdir)

    # copy the model files to the output directory
    print "copying the model files", jobname + ".*", "to", outdir, "\n"
    copy_models(jobname, indir, outdir)

    # change the path names in the copied .input and .calc to relative paths
    print "changing absolute paths to relative paths in the copied .input and .calc files\n"
    copy_inputfilename = outdir + os.sep + inputfilename
    copy_calcfilename = outdir + os.sep + calcfilename
    change_path(copy_inputfilename, "CALC FILENAME:", indir, "./")
    change_path(copy_inputfilename, "CORE CONF FILENAME:", indir, "./")
    change_path(copy_inputfilename, "OUTPUT FILENAME:", outdir, "./")
    change_path(copy_inputfilename, "PULSAR CONFIG FILE:", indir, "./")
    change_path(copy_calcfilename, "IM FILENAME:", indir, "./")
    change_path(copy_calcfilename, "FLAG FILENAME:", indir, "./")

    # copy job control files to output directory, and rename
    print "copying the job control files", passname + ".[joblist|v2d]", "to", \
            outdir, "\n"
    copy_jobcontrol(passname, jobname, indir, outdir, ".joblist")
    copy_jobcontrol(passname, jobname, indir, outdir, ".v2d")

    outputvex = outdir + jobname + ".vex"
    print "copying the vex file", vexfilename, "to", outputvex, "\n"
    shutil.copy2(vexfilename, outputvex)

# and copy the .vex and .v2d unaltered for ease of reference in the output dir
print "copying the vex and v2d files", vexfilename, v2dfilename, "to", \
        outdir, "\n"
shutil.copy2(vexfilename, outdir)
shutil.copy2(v2dfilename, outdir)
# if pulsar binning, then get the binconfig and polyco too
if binconfigfilename:
    print "copying the binconfig and polyco files", binconfigfilename, \
        polycofilename, "to", outdir, "\n"
    shutil.copy2(binconfigfilename, outdir)
    shutil.copy2(polycofilename, outdir)

if not options.nopause:
    raw_input("Press return to initiate the correlator job or ^C to quit ")


# set the $DIFX_MESSAGE_PORT as late as possible in the processing to avoid
# clashes with other espresso invocations
difx_message_port = set_difx_message_port(50201)
espresso_env = os.environ.copy()
espresso_env["DIFX_MESSAGE_PORT"] = str(difx_message_port)
os.environ["DIFX_MESSAGE_PORT"] = str(difx_message_port)

# create the mpi files for each job
for jobname in sorted(corrjoblist.keys()):
    # run lbafilecheck to get the new machines and .threads files
    datafilename = expname + ".datafiles"
    run_lbafilecheck(
            datafilename, corrjoblist[jobname]["stations"],
            options.computehead, options.no_rmaps_seq, options.interactive)

    # duplicate the run and thread and machines files for the full number of
    # jobs
    print "\nduplicating the run file, machines file and .threads files for ", \
            jobname, "\n"
    if options.jobtime:
        jobtime = options.jobtime
    else:
        jobtime = corrjoblist[jobname]["joblen"]

    ntasks_per_node = 1
    if (not options.interactive) and options.computehead:
        ntasks_per_node = 2
    make_new_runfiles(
            jobname, expname, jobtime, str(difx_message_port),
            str(ntasks_per_node))


logfiles = []


# run each job
try:
    if options.interactive:
        run_interactive(corrjoblist, outdir)
    else:
        run_batch(corrjoblist, outdir)
finally:

    ## kill the difxwatch process (kill -INT so it cleans itself up)
    #if difxwatch:
    #    # if no difxwatch was started because one was already running then this
    #    # does nothing
    #    os.kill(difxwatch.pid, 2)

    # plot the speedup factor (this forks a process - must clean up later)
    try:
        speedup = plot_speedup(logfiles, outdir, passname)
    except:
        print "Could not plot speedup factor!"

    # let someone know we've finished
    if get_email:
        try:
            print "\nEmailing", emailserver.user
            message = str(sorted(corrjoblist.keys())) + " finished"
            emailserver.connect()
            emailserver.sendmail(message)
            emailserver.disconnect()
        except:
            print "No notification email sent"

    # and enter an operator comment
    raw_input(
            "\nHit return, then update the operator comment, minimally: PROD/CLOCK/TEST/FAIL")
    fill_operator_log(operator_log)
    for jobname in corrjoblist.keys():
        operator_joblog = outdir + jobname + ".comment.txt"
        shutil.copy2(operator_log, operator_joblog)

    # clean up the forked plot process
    print "\n\nWaiting for plotting process", speedup.pid, "to finish"
    plotmsg, ploterr = speedup.communicate()
    print plotmsg, ploterr
