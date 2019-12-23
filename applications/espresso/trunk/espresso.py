#!/usr/bin/env python2
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


from __future__ import print_function, division
import subprocess
import optparse
import re
import shutil
import os
import sys
import time
import fileinput
import pprint
import getpass
import psutil
import espressolib


def run_vex2difx(v2dfilename, vex2difx_options):
    """run vex2difx, and wait for completion"""

    command = " ".join(["vex2difx", vex2difx_options, v2dfilename])
    print (command)
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def change_path(inputfilename, changeparm, oldpath, newpath):
    """modify paths in the input file"""

    for line in fileinput.FileInput(inputfilename, inplace=1, backup=".org"):
        if re.match(changeparm, line):
            line = line.replace(oldpath, newpath)
        print (line, end="")

    fileinput.close()


def run_calcif2(jobname, calcfilename):
    """tidy up old calcif2 files and run calcif2 again"""

    calcoutputfiles = [".uvw", ".rate", ".im", ".delay"]
    for filename in calcoutputfiles:
        # clear up old calc files so we are sure it completed
        filename = jobname + filename
        if os.path.exists(filename):
            os.remove(filename)
    command = " ".join(["calcif2", calcfilename])
    print (command)
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def backup_oldrun(jobname, outdir, backupdir):
    """back up previous correlator job to subdirectory"""

    if os.path.exists(outdir):
        print ("will move old jobs to", backupdir)
        dirlist = os.listdir(outdir)
        for filename in dirlist:
            if jobname in filename and not re.match(r"\.", filename):
                os.renames(outdir + filename, backupdir + filename)


def copy_models(jobname, indir, outdir):
    """copy all files with the jobname in their name to the output directory.

    This will include all the files needed to run difx2fits, and other
    interesting logs.
    """

    dirlist = os.listdir(indir)
    for filename in dirlist:
        if jobname + "." in filename and not re.match(r"\.", filename):
            if os.path.isfile(filename):
                shutil.copy2(indir + filename, outdir)


def copy_jobcontrol(expname, jobname, indir, outdir, extension):
    """copy files to the output directory for archiving purposes.

    Rename to match the jobname.
    """

    infile = indir + expname + extension
    outfile = outdir + jobname + extension
    if os.path.isfile(infile):
        shutil.copy2(infile, outfile)
    else:
        sys.stderr.write(infile + " not found!")


def fix_paths(inputfilename, calcfilename, indir, outdir):
    """Make paths in input files relative instead of absolute"""

    copy_inputfilename = outdir + os.sep + inputfilename
    copy_calcfilename = outdir + os.sep + calcfilename
    change_path(copy_inputfilename, "CALC FILENAME:", indir, "./")
    change_path(copy_inputfilename, "CORE CONF FILENAME:", indir, "./")
    change_path(copy_inputfilename, "OUTPUT FILENAME:", outdir, "./")
    change_path(copy_inputfilename, "PULSAR CONFIG FILE:", indir, "./")
    change_path(copy_calcfilename, "IM FILENAME:", indir, "./")
    change_path(copy_calcfilename, "FLAG FILENAME:", indir, "./")


def make_new_runfiles(
        jobname, expname, jobtime, difx_message_port,
        ntasks_per_node=1):
    """make copies of the prototype run and .thread files"""

    runfile = "run_" + jobname
    shutil.copy("run", runfile)
    for line in fileinput.FileInput(runfile, inplace=1):
        # update placeholders in prototype
        line = re.sub(r"{JOBNAME}", jobname, line)
        line = re.sub(r"{TIME}", jobtime, line)
        line = re.sub(r"{DIFX_MESSAGE_PORT}", difx_message_port, line)
        line = re.sub(r"{NTASKS-PER-NODE}", ntasks_per_node, line)
        print (line, end="")
    fileinput.close()
    os.chmod(runfile, 0775)

    threadfilename = jobname + ".threads"
    shutil.copy(expname + ".threads", threadfilename)

    machinesfilename = jobname + ".machines"
    shutil.copy(expname + ".machines", machinesfilename)


def predict_speedup(tops, joblen):
    """Placeholder for future idea - could be complicated"""

    speedup = (joblen*0.8e5)/tops
    return speedup


def parse_joblistfile(joblistfilename, set_speedup=None):
    """Get the full list of jobs from the joblist file

    Return a dictionary. Keys are the job names. For each jobname record a list
    of stations under key 'stations' and a list of job length under key
    'joblen'
    """

    joblistfile = open(joblistfilename).readlines()

    joblistfile.pop(0)
    joblist = dict()
    for line in joblistfile:
        jobvals = line.split()
        jobname, jobstart, jobend = jobvals[0:3]
        tops = jobvals[6]
        joblen = (float(jobend) - float(jobstart))
        if set_speedup is None:
            speedup = predict_speedup(tops, joblen)
        else:
            speedup = set_speedup
        joblist[jobname] = dict()
        stations = re.search(r"#\s+(.*)", line).group(1)
        joblist[jobname]["stations"] = stations

        job_time = joblen/speedup
        # add 10 minutes for startup
        job_time += 10./(24.*60.)
        days, hours, minutes, seconds = espressolib.daysToDhms(job_time)
        joblist[jobname]["jobtime"] = (
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
        datafilename, stations, computehead, no_rmaps_seq, interactive,
        ntasks_per_node):
    """run lbafilecheck creating machines and .threads files for this job"""

    stations = stations.strip()
    stations = re.sub(r"\s+", ",", stations)
    stations = "'" + stations + "'"
    #options = ""
    options = " ".join(["--ntasks_per_node", str(ntasks_per_node)])
    if computehead:
        options = " ".join([options, "-H"])
    if no_rmaps_seq:
        options = " ".join([options, "-M"])
    if not interactive:
        options = " ".join([options, "-n"])

    command = " ".join(
            ["lbafilecheck.py -F", options, "-s", stations, datafilename])
    print (command)
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def fill_operator_log(logfile):
    """Fire up an editor for operator comments"""

    editor = os.environ.get("EDITOR", "vim")
    command = " ".join([editor, logfile])
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


def plot_speedup(logfiles, outdir, expname):
    """plot the speedup factor from the difx log file"""

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
    good_jobs = []
    bad_jobs = []
    job_errors = []
    errors = []
    difxwatch = None
    for jobname in sorted(corrjoblist.keys()):
        if options.difxwatch:
            print ("starting difxwatch in the background")
            difxwatch = subprocess.Popen(["difxwatch", "-i 600"])
        else:
            try:
                command = ["pkill", "-2", "-f", "difxwatch"]
                code = subprocess.call(
                        command, stdout=sys.stdout, stderr=sys.stderr)
            except:
                pass

        # start the correlator log
        print ("starting errormon2 in the background")
        errormon2 = subprocess.Popen("errormon2")
        #errormon2 = subprocess.Popen('errormon2 2>&1| grep ' + jobname, shell=True)

        try:
            runfile = jobname
            runfile = "./run_" + runfile
            print ("starting the correlator running", runfile)
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
            job_ok, job_errors = write_difxlog("./log", outdir, jobname)
            if job_ok:
                good_jobs.append(jobname)
            else:
                bad_jobs.append(jobname)
            errors += job_errors

    return good_jobs, bad_jobs, errors


def filter_log(infile, jobname):
    """Filter specified jobname from log file"""

    filtered_file = []
    for line in open(infile):
        if re.search(r"\s"+jobname+r"\s", line):
            filtered_file.append(line)
    return filtered_file


def wait_for_file(filename):
    """Wait until specified file is available"""

    while not os.path.exists(filename):
        print ("Waiting for", filename)
        time.sleep(1)
    print (filename, "found!")


def batchq_slurm(jobnames):
    """Make sure the squeue format accommodates the full job name"""

    longest = 0
    for jobname in jobnames:
        if len(jobname) > longest:
            longest = len(jobname)
    squeue_format = "%8i %8u %.{:d}j %.19S %.19e %.10L %.5D %.10Q".format(
            longest+3)
    batch_q = 'squeue -o "{0:s}" -n'.format(squeue_format)
    return batch_q


def check_batch(jobnames):
    """Determine batch commands to use based on environment"""

    if espressolib.which("sbatch"):
        batch_launch = "sbatch"
        batch_q = batchq_slurm(jobnames)
        batch_cancel = "scancel -n"
        batch_sep = ","
    elif espressolib.which("qstat"):
        batch_launch = "qsub"
        batch_q = "qstat"
        batch_cancel = "canceljob"
        batch_sep = " "
    else:
        raise Exception("No sbatch or qsub found in path!")

    return batch_launch, batch_q, batch_cancel, batch_sep


def get_jobids(jobnames, batchenv):
    """PBS commands only take job ids, not job names. Must translate. Slurm
    commands can use the job names, so no translation required"""

    jobids = []
    if batchenv == "sbatch":
        jobids = jobnames
    elif batchenv == "qsub":
        for jobname in jobnames:
            jobid = None
            while not jobid:
                # try repeatedly in case slow to report
                command = "qselect -N {0}".format(jobname)
                jobid = subprocess.Popen(
                        command, stdout=subprocess.PIPE,
                        shell=True).communicate()[0]
                jobid = jobid.strip()
            jobids.append(jobid)

    return jobids


def run_batch(corrjoblist, outdir):
    """Run jobs in a batch environment"""

    # get the batch commands for the slurm or pbs
    batch_launch, batch_q, batch_cancel, batch_sep = check_batch(
            corrjoblist.keys())

    # start the correlator log
    errormon_log = "./log"
    print ("starting errormon2 in the background")
    try:
        os.remove(errormon_log)
        print ("removed old", errormon_log)
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
        time.sleep(1)
        print ("renaming log to", pass_logfilename)
        shutil.move("./log", pass_logfilename)
    except:
        print (errormon_log, "not found! No difxlog will be produced")

    # submit all the jobs to the batch scheduler
    for jobname in sorted(corrjoblist.keys()):
        try:
            runfile = "{0} ./run_{1}".format(batch_launch, jobname)
            print ("starting the correlator with:", runfile)
            subprocess.check_call(
                    runfile, shell=True, stdout=sys.stdout, env=espresso_env)
        except:
            pass

    # Just wait until the jobs have completed. Operator hits ^C to progress.
    # Enter will provide queue report.
    jobids = get_jobids(sorted(corrjoblist.keys()), batch_launch)
    queue_command = " ".join([batch_q, batch_sep.join(jobids)])
    while True:
        try:
            #command = ['squeue', '-u', '$USER']
            print ("-" * 78, "\n")
            try:
                # remind us what the job is for
                print (open(operator_log).read())
            except:
                pass
            running_jobs = print_queue(queue_command, jobids)
            if running_jobs:
                raw_input(
                        "Jobs submitted - hit ^C to cancel jobs."
                        " Hit return to see list of running jobs.")
            else:
                print ()
                print ("-" * 78)
                print ("All jobs completed:", " ".join(jobids))
                time.sleep(1)
                break
        except KeyboardInterrupt:
            for jobname in jobids:
                command = " ".join([batch_cancel, jobname])
                print (command)
                subprocess.check_call(command, stdout=sys.stdout, shell=True)
            break
        except:
            # qstat (PBS) on an empty queue will throw an error which we catch
            # here.
            print ("jobs", " ".join([jobids]), "complete")
            break

    # tidy up log files for each job
    good_jobs = []
    bad_jobs = []
    job_errors = []
    errors = []
    for jobname in sorted(corrjoblist.keys()):
        # we're finished with the logs..
        os.kill(errormon2.pid, 9)
        #time.sleep(1)
        job_ok, job_errors = write_difxlog(pass_logfilename, outdir, jobname)
        if job_ok:
            good_jobs.append(jobname)
        else:
            bad_jobs.append(jobname)
        errors += job_errors

    return good_jobs, bad_jobs, errors


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
            print ("DIFX_MESSAGE_PORT", difx_message_port, "in use")
            difx_message_port += 1
        else:
            print ("DIFX_MESSAGE_PORT=", difx_message_port)
            break

    return difx_message_port


def write_difxlog(log_in, outdir, jobname):
    """Extract log entries for a given job and write to standard file"""

    logfilename = outdir + jobname + ".difxlog"
    logfiles.append(jobname + ".difxlog")
    logfile = open(logfilename, "w")
    print ("filtering the log file and copying to", logfile.name)
    #shutil.copy2('log', logfile)
    log_lines = filter_log(log_in, jobname)
    job_finish = False
    job_errors = []
    for line in log_lines:
        logfile.write(line)
        if "BYE" in line:
            job_finish = True
        elif "ERROR" in line:
            job_errors.append(line)
    logfile.close()

    job_ok = False
    if job_finish and not job_errors:
        job_ok = True
    if not job_finish:
        job_errors.append(jobname + " did not finish")

    return job_ok, job_errors


def print_queue(queue_command, jobids):
    """Print status of jobs in batch queue"""

    #print queue_command
    running_jobs = []
    queue_info = subprocess.Popen(
            queue_command, stdout=subprocess.PIPE, shell=True).communicate()[0]
    print (queue_info)
    print ("Completed jobs: ", end="")
    for jobname in jobids:
        if jobname not in queue_info:
            print (jobname, end=" ")
        else:
            running_jobs.append(jobname)
    print ()
    return running_jobs

def get_options():
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
explicitly)
"""

    parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
    parser.add_option(
            "--force", "-f",
            dest="force", action="store_true", default=False,
            help="run vex2difx with -f switch")
    parser.add_option(
            "--clock", "-c",
            dest="clockjob", action="store_true", default=False,
            help="Store output in a clock subdirectory. Also pass -f to"
            " vex2difx")
    parser.add_option(
            "--test", "-t", dest="testjob", action="store_true", default=False,
            help="Store output in a test subdirectory. Also passes -f to"
            " vex2difx")
    parser.add_option(
            "--nocalc", "-C",
            dest="nocalc", action="store_true", default=False,
            help="Do not re-run calc")
    parser.add_option(
            "--novex", "-n",
            dest="novex", action="store_true", default=False,
            help="Do not re-run vex2difx")
    #parser.add_option(
    #        "--nofilelist", "-F",
    #        dest="nofilelist", action="store_true", default=False,
    #        help="Do not re-generate filelists (with lbafilecheck)")
    parser.add_option(
            "--nopause", "-p",
            dest="nopause", action="store_true", default=False,
            help="Do not pause after running calc - proceed straight to "
            " correlation")
    parser.add_option(
            "--alljobs", "-a",
            type="str", dest="expt_all", default=None,
            help="Correlate all jobs produced by vex2difx for the experiment"
            " specified (no other arguments required)")
    parser.add_option(
            "--nocomputehead", "-H",
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
            help="Run difxwatch (only valid for interactive jobs)")
    parser.add_option(
            "--interactive", "-i",
            dest="interactive", action="store_true", default=False,
            help="Run interactively, else assume slurm/pbs batch jobs")
    parser.add_option(
            "--jobtime", "-j",
            type="str", dest="jobtime", default=None,
            help="Max. job time for batch jobs."
            " Default is joblength * speedup + 10 mins."
            " Format = hh:mm:ss")
    parser.add_option(
            "--speedup", "-s",
            type="float", dest="predicted_speedup", default=0.7,
            help="Predicted speedup factor to determine job run time."
            " Default=%default")
    parser.add_option(
            "--ntasks_per_node",
            type="int", dest="ntasks_per_node", default=None,
            help="Number of MPI processes per node. This is for testing"
            " purposes on batch systems only!")
    
    (options, args) = parser.parse_args()
    return options, args


# Main program start.
options, args = get_options()

if options.testjob and options.clockjob:
    raise Exception("Don't use both -t and -c together!")

if len(args) < 1 and not options.expt_all:
    parser.print_help()
    parser.error("Give job name(s)")

# multiple tasks per node only makes sense with computehead=true
if options.ntasks_per_node > 1:
    options.computehead = True

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
            "\nHit return, then enter an operator comment, minimally:"
            " PROD/CLOCK/TEST/FAIL")
    fill_operator_log(operator_log)
except:
    print ("Operator comment not saved!")

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
                "Enter *gmail* address and password for notifications"
                " (or return to ignore):\n")
        if user:
            emailserver = espressolib.Email(user, getpass.getpass())
            emailserver.connect()
            emailserver.disconnect()
            break
        else:
            get_email = False
    except:
        print ("Connection failed - try again")


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
if options.clockjob or options.testjob or options.force:
    vex2difx_options = " -f "

#if not options.nofilelist:
#    # re-generate file lists before full production runs, just in case
#    if (not (options.clockjob or options.testjob)) and options.alljobs:
#        datafilename = expname + ".datafiles"
#        command = " ".join(["lbafilecheck.py", datafilename])
#        print command
#        subprocess.check_call(command, stdout=sys.stdout, shell=True)

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

print ("job list to correlate:\n", pprint.pformat(corrjoblist), "\n")

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
    print ("making the output directory", outdir)
    os.makedirs(outdir)


# do the prep work for each job.
for jobname in sorted(corrjoblist.keys()):
    # figure out filenames, directories, etc. using normal difx/cuppa
    # conventions

    inputfilename = jobname + ".input"
    calcfilename = jobname + ".calc"

    # fix the output filename to point at the cuppa data disk
    print ()
    if not options.novex:
        print (
                "renaming the 'OUTPUT FILENAME' in", inputfilename, "from",
                indir, "to", outdir)
        change_path(inputfilename, 'OUTPUT FILENAME:', indir, outdir)

    # run calcif2
    if not options.nocalc:
        run_calcif2(jobname, calcfilename)

    # copy any old jobs to a backupdir
    backup_oldrun(jobname, outdir, backupdir)

    # copy the model files to the output directory
    print ("copying the model files", jobname + ".*", "to", outdir)
    copy_models(jobname, indir, outdir)

    # change the path names in the copied .input and .calc to relative paths
    print(
            "changing absolute paths to relative paths in the copied .input"
            " and .calc files")
    fix_paths(inputfilename, calcfilename, indir, outdir)

    # copy job control files to output directory, and rename
    print (
            "copying the job control files", passname + ".[joblist|v2d]", "to",
            outdir)
    copy_jobcontrol(passname, jobname, indir, outdir, ".joblist")
    copy_jobcontrol(passname, jobname, indir, outdir, ".v2d")

    outputvex = outdir + jobname + ".vex"
    print ("copying the vex file", vexfilename, "to", outputvex)
    shutil.copy2(vexfilename, outputvex)

# and copy the .vex .v2d, and _notes.txt unaltered for ease of reference in the
# output dir
print (
        "\ncopying the vex and v2d files", vexfilename, v2dfilename, "to",
        outdir)
shutil.copy2(vexfilename, outdir)
shutil.copy2(v2dfilename, outdir)
notesfilename = expname + "_notes.txt"
if os.path.exists(notesfilename):
    print ("copying the notes file", notesfilename, "to", outdir)
    shutil.copy2(notesfilename, outdir)
# if pulsar binning, then get the binconfig and polyco too
if binconfigfilename:
    print (
            "copying the binconfig and polyco files", binconfigfilename,
            polycofilename, "to", outdir)
    shutil.copy2(binconfigfilename, outdir)
    shutil.copy2(polycofilename, outdir)
print ("\n")

if not options.nopause:
    raw_input("Press return to initiate the correlator job or ^C to quit")

# set the $DIFX_MESSAGE_PORT as late as possible in the processing to avoid
# clashes with other espresso invocations
difx_message_port = set_difx_message_port(50201)
espresso_env = os.environ.copy()
espresso_env["DIFX_MESSAGE_PORT"] = str(difx_message_port)
os.environ["DIFX_MESSAGE_PORT"] = str(difx_message_port)

# create the mpi files for each job
for jobname in sorted(corrjoblist.keys()):
    # run lbafilecheck to get the new machines and .threads files

    ntasks_per_node = 1
    if options.ntasks_per_node:
        ntasks_per_node = options.ntasks_per_node
    elif (not options.interactive) and options.computehead:
        ntasks_per_node = 2

    datafilename = expname + ".datafiles"
    run_lbafilecheck(
            datafilename, corrjoblist[jobname]["stations"],
            options.computehead, options.no_rmaps_seq, options.interactive,
            ntasks_per_node)

    # duplicate the run and thread and machines files for the full number of
    # jobs
    print (
            "duplicating the run file, machines file and .threads files for",
            jobname, "\n")
    if options.jobtime:
        jobtime = options.jobtime
    else:
        jobtime = corrjoblist[jobname]["jobtime"]

    make_new_runfiles(
            jobname, expname, jobtime, str(difx_message_port),
            str(ntasks_per_node))

logfiles = []

# run each job
good_jobs = []
bad_jobs = []
job_errors = []
try:
    if options.interactive:
        good_jobs, bad_jobs, job_errors = run_interactive(corrjoblist, outdir)
    else:
        good_jobs, bad_jobs, job_errors = run_batch(corrjoblist, outdir)
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
        print ("Could not plot speedup factor!")

    # let someone know we've finished
    if get_email:
        try:
            print ("\nEmailing", emailserver.user)
            message = str(sorted(corrjoblist.keys())) + " finished"
            emailserver.connect()
            emailserver.sendmail(message)
            emailserver.disconnect()
        except:
            print ("No notification email sent")

    # and enter an operator comment
    raw_input(
            "\nHit return, then update the operator comment, minimally:"
            " PROD/CLOCK/TEST/FAIL")
    fill_operator_log(operator_log)
    for jobname in corrjoblist.keys():
        operator_joblog = outdir + jobname + ".comment.txt"
        shutil.copy2(operator_log, operator_joblog)

    # print accumulated errors to screen for reference
    print ("Log errors:\n", "".join(job_errors))

    # clean up the forked plot process
    try:
        print ("\n\nWaiting for plotting process", speedup.pid, "to finish")
        plotmsg, ploterr = speedup.communicate()
        print (plotmsg, ploterr)
    except KeyboardInterrupt:
        speedup.kill()
        print ("Speedup plot not complete!")

    #if not options.clockjob and not options.testjob:
    print ("Successful jobs:")
    print (" ".join(good_jobs))
    print ("Jobs that may need re-doing:")
    if bad_jobs:
        #switches = [switch for switch in sys.argv[1:] if switch != "-a"]
        #print ("espresso.py", " ".join(switches+bad_jobs))
        print ("espresso.py", " ".join(bad_jobs))
    else:
        print ("None")
