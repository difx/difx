#!/usr/bin/env python3
# =======================================================================
# Copyright (C) 2022 Cormac Reynolds
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

# launch the correlator assuming job prep already done
# Cormac Reynolds: June 2022


from __future__ import print_function, division
import subprocess
import optparse
import re
import shutil
import os
import sys
import time
import pprint
from espressolib import espressolib


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
    espressolib.change_path(
            copy_inputfilename, "CALC FILENAME:", indir, "./")
    espressolib.change_path(
            copy_inputfilename, "CORE CONF FILENAME:", indir, "./")
    espressolib.change_path(
            copy_inputfilename, "OUTPUT FILENAME:", outdir, "./")
    espressolib.change_path(
            copy_inputfilename, "PULSAR CONFIG FILE:", indir, "./")
    espressolib.change_path(
            copy_calcfilename, "IM FILENAME:", indir, "./")
    espressolib.change_path(
            copy_calcfilename, "FLAG FILENAME:", indir, "./")
    espressolib.change_path(
            copy_calcfilename, "VEX FILENAME:", indir, "./")


def fill_operator_log(logfile):
    """Fire up an editor for operator comments"""

    editor = os.environ.get("EDITOR", "vim")
    command = " ".join([editor, logfile])
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


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


def run_batch(corrjoblist, outdir):
    """Run jobs in a batch environment"""

    jobnames = sorted(corrjoblist.keys())
    jobids = []
    # get the batch commands
    batch = espressolib.batchenv(jobnames)

    # submit all the jobs to the batch scheduler
    for jobname in jobnames:
        try:
            runfile = "{0} ./run_{1}".format(batch.launch, jobname)
            print ("starting the correlator with:", runfile)
            jobid = subprocess.Popen(
                    runfile, shell=True, stdout=subprocess.PIPE,
                    encoding="utf-8").communicate()[0]
            jobids.append(str(jobid).strip())
        except:
            pass

    # Just wait until the jobs have completed. Operator hits ^C to cancel jobs and exit.
    # Enter will provide queue report.
    #jobids = get_jobids(sorted(corrjoblist.keys()), batch_launch)
    while True:
        try:
            #command = ['squeue', '-u', '$USER']
            print ("-" * 78, "\n")
            try:
                # remind us what the job is for
                print (open(operator_log).read())
            except:
                pass
            print (time.asctime())
            running_jobs = print_queue(batch.q, jobnames)
            if running_jobs:
                input(
                        "Jobs submitted - hit ^C to cancel jobs."
                        " Hit return to see status of jobs.\n")
            else:
                print ()
                print ("-" * 78)
                print ("All jobs completed:", " ".join(jobnames))
                time.sleep(1)
                break
        except KeyboardInterrupt:
            for jobid in jobids:
                command = " ".join([batch.cancel, jobid])
                print (command)
                subprocess.check_call(command, stdout=sys.stdout, shell=True)
            break
        except:
            # qstat (PBS) on an empty queue will throw an error which we catch
            # here (along with any other unexpected exceptions).
            print ("jobs", " ".join([jobids]), "complete")
            break

    # tidy up log files for each job
    good_jobs = []
    bad_jobs = []
    job_errors = []
    errors = []
    speedups = {}
    for jobid, jobname in zip(jobids, jobnames):

        #time.sleep(1)
        job_logfilename = espressolib.get_difxlogname(outdir, jobname)
        if os.path.exists(job_logfilename):
            job_ok, job_errors = chk_difxlog(open(job_logfilename).readlines())
            job_errors = [
                    "{:s}: {:s}".format(jobname, err) for err in job_errors]
        else:
            job_ok = False
            job_errors = [job_logfilename + " does not exist\n"]
        if job_ok:
            good_jobs.append(jobname)
        else:
            bad_jobs.append(jobname)
        errors += job_errors[0:10]

        if batch.stats is not None:
            # print the user stats
            job_status = "Incomplete"
            if job_ok:
                job_status = "Complete"
            jobinfo = "{:s} {:.2f}m {:s}\n{:s} ({:d})\n".format(
                    jobname, corrjoblist[jobname]["joblen"], job_status,
                    corrjoblist[jobname]["stations"],
                    len(corrjoblist[jobname]["stations"].split()))
            joblen = corrjoblist[jobname]["joblen"]
            speedup_measured = write_usage_stats(
                    batch, jobid, jobname, jobinfo, joblen, outdir)
            speedups[jobname] = speedup_measured

    return good_jobs, bad_jobs, errors, speedups


def write_usage_stats(
        batch, jobid, jobname, jobinfo, joblen, outdir):
    """Print some stats on node usage and speedup factor"""

    usage_stats = get_usage_stats(batch.stats, jobid)
    usage_filename = outdir+"/"+jobname+".usage"
    usage_file = open(usage_filename, "w")
    usage_file.write(jobinfo)
    #usage_file.write(str(corrjoblist[jobname]) + "\n")

    # calculate the speedup
    speedup_measured, speedup_measured_wait = batch.speedup(
            jobid, joblen)
    usage_file.write("Speedup: {:.2f}\n".format(speedup_measured))
    usage_file.write("Speedup (inc. wait): {:.2f}\n".format(
            speedup_measured_wait))
    usage_file.write(usage_stats)
    usage_file.close()

    return speedup_measured


def get_usage_stats(batch_stats, jobid):
    """Get usage stats from the batch scheduler"""

    command = batch_stats.format(jobid)
    #print (command)
    usage_stats = subprocess.Popen(
            command, stdout=subprocess.PIPE, shell=True,
            encoding="utf-8").communicate()[0]
    
    return usage_stats


def write_difxlog(log_in, outdir, jobname):
    """Extract log entries for a given job and write to standard file
    
    Also check for errors and incomplete jobs
    """

    logfilename = get_difxlogname(outdir, jobname)
    #logfiles.append(logfilename)
    logfile = open(outdir+"/"+logfilename, "w")
    print ("filtering the log file and copying to", logfile.name)
    #shutil.copy2('log', logfile)
    log_lines = filter_log(log_in, jobname)
    logfile.write(log_lines)
    logfile.close()
    job_errors = []
    job_ok, job_errors = chk_difxlog(log_lines)

    return job_ok, job_errors


def chk_difxlog(logfile):
    """Check for errors and shutdown messages in a difx log file."""

    job_finish = False
    job_errors = []
    for line in logfile:
        if "BYE" in line:
            job_finish = True
        elif "ERROR" in line:
            job_errors.append(line)

    job_ok = False
    if job_finish and not job_errors:
        job_ok = True
    if not job_finish:
        job_errors.append(jobname + " did not finish\n")

    return job_ok, job_errors


def print_queue(queue_command, jobnames):
    """Print status of jobs in batch queue"""

    #print queue_command
    running_jobs = []
    queue_info = subprocess.Popen(
            queue_command, stdout=subprocess.PIPE, shell=True,
            encoding="utf-8").communicate()[0]
    print (queue_info)
    print ("Completed jobs: ", end="")
    for jobname in jobnames:
        if jobname not in queue_info:
            print (jobname, end=" ")
        else:
            running_jobs.append(jobname)
    print ()
    return running_jobs


def parse_inputfile(inputfilename):
    """extract output directory from DiFX input file"""

    inputfile = open(inputfilename).readlines()
    outdir = str()
    for line in inputfile:
        # remove comments
        line = re.sub(r"#.*", "", line)
        outputfilematch = re.search(r"OUTPUT FILENAME:\s*(\S*)", line)
        if outputfilematch:
            outfile = outputfilematch.group(1)
            outdir = os.path.split(outfile)[0] + os.sep
            return outdir

    return None


def get_options(usage):
# parse the options

    parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
    parser.add_option(
            "--interactive", "-i",
            dest="interactive", action="store_true", default=False,
            help="Run interactively, else assume slurm/pbs batch jobs")
    parser.add_option(
            "--alljobs", "-a",
            type="str", dest="expt_all", default=None,
            help="Correlate all jobs produced by vex2difx for the experiment"
            " specified (no other arguments required)")
    
    (options, args) = parser.parse_args()
    return options, args


# Main program start.
usage = """%prog [options] <jobname>
    will:
    move previous correlator job to backup directory
    copy model information to correlator data area
    spawn the correlator jobs
    accept an operator comment for storing with the output
    produce some usage stats

<jobname> may be a space separated list.
<jobname> may also include a python regular expression after the '_' in the job
name to match multiple jobs. (The job name up to the '_' must be given
explicitly)
"""

options, args = get_options(usage)

if len(args) < 1 and not options.expt_all:
    parser.print_help()
    parser.error("Give job name(s), or -a <expname>")

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

v2dfilename = passname + ".v2d"
vexfilename, binconfigfilename = espressolib.parse_v2dfile(v2dfilename)
if not vexfilename:
    raise Exception("Could not find VEX file in " + v2dfilename)

if binconfigfilename:
    polycofilename = espressolib.parse_binconfig(binconfigfilename)
    if not polycofilename:
        raise Exception("Could not find polycofile in " + binconfigfilename)

operator_log = passname + "_comment.txt"
try:
    input(
            "\nHit return, then enter an operator comment, minimally:"
            " PROD/CLOCK/TEST/FAIL")
    fill_operator_log(operator_log)
except:
    print ("Operator comment not saved!")

joblistfilename = passname + ".joblist"
(fulljoblist) = espressolib.parse_joblistfile(joblistfilename)

# figure out the list of jobs to run this time
corrjoblist = dict()
# if no -a, then match any patterns given on the command line
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

# get the paths of our input directory
indir = os.getcwd() + os.sep

# do the prep work for each job.
for jobname in sorted(corrjoblist.keys()):
    # figure out filenames, directories, etc. using normal difx conventions

    inputfilename = jobname + ".input"
    calcfilename = jobname + ".calc"

    # extract outdir from the inputfile
    outdir = parse_inputfile(inputfilename)
    # get a unique name for a backup directory based on the current time.
    backupdir = outdir + time.strftime("%Y-%m-%d-%H-%M-%S") + os.sep
    
    if not os.path.exists(outdir):
        print ("making the output directory", outdir)
        os.makedirs(outdir)

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

# and copy the .vex, .v2d and _notes.txt unaltered for ease of reference in the
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

difx_message_port = os.environ.get("DIFX_MESSAGE_PORT", 50201)
#espresso_env["DIFX_MESSAGE_PORT"] = difx_message_port

# run each job
good_jobs = []
bad_jobs = []
job_errors = []
speedups = []
try:
    if options.interactive:
        good_jobs, bad_jobs, job_errors = run_interactive(corrjoblist, outdir)
    else:
        good_jobs, bad_jobs, job_errors, speedups = run_batch(
                corrjoblist, outdir)
finally:
    # enter an operator comment
    input(
            "\nHit return, then update the operator comment, minimally:"
            " PROD/CLOCK/TEST/FAIL")
    fill_operator_log(operator_log)
    for jobname in corrjoblist.keys():
        operator_joblog = outdir + jobname + ".comment.txt"
        try:
            shutil.copy2(operator_log, operator_joblog)
        except:
            sys.stderr.write("{:s} not copied!\n".format(operator_log))

    # print accumulated errors to screen for reference
    print ("Log errors:\n", "".join(job_errors), "\n")

    print ("Successful jobs (speedup):")
    print (" ".join(
        ["{:s} ({:.1f})".format(job, speedups.get(job,0.)) 
        for job in good_jobs]))
    print ("\nJobs that may need re-doing:")
    if bad_jobs:
        print (" ".join(
            ["{:s} ({:.1f})".format(job, speedups.get(job,0.)) 
                for job in bad_jobs]))
        print ("\nespressolaunch.py", " ".join(bad_jobs))
    else:
        print ("None")
