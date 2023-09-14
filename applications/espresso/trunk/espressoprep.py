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

# take care of a few logistics before launching the correlator
# suitable for use in singularity container
# Cormac Reynolds: June 2022


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
import espressolib


def run_vex2difx(v2dfilename, vex2difx_options):
    """run vex2difx, and wait for completion"""

    command = " ".join(["vex2difx", vex2difx_options, v2dfilename])
    print (command)
    subprocess.check_call(command, stdout=sys.stdout, shell=True)


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


def make_new_runfiles(
        jobname, expname, jobtime, difx_message_port, difx_image,
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
        line = re.sub(
                r"{DIFXLOGNAME}", 
                espressolib.get_difxlogname(outdir, jobname),
                line)
        line = re.sub(r"{DIFX_IMAGE}", difx_image, line)
        print (line, end="")
    fileinput.close()
    os.chmod(runfile, 0o775)

    threadfilename = jobname + ".threads"
    shutil.copy(expname + ".threads", threadfilename)

    machinesfilename = jobname + ".machines"
    shutil.copy(expname + ".machines", machinesfilename)


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


def get_options(usage):
    '''parse the options'''
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
    parser.add_option(
            "--nopause", "-p",
            dest="nopause", action="store_true", default=False,
            help="Do not pause after running calc - proceed straight to "
            " correlation")
    parser.add_option(
            "--nocomputehead", "-H",
            dest="computehead", action="store_false", default=True,
            help="Don't Use head and datastream nodes as compute nodes")
    parser.add_option(
            "--no_rmaps_seq", "-M",
            dest="no_rmaps_seq", action="store_true", default=False,
            help="Don't Pass the '--mca rmaps seq' instruction to mpirun")
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
            type="int", dest="ntasks_per_node", default=1,
            help="Number of MPI processes per node. Default=%default")
    parser.add_option(
            "--interactive", "-i",
            dest="interactive", action="store_true", default=False,
            help="Run interactively, else assume slurm/pbs batch jobs")
    
    (options, args) = parser.parse_args()
    return options, args


# Main program start.
usage = """%prog <expname>
    will:
    run "vex2difx <expname>.v2d"
    correct the output file names
    run calcif2
    prepare job launch files (run*, *threads, *machines)

"""

options, args = get_options(usage)

if options.testjob and options.clockjob:
    raise Exception("Don't use both -t and -c together!")

if len(args) < 1:
    parser.print_help()
    parser.error("Give v2d filename")

# Can be overridden with the options switch.
ntasks_per_node = int(options.ntasks_per_node)
# multiple tasks per node only makes sense with computehead=true
if ntasks_per_node > 1:
    if options.computehead is False:
        print ("ntasks_per_node > 1, setting computehead to True")
        options.computehead = True

# Determine the name of the correlator pass from the v2d filename
v2dfilename = args[0] + ".v2d"
print ("v2dfilename", v2dfilename)
passname = re.sub(".v2d$", "", v2dfilename)
print ("passname", passname)

# a '-' is used to distinguish different correlator passes. If only one pass,
# then the expname and passname are the same
passid = str()
expname = passname
if "-" in passname:
    expname, passid = expname.split("-")

# preliminaries: clear old input files out of the way
if not options.novex:
   for filename in os.listdir(os.getcwd()):
       if re.match(passname + r"_\d+\.input$", filename):
           os.rename(filename, filename + ".bak")

vex2difx_options = ""
if options.clockjob or options.testjob or options.force:
    vex2difx_options = "-f"

# run vex2difx.
vexfilename, binconfigfilename = espressolib.parse_v2dfile(v2dfilename)
if not vexfilename:
    raise Exception("Could not find VEX file in " + v2dfilename)

if binconfigfilename:
    polycofilename = espressolib.parse_binconfig(binconfigfilename)
    if not polycofilename:
        raise Exception("Could not find polycofile in " + binconfigfilename)

if not options.novex:
    run_vex2difx(v2dfilename, vex2difx_options)

joblistfilename = passname + ".joblist"
(corrjoblist) = espressolib.parse_joblistfile(
        joblistfilename, options.predicted_speedup)

print ("job list:\n", pprint.pformat(corrjoblist), "\n")

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

# do the prep work for each job.
for jobname in sorted(corrjoblist.keys()):
    # figure out filenames, directories, etc. using normal difx conventions
    inputfilename = jobname + ".input"
    calcfilename = jobname + ".calc"

    # fix the output filename to point at the data disk
    print ()
    if not options.novex:
        print (
                "renaming the 'OUTPUT FILENAME' in", inputfilename, "from",
                indir, "to", outdir)
        espressolib.change_path(
                inputfilename, 'OUTPUT FILENAME:', indir, outdir)

    # run calcif2
    if not options.nocalc:
        run_calcif2(jobname, calcfilename)


# set the $DIFX_MESSAGE_PORT 
difx_message_port = os.environ.get("DIFX_MESSAGE_PORT", 50201)
difx_image = os.environ.get("DIFX_IMAGE", "")

# create the mpi files for each job
for jobname in sorted(corrjoblist.keys()):
    # run lbafilecheck to get the new machines and .threads files

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
            jobname, expname, jobtime, str(difx_message_port), str(difx_image),
            str(ntasks_per_node))

print("Job prep complete. Now launch the run files at your leisure. espressolaunch.py may do this for you.")
