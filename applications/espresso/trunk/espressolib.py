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

# Some functions used by the Espresso scripts
# Cormac Reynolds, original version: 2011 November 22


from __future__ import print_function, division
import re
import os
import fcntl
import smtplib
import sys
from email.mime.text import MIMEText
#import mx.DateTime
import astropy.time
import datetime
import subprocess


def get_corrhosts(hostsfilename):
    """Parse the host list file.

    Return a dict where the key is the hostname.
    The values are a list. The first element of the list is the number of
    threads. The second element is a list of data areas.
    """

    hostsfile = open(hostsfilename, "r").readlines()

    hosts = dict()
    version = int()
    for linenum, line in enumerate(hostsfile):
        line.strip()
        # strip comments
        line = re.sub(r"#.*", "", line)

        if linenum == 0:
            version = None
            # first line should be the version number
            try:
                version = re.match(r"version\s*=\s*(\d+)", line).group(1)
                version = int(version)
            except:
                raise Exception("First line must be version number!")
            if version != 1:
                sys.stderr.write(
                        "Warning: version number in $DIFX_MACHINES is not 1."
                        " This may not work as expected\n")
            continue

        hostdata = line.split(",")
        hostname_list = hostdata[0].strip()
        if not hostname_list:
            # empty line
            continue
        try:
            # host disabled?
            if int(hostdata[1]) > 0:
                enabled = 1
            else:
                enabled = 0
        except:
            # missing the host enabled column
            raise Exception(
                    " ".join(["Badly formatted file:", hostsfilename,
                    str(linenum)]))

        try:
            # number of compute threads
            hostthreads = int(hostdata[2])
        except:
            hostthreads = 0

        hostdisks = []
        try:
            # list of urls for storing data
            hosturls = hostdata[3].split()
            for url in hosturls:
                filepath = re.match("file://(.*)", url)
                if (filepath):
                    hostdisks.append(filepath.group(1))
        except:
            # no urls given, not a data node
            hostdisks = []

        # expansion of zero-padded integers is allowed.
        hostnames = expand0int(hostname_list)
        for hostname in hostnames:
            if hostname in hosts.keys():
                del(hosts[hostname])
            if enabled:
                hosts[hostname] = [hostthreads, hostdisks]

    return hosts


def expandstr(inputstr):
    """expand patterns in a string

    Expand a string that may contain a pattern into a list of all
    strings that match the pattern
    """

    outputstrs = [inputstr]
    globpattern = r"\[.*?\]"
    for patternmatch in re.finditer(globpattern, inputstr):
        newoutputstrs = []
        startrange = False
        pattern = inputstr[patternmatch.start()+1: patternmatch.end()-1]
        for outputstr in outputstrs:
            lastchar = str()
            for character in pattern:
                if character != "-" and not startrange:
                    newoutputstrs.append(
                            re.sub(globpattern, character, outputstr, 1))
                    lastchar = character
                elif startrange:
                    startrange = False
                    for i in range(ord(lastchar)+1, ord(character)+1):
                        newoutputstrs.append(
                                re.sub(globpattern, chr(i), outputstr, 1))

                elif character == "-":
                    startrange = True
        outputstrs = newoutputstrs

    return outputstrs


def expand0int(inputstr):
    """Expand a string with an integer range

    Expand a string that may contain a range of zero-padded integers into
    a list of all strings that match the given integer range
    """

    outputstrs = [inputstr]
    globpattern = r"\[(.*?)\]"
    #for patternmatch in re.finditer(globpattern, inputstr):
    int_range = re.search(globpattern, inputstr)
    if int_range:
        outputstrs = []
        startrange, endrange = int_range.group(0)[1:-1].split("-")
        for i in range(int(startrange), int(endrange)+1):
            outputstrs.append(
                    re.sub(globpattern, str(i).zfill(len(startrange)),
                    inputstr))

    return outputstrs


def which(program):
    """Search $PATH for an executable, kind of like the unix 'which' command"""

    for path in os.environ.get("PATH").split(os.pathsep):
        testpath = os.path.join(path, program)
        if os.access(testpath, os.X_OK):
            programpath = testpath
            return programpath

    return None


def openlock(filename):
    """Open a file, then lock it"""
    lockedfile = open(filename, "a")
    fcntl.flock(lockedfile.fileno(), fcntl.LOCK_EX)

    return lockedfile


#def mjd2vex(indate):
#    """converts indate from mjd to vex or vice versa. Deprecated: use
#convertdate instead"""
#
#    vexformat = "%Yy%jd%Hh%Mm%Ss"
#    try:
#        outdate = mx.DateTime.strptime(indate, vexformat).mjd
#    except:
#        try:
#            gregdate = mx.DateTime.DateTimeFromMJD(float(indate))
#            outdate = gregdate.strftime(vexformat)
#        except:
#            raise Exception(
#                    "Accepts dates only in VEX format,"
#                    " e.g. 2010y154d12h45m52s, or MJD")
#
#    return outdate

#class TimeVLBA(Time

def convertdate(indate, outformat="mjd", strict=True):
    """converts between DiFX date formats (mjd, vex, iso, vlba)

    Example formats:
    mjd: 58119.0
    vex: 2018y001d00h00m00s
    vex (truncated to day): 2018y001d
    iso: 2018-01-01T00:00:00
    iso (date only): 2018-01-01
    vlba: 2018JAN01-00:00:00
    """

    # MJD is a float for input, and built in to the module for output.
    # ISO8601 is built in to the module, but the output format lacks the "T",
    # so use our own format
    vexformat = "%Yy%jd%Hh%Mm%Ss"
    vlbaformat = "%Y%b%d-%H:%M:%S"
    #isoformat = "%Y-%m-%dT%H:%M:%S"
    # date only part of ISO8601
    #isoformat_d = "%Y-%m-%d"

    if outformat == "iso":
        outformat = "isot"

    # vex format can truncate from the right, have 2 digits for the year, and
    # decimal seconds
    vexformat_in = vexformat
    if isinstance(indate, str) and "y" in indate:
        if indate[2] == "y":
            vexformat_in = vexformat_in.replace("%Y", "%y")
        vexlen = 3
        if "d" in indate:
            vexlen = 6
            if "h" in indate:
                vexlen = 9
                if "m" in indate:
                    vexlen = 12
                    if "s" in indate:
                        vexlen = 15
        vexformat_in = vexformat_in[0:vexlen]
        if "." in indate:
            vexformat_in = vexformat_in.replace("s", ".%fs")

    # built-in formats
    timeformats1 = ["mjd", "iso", "isot"]
    # strftime formats
    timeformats2 = [vexformat_in, vlbaformat]

    # convert the input time to a datetime

    date = None
    for timeformat in timeformats1:
        try:
        # MJD?
            if timeformat == "mjd":
                indate = float(indate)
            date = astropy.time.Time(indate, format=timeformat)
        except ValueError:
            continue
    if date is None:
        for timeformat in timeformats2:
            try:
                date = datetime.datetime.strptime(indate, timeformat)
                date = astropy.time.Time(date)
                break
            except ValueError:
                pass

    if date is None:
        if strict:
            raise Exception(
                    "Accepts dates only in MJD, Vex, VLBA or ISO8601 formats")
        else:
            return indate

    if outformat in ["mjd", "isot"]:
        date.format = outformat
        outdate = date.value
    elif outformat == "vlba":
        date.format = "datetime"
        outdate = datetime.datetime.strftime(date.value, vlbaformat).upper()
    elif outformat == "vex":
        date.format = "datetime"
        outdate = datetime.datetime.strftime(date.value, vexformat)
    else:
        raise Exception(
                "Output format not recognised, choose from: mjd, vex, iso "
                " or vlba")

    return outdate


def daysToDhms(fracdays):
    """Convert time in decimal days to days, hours, mins, secs"""
    days = int(fracdays)
    remainder = fracdays - days
    hours = int(remainder * 24.)
    remainder = remainder*24. - hours
    minutes = int(remainder*60)
    remainder = remainder*60 - minutes
    seconds = int(remainder*60)
    return [days, hours, minutes, seconds]


#def email(user, passwd, message):
#    """Simple gmail notification message"""
#
#    msg = MIMEText(message)
#
#    msg["Subject"] = "Correlator notification"
#    msg["From"] = user
#    msg["To"] = user
#
#    server = smtplib.SMTP("smtp.gmail.com:587")
#    server.starttls()
#    server.login(user,passwd)
#    server.sendmail(user, user, msg.as_string())
#    server.quit()


class Email:
    """Simple gmail notification message.

    Logs in to gmail server of the given account and sends an email to that
    same account
    """

    def __init__(self, user, passwd):
        self.user = user
        self.passwd = passwd
        self.server = None

    def connect(self):
        """Set up the connection to the SMTP server"""
        self.server = smtplib.SMTP("smtp.gmail.com:587")
        self.server.starttls()
        try:
            self.server.login(self.user, self.passwd)
        except Exception as connectionError:
            print (connectionError)
            raise

    def sendmail(self, message):
        """Send the given message via the already prepared SMTP connection"""
        msg = MIMEText(message)
        msg["Subject"] = "Correlator notification"
        msg["From"] = self.user
        msg["To"] = self.user
        self.server.sendmail(self.user, self.user, msg.as_string())

    def disconnect(self):
        """Quit the SMTP connection"""
        self.server.quit()


class Msg_Filter:
    """Filter an input string based on match with some other string(s)

    Writes output to a file (or similar object) after filtering.
    First argument is the desired output object, remainder are the strings to
    filter on.
    Should itself look like a file object. Filter is case-insensitive
    """

    def __init__(self, filename, *args):
        self._file = filename
        self._filters = args

    def write(self, string):
        for filter_text in self._filters:
            if filter_text.lower() in string.lower():
                self._file.write(string)
                break
        self.flush()

    def flush(self):
        self._file.flush()

    def fileno(self):
        self._file.fileno()


def dhms2sec(time_in):
    """convert [DD-[HH:]]MM:SS to secs"""

    sec = 0
    time_split = time_in.split(":")
    sec = int(time_split.pop())
    sec += int(time_split.pop())*60
    if time_split:
        dayhour = time_split[0].split("-")
        sec += int(dayhour.pop())*3600
        if dayhour:
           sec += int(dayhour[0])*3600*24

    return sec


class batchenv:
    """Set the batch commands to use. SLURM and PBS are in principle both
    supported but PBS has not been tested in a long time. Everyone uses SLURM
    now, right?
    
    If automatic detection does not work, can explicitly call self.set_style
    with either 'slurm' or 'pbs' as argument.
    """

    def __init__(self, jobnames): 
        self._jobnames = jobnames
        if which("sbatch"):
            self.set_style("slurm")
        elif which("qsub"):
            self.set_style("pbs")
        else:
            sys.stderr.write(
                    "No sbatch or qsub found in path."
                    " You can call self.set_style explicitly\n"
                    )
        return

    def set_style(self, style):
        """Set the batch style - either 'slurm' or 'pbs'"""

        self.launch = None
        self.cancel = None
        self.stats = None
        self.q = None
        self.speedup = None

        self._style = style
        self._set_commands()
        return

    def _set_commands(self):
        """Set the batch commands for either SLURM or PBS

        PBS version probably broken right now - let me know if anyone still
        uses it
        """

        if self._style == "slurm":
            self.launch = "sbatch --parsable --export=ALL"
            #self.cancel = "scancel -n"
            self.cancel = "scancel"
            self.stats = ("sacct -j {:s} -X --format "
                "JobName%-15,JobId,elapsed,NNodes,Time,Reserved")
            self.q = self._batchq_slurm(self._jobnames)
            self.speedup = self._slurm_speedup
        elif self._style == "pbs":
            self.launch = "qsub"
            self.cancel = "canceljob"
            self.q = " ".join(["qstat", " ".join(self._jobnames)])
            self.stats = None
            self.speedup = self._pbs_speedup
        else:
            #raise Exception("No sbatch or qsub found in path!")
            sys.stderr.write(
                    "Batch environment '{:s}' unknown.\n".format(self._style)
                    )
        return

    def _slurm_speedup(self, jobid, joblen):
        """Run an sacct command and parse the output to calculate job
        speedup
        """
    
        speedup = 0.
        speedup_wait = 0.
        try:
            command = "sacct -j {:s} -X --format ElapsedRaw -P -n".format(
                    jobid)
            elapsed = subprocess.Popen(
                    command, stdout=subprocess.PIPE,
                    shell=True, encoding="utf-8").communicate()[0]
            elapsed = int(elapsed)
            command = "sacct -j {:s} -X --format Reserved -P -n".format(jobid)
            #print(command)
            reserved = subprocess.Popen(
                    command, stdout=subprocess.PIPE,
                    shell=True, encoding="utf-8").communicate()[0]
            reserved = dhms2sec(reserved)
            speedup = joblen*60./elapsed
            speedup_wait = joblen*60./(elapsed+reserved)
        except:
            speedup = 0.0
            speedup_wait = 0.0
    
        return speedup, speedup_wait

    def _pbs_speedup(self, jobid, joblen):
        """Don't know how to do this yet"""
        return 0.0, 0.0

    def _batchq_slurm(self, jobnames):
        """Return a SLURM queue command for the current jobs. 
        
        Make sure the squeue format accommodates the full job name"""
    
        longest = 0
        batch_q = None
        for jobname in jobnames:
            if len(jobname) > longest:
                longest = len(jobname)
        squeue_format = (
                "%8i %8u %.{:d}j %.3t %.19S %.19e %.10L %.5D %.10Q".format(
                longest+3))
        batch_q = 'squeue -o "{0:s}" -n {1:s}'.format(
                squeue_format, ",".join(jobnames))
        return batch_q
