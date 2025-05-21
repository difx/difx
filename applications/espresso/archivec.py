#!/usr/bin/env python3
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

# archive DiFX data files to the Pawsey permanent data store. Tar the plethora
# of small files before transfer. Transfer FITS and other user files
# unmodified.
# Cormac Reynolds: Dec 2014


from __future__ import print_function, division
import optparse
import os
import re
import subprocess
import sys
import shutil
import traceback
import glob
import hashlib


def taritup(tardir, tarfile, infile, gzip=False):
    '''tar up the provided files, with option to compress the output'''

    print ("tarring up", tarfile)
    taroptions = options.taroptions
    if gzip:
        taroptions = " ".join([taroptions, "-z"])
    command = " ".join(["tar", taroptions, "-cf", archdir+tarfile, infile])
    if options.verbose:
        print ("\n" + command)
    subprocess.check_call(
            command, shell=True, stdout=sys.stdout, stderr=subprocess.PIPE)

    # and print tar listing for reference
    print ("creating listing for", tarfile)
    command = " ".join(
            ["tar -tf", tardir+tarfile, ">", tardir+tarfile+".list"])
    subprocess.check_call(
            command, shell=True, stdout=sys.stdout, stderr=subprocess.PIPE)

def do_checksum(filename, checksum_type="md5"):
    '''returns a checksum (default md5) for a file'''
    with open(filename, mode="rb") as f:
        checksum = hashlib.file_digest(f, checksum_type)
    return checksum


usage = """%prog <path> <destination>
will transfer <path> and all its subdirectories to <destination> on
data.pawsey.org.au using pshell. Most files are tarred before transfer, but
special files and large files are transferred unmodified. Files are first
tarred/copied to $ARCHTMP before transfer. Also computes checksums for FITS
files.

e.g.
%prog $CORR_DATA/v252aw /projects/VLBI/Archive/LBA/v252
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--maxtarsize", "-m",
        type="float", dest="maxtarsize", default=1000,
        help="files larger than MAXTARSIZE (MB) will be transferred untarred"
        " [default = %default]")
parser.add_option(
        "--taroptions", "-t",
        type="str", dest="taroptions", default=" ",
        help="specify additional options to pass to tar")
parser.add_option(
        "--verbose", "-v",
        dest="verbose", action="store_true", default=False,
        help="Be verbose")
parser.add_option(
        "--keeparch", "-k",
        dest="keeparch", action="store_true", default=False,
        help="Keep the tar archive directory after it's been transferred")
parser.add_option(
        "--onejob", "-j",
        dest="onejob", action="store_true", default=False,
        help="Each job gets its own tar file (instead of each pass)")
parser.add_option(
        "--skip_copy", "-s",
        dest="skip_copy", action="store_true", default=False,
        help="Skip the initial copy/tar step and assume $ARCHTMP has all data to be transferred")
#parser.add_option(
#        "--exclude", "-e",
#        type="str", dest="exclude",  default="slurm-*.out",
#        help="Exclude files matching the regexp")

(options, args) = parser.parse_args()

if len(args) < 2:
    parser.print_help()
    parser.error("Give source and destination directories!")
localdir = args[0]
pawseydir = args[1]

# get the list of subdirectories. Note that we don't want to tar large files,
# or files that archive users want to access directly.
# In each  subdirectory form a list of files to be tarred, and a list of files
# to transfer unmodified (both tarred files and large files will be
# transferred).

expname = os.path.normpath(localdir).split("/")[-1]
archdir = os.environ.get("ARCHTMP") + os.sep + expname + os.sep
if not archdir:
    archdir = "/tmp/"
    print (
            "$ARCHTMP not set - using /tmp instead. Setting $ARCHTMP to a",
            "directory on the same filesystem as the data is preferable")
mark4file = str()
os.chdir(localdir)
tarlists = dict()
transfer = []
publish = []

# first create md5 checksums for the FITS files
if not options.skip_copy:
    for filename in glob.glob("*.FITS"):
        md5sum = do_checksum(filename)
        md5filename = filename + ".md5"
        if options.verbose:
            print (f"writing {md5filename} with checksum for {filename}")
        with open(md5filename, mode="w") as f:
            # don't write the filename as you would in normal md5sum because CASDA...
            f.write(f"{md5sum.hexdigest()}\n")
            #f.write(f"{md5sum.hexdigest()}  {filename}\n")
            #f.write(" ".join([md5sum.hexdigest(), filename]) + "\n")
        #print(" ".join([md5sum.hexdigest(), filename]), file=open(md5filename, mode="w"))

for filename in os.listdir(os.curdir):

    # tar separate passes independently, so figure out pass names in use.
    # Default is just the expname.
    passname = expname
    if filename.startswith(expname+"-"):
        passname = re.sub(r"[_\.].*", "", filename)

    targroup = passname

    # option to tar each job independently (useful for v. large experiments)
    if options.onejob:
        if re.match(passname+r"_\d", filename):
            targroup = re.match(passname+r"_\d+", filename).group(0)

    if targroup not in tarlists.keys():
        tarlists[targroup] = str()

    # deal with Mark4 output, clocks, test and old runs as special cases
    if re.search(r"^\d\d\d\d$", filename):
        # deal with this later in its own tar file
        mark4file = filename
        continue
    if filename == "clocks":
        # deal with this later in its own tar file
        continue
    if filename == "test":
        # ignore this one
        print ("skipping", filename)
        continue
    if re.match(r"\d\d\d\d-\d\d-\d\d-\d\d-\d\d-\d\d", filename):
        # ignore these (old, superseded jobs).
        print ("skipping", filename)
        continue
    if re.match(r"slurm-.*\.out", filename):
        # ignore slurm files that litter the place
        print ("skipping", filename)
        continue

    # certain file extensions never get tarred
    notar_ext = [
            r".fits\Z", ".rpf", ".uvfits", ".mark4", ".tar", passname+".v2d",
            expname+".vex", expname+".skd", "notes.txt", ".md5"]
    fileWithPath = os.path.join(os.path.abspath(os.curdir), filename)
    notar = False
    for extension in notar_ext:
        if re.search(extension, filename, re.IGNORECASE):
            notar = True
            publish.append(filename)
            break

    # only tar small files
    if os.path.getsize(fileWithPath)/1e6 > options.maxtarsize:
        notar = True

    if (os.path.exists(fileWithPath) and notar):
        # transfer this large (or special) file without tarring
        transfer.append(re.escape(fileWithPath))
    else:
        # add to list of files to be tarred
        tarlists[targroup] += " " + re.escape(filename)

# create the output directory
command = " ".join(["mkdir -p", archdir])
subprocess.check_call(command, shell=True, stdout=sys.stdout)

if not options.skip_copy:
    # tar up small files in this directory to Archive area, one correlator pass at
    # a time
    for targroup in tarlists.keys():
        if tarlists[targroup]:
            tarfile = targroup + ".tar"
            taritup(archdir, tarfile, tarlists[targroup])
    
    # transfer each of the large files in turn
    print ("copying files")
    for srcfile in transfer:
        command = " ".join(["cp", "-l", srcfile, archdir])
        if options.verbose:
            print ("\n" + command)
        subprocess.check_call(command, shell=True, stdout=sys.stdout)
    
    # now tar up the clocks subdirectory
    if os.path.exists("clocks"):
        taritup(archdir, "clocks.tar", "clocks")

# and the mark4 output dir
if mark4file:
    mark4tarfile = expname.upper()+".MARK4.tar.gz"
    if not options.skip_copy:
        taritup(archdir, mark4tarfile, mark4file, gzip=True)
    publish.append(mark4tarfile)

# now transfer the lot to data.pawsey.org.au
try:
    # create the parent directory if required
    command = " ".join(["pshell", '"', "mkdir", pawseydir, '"'])
    print (command)
    subprocess.check_output(command, shell=True)
except:
    # pshell throws an error if mkdir already exists
    print (
            "Parent directory exists or cannot be created: {}".format(
            pawseydir))
else:
    print ("Created: {}".format(pawseydir))

try:
    # put the contents of archdir
    command = " ".join(
            ["pshell", '"', "cd", pawseydir, "&& put", archdir, '"'])
    print (command)
    subprocess.check_call(
            command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
except KeyboardInterrupt:
    raise Exception("Forced quit")
except:
    print ("pshell transfer failed - check if your delegation expired")
    print ("Contents of {} have not been deleted".format(archdir))
    traceback.print_exc()
else:
    # publish the public files
    try:
        for srcfile in publish:
            # unpublish first to work round mediaflux versioning bug for
            # updated files
            command = " ".join(
                    ["pshell", '"', "cd", pawseydir + os.sep + expname,
                    "&& unpublish", srcfile, '"'])
            subprocess.check_call(
                    command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
            command = " ".join(
                    ["pshell", '"', "cd", pawseydir + os.sep + expname,
                    "&& publish", srcfile, '"'])
            subprocess.check_call(
                    command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
    except:
        print ("Files not made public - please use web portal")
        traceback.print_exc()
    if not options.keeparch:
        shutil.rmtree(archdir)
    print ("All done!")
