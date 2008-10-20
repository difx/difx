#!/usr/bin/python
"""
Functions for downloading and parsing eop parameters

TODO Work out the duration of the observation (in days) automatically
"""

import sys
import os.path
import getopt
from urllib import urlopen, urlretrieve

import observation
from pfile import print_parameter
from astro import jd2mjd, mjd2jd, mjd2datetime

def compare_eop_headers(eop_path, eop_url):
    """
    Compare headers of local and remote file.

    if local file cannot be found/read return false
    if remote file cannot be found raise Error
    if local file and remote file headers don't match return False
    if local file and remote file headers do match return True
    """
    print 'reading local eop file header'
    try:
        leop = open(eop_path, 'r')
        lheader = leop.readline()
        leop.close()
    except IOError:
        print 'valid local eop file could not be found at ' + eop_path
        return False

    print 'reading remote eop file header'
    try:
        reop = urlopen(eop_url)
        rheader = reop.readline()
        reop.close()
    except:
        print "Can't read remote file"
        raise
    if lheader == rheader:
        print 'remote and local files are the same version'
        return True
    else:
        return False

def get_eop_files(path = None, eop_url = None, iat_url = None, force = False):
    """
    Download two files from which the EOPs are generated.
    
    As default the values path, eop_url and iat_url are read from observation.py
    
    The file names are taken from the urls.

    the file at iat_url is downloaded each time.

    the file at eop_url is downloaded only if the header of the file 
    is different from the file currently at path. Or if force == True.

    Returns the paths to the two files as a tuple:
    (eop_path, iat_path)
    """ 
    if not path:
        path = observation.eop_path
    if not eop_url:
        eop_url = observation.eop_url
    if not iat_url:
        iat_url = observation.iat_url

    eop_file = os.path.basename(eop_url)
    iat_file = os.path.basename(iat_url)
    eop_path = os.path.join(path, eop_file)
    iat_path = os.path.join(path, iat_file)
    
    print 'downloading iat file ' + iat_url
    print 'to                   ' + iat_path
    urlretrieve(iat_url, iat_path)
    
    if not compare_eop_headers(eop_path, eop_url) or force:
        print 'downloading eop file ' + eop_url
        print 'to                   ' + eop_path
        try:
            urlretrieve(eop_url, eop_path)
        except:
            raise RuntimeError, 'Error downloading eop file'
    return eop_path, iat_path

def create_eop_list(mjd, eop_file, iat_file, extra = 2, ndays = 0):
    """
    Read eop and iat files and create a simple array of the parameters
    in units correct for the calc file
    """
    try:
        eop_file = open(eop_file, 'r')
    except IOError:
        print 'Error: Valid local eop file could not be found at ' + eop_file
        print '       Try running with --download flag'
        raise
    try:
        iat_file = open(iat_file, 'r')
    except IOError:
        print 'Error: Valid local iat file could not be found at ' + iat_file
        print '       Try running with --download flag'
        raise
    jd = mjd2jd(mjd)
    last = 0.
    for line in iat_file:
        line = line.split()
        if float(line[4]) > jd - extra:
            tai_utc1 = last
            if float(line[4]) < jd + ndays + extra:
                print "WARNING: leap second near observation"
                print "         edit .calc file by hand"
            break
        last = float(line[6])
    if not tai_utc1:
        raise RuntimeError, "invalid utc file"

    timemjd = []
    tai_utc = []
    ut1_utc = []
    xpole = []
    ypole = []

    #skip header
    header = eop_file.readline()

    for line in eop_file:
        if line.startswith('#'):
            continue
        line = line.split()
        jd1 = float(line[0])
        if (jd - extra <= jd1 <=jd + ndays + extra):
            timemjd.append(jd2mjd(float(line[0])))
            tai_utc.append(tai_utc1)
            ut1_utc.append((float(line[3]) / 1000000.) + tai_utc1)
            xpole.append(float(line[1]) / 10.)
            ypole.append(float(line[2]) / 10.)
            continue
        if jd1 > jd + ndays + extra:
            return timemjd, tai_utc, ut1_utc, xpole, ypole
    raise RuntimeError, 'error with eop file'

def create_eop_table(mjd, eop_file, iat_file, calcfile, extra = 2, ndays = 0):
    """
    call create_eop_list and print a calc table to calcfile
    """
    print 'Writing EOP table of calc file'
    eop_array = create_eop_list(mjd, eop_file, iat_file, extra, ndays)
    num = len(eop_array[0])
    print_parameter("NUM EOP", str(num), calcfile, 22)
    for i in range(num):
        print_parameter('EOP ' + str(i) + ' TIME (mjd)',     '%.6f' % eop_array[0][i], calcfile, 22)
        print_parameter('EOP ' + str(i) + ' TAI_UTC (sec)',  '%.0f' % eop_array[1][i], calcfile, 22)
        print_parameter('EOP ' + str(i) + ' UT1_UTC (sec)',  '%.6f' % eop_array[2][i], calcfile, 22)
        print_parameter('EOP ' + str(i) + ' XPOLE (arcsec)', '%.6f' % eop_array[3][i], calcfile, 22)
        print_parameter('EOP ' + str(i) + ' YPOLE (arcsec)', '%.6f' % eop_array[4][i], calcfile, 22)

def write_eops(mjd, calcfile, extra = None, download = None, force = None, ndays = 0):
    """
    Write eops in calcfile format to calcfile
    
    calcfile is an open file object
    """
    if extra == None:
        extra = observation.eop_extra
    if download == None:
        download = observation.eop_download
    if force == None:
        force = observation.eop_force
    if download:
        eop_file, iat_file = get_eop_files(force = force)
    else:
        eop_file = os.path.join(observation.eop_path, os.path.basename(observation.eop_url))
        iat_file = os.path.join(observation.iat_path, os.path.basename(observation.iat_url))
    create_eop_table(mjd, eop_file, iat_file, calcfile, extra, ndays)

def main():
    """
Write a calc EOP table to stdout

Usage:
    eop.py mjd 
Options:
-e  --extra extra
-d  --download 
-f  --force
-n  --ndays 

--extra is the number of eops to extract on either side of mjd. E.g. extra = 5
will result in 11 eops centred on mjd

--download means that a new set of eops are downloaded

--force forces a download of the eop file, even if the header hasn't changed.

--ndays allows you to specify the length of the observation
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "b:dfn:", ["buffer=", "download", "force", "ndays="])
    except getopt.GetoptError, err:
        print 'Error: ' + err
        print main.__doc__
        sys.exit(2)
    if not len(args) == 1:
        print 'Error: Wrong number of arguments.'

    # set defaults
    extra = None
    download = None
    force = None
    ndays = 0

    # read arguments
    mjd = float(args[0])

    for o, a in opts:
        if o in ("-b", "--buffer"):
            extra = int(a)
        elif o in ("-d", "--download"):
            download = True
        elif o in ("-f", "--force"):
            force = True
        elif o in ("-n", "--ndays"):
            ndays = int(a)

    write_eops(mjd, sys.stdout, extra, download, force, ndays)

if __name__ == "__main__":
    main()
