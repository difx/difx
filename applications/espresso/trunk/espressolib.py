#!/usr/bin/python
# Some functions used by the Espresso scripts
# Cormac Reynolds, original version: 2011 November 22
import re, os, fcntl, mx.DateTime

def get_corrhosts(hostsfilename):
    '''Parse the host list file. Return a dict where the key is the hostname.
    The values are a list. The first element of the list is the number of
    threads. The second element is a list of data areas.'''

    hostsfile = open(hostsfilename, 'r').readlines()

    hosts = dict()
    for line in hostsfile:
        line.strip()
        # strip comments
        line = re.sub(r'#.*','', line)
        hostdata = line.split(',')
        hostname = hostdata[0].strip()
        if not hostname:
            # empty line
            continue
        try:
            hostthreads = int(hostdata[1])
        except:
            hostthreads = 6
        try:
            hostdisks = hostdata[2].split()
        except:
            hostdisks = []


        hosts[hostname] = [hostthreads, hostdisks]

    return hosts

def which(program):
    '''Search $PATH for an executable, kind of like the unix 'which' command''' 

    for path in os.environ.get('PATH').split(os.pathsep):
        testpath = os.path.join(path, program)
        if os.access(testpath, os.X_OK):
            programpath = testpath
            return programpath

    return None

def openlock(filename):
    '''Open a file, then lock it'''
    lockedfile = open(filename, 'a')
    fcntl.flock(lockedfile.fileno(), fcntl.LOCK_EX)

    return lockedfile

def mjd2vex(indate):
    '''converts indate from mjd to vex or vice versa'''

    try:
        outdate = mx.DateTime.strptime(indate, '%Yy%jd%Hh%Mm%Ss').mjd
    except:
        try:
            gregdate = mx.DateTime.DateTimeFromMJD(float(indate))
            outdate = "%04dy%03dd%02dh%02dm%02ds" % (gregdate.year, gregdate.day_of_year, gregdate.hour, gregdate.minute, gregdate.second)
        except:
            raise Exception("Accepts dates only in VEX format, e.g. 2010y154d12h45m52s, or MJD")

    return outdate
    
    


