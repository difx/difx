#!/usr/bin/python
# Some functions used by the Espresso scripts
# Cormac Reynolds, original version: 2011 November 22
import re, os, fcntl, mx.DateTime, smtplib, sys
from email.mime.text import MIMEText

def get_corrhosts(hostsfilename):
    '''Parse the host list file. Return a dict where the key is the hostname.
    The values are a list. The first element of the list is the number of
    threads. The second element is a list of data areas.'''

    hostsfile = open(hostsfilename, 'r').readlines()

    hosts = dict()
    version = int()
    for line in enumerate(hostsfile):
        linenum, line = line
        line.strip()
        # strip comments
        line = re.sub(r'#.*','', line)

        if linenum == 0:
            # first line should be the version number
            try:
                version = re.match(r'version\s*=\s*(\d+)', line).group(0)
            except:
                raise Exception('First line must be version number!')
            continue
            if version != 1:
                print "Warning: version number in $DIFX_MACHINES is not 1. This may not work as expected"

        hostdata = line.split(',')
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
            raise Exception(" ".join(['Badly formatted file:', hostsfilename, str(linenum)]) )

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
                filepath = re.match('file://(.*)', url)
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
    '''expand a string that may contain a pattern into a list of all
    strings that match the pattern'''

    outputstrs = [inputstr]
    globpattern = '\[.*?\]'
    for patternmatch in re.finditer(globpattern, inputstr):
        newoutputstrs = []
        startrange = False
        pattern = inputstr[patternmatch.start()+1 : patternmatch.end()-1]
        for outputstr in outputstrs:
            lastchar = str()
            for character in pattern:
                if character != '-' and not startrange:
                    newoutputstrs.append(re.sub(globpattern, character, outputstr, 1))
                    lastchar = character
                elif startrange:
                    startrange = False
                    for i in range(ord(lastchar)+1, ord(character)+1):
                        newoutputstrs.append(re.sub(globpattern, chr(i), outputstr, 1))

                elif character == '-':
                    startrange = True
        outputstrs = newoutputstrs

    return outputstrs

def expand0int(inputstr):
    '''expand a string that may contain a range of zero-padded integers into
    a list of all strings that match the given integer range'''

    outputstrs = [inputstr]
    globpattern = '\[(.*?)\]'
    #for patternmatch in re.finditer(globpattern, inputstr):
    int_range = re.search(globpattern, inputstr)
    if int_range:
        outputstrs = []
        startrange, endrange = int_range.group(0)[1:-1].split('-')
        for i in range(int(startrange), int(endrange)+1):
            outputstrs.append(re.sub(globpattern, str(i).zfill(len(startrange)), inputstr))

    return outputstrs


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
    '''converts indate from mjd to vex or vice versa. Deprecated: use convertdate instead'''

    vexformat = '%Yy%jd%Hh%Mm%Ss'
    try:
        outdate = mx.DateTime.strptime(indate, vexformat).mjd
    except:
        try:
            gregdate = mx.DateTime.DateTimeFromMJD(float(indate))
            outdate = gregdate.strftime(vexformat)
        except:
            raise Exception("Accepts dates only in VEX format, e.g. 2010y154d12h45m52s, or MJD")

    return outdate

def convertdate(indate, outformat='mjd'):
    '''converts between DiFX date formats (mjd, vex, iso, vlba)'''
    
    # MJD is a float for input, and built in to the module for output.
    # ISO8601 is built in to the module, but the output format lacks the 'T', so use our own format
    vexformat = '%Yy%jd%Hh%Mm%Ss'
    vlbaformat = '%Y%b%d-%H:%M:%S'
    isoformat = '%Y-%m-%dT%H:%M:%S'

    # vex format can truncate from the right, have 2 digits for the year, and
    # decimal seconds
    vexformat_in = vexformat
    if type(indate) is str and 'y' in indate:
        if indate[2] == 'y':
            vexformat_in = vexformat_in.replace('%Y', '%y')
        vexlen = 3
        if 'd' in indate:
            vexlen = 6
            if 'h' in indate:
                vexlen = 9
                if 'm' in indate:
                    vexlen = 12
                    if 's' in indate:
                        vexlen = 15
        vexformat_in = vexformat_in[0:vexlen]
        if '.' in indate:
            vexformat_in = vexformat_in.replace('s', '.%fs')

    timeformats = [vexformat_in, vlbaformat, isoformat]


    # convert the input time to a datetime

    date = None
    try:
        # MJD?
        date = mx.DateTime.DateTimeFromMJD(float(indate))
    except ValueError:
        # other formats
        for timeformat in timeformats:
            try:
                date = mx.DateTime.strptime(indate, timeformat)
                break
            except ValueError:
                pass

    if not date:
        raise Exception("Accepts dates only in MJD, Vex, VLBA or ISO8601 formats")

    if outformat == 'mjd':
        outdate = date.mjd
    elif outformat == 'iso':
        outdate = date.strftime(isoformat)
    elif outformat == 'vlba':
        outdate = date.strftime(vlbaformat).upper()
    elif outformat == 'vex':
        outdate = date.strftime(vexformat)
    else:
        raise Exception("Output format not recognised, choose from: mjd, vex, iso or vlba")

    return outdate
    
#def email(user, passwd, message):
#    '''Simple gmail notification message'''
#
#    msg = MIMEText(message)
#
#    msg['Subject'] = 'Correlator notification'
#    msg['From'] = user
#    msg['To'] = user
#
#    server = smtplib.SMTP('smtp.gmail.com:587')  
#    server.starttls()  
#    server.login(user,passwd)  
#    server.sendmail(user, user, msg.as_string())  
#    server.quit()  

class Email:
    '''Simple gmail notification message. Logs in to gmail server of the given account and sends an email to that same account'''

    def __init__(self, user, passwd):
        self.user = user
        self.passwd = passwd

    def connect(self):
        '''Set up the connection to the SMTP server'''
        self.server = smtplib.SMTP('smtp.gmail.com:587')  
        self.server.starttls()  
        try:
            self.server.login(self.user, self.passwd)  
        except Exception as connectionError:
            print connectionError
            raise 


    def sendmail(self, message):
        '''Send the given message via the already prepared SMTP connection'''
        msg = MIMEText(message)
        msg['Subject'] = 'Correlator notification'
        msg['From'] = self.user
        msg['To'] = self.user
        self.server.sendmail(self.user, self.user, msg.as_string())  

    def disconnect(self):
        '''Quit the SMTP connection'''
        self.server.quit()  


class Msg_Filter:
    """ writes output to a file (or similar object) after filtering. First
    argument is the desired output object, remainder are the strings to
    filter on. Should itself look like a file object. Filter is
    case-insensitive"""

    def __init__(self, file, *args):
        self._file = file
        self._filters = args

    def write(self, string):
        for filter_text in self._filters:
            if filter_text.lower() in string.lower():
                self._file.write (string)
                break
        self.flush()

    def flush(self):
        self._file.flush()

    def fileno(self):
        self._file.fileno()
