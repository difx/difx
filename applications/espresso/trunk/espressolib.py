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
    timeformats = [vexformat, vlbaformat, isoformat]

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


