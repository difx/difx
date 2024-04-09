#! /usr/bin/env python3
# coding: latin-1


import argparse
import os
import sys
import subprocess
import ftplib
import pycurl
import fnmatch
from datetime import datetime, timedelta
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.business.experimentaction import *
from difxdb.model.dbConnection import Schema, Connection
from io import BytesIO

fringeDir="FRINGE"
exportDir="EXPORT"
filesDir="FILES"
snrDir = "SNR"
versionPrefix="v"

geoFTPServer = "ivs.bkg.bund.de"
    
def curlDownload (url, remotePath, patterns, localPath):

  data = BytesIO()
  c = pycurl.Curl()

  baseurl = "ftp://%s/%s" % (geoFTPServer,remotePath)
  print (url)
  c.setopt(c.URL, baseurl)
  c.setopt(c.USERNAME, 'anonymous')
  c.setopt(c.USERPWD, 'anonymous')
  c.setopt(c.WRITEFUNCTION, data.write)
  c.setopt(c.USE_SSL, True)
#  c.setopt(c.VERBOSE, True)

  try:
    c. perform()
  except pycurl.error as e:
    print ("No experiment files found at %s. Skipping." % (baseurl))
    return()
    
  c.close()

  resp = data.getvalue().decode('iso-8859-1')

  for pattern in patterns:
    filtered = fnmatch.filter(resp.split(), pattern)
    for file in filtered:
      fp = open('%s/%s' % (localPath, file), "wb")
      url = baseurl + file
      print ("Downloading: %s from %s" % (file, url))
      c = pycurl.Curl()
      c.setopt(c.URL, url)
      c.setopt(c.USERNAME, 'anonymous')
      c.setopt(c.USERPWD, 'anonymous')
      c.setopt(c.WRITEDATA, fp)
      c.setopt(c.USE_SSL, True)
      #c.setopt(c.VERBOSE, True)
      c. perform()
      c.close()
      fp.close()

  return

def ftpDownload(url, remotePath,localPath):

    ftp = ftplib.FTP(url, 'anonymous', 'anonymous')
    ftp.set_pasv(1)
    filelist = []

    try:
      filelist = ftp.nlst(remotePath)
    #except ftplib.error_perm, resp:
    except ftplib.error_temp as e:
        print (e)

    for remotefile in filelist:
        file = os.path.basename(remotefile)
        localFile=localPath+"/{}".format(file)
        with open(localFile, 'wb') as f:
            print ("Downloading: %s from %s" % (file, url))
            ftp.retrlines("RETR %s" % remotefile, lambda s, w = f.write: w(s +'\n'))

def makeSummary(code):
    with open(args.path + '/SUMMARY', 'w+') as f:
        cmd = ['difxdbExperiment', code]
        p = subprocess.Popen(cmd, stdout=f)
        p.communicate()

def makeTapelogObs(code):
    with open(args.path + '/TAPELOG_OBS', 'w+') as f:
        cmd = ['difxdbTapelog', code]
        p = subprocess.Popen(cmd, stdout=f)
        p.communicate()

def expUpdate():

    # check if experiment path exists already
    if not os.path.isdir(args.path):
        sys.exit("ERROR: directory %s does not exist. Run without -u option to create it." % args.path)

    # obtain experiment code
    codeFile = open(args.path + "/.code")
    code = codeFile.read()
    # create summary file with experiment details
    makeSummary(code)
    print ("Updated %s/SUMMARY" % (args.path) )

    #create TAPELOG_OBS file from database contents
    makeTapelogObs(code)
    print ("Updated %s/TAPELOG_OBS" % (args.path) )
    return

def expFinalize():
  return

def createDir(path):
      try:
          os.mkdir(path)
      except:
          sys.exit("ERROR: Cannot create directory (%s)" % path)


def expPrepare(code):

    # check if experiment code exists in the database
    session = dbConn.session()
    while True:
        if not experimentExists(session, code):
            print ("The database contains no entry for experiment code: %s" % (code))
            print ("Enter a valid experiment code to be used for the directory: %s" % (args.path))
            code = input("Code [return to exit]: ").upper()
            if not code:
                session.close()
                sys.exit()
        else:
            session.close()
            break

    # check if experiment path exists already
    if os.path.isdir(args.path):
        sys.exit("ERROR: experiment directory (%s) already exists. (either delete it or use -u option to update it)" % args.path)

    # if not create it
    try:
        os.mkdir(args.path)
    except:
        sys.exit("ERROR: Cannot create experiment directory (%s)" % args.path)

    # create subdirectories
    dirs = [fringeDir,  exportDir, snrDir, versionPrefix+"1"]
    for dir in dirs:
      path=args.path + "/" + dir
      createDir(path)

    # create hidden file containing the experiment code
    f = open (args.path + "/.code", "w+")
    f.write(code)
    f.close()
    
    # create summary file with experiment details
    makeSummary(code)

    #create TAPELOG_OBS file from database contents
    makeTapelogObs(code)

    # create dummy README file in the experiment directory
    out = open (args.path + '/README', 'w+')
    out.write("-----------------------------------------------\n")
    out.write("General correlation notes for experiment %s" % (code))
    out.write("\n-----------------------------------------------\n")
    out.close()
    
    # create dummy README file in Version 1 subdirectory
    out = open (args.path + "/" + versionPrefix + '1/README', 'w+')
    out.write("---------------------------------------------\n")
    out.write("Contents: Correlation production run (Ver1)")
    out.write("\n---------------------------------------------\n")
    out.close()

    # obtain experiment files from FTP areas
    resp = ''
    while resp not in ['y','n']:
        resp = input ("Download observation files (vex, logs, eop etc.)? [y/n]")
    if resp.strip() == 'y':
        while True:
            year = input("year of observation? [YYYY]:")
            if year.isdigit() and len(year) == 4:
                break
            else:
                print ("illegal input. Try again.")
        while True:
            doy = input ("day of year of observation? :") 
            if doy.isdigit():
                break
            else:
                print ("illegal input. Try again.")
            
        obsDate = datetime.strptime(year + "-" + doy, "%Y-%j")
        obsStr = obsDate.strftime("%b%y").lower()
        eopStr = (obsDate+timedelta(days=-2)).strftime("%Y-%j")
        
        # get observation files
        os.chdir(args.path)
        os.system("getObsFiles %s %s" % (os.path.basename(args.path), obsStr))
        os.system("geteop.pl %s %d" % (eopStr, 5))

        # get geodetic files
        if not os.path.exists(filesDir):
            os.mkdir(filesDir)
        curlDownload(geoFTPServer, "/pub/vlbi/ivsdata/aux/%s/%s/"% (year, code.lower()), ["*.log", "*.skd", "*.vex", "*.txt"], filesDir)

    return

if __name__ == "__main__":

    description="A script to prepare and maintain the state of a DiFX correlation directory"
    parser = argparse.ArgumentParser(description=description)

    mode = parser.add_mutually_exclusive_group(required=False)
#    mode.add_argument("-i", "--init", action="store_true", help="prepare the initial experiment correlation directory")
    mode.add_argument("-u", "--update", action="store_true", help="update the experiment correlation directory")
    #mode.add_argument("-f", "--finalize", action="store_true", help="finalize the experiment correlation directory")

    parser.add_argument("path", help="the path of the experiment directory")

    try:
        args = parser.parse_args()
    except:
        parser.print_help()
        sys.exit(1)

    # obtain experiment code from the trailing part of the path
    code = os.path.basename(args.path).upper()

  
    dbConn = None
    settings = {}

    configName = 'difxdb.ini'

    #  check for DIFXROOT environment
    if (os.getenv("DIFXROOT") == None):
        sys.exit("Error: environment variable DIFXROOT must be defined.")
    settings["difxRoot"] = os.getenv("DIFXROOT")
    settings["configFile"] = settings["difxRoot"] + "/conf/" + configName



    # read the configuration file
    config = DifxDbConfig(settings["configFile"], create=False)
    connection = Connection()
    connection.type = config.get("Database", "type")
    connection.server = config.get("Database", "server")
    connection.port = config.get("Database", "port")
    connection.user = config.get("Database", "user")
    connection.password = config.get("Database", "password")
    connection.database = config.get("Database", "database")
    connection.echo = False

    dbConn = Schema(connection)

    if args.update:
        expUpdate()
    #elif args.finalize:
    #    expFinalize()
    else:
        expPrepare(code)

    print ("DONE")
