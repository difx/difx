#! /usr/bin/python
# coding: latin-1

#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

import gzip
import pexpect
import os
import sys
import re
import time
import datetime
import shutil
import subprocess
import getpass
import glob
import shutil
import logging
from optparse import OptionParser
from string import lower, strip
from os.path import isdir
from subprocess import Popen, PIPE
try:
    from difxdb.business.versionhistoryaction import *
    from difxdb.difxdbconfig import DifxDbConfig
    from difxdb.business.experimentaction import *
    from difxdb.model.dbConnection import Schema, Connection
except:
    sys.exit("No DiFX version has been selected or the difxdb libraries haven't been installed")


__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision$"
__date__ ="$Date$"
__lastAuthor__="$Author$"

minSchemaMajor = 1
minSchemaMinor = 1
krbDomain = "MPIFR-BONN.MPG.DE"

fringeDir = "FRINGE"
filesDir = "FILES"
exportDir = "EXPORT"
versionPrefix = "v"
tmpDir = "tmp_difxarchive"
logger = None

def getUsage():
    
    usage = ""
    usage += "%s   %s  %s (last changes by %s) \n" % (__prog__, __build__, __author__, __lastAuthor__)
    usage += "A program to archive a correlated difx experiment.\n"
    usage += "Usage: %s <expcode> <path> \n\n"  % __prog__
    usage += "<expcode>:   the experiment code e.g. ey010a\n"
    usage += "<path>:  the path to the experiment directory\n"
    usage += "NOTE: %s requires the DIFXROOT environment to be defined." % __prog__
    usage += "The program reads the database configuration and other parameters from difxdb.ini located under $DIFXROOT/conf."

    
    return(usage)

def exitOnError(exception):
	'''
	Exit routine to be called whenever an error/exception has occured
	'''
        print exception
        logger.error(exception)
	
        # destroy kerberos tickets
        destroyTicket()

        logger.info("Aborting")
        
	exit(1)

def getTicket(user):
    
    logger.info ("Obtaining kerberos ticket for user %s" % user)
    password = getpass.getpass("Enter password for user %s:" % (user))
    
    kinit = '/usr/bin/kinit'
    kinit_args = [ kinit, '%s@%s' % (user,krbDomain) ]
    kinit = Popen(kinit_args, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    kinit.stdin.write('%s\n' % password)
    kinit.wait()


def destroyTicket():
    logger.info ("Destroying kerberos ticket" )
    subprocess.call('/usr/bin/kdestroy')
    

def readConfig():
    
    
    if (os.getenv("DIFXROOT") == None):
        exitOnError("DIFXROOT environment must be defined.")
        #sys.exit("Error: DIFXROOT environment must be defined.")
            
    configName = os.getenv("DIFXROOT") + "/conf/difxdb.ini"
        
    config = DifxDbConfig(configName)
    
    if not config.sectionExists("difxarchive"):
        
        logger.info ("Configuration file does not contain a section for difxarchive yet. Adding a default one for you. Please edit %s and restart difxarchive." % configName)
        config.addSection("difxarchive")
        config.set('difxarchive', 'archiveserver', 'ADD_ARCHIVE_SERVER')
        config.set('difxarchive', 'defaultuser', 'ADD_DEFAULT_USER')
        config.set('difxarchive', 'archiveremotepath', 'ADD_REMOTE_PATH')
        config.set('difxarchive', 'refbackuppath', 'ADD_PATH')
        
        config.writeConfig()
        exit(0)
    
    return (config)

def getTransferFileCount(source, destination, options=""):
	
    cmd = 'rsync -az --stats --dry-run %s %s %s' % ( options, source, destination) 
    proc = subprocess.Popen(cmd,
                                       shell=True,
                                       stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE,
                                       )
    
    remainder = proc.communicate()[0]
    
    matchTotal = re.findall(r'Number of files: (\d+)', remainder)
    totalCount = int(matchTotal[0])
    mn = re.findall(r'Number of files transferred: (\d+)', remainder)
    fileCount = int(mn[0])
    
#    print "Number of files to be transferred: %d " % fileCount
    
    return(totalCount, fileCount)

def syncDir(path, user, config, fileCount):
    
    server = config.get("difxarchive", "archiveserver")
    remotePath = config.get("difxarchive", "archiveremotepath")
    
    logger.info( "Syncing files from %s to: %s" % (path, server))
    
    cmd = 'rsync -av  --progress %s %s@%s:%s' % ( path, user, server, remotePath) 
        
    proc = subprocess.Popen(cmd,
                                       shell=True,
                                       stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE,
                                       )
    
    while True:
        output = proc.stdout.readline()

        if not output:
            break
            
        if 'to-check' in output:
            
             m = re.findall(r'to-check=(\d+)/(\d+)', output)
             progress = (100 * (int(m[0][1]) - int(m[0][0]))) / fileCount
             sys.stdout.write('\rDone: %s %% ' % progress)
             #sys.stdout.write('\rRemaining: %s / %s' % (m[0][0], m[0][1]) )
             sys.stdout.flush()
             
             if int(m[0][0]) == 0:
                      break
                      
    sys.stdout.write('\n')
    return

def buildReferenceOptions():
    
    includePattern = ["*.vex", "*.obs", "*.skd", "*.v2d", "*.input", "*.difxlog", "*.log", "cf_*", "rf_*"]
    
    cmd = " --include '*/' "
    for pattern in includePattern:
        cmd += " --include '%s' " % pattern
    
    cmd += " --exclude '*' --exclude '*.difx' "
    
    return(cmd)
    
    
def syncReferenceDir(path, referencePath, fileCount, options):
    
    
    # check that destination path has trailing slash
    if not referencePath.endswith(os.path.sep):
        referencePath += os.path.sep
        
    # check that source path has NO trailing slash
    if  path.endswith(os.path.sep):
        path = path[:-1]
     
    cmd = "rsync -av --progress " + options
    
    cmd += path + " " + referencePath
    
    logger.info( "Syncing reference files from %s to: %s" % (path, referencePath))
    
    proc = subprocess.Popen(cmd,
                                       shell=True,
                                       stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE,
                                       )
    
    while True:
        output = proc.stdout.readline()
        
        if not output:
            break
            
        if 'to-check' in output:
             m = re.findall(r'to-check=(\d+)/(\d+)', output)
             progress = (100 * (int(m[0][1]) - int(m[0][0]))) / fileCount
             sys.stdout.write('\rDone: %s %% ' % progress)
             sys.stdout.flush()

             if int(m[0][0]) == 0 :
                      break
    
    sys.stdout.write('\n')
    return

def packDirectory(rootPath, packDir, outDir, filename):

    zipFile = "%s/%s.gz" % (outDir, filename)

    baseDir = os.path.dirname(rootPath)
    expDir = os.path.basename(rootPath)

    if len(baseDir) == 0:
        baseDir = "."

    #print "baseDir=%s expDir=%s" % (baseDir, expDir)

    # if packDir contains a list of files the path needs to be modified
    if type(packDir) == list:
        tmp = ""
        for file in packDir:
            tmp += "%s/%s " % (expDir, file)
        packDir = tmp
    else:
        packDir = "%s/%s" % (expDir, packDir)

    filename = "%s/%s" % (outDir, filename)

    #print rootPath, packDir, outDir, filename


    if os.path.isfile(filename):
        exitOnError("File already exists (%s)" % (filename))
    if os.path.isfile(zipFile):
        exitOnError("File already exists (%s)" % (zipFile))

    errorCount = 0

    logger.info("Creating tar file %s" % (filename))
    # create tar file
    command = "tar -cWvf %s %s" % (filename, packDir)
    #print command
    os.chdir(baseDir)
    #print baseDir
    child = pexpect.spawn(command, logfile = sys.stdout)
    child.expect([pexpect.EOF,pexpect.TIMEOUT])

    logger.info("Finished writing tar file %s" % (filename))

    logger.info("Zipping tar file %s" % (zipFile))
    try:
        with open(filename, 'rb') as f_in, gzip.open(zipFile, 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)
    except:
        exitOnError("An error has occured while trying to zip %s" % (filename))
    
    if not os.path.isfile(zipFile):
        exitOnError("The zip file has not been created: %s" % (zipFile))

    # now remove the tar file
    if os.path.isfile(filename):
        os.remove(filename)
    
    # zipping the tar file
    #errorCount = 0
    #p = subprocess.Popen(['gzip', filename], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #for line in iter(p.stderr.readline,''):
    #    logger.error(line.rstrip())
    #    errorCount +=1
    #if errorCount > 0:
    #    exitOnError("An error has occured while trying to zip %s" % (filename))
#
#    for line in iter(p.stdout.readline,''):
#        logger.info(line.rstrip())
    logger.info("Finished zipping tar file %s" % (zipFile))

def setupLoggers(logPath):
    
    global logger 

    logger = logging.getLogger("difxarchive")
    logger.setLevel(logging.INFO)
    # logging to file
    
    fh = logging.FileHandler(logPath + "/difxarchive.log")
    fh.setLevel(logging.INFO)
    fh.setFormatter(logging.Formatter('%(asctime)s - %(levelname)-7s - %(message)s'))

    # console handler
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO) 
    ch.setFormatter(logging.Formatter('%(levelname)-7s - %(message)s'))

    logger.addHandler(fh)
    logger.addHandler(ch)

    
def confirmAction():
    
     # if --force option was used skip confirmation
    if not options.force:
            
        print 'Are you sure you want to proceed? [y/N]'
        a = lower(sys.stdin.readline())
        if strip(a) == 'y':
            print 'OK -- proceeding\n'
        else:
            print 'Not continuing.\n'

            # destroy kerberos tickets
            destroyTicket()
            exit(0)

def confirmArchiveDirs(archiveDirs):
    while True:
        print "-------------------------------------------------"
        print "Will archive all files in the top-level directory"
        print "as well as the following directories: ", ' ' .join(archiveDirs)
        print "-------------------------------------------------"
        while True:
            ret = raw_input("Proceed using this selection? [y/n] (n for making changes): ")
            ret = lower(ret.strip())
            if (ret == "n"):
                change = True
                break 
            elif (ret == "y"):
                change = False
                break

        if change:
            ret = raw_input("Specify directories to be archived (space separated list): ")
            archiveDirs = ret.split()
        else:
            break

    return archiveDirs

    
def getArchiveDirs(path):

    archiveDirs = []
    prodDirs = []
    searchStr = "%s(\d+)" % versionPrefix
    version = 0
    reProdDir = re.compile(searchStr)

    # find all files and subdirectories under path
    for dirpath, dirnames, filenames in os.walk(path):
        # inspect the top level directories
        if fringeDir in dirnames:
            archiveDirs.append(fringeDir)
        if filesDir in dirnames:
            archiveDirs.append(filesDir)
        if exportDir in dirnames:
            archiveDirs.append(exportDir)

        # find production run directory with highest version count
        for dir in dirnames:
            match = reProdDir.match(dir)
            if match:
                if match.group(1) > version:
                    prodDirs = []
                if match.group(1) >= version:
                    prodDirs.append(dir)
                    version = match.group(1)

        for dir in prodDirs:
            archiveDirs.append(dir)
    
        # remove archive directory from list of candidate directories
        if tmpDir in dirnames:
            dirnames.remove(tmpDir)


        print "-------------------------------------------"
        print "Found the following top-level directories: ", ' ' . join(dirnames)
        archiveDirs = confirmArchiveDirs(archiveDirs)
        return archiveDirs, filenames

    
def cleanup():

    logging.shutdown()
    time.sleep(1)
    tmpPath = "%s/%s" % (path, tmpDir)
    if os.path.isdir(tmpPath):
        # remove the tmp_difxlog directory and all its subdirs
        shutil.rmtree(tmpPath, ignore_errors=False)
    
if __name__ == "__main__":

    usage = getUsage()
    
    parser = OptionParser(version="%prog " + __build__, usage=usage)
    
    parser.add_option("-u", "--user", dest="user", type="string" ,action="store", help="Do the archival as the specified user. This overrides the defaultuser directive in difxdb.ini")
    parser.add_option("-f", "--force", dest="force" ,action="store_true", default=False, help="Delete files without further confirmation ")
    parser.add_option("-a", "--all", dest="all" ,action="store_true", default=False, help="Backup all files and directories.")
    parser.add_option("-D", "--db-only", dest="dbOnly" ,action="store_true", default=False, help="Update database only, don't copy files (use with care!) ")
    parser.add_option("-k", "--keep", dest="keep" ,action="store_true", default=False, help="Keep files on local disk after archiving.")
    # parse the command line. Options will be stored in the options list. Leftover arguments will be stored in the args list
    (options, args) = parser.parse_args()   
     
    if len(args) < 2 :
	parser.print_help()
	exit(0)

    code = lower(args[0])
    path = args[1]

    # remove a trailing slash if it exists
    if path[-1:] == "/":
	path = path[0:-1]

    
    if not options.dbOnly:
        # check that path exists
        if not isdir(path):
            sys.exit("Directory %s does not exist" % (path))

    
    # create temporary directories for holding the archival products
    archiveDir = "%s/%s/%s" % (path, tmpDir, code)
    os.mkdir(path + "/" + tmpDir)
    os.mkdir(archiveDir)

    # setup the console and file logger
    logPath = "%s/%s" % (path, tmpDir)
    setupLoggers(logPath)

    logger.info("Starting difxarchive") 

    # try to open the database connection
    config = readConfig()
    connection = Connection(config)
    connection.echo = False
    
    if options.user is None:
       user = config.get("difxarchive", "defaultuser")
    else:
       user = options.user

    try:
        dbConn = Schema(connection)
        session = dbConn.session()
    except Exception as e:
        exitOnError(e)

    # check that experiment exists in the database
    if not experimentExists(session, code):
        exitOnError("Experiment with code %s not found in the database." % code)
        
    # check if the experiment has already been archived
    if isExperimentArchived(session, code):
        print "Experiment has been archived already."
        confirmAction()
    
    if not isSchemaVersion(session, minSchemaMajor, minSchemaMinor):
        major, minor = getCurrentSchemaVersionNumber(session)
        exitOnError("Current difxdb database schema is %s.%s but %s.%s is minimum requirement." % (major, minor, minSchemaMajor, minSchemaMinor))
    
    session.close()
    
    server = config.get("difxarchive", "archiveserver")
    remotePath = config.get("difxarchive", "archiveremotepath")
    
    destination = user + "@" + server + ":" + remotePath

    try:
        if not options.dbOnly:
	
            # obtain kerberos ticket
            getTicket(user)
                
            # construct the list of files and directories to be archived
            archiveDirs, filenames = getArchiveDirs(path)

            # pack the directories
            #print archiveDir, archiveDirs
            for dir in archiveDirs:
                logger.info("Now packing %s" % dir)
                packDirectory(path, dir, archiveDir, dir + ".tar")
                logger.info("Done packing %s" % dir)
            # now pack the top-level files
            packDirectory(path, filenames, archiveDir, "main.tar")

            # Now sync the tar files to the backup server
            passCount = 0
            while True:
                total, fileCount = getTransferFileCount(archiveDir, destination)
                if (fileCount == 0):
                    break

                # copy files to the archive server
                syncDir(archiveDir, user, config, total )
                
                logger.info ("Pass %d. Syncing %d files" % (passCount, fileCount))
                passCount +=1
            logger.info("Finished")
                

            # Now copy the reference files
            destDir = "%s/%s" % (config.get("difxarchive", "refbackuppath"), code)
            if not os.path.isdir(destDir):
                os.mkdir(destDir) 

            for dir in archiveDirs:
                srcDir = "%s/%s" % (path, dir)
                syncOptions = buildReferenceOptions()
                passCount = 0
                while True:
                    total, fileCount = getTransferFileCount(srcDir, destDir, syncOptions)
                    if (fileCount == 0):
                        break
                    syncReferenceDir(srcDir, destDir, total, syncOptions)

                    logger.info ("Pass %d. Syncing %d files" % (passCount, fileCount))
                    passCount +=1

            # copy all files in filesDir
            passCount = 0
            while True:
                srcDir = "%s/%s" % (path, filesDir)
                if not os.path.isdir(srcDir):
                    break
                syncOptions = "--exclude '*/*/' --include '*' " 
                total, fileCount = getTransferFileCount(srcDir, destDir, syncOptions)
                if (fileCount == 0):
                    break
                syncReferenceDir(srcDir, destDir, total, syncOptions)
                logger.info ("Pass %d. Syncing %d files" % (passCount, fileCount))
                passCount +=1

            # copy all top-level files
            passCount = 0
            while True:
                srcDir = "%s/*" % (path)
                syncOptions = "--exclude '*/' --exclude 'difxarchive.log' --include '*' " 
                total, fileCount = getTransferFileCount(srcDir, destDir, syncOptions)
                if (fileCount == 0):
                    break
                syncReferenceDir(srcDir, destDir, total, syncOptions)
                logger.info ("Pass %d. Syncing %d files" % (passCount, fileCount))
                passCount +=1

        # update database
        session = dbConn.session()
        experiment = getExperimentByCode(session, code)
        experiment.dateArchived = datetime.datetime.now()
        experiment.archivedBy = user
        session.commit()
        session.flush()
        session.close()
        logger.info("Updated database status")

        # finally upload difxarchive.log
        shutil.copy(logPath + "/difxarchive.log", archiveDir)
        syncDir(archiveDir, user, config, 1)
        syncReferenceDir(logPath + "/difxarchive.log", destDir, 1, "")

        if not options.dbOnly and not options.keep:
            # delete files
            print 'Archival process completed. Now deleting path %s including all files and subdirectories' % path
            confirmAction()
            logger.info("Deleted %s" % (path))
#
            shutil.rmtree(path, ignore_errors=True)
        
        

    except Exception as e:
        exitOnError(e)
    except KeyboardInterrupt:
        sys.exit(1)
    finally:
        # destroy kerberos tickets
        destroyTicket()
        logger.info ("Done")
        cleanup()
        
    
   
