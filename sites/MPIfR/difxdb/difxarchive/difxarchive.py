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

import tarfile
import pexpect
import os
import sys
import re
import time
import datetime
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
logfile = ""
tmpPath = ""
code = ""
logger = None
tmpEnv = os.environ

def getUsage():
    
    usage = ""
    usage += "%s   %s  %s (last changes by %s) \n" % (__prog__, __build__, __author__, __lastAuthor__)
    usage += "A program to archive a correlated difx experiment.\n\n"
    usage += "Usage: %s <expcode> <path> \n\n"  % __prog__
    usage += "<expcode>: the experiment code e.g. ey010a\n"
    usage += "<path>:    the path to the experiment directory\n"
    if (os.getenv("DIFXROOT") == None):
        usage += "NOTE: %s requires the DIFXROOT environment to be defined.\n" % __prog__
    usage += "The program reads the database configuration and other parameters from difxdb.ini located under $DIFXROOT/conf."

    
    return(usage)

def exitOnError(exception):
    '''
    Exit routine to be called whenever an error/exception has occured
    '''

    # print("Unexpected error:", sys.exc_info()[0])
    logger.error(exception)

    # destroy kerberos tickets
    destroyTicket()

    logger.info("Aborting")
    cleanup()

    exit(1)


def renewTicket(user):
    '''
    renews the kerberos ticket (needed for jobs that run for a very long time
    '''
    cmd = '/usr/bin/kinit -R %s@%s' % (user, krbDomain)
    kinit = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True,env=tmpEnv)
    kinit.wait()


def getTicket(user):
    '''
    obtains a kerberos ticket for the given user
    '''
    global tmpEnv

    logger.info ("Obtaining kerberos ticket for user %s" % user)
    password = getpass.getpass("Enter password for user %s:" % (user))

    # temporarily set different Kerberos cache KRB5CCNAME
    tmpEnv["KRB5CCNAME"] = "/tmp/krb5cc_%s" % (user)

    kinitcmd = '/usr/bin/kinit'
    kinit_args = [ kinitcmd, '-l 48h', '-r 30d','%s@%s' % (user,krbDomain) ]
    kinit = Popen(kinit_args, stdin=PIPE, stdout=PIPE, stderr=PIPE, env=tmpEnv)
    kinit.stdin.write('%s\n' % password)
    kinit.wait()


def destroyTicket():
    logger.info ("Destroying kerberos ticket" )
    Popen(['/usr/bin/kdestroy'], stdin=PIPE, stdout=PIPE, stderr=PIPE, env=tmpEnv)


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


def getTransferFileCount(source, destination, rsyncOptions=""):
	
    cmd = 'rsync -az --stats --dry-run %s %s %s' % ( rsyncOptions, source, destination) 
    if options.verbose:
        logger.info("Executing: " + str(cmd))
    proc = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, env=tmpEnv)

    remainder = proc.communicate()[0]
    
    matchTotal = re.findall(r'Number of files: (\d+)', remainder)
    totalCount = int(matchTotal[0])
    mn = re.findall(r'Number of files transferred: (\d+)', remainder)
    fileCount = int(mn[0])
    
    if options.verbose:
        logger.info ("Number of files to be transferred: %d " % fileCount)
    
    return(totalCount, fileCount)


def syncDir(path, user, config, fileCount):
    
    server = config.get("difxarchive", "archiveserver")
    remotePath = config.get("difxarchive", "archiveremotepath")
    
    logger.info( "Syncing files from %s to: %s" % (path, server))
    
    cmd = 'rsync -av --no-perms --chmod=ugo=rwX --progress %s %s@%s:%s' % ( path, user, server, remotePath) 
        
    proc = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, env=tmpEnv)
    
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
    
    cmd = " --exclude '*' --include '*/' "
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
    
    print (cmd)
    logger.info( "Syncing reference files from %s to: %s" % (path, referencePath))

    proc = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE,env=tmpEnv)    
    
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


def makeTarfilename(outDir, filename):
    '''
    Generate full-path filename of tar file and suffix
    '''

    if options.zip:
        tarfilename = "%s/%s.gz" % (outDir, filename)
    else:
        tarfilename = "%s/%s" % (outDir, filename)
    return tarfilename


def makeBasedir(rootPath):
    baseDir = os.path.dirname(rootPath)
    if len(baseDir) == 0:
        baseDir = "."
    return baseDir


def makeFullPackdirName(rootPath, packDir):
    '''
    Return full path name (?) for root and subdir (?)
    '''

    baseDir = makeBasedir(rootPath)
    expDir = os.path.basename(rootPath)
    if rootPath == packDir:
        actualPackDir = expDir + "/*"
    else:
        actualPackDir = "%s/%s" % (expDir, packDir)
    return actualPackDir


def archiveExists(outDir, filename):
    '''
    Check if archive file exists. Does not check completeness.
    :return True if archive file exists
    '''

    tarfilename = makeTarfilename(outDir, filename)
    return os.path.isfile(tarfilename)


def removeArchive(outDir, filename):
    '''
    Remove archive file if it (still) exists
    '''

    tarfilename = makeTarfilename(outDir, filename)
    if os.path.isfile(tarfilename):
        try:
            os.remove(tarfilename)
        except OSError as e:
            exitOnError("Could not remove old tar: %s: %s" % (tarfilename,str(e)))


def verifyArchive(filename, fatal=True):
    '''
    Verifies the tar contents against the file system 
    :param fatal: bool Sets behaviour at verification errors to either exit (default:True) or return.
    :return Returns list of files in tar upon success, or 'None' upon error when in non-fatal.
    '''

    # get archive file info
    if options.zip:
        tar = tarfile.open(filename, "r:gz")
    else:
        tar = tarfile.open(filename, "r")

    filesInTar = []
    dirsInTar = []

    logger.info("Verifying the contents of the archive %s against files" % (filename))
    for tarinfo in tar:

        if tarinfo.isreg():
            # verify file exists in the filesystem
            if not os.path.exists(tarinfo.name):
                if fatal:
                    exitOnError("The archive contains a file which does not exist in the filesystem: %s" % (tarinfo.name))
                logger.error("The archive contains a file which does not exist in the filesystem: %s" % (tarinfo.name))
                return None

            # get info for file in the filesystem
            statinfo = os.stat(tarinfo.name)

            # verify file sizes
            if tarinfo.size != statinfo.st_size:
                if fatal:
                    exitOnError("The archive filesize differs from the filesystem size for file %s" % (tarinfo.name))
                logger.error("The archive filesize differs from the filesystem size for file %s" % (tarinfo.name))
                return None

            # verify timestamps (note: os.stat() may return decimal time, while tarinfo might not!)
            if int(tarinfo.mtime) != int(statinfo.st_mtime):
                 if fatal:
                     exitOnError("The archive file time differs from the filesystem time for file %s" % (tarinfo.name))
                 logger.error("The archive file time differs from the filesystem time for file %s" % (tarinfo.name))
                 return None

            # success if we reached this
            if options.verbose:
                logger.info("Verifying: " + str(tarinfo.name) + " size=" + str(tarinfo.size) + "fs size=" + str(statinfo.st_size) + " OK")

            filesInTar.append(tarinfo.name)

        elif tarinfo.isdir():
            dirsInTar.append(tarinfo.name)

    tar.close()

    logger.info( "Contents of the archive %s OK" % (filename))
    return filesInTar


def verifyCompleteness(tarfiles, packDir, fatal=True):
    '''
    Checks that all files in the packDir and subdirectories are contained in the tarfiles
    :param fatal: bool Sets behaviour at verification errors to either exit (default:True) or return.
    :return Returns True upon success, or False upon tar incompleteness when in non-fatal mode.
    '''

    logger.info("Checking completeness of %d files in %s against content of archive" % (len(tarfiles),packDir))

    for subdir, dirs, files in os.walk(packDir):
        for file in files:
            checkFile = "%s/%s" % (subdir, file)
            if checkFile not in tarfiles:
                if fatal:
                    exitOnError("Disk file %s not found in archive" % (checkFile))
                logger.error("Disk file %s not found in archive" % (checkFile))
                return False
            if options.verbose:
                logger.info("Verifying: local %s in tar OK" % (checkFile))
    logger.info("Archive is complete")
    return True


def verifyMutualConsistency(rootPath, packDir, outDir, filename, fatal=True):
    '''
    Checks that all files found in tar also exist in packDir, and vice versa
    :param fatal: bool Sets behaviour at verification errors to either exit (default:True) or return.
    :return True when everything is consistent, False upon incompleteness when in non-fatal mode
    '''

    baseDir = makeBasedir(rootPath)
    os.chdir(baseDir)

    tarfilename = makeTarfilename(outDir, filename)
    filesintar = verifyArchive(tarfilename, fatal=fatal)
    if filesintar is None:
        return False

    actualPackDir = makeFullPackdirName(rootPath, packDir)
    complete = verifyCompleteness(filesintar, actualPackDir, fatal=fatal)
    return complete


def packDirectory(rootPath, packDir, outDir, filename, recurse=True):
    '''
    Pack content of a directory into a new tar file.
    The caller should afterwards call verifyMutualConsistency().
    '''

    tarfilename = makeTarfilename(outDir, filename)
    actualPackDir = makeFullPackdirName(rootPath, packDir)

    if os.path.isfile(tarfilename):
        exitOnError("File already exists (%s)" % (tarfilename))

    errorCount = 0

    logger.info("Creating tar file %s" % (tarfilename))
#
    tarOpts = " --create "

    if options.zip:
        tarOpts += " --gzip "

    if options.verbose:
        tarOpts += " --verbose "

    if recurse == False:
        tarOpts += " --no-recursion "

    # Change directory to top-level of directory to be packed
    baseDir = makeBasedir(rootPath)
    os.chdir(baseDir)

#    if tarMode == "directory":

    command = '/bin/bash -c "tar %s --file - %s | pv -p --timer --rate --bytes > %s"' % (tarOpts, actualPackDir, tarfilename)
    if options.verbose:
        print ("Executing command: ", command)

    child = pexpect.spawn(command, logfile = sys.stdout, timeout=None)
    ret = child.expect([pexpect.EOF,pexpect.TIMEOUT, "Error"])
    if ret != 0:
        exitOnError("Error creating archive: %s " % (tarfilename))


def packDirectoryResumably(rootPath, packDir, outDir, tarname, recurse=True):
    '''
    Pack content of a directory into a new tar file.
    If the --keep-tar option is active, a pre-existing tar is checked
    for reusability. It is rebuilt if not usable.
    '''

    repackage = True # default

    logger.info("Now checking %s" % packDir)

    if options.keepTar:
        if archiveExists(archiveDir, tarname):
            logger.info("Found existing %s, checking content" % (tarname))
            repackage = not verifyMutualConsistency(rootPath, packDir, archiveDir, tarname, fatal=False)
            logger.info("Content of existing %s is consistent with files: %s" % (tarname,str(not repackage)))

    if repackage:
       logger.info("Now packing %s" % packDir)
       removeArchive(archiveDir, tarname)
       packDirectory(rootPath, packDir, archiveDir, tarname, recurse=recurse)
       verifyMutualConsistency(rootPath, packDir, archiveDir, tarname)
       logger.info("Done packing %s" % packDir)
    else:
       logger.info("Verified %s is complete and consistent against %s/%s" % (tarname, rootPath, packDir))


def setupLoggers(logPath, code):
    
    global logger 
    global logfile

    logger = logging.getLogger("difxarchive")
    logger.setLevel(logging.INFO)
    # logging to file
    
    logfile = "%s/difxarchive_%s.log" % (logPath, code)
    fh = logging.FileHandler(logfile)
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
            
        print ('Are you sure you want to proceed? [y/N]')
        a = lower(sys.stdin.readline())
        if strip(a) == 'y':
            print ('OK -- proceeding\n')
        else:
            print ('Not continuing.\n')

            # destroy kerberos tickets
            destroyTicket()
            exit(0)


def confirmArchiveDirs(archiveDirs):
    while True:

	if len(archiveDirs) == 0:
		dirs = "None"
	else:
		dirs = ' ' .join(archiveDirs)

        print ("-------------------------------------------------")
        print ("Will archive all files in the top-level directory")
        print ("plus the following directories: ", dirs)
        print ("-------------------------------------------------")

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

        if options.all:
            archiveDirs = dirnames
        else:

            print ("-------------------------------------------")
            print ("Found the following top-level directories: ", ' ' . join(dirnames))
            archiveDirs = confirmArchiveDirs(archiveDirs)

        return archiveDirs, filenames

    
def cleanup():

    logging.shutdown()
    time.sleep(1)
    path = "%s/%s/%s" % (tmpPath, tmpDir, code)
    if os.path.isdir(path):
        # remove the tmp_difxlog directory and all its subdirs
        if not options.keepTar:
            shutil.rmtree(path, ignore_errors=False)
    
if __name__ == "__main__":

    usage = getUsage()
    
    parser = OptionParser(version="%prog " + __build__, usage=usage)
    
    parser.add_option("-u", "--user", dest="user", type="string" ,action="store", help="Do the archival as the specified user. This overrides the defaultuser directive in difxdb.ini")
    parser.add_option("-f", "--force", dest="force" ,action="store_true", default=False, help="Delete files without further confirmation ")
    parser.add_option("-a", "--all", dest="all" ,action="store_true", default=False, help="Backup all files and directories.")
    parser.add_option("-D", "--db-only", dest="dbOnly" ,action="store_true", default=False, help="Update database only, don't copy files (use with care!) ")
    parser.add_option("-k", "--keep", dest="keep" ,action="store_true", default=False, help="Keep files on local disk after archiving.")
    parser.add_option("-K", "--keep-tar", dest="keepTar" ,action="store_true", default=False, help="For resumability, retain tar files until successfully uploaded. Allows difxarchive to be called again to continue interrupted archiving (implies --keep)")
    parser.add_option("-v", "--verbose", action="store_true", default=False, help="Enable verbose output")
    parser.add_option("-z", "--zip", action="store_true", default=False, help="Zip archive")
    parser.add_option("-t", "--tmp-path", dest="tmpPath", default=None, help="Path under which the temporary directory will be created that will hold the archive tars.")


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

    if options.keepTar:
        # have to keep original files as well to be able to verify & resume
        options.keep = True

    # create temporary directories for holding the archival products
    if (options.tmpPath):
        tmpPath = options.tmpPath
    else:
        tmpPath = path

    try:
        os.mkdir(tmpPath + "/" + tmpDir)
    except:
        pass

    archiveDir = "%s/%s/%s" % (tmpPath, tmpDir, code)
    try:
        os.mkdir(archiveDir)
    except OSError as e:
        if not options.keepTar:
            sys.exit(e)

    # setup the console and file logger
    logPath = "%s/%s" % (tmpPath, tmpDir)
    setupLoggers(logPath, code)

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
        print ("Experiment has been archived already.")
        confirmAction()
    
    if not isSchemaVersion(session, minSchemaMajor, minSchemaMinor):
        major, minor = getCurrentSchemaVersionNumber(session)
        exitOnError("Current difxdb database schema is %s.%s but %s.%s is minimum requirement." % (major, minor, minSchemaMajor, minSchemaMinor))
    
    session.close()
    
    server = config.get("difxarchive", "archiveserver")
    remotePath = config.get("difxarchive", "archiveremotepath")
    
    destination = user + "@" + server + ":" + remotePath

    completeSuccess = False

    try:
        if not options.dbOnly:
	
            # obtain kerberos ticket
            getTicket(user)
                
            # construct the list of files and directories to be archived
            archiveDirs, filenames = getArchiveDirs(path)

            # pack the directories
            for dir in archiveDirs:
                packDirectoryResumably(path, dir, archiveDir, dir + ".tar", recurse=True)
                renewTicket(user)
                logger.info("Alive")
            ## now pack the top-level files
            packDirectoryResumably(path, path, archiveDir, "main.tar", recurse=False)
            renewTicket(user)

            # Now sync the tar files to the backup server
            passCount = 0
            while True:
                total, fileCount = getTransferFileCount(archiveDir, destination)
                
                if options.verbose:
                    print ("Remaining files to be transfered: ", fileCount)

                if (fileCount == 0):
                    break

                # copy files to the archive server
                syncDir(archiveDir, user, config, total )
                
                logger.info ("Pass %d. Syncing %d files" % (passCount, fileCount))
                passCount +=1
            logger.info("Finished")
            renewTicket(user)
                

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
                syncOptions = "--exclude '*/' --exclude '" + logfile + "' "
		syncOptions += "--exclude '*.calc' " 
		syncOptions += "--exclude '*.im' " 
		syncOptions += "--exclude '*.threads' " 
		syncOptions += "--exclude '*.machines' " 
		syncOptions += "--exclude '*.flag' " 
		syncOptions += "--exclude '*.fits' " 
		syncOptions += "--exclude '*.FITS' " 
		syncOptions += "--exclude '*.jobmatrix' " 
		
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

        # finally upload logfile
        shutil.copy(logfile, archiveDir)
        syncDir(archiveDir, user, config, 1)
        syncReferenceDir(logfile, destDir, 1, "")

        if not options.dbOnly and not options.keep:
            # delete files
            print ('Archival process completed. Now deleting path %s including all files and subdirectories' % path)
            confirmAction()
            logger.info("Deleted %s" % (path))
#
            shutil.rmtree(path, ignore_errors=True)
        
        
        completeSuccess = True

    except Exception as e:
        pass
        #exitOnError(e)
    except KeyboardInterrupt:
        sys.exit(1)
    finally:
        # destroy kerberos tickets
        destroyTicket()
        logger.info ("Done")
        if completeSuccess:
            # allow deletion of tar files in cleanup()
            options.keepTar = False
        cleanup()
        
    
   
