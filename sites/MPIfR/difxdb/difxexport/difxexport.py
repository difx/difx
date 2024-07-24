#! /usr/bin/env python3
# coding: latin-1

import os
import shutil
import sys
import random
import argparse
import subprocess
import re
import time
import hashlib
from difxdb.model.dbConnection import Schema, Connection
from difxdb.model.model import ExportFile
from difxdb.business.versionhistoryaction import *
from difxdb.business.experimentaction import *
from difxdb.business.exportfileaction import *
from difxdb.difxdbconfig import DifxDbConfig

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision: 10885 $"
__date__ ="$Date: 2023-02-14 18:11:54 +0800 (二, 2023-02-14) $"
__lastAuthor__="$Author: JanWagner $"

ftpPath = "/ftp/vlbiarchive/correlator"
exportName = "EXPORT"

# minimum database schema version required by program
minSchemaMajor = 1
minSchemaMinor = 5

try:
    unichr
except NameError:
    unichr = chr


def description():
    desc = "A program to export the correlation data products (e.g. FITS files) to the FTP server for download by the PIs. "
    desc += "The export path contains a randomly generated name (default 10 characters). "
    desc += "For all exported files partial md5 checksums are calculated and are stored in the database together with the file names and export paths.\n "
    desc += "The files to be exported are expected to be located in subdirectories underneath a directory named EXPORT."
    desc += "When re-running the program on an experiment all previously exported files will be removed from the FTP server and the database references will be deleted! "
    return desc


def getTransferFileCount(source, destination, options=""):
    '''
    Determines the number of files to be transfered by rsync operation
    '''
        
    cmd = 'rsync -az --stats --dry-run %s %s %s' % ( options, source, destination) 
    proc = subprocess.Popen(cmd, encoding='utf8',
                            shell=True,
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            )
    
    remainder = proc.communicate()[0]
    
    
    matchTotal = re.findall(r'Number of files: (\d+)', remainder)
    totalCount = int(matchTotal[0])
    mn = re.findall(r'Number of .*? files transferred: (\d+)', remainder)
    fileCount = int(mn[0])
    
    # print("Number of files to be transferred: %d " % (fileCount))
    
    return(totalCount, fileCount)


def partialChecksum(filePath):
    
    cmd = 'head -c 1000000 "%s" | md5sum' % ( filePath)
    output = ""
    checksum = ""

    proc = subprocess.Popen(cmd, encoding='utf8',
                           shell=True,
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           )
    while True:
        output = proc.stdout.readline()
        if not output:
            break
        checksum = output.split(" ")[0]
    return checksum


def syncDir(srcPath, destPath, fileCount, dryRun):
    '''
    sync source to dest using rsync
    '''

    print("Copying files to: %s" % (destPath))
    if dryRun:
        cmd = 'rsync -av  --dry-run %s %s' % (srcPath, destPath) 
    else:    
        cmd = 'rsync -av  --progress %s %s' % (srcPath, destPath) 
        
    proc = subprocess.Popen(cmd, encoding='utf8',
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
             sys.stdout.write('\rDone: %s %%' % progress)
             #sys.stdout.write('\rRemaining: %s / %s' % (m[0][0], m[0][1]) )
             sys.stdout.flush()
             
             if int(m[0][0]) == 0:
                      break
                      
    print('\n')

    if not dryRun:
        cmd = '/usr/bin/chmod 2775 %s' % (destPath)
        print('Updating FTP directory permissions: %s' % (cmd))
        proc = subprocess.Popen(cmd, shell=True)
        proc.wait()

        cmd = '/usr/bin/chmod 644 %s/*' % (destPath)
        print('Updating file permissions: %s' % (cmd))
        proc = subprocess.Popen(cmd, shell=True)
        proc.wait()
    
    return    


def readConfig():
    '''
    Read difxdb configuration
    '''
    
    if (os.getenv("DIFXROOT") == None):
        sys.exit("Error: DIFXROOT environment must be defined.")
            
    configName = os.getenv("DIFXROOT") + "/conf/difxdb.ini"
        
    config = DifxDbConfig(configName)
    
    return (config)


def confirmAction():
    
    # if --force option was used skip confirmation
    #if not options.force:
            
    print('Are you sure you want to proceed? [y/N]')
    a = sys.stdin.readline()
    a = a.lower()
    a = a.strip()
    if strip(a) == 'y':
        print('OK -- proceeding\n')
    else:
        print('Not continuing.\n')

        sys.exit(0)


def randomString(len=10):
    '''
    Build string of random characters
    '''
        
    randomString=""
    for i in range(len):
        randomString += (str(unichr(random.randint(97,122)))) #intended to put tab space.
    return randomString       


def deleteExportFiles(session, expCode):
    '''
    Will delete export files on the FTP server and will clear all associations to the experiment in the database
    '''
    experiment = getExperimentByCode(session, expCode)
    
    delDirs = []
    rootDirs = []
    # delete files
    for file in experiment.exportFiles:
        filePath = file.exportPath + "/" + file.filename
             
        if file.exportPath not in delDirs:
            delDirs.append(file.exportPath)
    
        if os.path.isfile(filePath):
            try:
                os.remove(filePath)
                print("Removed: %s" % (filePath))
            except:
                session.close()
                sys.exit("Cannot delete file: %s. Aborting!" % filePath)
     
    # delete exp directories
    for dir in delDirs:
        try:
            if os.path.isdir(dir):
                shutil.rmtree(dir, ignore_errors=True)
        except:
            session.close()
            sys.exit("Cannot delete directory: %s. Aborting!" % dir)
    
    deleteExportFilesbyExperimentId(session, experiment.id)

    session.flush()
    session.commit()


def exitOnError(exception):
    '''
    Exit routine to be called whenever an error/exception has occured
    '''
    print("\nERROR: %s. Aborting\n\n" % (exception))
        
    sys.exit(1)


def getRemoteDirName(dir, files):

    dirName = dirName = ftpPath + "/" + args.exp + "_" + dir + "_" + randomString()

    # in the update case obtain the exiting export directory name
    if args.update:
        for file in files:
            expDir = os.path.basename(os.path.normpath(file.exportPath)).split("_")
            if (dir == expDir[1]):
                dirName = ftpPath + "/" + args.exp + "_" + dir + "_" + expDir[2]
                break

    return(dirName)
    

#######################    
# Beginning of program        
#######################
parser = argparse.ArgumentParser(prog='PROG',description=description())

#parser.add_argument('--dry-run', dest='dryRun', action='store_true', default=False, help='Dry run only. Do not actually archive files.')
parser.add_argument('--update', action='store_true', default=False, help='Update an existing export (files and checksums).')
parser.add_argument('exp', help='The experiment code')
parser.add_argument('expDir', help='The full path to the experiment directory.')

args = parser.parse_args()
args.dryRun = False

if args.expDir.endswith('/'):
    args.expDir= args.expDir[:-1]
args.rootDir = args.expDir + "/" + exportName


# check that directory exists and contains a EXPORT subdirectory
if not os.path.exists(args.expDir):
    sys.exit("Experiment directory does not exist: %s\n" % args.rootDir)
if not os.path.exists(args.rootDir):
    sys.exit("Experiment directory (%s) does not contain a %s subdirectory\n" % (args.expDir, exportName))
        
# check if EXPORT subdirectory is empty
if not os.listdir(args.rootDir):
    sys.exit("FITS_EXPORT subdirectory contains no files: %s\n" % args.rootDir)

# check that ftp path exists
if not os.path.exists(ftpPath):
    sys.exit("FTP export directory does not exist: %s\n" % ftpPath)
        
# open database connection
config = readConfig()
connection = Connection(config)
connection.echo = False
try:
    dbConn = Schema(connection)
    session = dbConn.session()
except Exception as e:
    exitOnError(e)

if not isSchemaVersion(session, minSchemaMajor, minSchemaMinor):
    major, minor = getCurrentSchemaVersionNumber(session)
    print("Current difxdb database schema is %s.%s but %s.%s is minimum requirement." % (major, minor, minSchemaMajor, minSchemaMinor))
    sys.exit(1)

# check that experiment exists
if not experimentExists(session, args.exp):
    session.close()
    sys.exit("Experiment with code: %s does not exist.\n" % args.exp)
    
# loop over subdirectories
dirs = next(os.walk(args.rootDir))[1]
if dirs == []:
    sys.exit("%s directory must contain at least one subdirectory\n" % exportName)        

# check if export files already exist for this experiment
experiment = getExperimentByCode(session, args.exp)
expId = experiment.id
files = getExportFiles(session, args.exp)

if len(files) != 0:

    if args.update == False:
        print("-----------------------------------------------------------------------------")
        print("The experiment %s already has one or more associated exported files:" % (args.exp))
        print("name     path            checksum            creation date")
        print("-----------------------------------------------------------------------------")

        for file in files:
            print(file.filename, file.exportPath, file.checksum, file.dateCreated)
        print("-----------------------------------------------------------------------------")
        
        print("When proceeding all files will be deleted and will be replaced by the contents of %s\n\n" % (args.rootDir))
        confirmAction()
        

        if args.dryRun == False:
            deleteExportFiles(session, args.exp)

    else:
        for file in files:
            session.delete(file)

        session.flush()
        session.commit()

#for file in files:
#    print(file.filename, file.exportPath, file.checksum, file.dateCreated)

exportFiles = []
session.close()

# loop over all source directories
for dir in dirs:
    srcFiles = {}
    dstFiles = {}
    srcDir = args.rootDir + "/" + dir + "/"
    #exportDir = ftpPath + "/" + args.exp + "_" + dir + "_" + randomString()
    exportDir = getRemoteDirName(dir, files)

           
    # loop over src files and determine checksum
    for file in  os.listdir(srcDir):
        print("Processing: %s" % (srcDir+file))
        srcFiles[file] = partialChecksum(srcDir+file)

    # create directory on FTP server
    if not os.path.exists(exportDir) and not args.dryRun:
        try:
            os.makedirs(exportDir)
        except:
            sys.exit("Cannot create directory: %s\n" % exportDir)

    # rsync files 
    while True:
        total, fileCount = getTransferFileCount(srcDir, exportDir)

        if (fileCount == 0):
            break

        # copy files to the archive server
        syncDir(srcDir, exportDir, total, args.dryRun)

        if args.dryRun:
            break

    # pause for a while to allow flushing of rsync 
    time.sleep(5)

    if args.dryRun:
        sys.exit(0)

    exportPath = exportDir 
    # loop over all files in the export dir and determine checksums
    for file in os.listdir(exportPath):
        # calculate md5 checksum of first 100MB
        filePath = exportDir + "/" + file
        dstFiles[file]= partialChecksum(filePath)

    # verify src against dst
    print("Verfifying export to: %s" % (exportDir))
    for file in srcFiles:
        if file not in dstFiles:
            print("Source file: %s is missing in the export location: %s" % (file, exportPath))
            # TODO REMOVE exported files
            sys.exit(1)
        else:
            if srcFiles[file] != dstFiles[file]:
                print("Checksums differ for file %s" % (file))
                sys.exit(1)
            print("OK file: %s\t\t src-checksum: %s dst-checksum: %s" % (file, srcFiles[file], dstFiles[file]))
            # remove item from dstFiles
            del dstFiles[file]

    extra = 0
    for file in dstFiles:
        if args.update:
            os.remove(exportPath + "/" + file)
            print("Removed: %s/%s" % (exportPath, file))
            continue
            
        print("Export location contains a file not found at the src directory: %s" % (file))
        extra += 1
    
    if extra > 0:
        sys.exit(1)

    # update database
    for file in srcFiles:
        print("updating database with file %s" % (file))
        exportFile = ExportFile()
        exportFile.experimentID = expId
        exportFile.filename = file
        exportFile.exportPath = exportPath
        exportFile.checksum = srcFiles[file]
        exportFiles.append(exportFile)

session = dbConn.session()

experiment = getExperimentByCode(session, args.exp)
experiment.exportFiles = exportFiles
session.commit()
session.flush()
session.close()

print("Done")
