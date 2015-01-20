#!/usr/bin/python
# archive DiFX data files to the iVEC permanent data store. Tar the plethora of
# small files before transfer. Transfer FITS and other user files unmodified.   
# Cormac Reynolds: Dec 2014

import optparse, os, re, subprocess, sys, shutil

def taritup(tardir, tarfile, infile, gzip=False):

    print "tarring up", tarfile
    taroptions = options.taroptions
    if gzip:
        taroptions = " ".join([taroptions, '-z'])
    command = " ".join(['tar', taroptions, '-cf', archdir+tarfile, infile])
    if options.verbose:
        print '\n' + command
    subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=subprocess.PIPE)

    # and print tar listing for reference
    print "creating listing for", tarfile
    command = " ".join(["tar -tf", tardir+tarfile, ">", tardir+tarfile+'.list'])
    subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=subprocess.PIPE)

usage = '''%prog <path> <destination>
will transfer <path> and all its subdirectories to <destination> on data.ivec.org using ashell.py. Most files are tarred before transfer, but special files and large files are transferred unmodified. Files are first tarred/copied to /data/corr/Archive/ before transfer.

e.g.
%prog /data/corr/corrdat/vt13b VLBI/Archive/Curtin/vt13/vt13b
'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
parser.add_option( "--maxtarsize", "-m",
        type='float', dest="maxtarsize", default=1000,
        help='files larger than MAXTARSIZE (MB) will be transferred untarred [default = %default]' )
parser.add_option( "--taroptions", "-t",
        type='str', dest="taroptions", default=' ',
        help='specify additional options to pass to tar' )
parser.add_option( "--verbose", "-v",
        dest="verbose", action="store_true", default=False,
        help='Be verbose' )
parser.add_option( "--keeparch", "-k",
        dest="keeparch", action="store_true", default=False,
        help="Keep the tar archive directory after it's been transferred" )
parser.add_option( "--onejob", "-j",
        dest="onejob", action="store_true", default=False,
        help="Each job gets its own tar file (instead of each pass)" )

(options, args) = parser.parse_args()

if len(args) < 2:
    parser.print_help()
    parser.error("Give source and destination directories!")


# get the list of subdirectories. Note that we don't want to tar large files,
# or files that archive users want to access directly.
# In each  subdirectory form a list of files to be tarred, and a list of files
# to transfer unmodified (both tarred files and large files will be
# transferred).

#archdir = args[1] 
expname = os.path.normpath(args[0]).split('/')[-1]
archdir = os.environ.get('ARCHTMP') + os.sep + expname + os.sep
if not archdir:
    print '$ARCHTMP not set - using /tmp instead. Setting $ARCHTMP to a directory on the same filesystem as the data is preferable'
    archdir = '/tmp/'
mark4file = str()
os.chdir(args[0])
tarlists = dict()
transfer = []


for filename in os.listdir(os.curdir):

    # tar separate passes independently, so figure out pass names in use.
    # Default is just the expname.
    passname = expname
    if filename.startswith(expname+'-'):
        passname = re.sub('[_\.].*', '', filename)
    if options.onejob:
        if filename.startswith(passname+'_'):
            passname = re.match(passname+'_\d\d', filename).group(0)

    if not passname in tarlists.keys():
        tarlists[passname] = str()

    # deal with Mark4 output, clocks, test and old runs as special cases
    if re.search('^\d\d\d\d$', filename):
        # deal with this later in its own tar file
        mark4file = filename
        continue
    if filename == 'clocks':
        # deal with this later in its own tar file
        continue
    if filename == 'test':
        print 'skipping', filename
        # ignore this one
        continue
    if re.match('\d\d\d\d-\d\d-\d\d-\d\d-\d\d-\d\d', filename):
        print 'skipping', filename
        # ignore these (old, superseded jobs).
        continue

    # certain file names never get tarred 
    notar_ext = ['.fits', '.rpf', '.uvfits', '.mark4', '.tar', expname+'.v2d', expname+'.vex', expname+'.skd', 'notes.txt']
    fileWithPath = os.path.join(os.path.abspath(os.curdir), filename)
    notar = False
    for extension in notar_ext:
        if re.search(extension, filename, re.IGNORECASE):
            notar = True
            break


    # only tar small files
    if os.path.getsize(fileWithPath)/1e6 > options.maxtarsize:
        notar = True

    if (os.path.exists(fileWithPath) and notar):
        # transfer this large file without tarring
        transfer.append(re.escape(fileWithPath))
    else:
        # add to list of files to be tarred
         tarlists[passname] +=  " " + re.escape(filename)



# create the output directory
command = " ".join(['mkdir -p', archdir ])
subprocess.check_call(command, shell=True, stdout=sys.stdout)


# tar up small files in this directory to Archive area, one correlator pass at
# a time
for passname in tarlists.keys():
    if tarlists[passname]:
        tarfile =  passname + '.tar'
        taritup(archdir, tarfile, tarlists[passname])

# transfer each of the large files in turn
print "copying files"
for srcfile in transfer:
    command = " ".join(["cp -l", srcfile, archdir])
    if options.verbose:
        print '\n' + command
    subprocess.check_call(command, shell=True, stdout=sys.stdout)

# now tar up the clocks subdirectory
if os.path.exists('clocks'):
    taritup(archdir, 'clocks.tar', 'clocks')

# and the mark4 output dir
if mark4file:
    taritup(archdir, expname.upper()+'.MARK4.tar.gz', mark4file, gzip=True)


# now archive the lot to data.ivec.org
#os.chdir(archdir)
#command = " ".join(["ashell.py login + delegate 100"])
#subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
while True:
    try:
        command = " ".join(['ashell.py "login cormac Snuff1:since + cf', args[1], "+ put", archdir, '"'])
        print command
        subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
        break
    except KeyboardInterrupt:
        raise Exception('Forced quit')
    except:
        print 'trying again'
        #command = " ".join(['ashell.py "login + delegate 100"'])
        #subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)

if not options.keeparch:
    shutil.rmtree(archdir)

print 'All done!'
