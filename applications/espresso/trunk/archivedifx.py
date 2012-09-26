#!/usr/bin/python
# archive DiFX data files to a remote machine. Tar the plethora of small files
# before transfer. Transfer larger files unmodified. Preserve the original
# directory structure. Uses globus-url-copy by default or scp/ssh on request. 
# Cormac Reynolds: Jan 2012

import optparse, os, re, subprocess, sys

def splithost(hostdir):
    '''split a host/directory (e.g. cormac@cortex.ivec.org/pbstore/) into host and directory parts'''
    host, directory = re.search(r'(.*?)(/.*)', hostdir).group(1,2)

    return host, directory

def transfer_command(protocol, preamble, source, dest):
    '''return a suitable command string for either an ssh or gridftp based transfer'''

    command = str()
    if protocol == 'ssh':
        host, directory = splithost(dest)
        if source == '-':
            command = preamble + ' ssh ' + host + ' "cat > ' + directory + '"'
        else:
            scpoptions = ' '
            if not options.verbose:
                scpoptions = ' -q '
            command = preamble + ' scp ' + scpoptions + source + ' ' + host + ':' + directory

    elif protocol == 'gridftp':
        verbosity = ' '
        if options.verbose:
            verbosity = ' -vb '
        command = preamble + ' globus-url-copy -cd ' + options.globusoptions + verbosity  + source + ' sshftp://' + dest
    else:
        raise Exception('Unrecognised transfer protocol!')

    return command

usage = '''%prog <path> <destination>
will transfer <path> and all its subdirectories to <destination>. Small files are tarred before transfer. Larger files are transferred unmodified.

e.g.
%prog /data/corr/corrdata/vt13b cormac@cortex.ivec.org/pbstore/groupfs/astrotmp/Archive/vt13/vt13b
'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
parser.add_option( "--maxtarsize", "-m",
        type='float', dest="maxtarsize", default=50,
        help='files larger than MAXTARSIZE (MB) will be transferred untarred [default = %default]' )
parser.add_option( "--taroptions", "-t",
        type='str', dest="taroptions", default=' ',
        help='specify additional options to pass to tar' )
parser.add_option( "--globusoptions", "-g",
        type='str', dest="globusoptions", default=' ',
        help='specify additional options to pass to globus-url-copy' )
parser.add_option( "--ssh", "-s",
        dest="usessh", action="store_true", default=False,
        help='Use scp/ssh for transfers instead of GridFTP' )
parser.add_option( "--checksum", "-c",
        dest="doChecksum", action="store_true", default=False,
        help='Create checksum files and send to the destination' )
parser.add_option( "--verbose", "-v",
        dest="verbose", action="store_true", default=False,
        help='Be verbose' )
parser.add_option( "--ignorefails", "-i",
        dest="ignorefails", action="store_true", default=False,
        help='Continue even if some files cannot be transferred' )

(options, args) = parser.parse_args()

if len(args) < 2:
    parser.print_help()
    parser.error("Give source and destination directories!")


protocol = 'gridftp'
if options.usessh:
    protocol = 'ssh'


# get the list of subdirectories. Note that we don't want to tar large files.
# In each  subdirectory form a list of files to be tarred, and a list of files
# to transfer unmodified (both tarred files and large files will be
# transferred).

os.chdir(args[0])
for directory in os.walk(os.curdir):
    transfer = []
    tarlist = str()
    for file in directory[2]:
        fileWithPath = os.path.join(os.path.abspath(directory[0]), file)
        if (os.path.exists(fileWithPath) and os.path.getsize(fileWithPath)/1e6 > options.maxtarsize):
            # transfer this large file without tarring
            transfer.append(re.escape(fileWithPath))
        else:
            # add to list of files to be tarred
            tarlist += ' ' + re.escape(file)

    tarfile = args[1] + os.sep + directory[0] + os.sep + os.path.basename(os.path.abspath(directory[0])) + '.tar'

    # -cd flag to globus-url-copy (version 5) does not create the output
    # directory when the input is stdin. Clearly a bug. Work round it by
    # ensuring the output dir is created.
    host, rootdir = splithost(args[1]) 
    command = 'ssh ' + host + ' mkdir -p ' + rootdir + os.sep + directory[0]
    subprocess.check_call(command, shell=True, stdout=sys.stdout)

    # create a checksum if you are paranoid.
    if options.doChecksum:
        # just carry on if this fails for some reason
        try:
            checkfile = args[1] + os.sep + directory[0] + os.sep + 'check.cksum'
            preamble = 'cd ' + directory[0] + '; md5sum * | '
            command = transfer_command(protocol, preamble, '-', checkfile)
            if options.verbose:
                print '\n' + command
            subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
        except:
            print 'WARNING: Could not create checksum!'

    
    # tar up small files in this directory and transfer (on the fly)
    if tarlist:
        preamble = 'tar ' + options.taroptions + ' -C ' + directory[0] + ' -cf - ' + tarlist + ' | '
        command = transfer_command(protocol, preamble, '-', tarfile)
        if options.verbose:
            print '\n' + command
        errors = subprocess.Popen(command, shell=True, stdout=sys.stdout, stderr=subprocess.PIPE).communicate()[1]
        if errors:
            if options.ignorefails:
                print errors
                #print 'Warning! Could not transfer file'
            else:
                raise Exception(errors + '\nTransfer aborting!')


    # transfer each of the large files in turn
    for srcfile in transfer:
        destdir = args[1] + os.sep + directory[0] + os.sep
        command = transfer_command(protocol, '', srcfile, destdir)
        if options.verbose:
            print '\n' + command
        try:
            subprocess.check_call(command, shell=True, stdout=sys.stdout)
        except:
            if options.ignorefails:
                #print 'Warning! Could not transfer', srcfile
                pass
            else:
                raise Exception('Could not transfer ' + srcfile)
