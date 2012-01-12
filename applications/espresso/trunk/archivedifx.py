#!/usr/bin/python
# archive DiFX data files to a remote machine. Tar the plethora of small files
# before transfer. Transfer larger files unmodified. Preserve the original
# directory structure. Uses globus-url-copy by default or scp/ssh on request. 
# Cormac Reynolds: Jan 2012

import optparse, os, re, subprocess, sys

def splithost(hostdir):
    '''split a host/directory (e.g. cormac@cortex.ivec.org/pbstore/) into host and directory parts'''
    host, dir = re.search(r'(.*?)(/.*)', hostdir).group(1,2)

    return host, dir

def transfer_command(protocol, preamble, source, dest):
    '''return a suitable command string for either an ssh or gridftp based transfer'''

    command = str()
    if protocol == 'ssh':
        host, dir = splithost(dest)
        if source == '-':
            command = preamble + ' ssh ' + host + ' "cat > ' + dir + '"'
        else:
            command = preamble + ' scp ' + source + ' ' + host + ':' + dir

    elif protocol == 'gridftp':
        verbose = ''
        if options.verbose:
            verbose = ' -vb '
        command = preamble + ' globus-url-copy -cd ' + verbose + source + ' sshftp://' + dest
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
        type='float', dest="maxtarsize", default=10,
        help='files larger than MAXTARSIZE (MB) will be transferred untarred [default = %default]' )
parser.add_option( "--ssh", "-s",
        dest="usessh", action="store_true", default=False,
        help='Use scp/ssh for transfers instead of GridFTP' )
parser.add_option( "--checksum", "-c",
        dest="doChecksum", action="store_true", default=False,
        help='Create and send checksum files to the destination' )
parser.add_option( "--verbose", "-v",
        dest="verbose", action="store_true", default=False,
        help='Be verbose' )

(options, args) = parser.parse_args()

protocol = 'gridftp'
if options.usessh:
    protocol = 'ssh'


# get the list of subdirectories. Note that we don't want to tar large files.
# In each  subdirectory form a list of files to be tarred, and a list of files
# to transfer unmodified (both tarred files and large files will be
# transferred).

os.chdir(args[0])
for dir in os.walk(os.curdir):
    transfer = []
    tarlist = str()
    for file in dir[2]:
        fileWithPath = os.path.join(os.path.abspath(dir[0]), file)
        if (os.path.getsize(fileWithPath)/1e6 > options.maxtarsize):
            # transfer this large file without tarring
            transfer.append(fileWithPath)
        else:
            # add to list of files to be tarred
            tarlist += ' ' + file

    tarfile = args[1] + os.sep + dir[0] + os.sep + os.path.basename(os.path.abspath(dir[0])) + '.tar'

    # -cd flag to globus-url-copy (version 5) does not create the output
    # directory when the input is stdin. Clearly a bug. Work round it by
    # ensuring the output dir is created.
    host, rootdir = splithost(args[1]) 
    command = 'ssh ' + host + ' mkdir -p ' + rootdir + os.sep + dir[0]
    subprocess.check_call(command, shell=True, stdout=sys.stdout)

    # create a checksum if you are paranoid.
    if options.doChecksum:
        # just carry on if this fails for some reason
        try:
            checkfile = args[1] + os.sep + dir[0] + os.sep + 'check.cksum'
            preamble = 'cd ' + dir[0] + '; md5sum * | '
            command = transfer_command(protocol, preamble, '-', checkfile)
            if options.verbose:
                print command
            subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
        except:
            print 'WARNING: Could not create checksum!'

    
    # tar up small files in this directory and transfer (on the fly)
    if tarlist:
        preamble = 'tar -C ' + dir[0] + ' -cf - ' + tarlist + ' | '
        command = transfer_command(protocol, preamble, '-', tarfile)
        if options.verbose:
            print command
        subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)

    # transfer each of the large files in turn
    for srcfile in transfer:
        destdir = args[1] + os.sep + dir[0] + os.sep
        command = transfer_command(protocol, '', srcfile, destdir)
        if options.verbose:
            print command
        subprocess.check_call(command, shell=True, stdout=sys.stdout)
