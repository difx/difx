#!/usr/bin/python
# archive a DiFX data archive. Tar the plethora of small files before transfer.
# Transfer larger files unmodified. Preserve the original directory structure.
# Uses globus-url-copy (an ssh option could be added without too much trouble).
# Cormac Reynolds: Jan 2012

import optparse, os, re, subprocess, sys

usage = '''%prog <path> <destination>
will transfer <path> and all its subdirectories to <destination>. Small files (<10 MB) are tarred before transfer. Larger files are transferred unmodified.

e.g.
%prog /data/corr/corrdata/vt13b cormac@cortex.ivec.org/pbstore/groupfs/astrotmp/Archive/vt13/vt13b
'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
parser.add_option( "--maxtarsize", "-m",
        type='float', dest="maxtarsize", default=5,
        help='files larger than MAXTARSIZE (MB) will be transferred untarred [default = %default]' )
parser.add_option( "--checksum", "-c",
        dest="doChecksum", action="store_true", default=False,
        help='Create and send checksum files to the destination' )

(options, args) = parser.parse_args()

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

    # -cd flag to globus-url copy (version 5) does not create the output
    # directory when the input is stdin. Clearly a bug. Work round it by
    # ensuring the output dir is created.
    match = re.search(r'(.*?)/(.*)', args[1])
    host = match.group(1)
    rootdir = os.sep + match.group(2)
    command = 'ssh ' + host + ' mkdir -p ' + rootdir + os.sep + dir[0]
    subprocess.check_call(command, shell=True, stdout=sys.stdout)

    # create a checksum if you are paranoid.
    if options.doChecksum:
        # just carry on if this fails for some reason
        try:
            checkfile = args[1] + os.sep + dir[0] + os.sep + 'check.cksum'
            command = 'cd ' + dir[0] + ';' + ' cksum '  + '* | globus-url-copy -vb -cd - sshftp://' + checkfile
            print command
            subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
        except:
            print 'WARNING: Could not create cksum!'

    
    # tar up small files in this directory and transfer (on the fly)
    if tarlist:
        command = 'tar -C ' + dir[0] + ' -cf - ' + tarlist + ' | globus-url-copy -vb -cd - sshftp://' + tarfile
        print command
        subprocess.check_call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)

    # transfer each of the large files in turn
    for file in transfer:
        command = 'globus-url-copy -vb -cd file://' + file + ' sshftp://' + args[1] + os.sep + dir[0] + os.sep
        #print command
        subprocess.check_call(command, shell=True, stdout=sys.stdout)
