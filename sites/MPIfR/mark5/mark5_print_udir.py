#!/usr/bin/env python
'''
Print out a user directory file recovered from raw disk images of Mark5A/Mark5B.

Examples:
$ tail --bytes=11272704 /dev/sdf > sdf.udir  # note: always located at 11272704 from EOF
$ mark5_print_udir.py sdf.udir
   or directly
$ mark5_print_udir.py --blkdev /def/sdf
'''

import argparse
import ctypes
import os, sys, string

userdir_offset = 11272704  # location of user dir before from EOF

class Header(ctypes.LittleEndianStructure):
    _fields_ = [("dummy1", ctypes.c_uint32),
                ("sequence_number", ctypes.c_uint32, 28),
                ("dummy2", ctypes.c_uint32, 4)]

class UserDirMark5BC(ctypes.LittleEndianStructure):
    _fields_ = [("dir_version", ctypes.c_uint32),
                ("status", ctypes.c_uint32),
                ("VSN", ctypes.c_char * 32),
                ("cont_from_VSN", ctypes.c_char * 32),
                ("cont_to_VSN", ctypes.c_char * 32),
                ("spare", ctypes.c_char * 24)]

class UserDirMark5BCEntry(ctypes.LittleEndianStructure):
    _fields_ = [("datatype", ctypes.c_uint8, 8),
                ("scannumber", ctypes.c_uint32, 24),
                ("framelen", ctypes.c_uint16, 16),
                ("station", ctypes.c_char * 2),
                ("scanname", ctypes.c_char * 32),
                ("exp", ctypes.c_char * 8),
                ("start", ctypes.c_uint64),
                ("stop", ctypes.c_uint64),
                ("bcd", ctypes.c_uint64),
                ("frame0", ctypes.c_uint32),
                ("readoffset", ctypes.c_uint32),
                ("writeoffset", ctypes.c_uint32),
                ("bitstreammask", ctypes.c_uint32),
                ("filler", ctypes.c_char * 40)]

MK5A_DIR_MAXSCANS = 1024
MK5A_EXTNAME_MAXLEN = 64
class UserDirMark5A(ctypes.LittleEndianStructure):
    _fields_ = [("numscans", ctypes.c_uint32),
                ("nextscan", ctypes.c_uint32),
                #("allnames", ctypes.c_char * (MK5A_EXTNAME_MAXLEN*MK5A_DIR_MAXSCANS)),
                ("allnames", ctypes.c_ubyte * (MK5A_EXTNAME_MAXLEN*MK5A_DIR_MAXSCANS)),
                ("startbyte", ctypes.c_uint64 * MK5A_DIR_MAXSCANS),
                ("length", ctypes.c_uint64 * MK5A_DIR_MAXSCANS),
                ("writeoffset", ctypes.c_uint64),
                ("readoffset", ctypes.c_uint64),
                ("playrate", ctypes.c_double),
                ("vsn", ctypes.c_char * 32)]


##############################################################################################

def print_scans_mark5a(inputs_, doSeek=False):
    '''
    Reads a really old Mark5A-format user directory from Mark5 raw disks.
    Inspects the first valid file descriptor only.
    '''
    udirinfo = UserDirMark5A()
    for input in inputs_:

        if input == None: continue

        if doSeek:
            input.seek(-userdir_offset, 2)

        input.readinto(udirinfo)
        if udirinfo.numscans > MK5A_DIR_MAXSCANS:
            continue

        print("Module has VSN %s and %d scans" % (udirinfo.vsn,udirinfo.numscans))
        # print(udirinfo.allnames, len(udirinfo.allnames))

        usable_scans = 0
        for n in range(udirinfo.numscans):
            fileext = '.m5a'
            scanname = ''.join([chr(i) for i in udirinfo.allnames[(n*MK5A_EXTNAME_MAXLEN):((n+1)*MK5A_EXTNAME_MAXLEN)]]).rstrip('\x00')

            print("entry %4d : type %s : name %s : start %d stop %d" % (n,fileext,scanname,udirinfo.startbyte[n],udirinfo.startbyte[n] + udirinfo.length[n])),

            if len(scanname) == 0:
                print('')
            elif udirinfo.length[n] <= 16:
                print(' too short')
            else:
                print('')
                usable_scans += 1

        print ("Module has {n} valid-looking scans in the Mark5A-format user directory".format(n=usable_scans))


def print_scans_mark5bc(inputs_, doSeek=False):
    '''
    Reads the user directory from Mark5 raw disks.
    '''
    udir = UserDirMark5BC()
    scan = UserDirMark5BCEntry()

    for input in inputs_:

        if input == None: continue

        if doSeek:
            input.seek(-userdir_offset, 2)

        input.readinto(udir)

        print('Module has VSN %s' % (udir.VSN))

        usable_scans = 0
        for n in range(8192):
            input.readinto(scan)
            if any(c not in string.printable for c in scan.scanname):
                break

            fileext = '.m5a'
            if scan.datatype == 9:
                fileext = '.vdif'
            scanname = '_'.join([scan.exp, scan.station, scan.scanname])

            print("entry %4d : type %s : name %s : start %d stop %d" % (n,fileext,scanname,scan.start,scan.stop)),
            if len(scanname) == 0:
                print('')
            elif scan.stop <= scan.start:
                print(' suspect')
            else:
                print('')
                usable_scans += 1

        print ("Module has {n} valid-looking scans in the Mark5B/5C-type user directory".format(n=usable_scans))


##############################################################################################

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("inputfile",
                        help="Input file to read data blocks from",
                        nargs="+")
    parser.add_argument('--mark5a', default=False, action='store_true', help='Directory is from a Mark5A module, rather than 5B/5C')
    parser.add_argument('--blkdev', default=False, action='store_true', help='File is a block device; extract user dir from the end of device')
    opts = parser.parse_args()

    rawsources = [open(filename, "rb") for filename in opts.inputfile]
    if opts.mark5a:
        print_scans_mark5a(rawsources, doSeek=opts.blkdev)
    else:
        print_scans_mark5bc(rawsources, doSeek=opts.blkdev)
