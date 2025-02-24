#!/usr/bin/env python3

#**************************************************************************
#   Copyright (C) 2024      by Helge Rottmann                             *
#                                                                         *
#   This program is free software; you can redistribute it and/or modify  *
#   it under the terms of the GNU General Public License as published by  *
#   the Free Software Foundation; either version 3 of the License, or     *
#   (at your option) any later version.                                   *
#                                                                         *
#   This program is distributed in the hope that it will be useful,       *
#   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
#   GNU General Public License for more details.                          *
#                                                                         *
#   You should have received a copy of the GNU General Public License     *
#   along with this program; if not, write to the                         *
#   Free Software Foundation, Inc.,                                       *
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
#**************************************************************************

# Note: this utility can run under python2.7 or python3

from sys import argv, exit
from os import getenv
import socket
import re
import argparse

hasDifxFile = True
#try:
#       from difxfile.difxmachines import DifxMachines
#except ImportError:
#       hasDifxFile == False

program = 'mk6control'
version = '1.0'
author  = 'Helge Rottmann <hrottmann@mpifr-bonn.mpg.de>'
verdate = '20240301'

defaultDifxMessagePort = 50200
defaultDifxMessageGroup = '224.2.2.1'

verbose = True

def getMark6List():
        mark6units = getenv('DIFX_MARK6')
        machinesfile = getenv('DIFX_MACHINES')

        print(mark6units, machinesfile, hasDifxFile)

        if hasDifxFile and machinesfile != None:
                mk6list = ''
                difxmachines = DifxMachines(machinesfile)
                mk6machines = difxmachines.getMk6NodeNames()
                for mk6 in mk6machines:
                        if mk6list == '':
                                mk6list = mk6[-2:]
                        else:
                                mk6list = mk6list + ',' + mk6[-2:]
                return mk6list
        elif mark6units != None:
                return mark6units
        else:
                return None
        


def sendCommand(cmd, units, verbose):
        src = socket.gethostname()
        dest = ''
        dest += '<to>%s</to>' % units

        message = \
          '<?xml version="1.0" encoding="UTF-8"?>\n' \
          '<difxMessage>' \
            '<header>' \
              '<from>%s</from>' \
              '%s' \
              '<mpiProcessId>-1</mpiProcessId>' \
              '<identifier>mk5control</identifier>' \
              '<type>DifxCommand</type>' \
            '</header>' \
            '<body>' \
              '<seqNumber>0</seqNumber>' \
              '<difxCommand>' \
                '<command>%s</command>' \
              '</difxCommand>' \
            '</body>' \
          '</difxMessage>' % (src, dest, cmd)

        if args.verbose:
                print ("Sending multicast message: ")
                print(message)

        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
        sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 2)
        sock.sendto(message.encode('utf-8'), (group, port))

def cmd_modinit(args):
    '''
    callback function for the modinit sub-command
    '''

    cmd = "modinit_" + str(args.slot) + "_" + args.vsn

    print ("-----------------------------------------------------------------------------------------")
    print ("You are about to initialize the module located in slot %d on %s" % (args.slot, args.mark6))
    print ("THIS WILL PERMANENTLY DELETE ALL DATA ON THE MODULE!")
    print ("-----------------------------------------------------------------------------------------")
    user_input = input("Do you really want to continue? (yes/no): ")
    if user_input.lower() == "yes":
        sendCommand(cmd, args.mark6, False)
        print ("started modinit. Watch progress with mk6mon")
    else:
        return
    return

def cmd_getvsn(args):
    '''
    callback function for the getvsn sub-command
    '''

    cmd = "getvsn_" + str(args.slot)
    sendCommand(cmd, args.mark6, True)

    return

# -------beginning of main

# read environment
port = getenv('DIFX_MESSAGE_PORT')
if port == None:
        port = defaultDifxMessagePort
else:
        port = int(port)
group = getenv('DIFX_MESSAGE_GROUP')
if group == None:
        group = defaultDifxMessageGroup



# create the top-level parser
parser = argparse.ArgumentParser(prog='mk6control')
parser.add_argument('--verbose', "-v",   action='store_true', help='give verbose output')

subparsers = parser.add_subparsers(help='sub-command help', dest='sub command')

# create the parser for the "modinit" command
parser_modinit = subparsers.add_parser('modinit', help='initialize a mark6 module. See %s modinit -h for additional help.' %(parser.prog))
parser_modinit.add_argument('mark6', help='The host name of the mark6 machine to execute the command on')
parser_modinit.add_argument('slot', type=int,  help='The mark6 slot containing the module to be initialized')
parser_modinit.add_argument('vsn',  help='The module VSN to be written during initialization')
parser_modinit.set_defaults(func=cmd_modinit)


# create the parser for the "getvsn" command
parser_getvsn = subparsers.add_parser('getvsn', help='getvsn help')
parser_getvsn.add_argument('mark6', help='The host name of the mark6 machine to execute the command on')
parser_getvsn.add_argument('slot', type=int,  help='The mark6 slot containing the module to be queried')
parser_getvsn.set_defaults(func=cmd_getvsn)

subparsers.required = True

args = parser.parse_args()
#try:
#    args = parser.parse_args()
#except:
#    parser.print_help()
#    exit(1)

print ("Using multicast group/port %s %d for sending and receiving messages" % (group, port))

# execute the method assigned to the command
if hasattr(args, "func"):
    args.func(args)

exit(0)

