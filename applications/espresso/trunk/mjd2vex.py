#!/usr/bin/python
# convert between DiFX date formats.
# Cormac Reynolds: June 2010

import optparse, espressolib

#parse the options
usage = '''%prog <date>
converts <date> from mjd to vex or vice versa
Multiple dates can be given (separated by spaces)'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
parser.add_option( "--format", "-f",
        type='str', dest="outformat", default=None,
        help='Output format (vex, vlba, iso, mjd). Default is mjd unless first input format is mjd, then vex is default')
(options, args) = parser.parse_args()

if len(args) < 1:
    parser.print_help()
    parser.error("At least 1 date required")

# by default, convert to MJD except for MJD which converts to vex
if not options.outformat:
    try:
        indate = float(args[0])
        options.outformat = 'vex'
    except ValueError:
        options.outformat = 'mjd'

for indate in args:
    print espressolib.convertdate(indate, options.outformat)
