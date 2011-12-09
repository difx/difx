#!/usr/bin/python
# convert the given mjd to a vex time or vice versa
# Cormac Reynolds: June 2010

import optparse, espressolib

#parse the options
usage = '''%prog <date>
converts <date> from mjd to vex or vice versa
Multiple dates can be given (separated by spaces)'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
(options, args) = parser.parse_args()

if len(args) < 1:
    parser.print_help()
    parser.error("At least 1 date required")

for indate in args:
    print espressolib.mjd2vex(indate)
