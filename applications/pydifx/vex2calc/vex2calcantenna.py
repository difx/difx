#!/usr/bin/python
"""
TODO make write_antenna more like log2clock returning a list rather than printing directly
"""
import sys
import observation

from pfile import print_parameter
from readvex import VexSite, VexAntenna


def write_antenna(vex_path, calcfile, antennas = None):
    site = VexSite(vex_path)
    antenna = VexAntenna(vex_path)
    # cross match telescope names against input file in final version?
    i = 0
    if not antennas:
        try:
            antennas = observation.antennas
        except AttributeError:
            antennas = site.id_dict().keys()
            antennas.sort()

    print "Writing antenna table of calc file"
    print_parameter("NUM TELESCOPES", len(antennas), calcfile)
    for telescope in antennas:
        name = telescope.upper()
        fullname = site.id_dict()[telescope]
        print_parameter("TELESCOPE %d NAME" % i, name, calcfile)
        # TODO look up different types of mount and how they are notated in vex file and
        # for calc file
        if True:
        #if antenna[site[telescope]['site_name']]['axis_type'] == ['az', 'el']:
            print_parameter("TELESCOPE %d MOUNT" % i,  'azel', calcfile)
        else:
            print antenna[site[fullname]['site_name']]['axis_type'][0]
            raise RuntimeError, 'unknown axis type'
        print_parameter("TELESCOPE %d OFFSET (m)" % i, '%.6f' % (antenna[fullname]['axis_offset'][-1]), calcfile)
        print_parameter("TELESCOPE %d X (m)" % i, '%.6f' % site[fullname]['site_position'][0], calcfile)
        print_parameter("TELESCOPE %d Y (m)" % i, '%.6f' % site[fullname]['site_position'][1], calcfile)
        print_parameter("TELESCOPE %d Z (m)" % i, '%.6f' % site[fullname]['site_position'][2], calcfile)
        #print_parameter("TELESCOPE %d SHELF" % i, "NONE", calcfile)
        i += 1

def main():
    """
Extract $SITE and $ANTENNA tables from vexfile to create .calc antenna table

Usage:
    vex2calcantenna.py vex.file ["TELESCOPE LIST"]

"TELESCOPE LIST" (optional) is a list of the telescope names as they appear in
the vex $SITE table, enclosed in "quotes" and seperated only by whitespace.

if TELESCOPE LIST is not defined, the antennas value will be read from observation.py

if antennas is not defined in observation.py, all antennas in the vex file will be used
in alphabetical order.
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit()
    try:
        vex_path = sys.argv[1]
    except IndexError:
        print main.__doc__
        sys.exit(2)

    try:
        antennas = sys.argv[2]
        antennas = antennas.split()
    except IndexError:
        antennas = None

    write_antenna(vex_path, sys.stdout, antennas)

if __name__ == '__main__':
    main()
