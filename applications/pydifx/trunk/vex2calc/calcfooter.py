#!/usr/bin/python
"""
Write last few lines of the calcfile.
"""
import sys

from pfile import print_parameter

def write_footer(rootname, calcfile = sys.stdout):
    """
    Write last 2 lines of the calc file.
    """
    print "Write last 2 lines of the calc file"
    print_parameter("DELAY FILENAME", rootname + '.delay', calcfile)
    print_parameter("UVW FILENAME",   rootname + '.uvw', calcfile)

def main():
    """
Write a calc footer to stdout

Usage:
    calcfooter.py rootname

    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    if len(sys.argv) > 3:
        print main.__doc__
        print "Error: Too many arguments"
        sys.exit(2)
    rootname = sys.argv[1]
    write_footer(rootname)

if __name__ == '__main__':
    main()
