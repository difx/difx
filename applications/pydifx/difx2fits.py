#!/usr/bin/python
"""
run difx2fits program.
"""
import sys
import getopt
from shutil import rmtree

import observation
from spawn import spawn, EOF

defreg = ["\r\n", EOF]

class spawnclass:
    def __init__(self, obj):
        import difxlog as log
        self.log = log

    def next(self, i, child):
        if i == 0:
            self.log.debug(child.before)
            return 0
        if i == 1:
            return 1

def run_difx2fits(basefilename, fitsfilename, options = None, delete = None):
    if options == None:
        options = observation.difx2fits_options
    if delete == None:
        delete = observation.difx2fits_delete

    spawn(' '.join(('difx2fits', options , basefilename, fitsfilename)), reg = defreg, reclass = spawnclass)
    
    if delete:
        rmtree(basefilename + '.difx')

def main():
    """
    Run difx2fits

    Usage:
        difx2fits.py basefilename [fitsfilename]

    If fitsfilename is not given, basefile name is converted
    to capitals and given the suffix '.FITS'

    difx2fits.py options:
    -d --delete   delete the .difx data after conversion

    difx2fits options:
      --average <nchan>
      -a        <nchan>   Average <nchan> channels

      --beginchan <chan>
      -b          <chan>  Skip <chan> correlated channels

      --no-model
      -n                  Don't write model (ML) table

      --outchans <nchan>
      -o         <nchan>  Write <nchan> channels

      --scale <scale>
      -s      <scale>     Scale visibility data by <scale>

      --verbose
      -v                  Be verbose.  -v -v for more!

    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "a:b:no:s:vv", ["average=", "beginchan=", "no-model", "outchans=", "scale=", "verbose"])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if not 1 <= len(args) <= 2:
        print "Error: Wrong number of Arguments"
        print main.__doc__
        sys.exit(2)

    # set defaults
    scale = False
    delete = False

    # read arguments
    basefilename = args[0]
    fitsfilename = basefilename.upper() + '.FITS'
    if len(args) > 1:
        fitsfilename = args[1]
    
    if len(opts) > 0:
        options = ''
        for o, a in opts:
            options += ' ' + ' '.join((o, a))
            if o in ("-d", "--delete"):
                delete = True
                opts.remove((o, a))
            if o in ("-s", "--scale"):
                scale = True
    else:
        options = None
    
    import difxlog as log
    if not scale:
        log.warning("No scaling of fits file!")

    run_difx2fits(basefilename, fitsfilename, options, delete)

if __name__ == "__main__":
    main()
