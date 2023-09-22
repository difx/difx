=== ParseDiFX Python parser for DiFX output files ===

This package parses DiFX .input files and extracts metadata.

It also provides access to visibility data records stored in
the Swinburne binary format in *.difx/DIFX_* files.

-- Prerequisites --

Python numpy is recommended. When numpy is available, the
parser uses numpy.fromfile(difxfile, dtype='complex64') to
read the raw visibility data.

-- Example --

import parseDiFX

difx = parseDiFX.DiFXFile()
difx.open('test_1000.input')

vr = difx.nextVisibilityRecord()
while vr.isvalid():
        h = vr.header
        info = 'MJD %d/%ds : ant %d x %d fq#%d %s : %d values' % (
                h.mjd, h.seconds, h.antenna1, h.antenna2,
                h.freqindex, h.polpair,
                len(vr.vis)
        )
        print (info)
        vr = difx.nextVisibilityRecord()

