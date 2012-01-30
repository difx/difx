#!/usr/local/bin/ParselTongue
"""
Quick ParselTongue hack to convert DELAY files to an SN table

Edit __main__ below to set parameters

The main functionality of the script requires only ParselTongue.

If you want the plotting and to run CLCAL you'll also need the EVN pipeline
http://www.jive.nl/wiki/doku.php?id=parseltongue:grimoire
or at least evn_funcs.py and evn_aips_tasks.py in your PYTHONPATH
(or you can just comment these parts out)

Note that only the first job is processed (or rather the first .difx
directory returned by glob("%s_*.difx" % obscode))

Note further that only the first line of the delay file is processed.
This results in a single SN line per antenna

Note that this script is UNTESTED with
 - LSB data
 - Band matching/zoom bands
"""
from glob import glob
from math import floor, sin, cos, pi

from AIPS import AIPS, AIPSDisk
from AIPSTask import AIPSTask
from AIPSData import AIPSUVData, AIPSImage
from Wizardry.AIPSData import AIPSUVData as wuv

def init_sn_row(row):
    row.antenna_no = 1
    row.freq_id = 0
    row.i_far_rot = 0.
    row.mbdelay1 = 0.
    row.mbdelay2 = 0.
    row.node_no = 0
    row.source_id = 0
    row.subarray = 1
    row.time = 0.
    row.time_interval = 0.
    for i in range(len(row.real1)):
        row.delay_1[i] = 0.0
        row.delay_2[i] = 0.0
        row.imag1[i] = 0.0
        row.imag2[i] = 0.0
        row.rate_1[i] = 0.0
        row.rate_2[i] = 0.0
        row.real1[i] = 1.0
        row.real2[i] = 1.0
        row.refant_1[i] = 0
        row.refant_2[i] = 0
        row.weight_1[i] = 1.0
        row.weight_2[i] = 1.0

def delay2sn(dfile, row, ant):
    """
    Write first line in DELAY file into SN table row

    dl - open DELAY file line
    row - SN table row
    ant - antenna number
    """

    #breaks after finding first valid line
    for line in dfile:
        if line.startswith('#'):
            continue
        line = line.split()
        npol = int(line[3])
        nsubband = int(line[5])
        if not len(row.real1) == nsubband/npol:
            print "Warning: Number of IFs in SN table %d  and DELAYS file %d do not match -- skipping to next line" % (len(row.real1), nsubband/npol)

        time = float(line[1])
        time_int = float(line[2])
        iat0 = floor(time - (time_int/2.0))
        row.antenna_no = ant
        row.time =  time - iat0
        row.time_interval = time_int

        delays = [float(x) for x in line[6:]]
        if len(delays) < 2*nsubband:
            print "Num of delays only %d" % len(delays)
        reffreq = delays[0]
        refdelay = -delays[1]*1e-6 #reference all delays to first IF
        iat0 = float(line[2])
        i = 0
        for pol in range(npol):
            for band in range(nsubband/npol):
                freq = delays[i*3]*1e6
                delay = -delays[i*3+1]*1e-6
                phase = -delays[i*3+2]

                #account for refdelay in phase
                #FIXME use divmod to remove rounding errors
                phase += 2*pi*refdelay*(reffreq-freq)

                real = cos(phase)
                imag = sin(phase)
                if pol == 0:
                    row.delay_1[band] = delay - refdelay
                    row.real1[band] = real
                    row.imag1[band] = imag
                else:
                    row.delay_2[band] = delay - refdelay
                    row.real2[band] = real
                    row.imag2[band] = imag
                i+=1
        return row


if __name__=='__main__':
    #Import a few convenience functions from the EVN pipeline
    from evn_funcs import plot
    from evn_aips_tasks import runclcal, runpossm, set_default_aparms

##############################################################################
    AIPS.userno = 3
    uvdata = AIPSUVData('MULTI', 'UVDATA', 1, 1)
    sn_in = 1 #can be any old sn table, just needed as a template
    sn_out = 2 #SN table to write (existing sn_out and above will be deleted!)
    cl_in = 1 #CLCAL gainver
    cl_out = 2 #CLCAL gainuse (existing cl_out and above will be deleted!)
    obscode = 'ba097' # used to find obscode_*.difx/DELAY_??
    plotref = [1,2,3,4,5] # only plot baselines to these antennas
    outdir = '.' # for plots
##############################################################################

    #delete tables above and including clout and snout
    while uvdata.table_highver('AIPS CL') >= cl_out:
        uvdata.zap_table('AIPS CL', 0)
    while uvdata.table_highver('AIPS SN') >= sn_out:
        uvdata.zap_table('AIPS SN', 0)

    #plot data without new table
    aparm = set_default_aparms('possm')
    uvdata.zap_table('AIPS PL', -1)
    runpossm(indata=uvdata, aparm=aparm, antennas=plotref)
    plot(uvdata, outdir + '/' + 'possm_uncal.PS')

    # map 2-letter antenna names to numbers
    antdir={}
    an=uvdata.table('AN', 1)
    for line in an:
        antcode=line.anname.rstrip()
        antnum=int(line.nosta)
        antdir[antcode]=antnum

    # set up new SN table
    wdata = wuv(uvdata)
    oldsn = wdata.table('SN', sn_in)
    # set up keywords
    newsn = wdata.attach_table('SN', sn_out)
    newsn.keywords['NO_ANT']   = oldsn.keywords['NO_ANT']
    newsn.keywords['NO_IF']    = oldsn.keywords['NO_IF']
    newsn.keywords['NO_NODES'] = oldsn.keywords['NO_NODES']
    newsn.keywords['NO_POL']   = oldsn.keywords['NO_POL']
    snrow = oldsn[0]
    init_sn_row(snrow)

    # go through delay files, reading first line
    # and adding a row to the SN table
    jobfiles = glob("%s_*.difx" % obscode)
    jobfiles.sort()
    if len(jobfiles) < 1:
        print "Error: .difx file not found"
    for jobfile in jobfiles:
        print "%s/DELAY_*" % jobfile
        delayfiles = glob("%s/DELAY_*" % jobfile)
        if delayfiles == []:
            print "Error: files not found"
        for filename in delayfiles:
            print "processing file %s" % filename
            infile = open(filename)
            try:
                antnum = antdir[filename[-2:]]
            except:
                print "Can't match file %s to any antenna in AN table. Skipping" % filename
                continue
            newsn.append(delay2sn(infile, snrow, antnum))
        break # just first job for now
    newsn.close()

    #runclcal
    runclcal(gainver=cl_in, gainuse=cl_out, indata=uvdata, snver=2, refant=1, interpol='')

    #plot data with new table
    aparm[1] = 1 # vector average 
    aparm[5] = 0
    aparm[6] = 0
    uvdata.zap_table('AIPS PL', -1)
    runpossm(indata=uvdata, aparm=aparm, docalib=1, antennas=plotref)
    plot(uvdata, outdir + '/' +  'possm_cal.PS')
