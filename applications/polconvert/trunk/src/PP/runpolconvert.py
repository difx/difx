#!/usr/bin/python
#
# Script to run PolConvert at the correlators from with CASA
# This version only works with SWIN files
#

import datetime
import os
import shutil

# Begin by verifying everthing that should be defined at this point.
# If we can't print something, that's probably enough for a test.

# Things that we are expecting to be provided from the QA2 processing
# We use a dictionary to allow name changes (which happened in development).
try:
    aantpath = ('%s.'+qa2['a'])%label # '%s.antenna.tab'%label
    calapphs = ('%s.'+qa2['c'])%label # '%s.calappphase.tab'%label
    bandpass = ('%s.'+qa2['b'])%label # '%s.bandpass-zphs.cal'%label
    dtermcal = ('%s.'+qa2['d'])%label # '%s.Df0'%label
    ampgains = ('%s.'+qa2['g'])%label # '%s.ampgains.cal.fluxscale'%label
    phsgains = ('%s.'+qa2['p'])%label # '%s.phasegains.cal'%label
    xyrelphs = ('%s.'+qa2['x'])%label # '%s.XY0amb-tcon'%label
    calgains = [aantpath, calapphs, dtermcal,
                bandpass, ampgains, phsgains, xyrelphs]
    for f in calgains:
        if not os.path.exists(f):
            raise Exception, ('Required calibration %s is missing'%f)
        print 'using ' + f
except Exception, ex:
    raise ex

# require constXYadd to be set to allow disabling table
try:
    if type(constXYadd) == bool:
        if constXYadd: print 'Disabling XY phase table'
        else:          print 'Using XY phase table ' + xyrelphs
    else:
        raise Exception, 'constXYadd must be set True or False'
except Exception, ex:
    raise ex

# Things defined in, e.g. drivepolconvert.py
try:
    print "Experiment %s with linear pol antenna index %s\non IFs %s" % (
        expName, str(linAnt), str(doIF))
    if plotIF > 0:
        print "Plotting IF %d on days %d .. %d)" % (
            plotIF, timeRange[0], timeRange[4])
except Exception, ex:
    raise ex

# one of of these should be True, the others False
try:
    if not (band3 or band6Lo or band6Hi):
        raise Exception, 'One of band3 or band6Lo or band6Hi must be True'
    if (band3 and band6Lo) or (band3 and band6Hi) or (band6Lo and band6Hi):
        raise Exception, 'Only one of band3 or band6Lo or band6Hi may be True'
    if band3: print 'Band 3 operation'
    if band6Lo: print 'Band 6 (Lo/SPW 0) operation'
    if band6Hi: print 'Band 6 (Hi/SPW 1) operation'
except Exception, ex:
    raise ex

#
# A method to drive Polconvert for anticipated Cycle4 needs.
# 
# Notice that PolConvert knows that we have a set of SWIN files, because we
# give a directory to the IDI keyword (instead of a file).
# plotAnt=2 selects the baseline betwen antennas 1(AA) and 2
#
def runPolConvert(label, band3=False, band6Lo=False, band6Hi=False,
    DiFXinput='', DiFXoutput='', DiFXsave='',
    timeRange=[], doTest=True, savename='', plotIF=-1, doIF=[], 
    XYadd=[0.0], XYratio=[1.0], linAnt=[1], plotAnt=-1):
    # based on common drivepolconvert inputs above
    gains = calgains[3:]
    interpolation = ['linear', 'self', 'linear', 'linear']
    dterm = calgains[2]
    Range = [] # do the entire scan
    calAPPTime = [0.0, 8.0] # half-a scan of tolerance

    # allow XY phase table to be dropped if it is noisy
    if constXYadd:
        gains = gains[0:3]
        interpolation = interpolation[0:3]

    if band3:
        spw=0
    elif band6Lo:
        spw=0
    elif band6Hi:
        spw=1
    else:
        raise Exception, 'No band selected for PolConvert to work with'

    if not os.path.exists(DiFXinput):
        raise Exception, 'No DiFX input %s'%DiFXinput
    if not os.path.exists(DiFXoutput):
        raise Exception, 'No DiFX output %s'%DiFXoutput
    if os.path.exists(DiFXsave):
        raise Exception, 'DiFX save dir exists %s'%DiFXsave

    # ok, save it and proceed
    os.rename(DiFXoutput, DiFXsave)

    # actually run PolConvert setting everything.
    try:
        polconvert(IDI=DiFXsave, OUTPUTIDI=DiFXoutput, DiFXinput=DiFXinput,
            linAntIdx=[1], Range=Range, ALMAant=aantpath,
            spw=spw, calAPP=calapphs, calAPPTime=calAPPTime,
            gains=[gains], interpolation=[interpolation],
            dterms=[dterm], amp_norm=True,
            XYadd=XYadd, XYratio=XYratio, swapXY=[False], IDI_conjugated=True,
            plotIF=plotIF, doIF=doIF, plotRange=timeRange,
            plotAnt=plotAnt, doTest=doTest)
    except Exception, ex:
        print 'Polconvert Exception'
        shutil.rmtree(DiFXoutput)
        os.rename(DiFXsave, DiFXoutput)
        raise ex

    # save the plots and other developer artifacts in a subdir
    pcprods = [ 'PolConvert.log', 'Fringe.plot%d.png'%plotAnt,
                'Kmatrix_AMP.png', 'Kmatrix_PHAS.png',
                'FRINGE.PEAKS.dat', 'POLCONVERT.FRINGE', 'POLCONVERT.GAINS',
                'POLCONVERT_STATION1.ANTAB', 'CONVERSION.MATRIX',
                'FRINGE.PEAKS', 'FRINGE.PLOTS' ]
    if savename != '':
        now = datetime.datetime.now()
        outdir = now.strftime(savename + '.polconvert-%Y-%m-%dT%H:%M:%S')
        os.mkdir(outdir)
        for art in pcprods:
            if os.path.exists(art):
                os.rename(art, outdir + '/' + art)

for job in djobs:
    # DiFX output dir and input files:
    DiFXout = '.'
    DiFXinput = ('%s/%s_%s.input' % (DiFXout,expName,job))
    SWIN = ('%s/%s_%s.difx' % (DiFXout,expName,job))
    SAVE = ('%s/%s_%s.save' % (DiFXout,expName,job))

    runPolConvert(label, band3=band3, band6Lo=band6Lo, band6Hi=band6Hi,
        DiFXinput=DiFXinput, DiFXoutput=SWIN, DiFXsave=SAVE,
        XYadd=XYadd, XYratio=XYratio,
        timeRange=timeRange, doTest=doTest, savename=expName + '_' + job,
        plotIF=plotIF, doIF=doIF, linAnt=linAnt, plotAnt=plotAnt)

#
# eof
#
