#!/usr/bin/python
#
# Script to run PolConvert at the correlators from with CASA
# This version only works with SWIN files
#

import datetime
import os
import shutil

# Things that we are provided from the QA2 processing
try:
    antpath = '%s.antenna.tab'%label
    CALAPP = '%s.calappphase.tab'%label
    bandpass = '%s.bandpass-zphs.cal'%label
    ampgains = '%s.ampgains.cal.fluxscale'%label
    phsgains = '%s.phasegains.cal'%label
    xyrelphs = '%s.XY0amb-tcon'%label
    for f in [antpath, CALAPP, bandpass, ampgains, phsgains, xyrelphs]:
        if not os.path.exists(f):
            raise Exception, ('Required calibration %s is missing'%f)
except Exception, ex:
    raise ex

# these should probably be defined:
#  expName linAnt doIF plotIF timeRange XYadd
# FIXME

# one of of these should be True, the others False
try:
    if not (band3 or band6Lo or band6Hi):
        raise Exception, 'One of band3 or band6Lo or band6Hi must be True'
    if (band3 and band6Lo) or (band3 and band6Hi) or (band6Lo and band6Hi):
        raise Exception, 'Only one of band3 or band6Lo or band6Hi may be True'
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
    XYadd=[0.0], linAnt=[1], plotAnt=-1):
    gains=['%s.bandpass-zphs.cal'%label,
        '%s.ampgains.cal.fluxscale'%label, '%s.phasegains.cal'%label,
        '%s.XY0amb-tcon'%label]
    Range = [] # do the entire scan
    calAPPTime = [0.0, 8.0] # half-a scan of tolerance

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
            linAntIdx=[1], Range=Range, ALMAant=antpath,
            spw=spw, calAPP=CALAPP, calAPPTime=calAPPTime,
            gains=[gains], dterms=['NONE'], amp_norm=True,
            XYadd=XYadd, swapXY=[False], IDI_conjugated=True,
            plotIF=plotIF, doIF=doIF, plotRange=timeRange,
            plotAnt=plotAnt, doTest=doTest)
    except Exception, ex:
        print 'Polconvert Exception'
        shutil.rmtree(DiFXoutput)
        os.rename(DiFXsave, DiFXoutput)
        raise ex

    # clean up for retry on problems at this point:
    # FIXME

    # save the plots in a subdir
    pcprods = [ 'PolConvert.log', 'Fringe.plot%d.png'%plotAnt,
                'Kmatrix_AMP.png', 'Kmatrix_PHAS.png',
                'FRINGE.PEAKS.dat', 'POLCONVERT.FRINGE', 'POLCONVERT.GAINS' ]
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
        DiFXinput=DiFXinput, DiFXoutput=SWIN, DiFXsave=SAVE, XYadd=XYadd,
        timeRange=timeRange, doTest=doTest, savename=expName + '_' + job,
        plotIF=plotIF, doIF=doIF, linAnt=linAnt, plotAnt=plotAnt)

#
# eof
#
