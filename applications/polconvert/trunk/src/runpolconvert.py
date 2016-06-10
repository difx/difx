#!/usr/bin/python
#
# Script to run PolConvert at the correlators from with CASA
# This version only works with SWIN files
#

import datetime
import os
import shutil

# Things that we are provided from the QA2 processing
if True:
    #antpath = '../polconvert-take2/concat.ms/ANTENNA'
    label = 'concat.ms.1'
    antpath = '%s.antenna.tab'%label
    CALAPP = '%s.calappphase.tab'%label

# which jobs to process
#if False:
    #band3=False
    #band6Lo=False
    #band6Hi=False
    #doTest=False
    #XYadd=[0.0]
    # DiFX work is organized by experiment and job
    #djobs=['179']
    # an antenna to plot with LMT=2, SMAP=3
    #plotAnt=2 # if used
    #plotAnt=3 # if used

# April 08 2016 setup
if band6Hi or band6Lo:
    exp='e16b08'
    # linear antenna ALMA=1
    linAnt=[1]
    # the zoom channels; plot central one
    # add 1 to ZOOM.FREQ.INDEX numbers
    SWINIFS = range(35,66)
    MidIF = SWINIFS[len(SWINIFS)/2]
    # first 10 days of observations
    trfixme = [0,0,0,0, 10,0,0,0]

# Some other setups...

#
# A method to drive Polconvert
# 
# Notice that PolConvert knows that we have a set of SWIN files, because we
# give a directory to the IDI keyword (instead of a file).
# plotAnt=2 selects the baseline betwen antennas 1(AA) and 2
#
def runPolConvert(label, band3=False, band6Lo=False, band6Hi=False,
    DiFXinput='', DiFXoutput='', DiFXsave='',
    timeRange=[], doTest=True, savename='', plotIF=0, doIF=[], 
    XYadd=[0.0], linAnt=[1], plotAnt=2):
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
            plotIF=plotIF, doIF=SWINIFS, plotRange=timeRange,
            plotAnt=plotAnt, doTest=doTest)
    except Exception, ex:
        print 'Polconvert Exception'
        shutil.rmtree(DiFXoutput)
        os.rename(DiFXsave, DiFXoutput)
        raise ex

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
    DiFXinput = ('%s/%s_%s.input' % (DiFXout,exp,job))
    SWIN = ('%s/%s_%s.difx' % (DiFXout,exp,job))
    SAVE = ('%s/%s_%s.save' % (DiFXout,exp,job))

    runPolConvert(label, band3=band3, band6Lo=band6Lo, band6Hi=band6Hi,
        DiFXinput=DiFXinput, DiFXoutput=SWIN, DiFXsave=SAVE, XYadd=XYadd,
        timeRange=trfixme, doTest=doTest, savename=exp + '_' + job,
        plotIF=MidIF, doIF=SWINIFS, linAnt=linAnt, plotAnt=plotAnt)

#
# eof
#
