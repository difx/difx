#!/usr/bin/python
#
# Script to run PolConvert at the correlators from with CASA
# This version only works with SWIN files
#

import datetime
import os
import shutil
import re
import sys

pcvers='1.7.4'

# Begin by verifying everthing that should be defined at this point.
# If we can't print something, that's probably enough for a test.

# Between v3 and v4 concatenated -> concatenated | calibrated
# if concatenated.ms is already in label, we'll assume v3 or earlier
# else use newer names here.
print '\nRunning PolConvert Wrapper with label ' + label + ' in ' + DiFXout
v4tables = None
lm = re.match('(.*)\.concatenated.ms', label)
if lm:
    conlabel = lm.group(1) + '.concatenated.ms'
    callabel = lm.group(1) + '.concatenated.ms'
    v4tables = False
else:
    conlabel = label + '.concatenated.ms'
    callabel = label + '.calibrated.ms'
    v4tables = True

# Things that we are expecting to be provided from the QA2 processing
# We use a dictionary to allow name changes (which happened in development).
try:
    aantpath = ('%s/%s.'+qa2['a'])%(DiFXout,conlabel) # ANTENNA
    calapphs = ('%s/%s.'+qa2['c'])%(DiFXout,conlabel) # calappphase
    dtermcal = ('%s/%s.'+qa2['d'])%(DiFXout,callabel) # Df0
    bandpass = ('%s/%s.'+qa2['b'])%(DiFXout,conlabel) # bandpass-zphs
    ampgains = ('%s/%s.'+qa2['g'])%(DiFXout,conlabel) # flux_inf.APP
    phsgains = ('%s/%s.'+qa2['p'])%(DiFXout,conlabel) # phase_int.APP*
    xyrelphs = ('%s/%s.'+qa2['x'])%(DiFXout,callabel) # XY0.APP or XY0.ALMA
    gxyampli = ('%s/%s.'+qa2['y'])%(DiFXout,callabel) # Gxyamp.APP/Gxyamp.ALMA
    if v4tables:
        calgains = [aantpath, calapphs, dtermcal,
                    bandpass, ampgains, phsgains, xyrelphs, gxyampli]
    else:
        calgains = [aantpath, calapphs, dtermcal,
                    bandpass, ampgains, phsgains, xyrelphs]
    for f in calgains:
        if not os.path.exists(f):
            raise Exception, ('Required calibration %s is missing'%f)
        print 'using ' + f
except Exception, ex:
    raise ex

# option to delete specific gain tables from the list
try:
    gdblst = ['bandpass', 'ampgains', 'phsgains', 'xyrelphs', 'gxyampli']
    if type(gainDel) == str and ',' in gainDel:
        for g in gainDel.split(','):
            print 'Deleting ' + gdblst[int(g)]
            del calgains[3+int(g)]
        print 'Revised calgains list is:'
        for c in calgains: print '    ', c
    elif gainDel == '':
        print 'No gain deletion requested'
    else:
        gainDel = ''
        print 'Overriding gainDel -- turning it off'
except Exception, ex:
    print type(gainDel)
    print 'gainDel not str?', str(ex)
    gainDel = ''
    print 'gain deletion turned off'

# option to control gain processing, which should be either
# 'T' (combine) or 'G' (split) for handling of X&Y all gains
# except XY0, bandpass or Gxyamp (which stay 'G'); the eventual
# gaintype list must have the same structure as gains, interpolation.
try:
    if not (gainmeth == 'T' or gainmeth == 'G'):
        gainmeth = 'G'
        print 'illegal gain type supplied, defaulting to', gainmeth
    if gainmeth == 'T': print 'X and Y cals will be combined'
    if gainmeth == 'G': print 'X and Y cals will be split'
except Exception, ex:
    gainmeth = 'G'
    print 'gain type not supplied, defaulting to', gainmeth

# option to average gains over some interval to cut down on noise
# this is a largely untested option: the preference is to do the
# averaging on the QA2 products prior to this stage
try:
    if type(XYavgTime) == float:
        if XYavgTime > 0.0: print 'Gains averaged over %f secs' % XYavgTime
        else:               print 'No time averaging of gains'
    else:
        print 'disabling XYavgTime'
        XYavgTime = 0.0
except Exception, ex:
    print 'XYavgTime not float?', str(ex)
    XYavgTime = 0.0
    print 'XYavgTime set to 0.0 (disabled)'

# option to turn off the amplitude calibration logic
try:
    if type(ampNorm) == float:
        if ampNorm: print 'Amplitude Normalization is ',ampNorm
        else:       print 'Amplitude Normalization is other:',ampNorm
    else:
        print 'Overriding ampNorm -- turning it on'
        ampNorm = 1.0
except Exception, ex:
    print 'ampNorm not float?', str(ex)
    ampNorm = 1.0
    print 'Amplitude Normalization is now',ampNorm

# option for fringe plot pixels
try:
    if type(numFrPltPix) == int:
        print 'Fringe plots with %d pixels at center' % numFrPltPix
    else:
        print 'Overriding numFrPltPix to 50'
        numFrPltPix = 50
except Exception, ex:
    print 'numFrPltPix not int?', str(ex)
    numFrPltPix = 50
    print 'Setting numFrPltPix to 50'

# option to use different peers per scan
# plotAntList.reverse() is to allow plotAntList.pop() below
try:
    if type(plotAntList) == list:
        plotAntList.reverse()
        print 'Popping antennas with indices',plotAntList
    else:
        raise Exception, 'plotAntList not a list'
except Exception, ex:
    print ex, 'Disabled plotAntList, using plotAnt', plotAnt
    plotAntList = []

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
        print "Plotting IF %s on days %d .. %d" % (
            str(plotIF), timeRange[0], timeRange[4])
except Exception, ex:
    raise ex

# Use 1..4 or -1 for the spw
try:
    if type(spwToUse) == int:
        if spwToUse in [-1, 1, 2, 3, 4]: pass
        else:                            spwToUse = -1
except Exception, ex:
    raise ex
print 'Spectral window request is for',spwToUse

#
# A method to drive Polconvert for anticipated Cycle4/5 needs.
# 
# Notice that PolConvert knows that we have a set of SWIN files, because we
# give a directory to the IDI keyword (instead of a file).
# plotAnt=2 selects the baseline betwen antennas 1(AA) and 2(whatever)
#
def runPolConvert(label, spw=-1, DiFXinput='',
    DiFXoutput='', DiFXcalc='', DiFXsave='',
    timeRange=[], doTest=True, savename='', plotIF=-1, doIF=[], 
    amp_norm=1.0, XYadd=[0.0], XYratio=[1.0], linAnt=[1], plotAnt=-1,
    npix=50, gainmeth='T', XYavgTime=0.0):
    # based on common drivepolconvert inputs above
    gains = calgains[3:]
    interpolation = ['linear', 'nearest', 'linear', 'linear']
    dterm = calgains[2]
    Range = []              # do the entire scan
    calAPPTime = [0.0, 8.0] # half-a scan of tolerance

    # allow XY phase table to be dropped if it is noisy
    if constXYadd:
        gains = gains[0:3]
        interpolation = interpolation[0:3]

    gaintype = map(lambda g: 'G' if ('XY0' in g or 'bandpass' in g or
        'Gxyamp' in g) else gainmeth, gains)

    # cover for optional tables
    while len(interpolation) < len(gains):
        interpolation.append('linear')
    print 'gains', len(gains), gains
    print 'interpolation', len(interpolation), interpolation
    print 'gaintype', len(gaintype), gaintype
    print 'PolConvert will use Spectral Window %d on %s' % (spw, label)

    if not os.path.exists(DiFXinput):
        raise Exception, 'No DiFX input %s'%DiFXinput
    if not os.path.exists(DiFXoutput):
        raise Exception, 'No DiFX output %s'%DiFXoutput
    if os.path.exists(DiFXsave):
        raise Exception, 'DiFX save dir exists %s'%DiFXsave

    # ok, save it and proceed
    os.rename(DiFXoutput, DiFXsave)

    # Now we actual run PolConvert setting (almost) everything.
    # Commented arguments are not needed for DiFX, but are
    # mentioned here as comments for clarity and coordination
    # with task_polconvert.py:^def polconvert(...)
    # CASA supplies defaults from the task xml file.
    try:
        print 'Calling PolConvert from runpolconvert'
        polconvert(IDI=DiFXsave, OUTPUTIDI=DiFXoutput,
            DiFXinput=DiFXinput, DiFXcalc=DiFXcalc,
            doIF=doIF,
            linAntIdx=linAnt, Range=Range, ALMAant=aantpath,
            spw=spw, calAPP=calapphs, calAPPTime=calAPPTime,
            #APPrefant,
            gains=[gains], interpolation=[interpolation],
            gainmode=[gaintype], XYavgTime=XYavgTime,
            dterms=[dterm], amp_norm=amp_norm,
            XYadd=XYadd,
            #XYdel,
            XYratio=XYratio, swapXY=[False],
            #swapRL,
            IDI_conjugated=True,
            plotIF=plotIF, plotRange=timeRange,
            plotAnt=plotAnt,
            #excludedAnts, doSolve, solint,
            doTest=doTest, npix=npix,
            solveAmp=False
            #, solveMethod=gradient, calstokes, calfield
            )
    except Exception, ex:
        print 'Polconvert Exception'
        if (os.path.exists(DiFXoutput)):
            shutil.rmtree(DiFXoutput)
        os.rename(DiFXsave, DiFXoutput)
        raise ex

    try:
        makeHistory(label, DiFXoutput, doIF=doIF, linAntIdx=linAntIdx,
            spw=spw, calAPPTime=calAPPTime, interpolation=[interpolation],
            gainmode=[gaintype], XYavgTime=XYavgTime, amp_norm=amp_norm,
            XYadd=XYadd, XYratio=XYratio, swapXY=[False])
    except Exception, ex:
        print 'Ignoring exception while writing history:', str(ex)

    # save the plots and other developer artifacts in a subdir
    pcprods = [ 'PolConvert.log', 'Fringe.plot%d.png'%plotAnt,
                'Kmatrix_AMP.png', 'Kmatrix_PHAS.png',
                'FRINGE.PEAKS.dat', 'POLCONVERT.FRINGE', 'POLCONVERT.GAINS',
                'POLCONVERT_STATION1.ANTAB', 'CONVERSION.MATRIX',
                'FRINGE.PEAKS', 'FRINGE.PLOTS' ]
    # this is used only in non-parallel execution
    if savename != '':
        now = datetime.datetime.now()
        outdir = now.strftime(savename + '.polconvert-%Y-%m-%dT%H.%M.%S')
        os.mkdir(outdir)
        for art in pcprods:
            if os.path.exists(art):
                os.rename(art, outdir + '/' + art)
        print savename + ' results moved to ' + outdir

def makeHistory(label, DiFXoutput, doIF=[], linAntIdx=[], spw=-1,
    calAPPTime=[], interpolation=[], gainmode=[], XYavgTime=0.0,
    amp_norm=1.0, XYadd=XYadd, XYratio=XYratio, swapXY=[]):
    '''
    Generate a history record for eventual use by difx2fits
    '''
    if not os.path.isdir(DiFXoutput):
        print 'No directory for polconvert history'
        return
    history = DiFXoutput + '/polconvert.history'
    fh = open(history, 'w')
    fh.write('PolConvert %s: %s\n' % (pcvers,
        datetime.datetime.now().strftime('%Y-%m-%dT%H.%M.%S')))
    fh.write('QA2 set: %s\n' % label)
    iflist = str(doIF)
    while len(iflist) > 0:
        fh.write('IF list: %s\n' % iflist[0:60])
        iflist = iflist[60:]
    fh.write('linAntIdx: %s == %s\n' % (str(linAntIdx), 'AA'))
    fh.write('spw: %s\n' % str(spw))
    fh.write('calAPPTime: %s\n' % str(calAPPTime))
    fh.write('interpolation: %s\n' % str(interpolation))
    fh.write('gainmode: %s\n' % str(gainmode))
    fh.write('XYavgTime: %s\n' % str(XYavgTime))
    fh.write('amp_norm: %s\n' % str(amp_norm))
    fh.write('XYadd: %s\n' % str(XYadd))
    fh.write('XYratio: %s\n' % str(XYratio))
    fh.write('swapXY: %s\n' % str(swapXY))
    fh.close()
    print 'Wrote history to', history

for job in djobs:
    # DiFX output dir and input files:
    # DiFXout is defined in the input.
    DiFXinput = ('%s/%s_%s.input' % (DiFXout,expName,job))
    #DiFXcalc = ('%s/%s_%s.calc' % (DiFXout,expName,job))
    #DiFXcalc is not used in the default ALMA implementation
    SWIN = ('%s/%s_%s.difx' % (DiFXout,expName,job))
    SAVE = ('%s/%s_%s.save' % (DiFXout,expName,job))

    # plotAntList was reversed so that pop delivers the next
    if len(plotAntList) > 0: usePlotAnt = plotAntList.pop()
    else:                    usePlotAnt = plotAnt

    if DiFXout == '.': thesavename = expName + '_' + job
    else:              thesavename = ''

    print '\nProceeding with job ' + job + '\n'
    runPolConvert(label, spw=spwToUse,
        DiFXinput=DiFXinput, DiFXcalc='', DiFXoutput=SWIN, DiFXsave=SAVE,
        amp_norm=ampNorm, XYadd=XYadd, XYratio=XYratio,
        timeRange=timeRange, doTest=doTest, savename=thesavename,
        plotIF=plotIF, doIF=doIF, linAnt=linAnt, plotAnt=usePlotAnt,
        npix=numFrPltPix, gainmeth=gainmeth, XYavgTime=XYavgTime)
    print '\nFinished with job ' + job + '\n'

# make sure the last of the log gets written out so we are sure we are done
print 'Finished with runpolconvert tasks'
sys.stdout.flush()

#
# eof
#
