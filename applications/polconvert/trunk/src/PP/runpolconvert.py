#!/usr/bin/python
#
# Script to run PolConvert at the correlators from with CASA
# This version only works with SWIN files
#

import datetime
import os
import shutil
import re

# Begin by verifying everthing that should be defined at this point.
# If we can't print something, that's probably enough for a test.

# Between v3 and v4 concatenated -> concatenated | calibrated
# if concatenated.ms is already in label, we'll assume v3 or earlier
# else use newer names here.
print '\nRunning PolConvert Wrapper with label ' + label
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
    aantpath = ('%s.'+qa2['a'])%conlabel # ANTENNA
    calapphs = ('%s.'+qa2['c'])%conlabel # calappphase
    dtermcal = ('%s.'+qa2['d'])%callabel # Df0
    bandpass = ('%s.'+qa2['b'])%conlabel # bandpass-zphs
    ampgains = ('%s.'+qa2['g'])%conlabel # flux_inf.APP
    phsgains = ('%s.'+qa2['p'])%conlabel # phase_int.APP
    xyrelphs = ('%s.'+qa2['x'])%callabel # XY0.APP or XY0.ALMA
    gxyampli = ('%s.'+qa2['y'])%callabel # Gxyamp.APP or Gxyamp.ALMA
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

# option to turn off the amplitude calibration logic
try:
    if type(ampNorm) == bool:
        if ampNorm: print 'Amplitude Normalization is done'
        else:       print 'Amplitude Normalization is off'
    else:
        print 'Overriding ampNorm -- turning it on'
        ampNorm = True
except Exception, ex:
    print 'ampNorm not bool?', str(ex)
    ampNorm = True
    print 'Amplitude Normalization is on'

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

# one of of these should be True, the others False
try:
    if not (band3 or band6Lo or band6Hi):
        raise Exception, 'One of band3 or band6Lo or band6Hi must be True'
    if (band3 and band6Lo) or (band3 and band6Hi) or (band6Lo and band6Hi):
        raise Exception, 'Only one of band3 or band6Lo or band6Hi may be True'
    if band3: print 'Band 3 operation'
    if band6Lo: print 'Band 6 Lo operation'
    if band6Hi: print 'Band 6 Hi operation'
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
    amp_norm=True, XYadd=[0.0], XYratio=[1.0], linAnt=[1], plotAnt=-1,
    npix=50, gainmeth='G'):
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
        'Gxyamp' in g) else 'T', gains)

    # cover for optional tables
    while len(interpolation) < len(gains):
        interpolation.append('linear')
    print 'gains', len(gains), gains
    print 'interpolation', len(interpolation), interpolation
    print 'gaintype', len(gaintype), gaintype

    if band3:
        spw=0
        bnd='Band 3'
    elif band6Lo:
        spw=2
        bnd='Band 6 Lo'
    elif band6Hi:
        spw=3
        bnd='Band 6 Hi'
    else:
        raise Exception, 'No band selected for PolConvert to work with'
    try:
        if spwToUse != 4: spw = spwToUse
    except:
        print 'spwToUse was not defined'
    print 'PolConvert will use Spectral Window %d for %s on %s' % (
        spw, bnd, label)

    if not os.path.exists(DiFXinput):
        raise Exception, 'No DiFX input %s'%DiFXinput
    if not os.path.exists(DiFXoutput):
        raise Exception, 'No DiFX output %s'%DiFXoutput
    if os.path.exists(DiFXsave):
        raise Exception, 'DiFX save dir exists %s'%DiFXsave

    # ok, save it and proceed
    os.rename(DiFXoutput, DiFXsave)

    # actually run PolConvert setting everything.
    # commented arguments are not needed for DiFX, but are
    # mentioned here as comments for clarity.  CASA supplies
    # defaults from the task xml file.
    # Note that this is hardwired to just one antenna conversion
    # even though PolConvert may do several.
    try:
        print 'Calling PolConvert from runpolconvert'
        polconvert(IDI=DiFXsave, OUTPUTIDI=DiFXoutput, DiFXinput=DiFXinput,
            #DiFXcalc,
            linAntIdx=[1], Range=Range, ALMAant=aantpath,
            spw=spw, calAPP=calapphs, calAPPTime=calAPPTime,
            #APPrefant,
            gains=[gains], interpolation=[interpolation],
            gaintype=[gaintype],
            dterms=[dterm], amp_norm=amp_norm,
            XYadd=XYadd,
            #XYdel,
            XYratio=XYratio, swapXY=[False], IDI_conjugated=True,
            plotIF=plotIF, doIF=doIF, plotRange=timeRange,
            plotAnt=plotAnt,
            #excludedAnts, doSolve, solint
            doTest=doTest, npix=npix,
            solveAmp=False
            # , solveMethod=gradient
            )
    except Exception, ex:
        print 'Polconvert Exception'
        if (os.path.exists(DiFXoutput)):
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
        outdir = now.strftime(savename + '.polconvert-%Y-%m-%dT%H.%M.%S')
        os.mkdir(outdir)
        for art in pcprods:
            if os.path.exists(art):
                os.rename(art, outdir + '/' + art)
        print savename + ' results moved to ' + outdir

for job in djobs:
    # DiFX output dir and input files:
    DiFXout = '.'
    DiFXinput = ('%s/%s_%s.input' % (DiFXout,expName,job))
    SWIN = ('%s/%s_%s.difx' % (DiFXout,expName,job))
    SAVE = ('%s/%s_%s.save' % (DiFXout,expName,job))

    print '\nProceeding with job ' + job + '\n'
    runPolConvert(label, band3=band3, band6Lo=band6Lo, band6Hi=band6Hi,
        DiFXinput=DiFXinput, DiFXoutput=SWIN, DiFXsave=SAVE,
        amp_norm=ampNorm, XYadd=XYadd, XYratio=XYratio,
        timeRange=timeRange, doTest=doTest, savename=expName + '_' + job,
        plotIF=plotIF, doIF=doIF, linAnt=linAnt, plotAnt=plotAnt,
        npix=numFrPltPix, gainmeth=gainmeth)

#
# eof
#
