#!/usr/bin/python
# 
# Script that is run in QA2 to generate PolConvert inputs.
# 

import numpy as np
import pylab as pl
import os
import sys
import shutil
import tarfile

#
# these are all arguments, starting with the products of asdmpolconvert.py
#
msname = 'concat.ms'
CALAPP = 'calappphase.tab'
ANTENNA = 'antenna.tab'

# to allow variant processing and keeping products separate
try:
    if type(label) is not str or len(label) == 0:
        label = msname + '.0'
except:
    label = msname + '.0'
print 'Generating PolConvert Products labelled as ' + label + '...'
print 'Executing processing steps ' + ','.join(map(str, mysteps))
#if label not in globals().keys(): label = msname + '.0'
#print label
#print locals().keys()
#print globals().keys()

try:
    if len(mysteps) == 0:
        mysteps = range(8)
except:
    mysteps = range(8)

# field ids from listobs for the targets
# deducible from listobs
bpcal = '0'
flxcal = '1'
pzncal = '2'
phscal = '3'
vlbical = '4,5,6'
science = '7'
# any antenna in the center (plotants)
refant = 'DV18'
# all windows '0,1' for band 6 '0' for band 3
spw = '0,1'
# integration time for polconvert solutions
intt = '4.032s'
# standard for setjy()
jystandard = 'Butler-JPL-Horizons 2012'

# list of atenna, spw, timerange
# at a minimum flag the sum antenna (pad APP01) as bad
flags = [['DV03', '', '']]

# by amplitude
# clip = [0.,0.012]
clip = []

# make a list of all sources excluding the flux calibrator
# sometimes we need a python list, sometimes a comma-sep list
# and in any case no duplicates.
flxlst = bpcal +','+ pzncal +','+ phscal +','+ vlbical +','+ science
flxfer = list(set(map(int, flxlst.split(','))))
flxlst = ','.join(map(str, flxfer))

#
# the code below this point should not need to be edited
#

# get names of all antennas
tb.open(msname+'/ANTENNA')
allants = tb.getcol('NAME')
tb.close()

# feedback for the user
TODO = {0:'Save original flags',
        1:'Flag bad data',
        2:'Calibrate bandpass',
        3:'Calibrate gains',
        4:'Calibrate phase offset on VLBI scans',
        5:'Calibrate phase offset on ALMA-only scans',
        6:'Calibrate (leakage) for D Terms',
        7:'Tar up deliverables'}

#
# STEP #0: save/restore the original flags
#
mystep = 0
if mystep in mysteps:
    print TODO[mystep]
    if os.path.exists(msname+'.flagversions/flags.Original'):
        print 'Original flags already exist! Will not overwrite!'
        flagmanager(vis=msname,mode='restore',versionname='Original')
    else:
        print 'Saving Original flags'
        flagdata(vis=msname,autocorr=True,flagbackup=False)
        flagmanager(vis=msname,mode='save',versionname='Original')
    print '\n'

#
# STEP #1: restore/apply user-defined flags
#
mystep = 1
if mystep in mysteps:
    print TODO[mystep]
    flagmanager(vis=msname,mode='restore',versionname='Original')
    flagmanager(vis=msname, mode = 'delete', versionname='BeforeCalibration')
    for flag in flags:
        flagdata(vis=msname, mode='manual',antenna=flag[0], spw=flag[1],
            timerange=flag[2], flagbackup=False)
    if len(clip) == 2:
        flagdata(vis=msname, mode='clip', clipminmax=clip, flagbackup=False)
    flagmanager(vis=msname, mode = 'save', versionname='BeforeCalibration')
    print '\n'

#
# STEP #2: bandpass calibration
#
mystep = 2
if mystep in mysteps:
    print TODO[mystep]
    # do the bandpass calibration with on-the-fly gaincal
    flagmanager(vis=msname, mode = 'restore', versionname='BeforeCalibration')
    os.system('rm -rf %s.prebandpass.cal'%label)
    gaincal(vis=msname, field=bpcal, spw=spw, solint='int', calmode='p',
        refant=refant, gaintype='G', caltable = '%s.prebandpass.cal'%label)
    os.system('rm -rf %s.bandpass.cal'%label)
    os.system('rm -rf %s.bandpass-zphs.cal'%label)
    bandpass(vis=msname, spw=spw, caltable= '%s.bandpass.cal'%label,
        field=bpcal, solint='inf', combine='scan', refant=refant,
        gaintable='%s.prebandpass.cal'%label,
        solnorm=True)
    shutil.copytree('%s.bandpass.cal'%label, '%s.bandpass-zphs.cal'%label)
    # make a table with zero-phase bandpass
    tb.open('%s.bandpass-zphs.cal'%label, nomodify=False)
    gains = tb.getcol('CPARAM')
    gains[:] = np.abs(gains)
    tb.putcol('CPARAM', gains)
    tb.close()
    # make lots of plots for checking
    os.system('rm -rf %s.bandpass.plots'%label)
    os.system('mkdir %s.bandpass.plots'%label)
    for antnam in allants:
        plotcal(caltable=label+'.prebandpass.cal',xaxis='time',yaxis='phase',
            plotrange=[0,0,-180,180], antenna=antnam, spw=spw,
            iteration='spw',subplot=211,
            figfile='%s.bandpass.plots/%s.PREBP.PHAS.png'%(label,antnam))
        for sp in spw.split(','):
            plotcal(caltable=label+'.bandpass.cal',xaxis='chan',yaxis='amp',
                antenna=antnam, spw=sp, iteration='spw',subplot=111,
                figfile='%s.bandpass.plots/%s.spw%s.BPAMP.png'%(
                    label,antnam,sp))
            plotcal(caltable=label+'.bandpass.cal',xaxis='chan',yaxis='phase',
                antenna=antnam, spw=sp, plotrange=[0,0,-180,180],
                iteration='spw',subplot=111,
                figfile='%s.bandpass.plots/%s.spw%s.BPPHAS.png'%(
                    label,antnam,sp))
    print '\n'

#
# STEP #3: absolute flux calibration applying the bandpass correction above
# Note: we are assuming point sources here.  Changes are required otherwise.
#
mystep = 3
if mystep in mysteps:
    print TODO[mystep]
    flagmanager(vis=msname, mode = 'restore', versionname='BeforeCalibration')
    clearcal(vis=msname)
    setjy(vis=msname, field=flxcal, standard = jystandard)
    os.system('rm -rf %s.phasegains.cal'%label)
    os.system('rm -rf %s.ampgains.cal'%label)
    # %s.phasegains.cal gets full bandpass on flux calibrator
    # and zero-phase bandbass on all other sources
    gaincal(vis=msname, field=flxcal, spw=spw, solint=intt, calmode='p',
        refant=refant, gaintype='G', caltable = '%s.phasegains.cal'%label,
        gaintable='%s.bandpass.cal'%label, gainfield=bpcal)
    gaincal(vis=msname, field=flxlst, spw=spw, solint=intt, calmode='p',
        refant=refant, gaintype='G', caltable = '%s.phasegains.cal'%label,
        gaintable='%s.bandpass-zphs.cal'%label, gainfield=bpcal, append=True)
    # and similarly for %s.ampgains.cal
    gaincal(vis=msname, field=flxcal, spw=spw, solint='inf', calmode='ap',
        refant=refant, gaintype='G', caltable = '%s.ampgains.cal'%label,
        gaintable=['%s.bandpass.cal'%label, '%s.phasegains.cal'%label],
        gainfield=[bpcal,''])
    gaincal(vis=msname, field=flxlst, spw=spw, solint='inf', calmode='ap',
        refant=refant, gaintype='G', caltable = '%s.ampgains.cal'%label,
        gaintable=['%s.bandpass-zphs.cal'%label, '%s.phasegains.cal'%label],
        gainfield=[bpcal,''], append=True)
    fluxscale(vis=msname, caltable='%s.ampgains.cal'%label, transfer=flxfer,
        fluxtable='%s.ampgains.cal.fluxscale'%label, reference=[flxcal])
    os.system('rm -rf %s.gains.plots'%label)
    os.system('mkdir %s.gains.plots'%label)
    for antnam in allants:
        plotcal(caltable=label+'.phasegains.cal',xaxis='time',yaxis='phase',
            antenna=antnam, spw=spw, plotrange=[0,0,-180,180],
            iteration='spw',subplot=211,
            figfile='%s.gains.plots/%s.PHASE.png'%(label,antnam))
        plotcal(caltable=label+'.ampgains.cal',xaxis='time',yaxis='amp',
            antenna=antnam, spw=spw, iteration='spw',subplot=211,
            figfile='%s.gains.plots/%s.AMP.png'%(label,antnam))
        plotcal(caltable=label+'.ampgains.cal.fluxscale',
            xaxis='time',yaxis='amp',
            antenna=antnam, spw=spw, iteration='spw',subplot=211,
            figfile='%s.gains.plots/%s.FLUX.png'%(label,antnam))
    print '\n'

#
# STEP #4: solve for the XY phase offset at the reference antenna
#   (refant here).  The XY phase can have 180 degrees ambiguity,
#   unless we can estimate Q and U from the time variation of the
#   visibility amplitudes!  In this step we restrict processing
#   to the phased scans (tcon).
#
mystep = 4
if mystep in mysteps:
    print TODO[mystep]
    flagmanager(vis=msname, mode = 'restore', versionname='BeforeCalibration')
    gainsvlbi=['%s.bandpass-zphs.cal'%label, '%s.ampgains.cal.fluxscale'%label,
        '%s.phasegains.cal'%label]
    gainfields = [bpcal,'','']
    preavg = 120
    # build a list of observing modes including active phasing
    tb.open('%s/STATE'%msname)
    modes = tb.getcol('OBS_MODE')
    wanted = [i for i in range(len(modes))
        if 'APPPHASE_ACTIVE' in modes[i]]
    tb.close()
    # build a list of active phasing scans for XY phase determination
    tb.open(msname)
    states = tb.getcol('STATE_ID')
    scans = tb.getcol('SCAN_NUMBER')
    vset = set()
    for w in wanted: vset = vset.union(set(scans[np.where(states == w)]))
    vlbiscans = ','.join(map(str,list(vset)))
    tb.close()
    os.system('rm -rf %s.XY0amb-tcon'%label)
    print 'Polarization calibration from VLBI scans'
    gaincal(vis=msname, caltable='%s.XY0amb-tcon'%label,
        field = pzncal,
        gaintype='XYf+QU',
        scan=vlbiscans,
        solint='inf',
        spw=spw,
        combine='scan,obs',
        preavg=preavg,
        refant=refant,
        smodel=[1,0,1,0], # starting model: I,Q,U,V, i.e. pure U
        gaintable=gainsvlbi, gainfield=gainfields)
    print 'Copy the polarization report to the README file as from Step #4.'
    print '\n'
    os.system('rm -rf ' + label + '.XY-CrossPhase-tcon.png')
    plotcal(label+'.XY0amb-tcon','chan','phase', antenna=refant, poln='X',
        subplot=211, iteration='spw',figfile=label+'.XY-CrossPhase-tcon.png')
    print '\n'

#
# STEP #5: same as step 4, but for the pure ALMA scans (tcoff).
#
mystep = 5
if mystep in mysteps:
    print TODO[mystep]
    flagmanager(vis=msname, mode = 'restore', versionname='BeforeCalibration')
    gainsalma=['%s.bandpass.cal'%label, '%s.ampgains.cal.fluxscale'%label,
        '%s.phasegains.cal'%label]
    gainfields = [bpcal,'','']
    preavg = 120
    # build a list of observing modes including active phasing
    tb.open('%s/STATE'%msname)
    modes = tb.getcol('OBS_MODE')
    wanted = [i for i in range(len(modes))
        if 'CALIBRATE_POLARIZATION' in modes[i]]
    tb.close()
    # build a list of active phasing scans for XY phase determination
    tb.open(msname)
    states = tb.getcol('STATE_ID')
    scans = tb.getcol('SCAN_NUMBER')
    vset = set()
    for w in wanted: vset = vset.union(set(scans[np.where(states == w)]))
    almascans = ','.join(map(str,list(vset)))
    tb.close()
    os.system('rm -rf %s.XY0amb-tcoff'%label)
    print 'Polarization calibration from ALMA scans'
    gaincal(vis=msname, caltable='%s.XY0amb-tcoff'%label,
        field = pzncal,
        gaintype='XYf+QU',
        scan=almascans,
        solint='inf',
        spw=spw,
        combine='scan,obs',
        preavg=preavg,
        refant=refant,
        smodel=[1,0,1,0], # starting model: I,Q,U,V, i.e. pure U
        gaintable=gainsalma, gainfield=gainfields)
    print 'Copy the polarization report to the README file as from Step #5.'
    print '\n'
    os.system('rm -rf ' + label + '.XY-CrossPhase-tcoff.png')
    plotcal(label+'.XY0amb-tcoff','chan','phase', antenna=refant, poln='X',
        subplot=211, iteration='spw',figfile=label+'.XY-CrossPhase-tcoff.png')
    print '\n'

#
# STEP #6: calculate the D Terms
#
mystep = 6
if mystep in mysteps:
    print TODO[mystep]
    print 'NOT YET IMPLEMENTED'
    print '\n'

#
# STEP #7: tar up deliverables
#
mystep = 7
if mystep in mysteps:
    print TODO[mystep]
    calapp = label + '.' + CALAPP
    antenna = label + '.' + ANTENNA
    msant = msname + '/ANTENNA'
    # canonically rename these tables with label
    print 'moving %s to %s' % (CALAPP, calapp)
    print 'moving %s to %s' % (msant, antenna)
    os.rename(CALAPP, calapp)
    os.rename(msant, antenna)
    deliverables = [
        '%s.prebandpass.cal'%label,
        '%s.bandpass.cal'%label, '%s.bandpass-zphs.cal'%label,
        '%s.ampgains.cal.fluxscale'%label, '%s.phasegains.cal'%label,
        '%s.XY-CrossPhase-tcon.png'%label, '%s.XY0amb-tcon'%label,
        '%s.XY-CrossPhase-tcoff.png'%label, '%s.XY0amb-tcoff'%label,
        '%s.bandpass.plots'%label, '%s.gains.plots'%label, calapp, antenna]
    if os.path.exists('%s.tgz'%label): os.unlink('%s.tgz'%label)
    tf = tarfile.open('%s.tgz'%label, mode='w:gz')
    for d in deliverables: tf.add(d)
    tf.close()
    # restore original names
    print 'moving %s to %s' % (calapp, CALAPP)
    print 'moving %s to %s' % (antenna, msant)
    os.rename(calapp, CALAPP)
    os.rename(antenna, msant)
    print 'Tarball %s.tgz'%label + ' is ready for delivery'
    print '\n'

#
# eof
#
