#!/usr/bin/python2
import os,sys,argparse

parser = argparse.ArgumentParser(description='Turn a snoopy log into a binconfig and polyco for DiFX.')
parser.add_argument('snoopylog', metavar='S', help='The snoopy log file')
parser.add_argument("-f", "--freq", type=float, default=-1, help="The reference freq at which snoopy DM was calculated")
parser.add_argument("--timediff", type=float, default=-99999, help="The time difference between the VCRAFT and snoopy log arrival times for the pulse, including geometric delay, in ms")
parser.add_argument("--corrstartmjd", type=float, default=-1, help="When the correlation will start")
args = parser.parse_args()
fakepulsarperiod = 10 # seconds

if args.freq < 0:
    print "You have to supply a frequency for the snoopy files! Don't be lazy, that's how accidents happen."
    sys.exit()

if args.timediff < -10000:
    print "You have to specify a timediff! It should just be the geometric delay from ASKAP to the geocentre, "
    print "now everything has been fixed.  Don't be lazy, that's how accidents happen."
    sys.exit()
timediffsec = args.timediff/1000.0

if args.corrstartmjd < 0:
    print "You have to specify a corrstartmjd. getGeometricDelay.py will give it to you"
    sys.exit()

with open(args.snoopylog) as snoopyin:
    snoopylines = snoopyin.readlines()
nocommentlines = []
for line in snoopylines:
    print line
    if len(line) > 1 and not line[0] =="#":
        nocommentlines.append(line)
        print "Snoopy info", nocommentlines
if len(nocommentlines) != 1:
    print "ERROR: No information found"
    sys.exit()
splitline = nocommentlines[0].split()
pulsewidthms = float(splitline[3])*1.7 # filterbank width = 1.7ms
dm = float(splitline[5])
mjd = float(splitline[7])

# Figure out the time at the midpoint of the pulse
midmjd = mjd - (dm * 0.00415 / ((args.freq/1e3)**2) - dm * 0.00415 / ((args.freq + 168)/1e3)**2)/86400.0
print dm
print "MJD at low frequency is ", mjd, "at middle of band is ", midmjd

# Figure out the best int time
subintsec = 0.13824
bestinttime = 2*(midmjd - args.corrstartmjd)*86400
nsubints = int(round(bestinttime / subintsec))
bestinttime = nsubints * subintsec
print "Best int time is", bestinttime
    
# Figure out when to start the polyco (go back by somewhere in the range 1-2 seconds, to an integer second boundary)
# Make the polyco reference time the same as the start of the file
polycorefmjdint = int(args.corrstartmjd)
polycorefseconds = int((args.corrstartmjd - int(args.corrstartmjd))*86400)
#if polycorefseconds < 0:
#    polycorefseconds += 86400
#    polycorefmjdint -= 1
hh = polycorefseconds/3600
mm = (polycorefseconds - hh*3600)/60
ss = polycorefseconds - (hh*3600 + mm*60)
polycorefmjdfloat = polycorefmjdint  + float(polycorefseconds)/86400.0

# Write out the polyco file
with open("craftfrb.polyco", "w") as polycoout:
    polycoout.write("fake+fake DD-MMM-YY %02d%02d%05.2f %.15f %.4f 0.0 0.0\n" % (hh,mm,ss,polycorefmjdfloat, dm))
    polycoout.write("0.0 %.3f 0 100 3 %.3f\n" % (1.0/float(fakepulsarperiod), args.freq))
    polycoout.write("0.00000000000000000E-99 0.00000000000000000E-99 0.00000000000000000E-99\n")
    polycoout.close()

# Now write out a binconfig for the gate
gatestartmjd = mjd - pulsewidthms/(2*86400000.0) # pulse width is in ms at this point
gateendmjd = gatestartmjd + pulsewidthms/86400000.0 # pulse width is in ms at this point
gatestartphase = (86400.0*(gatestartmjd - polycorefmjdfloat) + timediffsec)/fakepulsarperiod
gateendphase = (86400.0*(gateendmjd -  polycorefmjdfloat) + timediffsec)/fakepulsarperiod

with open("craftfrb.gate.binconfig", "w") as binconfout:
    binconfout.write("NUM POLYCO FILES:   1\n")
    binconfout.write("POLYCO FILE 0:      %s/craftfrb.polyco\n" % os.getcwd())
    binconfout.write("NUM PULSAR BINS:    2\n")
    binconfout.write("SCRUNCH OUTPUT:     TRUE\n")
    binconfout.write("BIN PHASE END 0:    %.9f\n" % gatestartphase)
    binconfout.write("BIN WEIGHT 0:       0.0\n")
    binconfout.write("BIN PHASE END 1:    %.9f\n" % gateendphase)
    binconfout.write("BIN WEIGHT 1:       1.0\n")
    binconfout.close()

# Make an RFI binconfig
rfistartphase1 = gatestartphase - 0.02/fakepulsarperiod # RFI gate (early side) starts 20ms before the start of the pulse
rfiendphase1 = gatestartphase - 0.004/fakepulsarperiod # RFI gate (early side) ends 4ms before the start of the pulse
rfistartphase2 = gateendphase + 0.004/fakepulsarperiod # RFI gate (late side) starts 4ms after the end of the pulse
rfiendphase2 = gateendphase + 0.02/fakepulsarperiod # RFI gate (early side) ends 20ms after the end of the pulse

with open("craftfrb.rfi.binconfig", "w") as binconfout:
    binconfout.write("NUM POLYCO FILES:   1\n")
    binconfout.write("POLYCO FILE 0:      %s/craftfrb.polyco\n" % os.getcwd())
    binconfout.write("NUM PULSAR BINS:    4\n")
    binconfout.write("SCRUNCH OUTPUT:     TRUE\n")
    binconfout.write("BIN PHASE END 0:    %.9f\n" % rfistartphase1)
    binconfout.write("BIN WEIGHT 0:       0.0\n")
    binconfout.write("BIN PHASE END 1:    %.9f\n" % rfiendphase1)
    binconfout.write("BIN WEIGHT 1:       1.0\n")
    binconfout.write("BIN PHASE END 2:    %.9f\n" % rfistartphase2)
    binconfout.write("BIN WEIGHT 2:       0.0\n")
    binconfout.write("BIN PHASE END 3:    %.9f\n" % rfiendphase2)
    binconfout.write("BIN WEIGHT 3:       1.0\n")
    binconfout.close()

# And make a high time resolution binconfig (216 microsec x bin width + 2ms either side)
binstartmjd = mjd - fakepulsarperiod*pulsewidthms/(2*86400000.0) # pulse width is in ms at this point
gateendmjd = gatestartmjd + pulsewidthms/86400000.0 # pulse width is in ms at this point
binmicrosec = 216
extrawidth = 2 # ms on either side of the snoopy detected pulse
binstartphase = gatestartphase - float(extrawidth)/(1000.0*fakepulsarperiod)
bindeltaphase = binmicrosec/(fakepulsarperiod*1e6)
numbins = int((pulsewidthms + 2*extrawidth)/(binmicrosec/1000.0))
with open("craftfrb.bin.binconfig", "w") as binconfout:
    binconfout.write("NUM POLYCO FILES:   1\n")
    binconfout.write("POLYCO FILE 0:      %s/craftfrb.polyco\n" % os.getcwd())
    binconfout.write("NUM PULSAR BINS:    %d\n" % (numbins+1))
    binconfout.write("SCRUNCH OUTPUT:     FALSE\n")
    for i in range(numbins+1):
        phasestr = ("BIN PHASE END %d:" % i).ljust(20)
        weightstr = ("BIN WEIGHT %d:" % i).ljust(20)
        binconfout.write("%s%.9f\n" % (phasestr, binstartphase + i*bindeltaphase))
        binconfout.write("%s1.0\n" % (weightstr))
    binconfout.close()
binscale = bindeltaphase / (rfiendphase2 + rfiendphase1 - rfistartphase2 - rfistartphase1)

# And also make a "finders" binconfig, with just 5 bins spanning from the end of RFI window 1
# through to the start of RFI window 2 (which will then normally be 2-3 ms wide each
numfinderbins = 5
bindeltaphase = (rfistartphase2 - rfiendphase1)/numfinderbins
with open("craftfrb.finder.binconfig", "w") as binconfout:
    binconfout.write("NUM POLYCO FILES:   1\n")
    binconfout.write("POLYCO FILE 0:      %s/craftfrb.polyco\n" % os.getcwd())
    binconfout.write("NUM PULSAR BINS:    %d\n" % (numfinderbins+1))
    binconfout.write("SCRUNCH OUTPUT:     FALSE\n")
    for i in range(numfinderbins+1):
        phasestr = ("BIN PHASE END %d:" % i).ljust(20)
        weightstr = ("BIN WEIGHT %d:" % i).ljust(20)
        binconfout.write("%s%.9f\n" % (phasestr, binstartphase + i*bindeltaphase))
        binconfout.write("%s1.0\n" % (weightstr))
    binconfout.close()
finderbinscale = bindeltaphase / (rfiendphase2 + rfiendphase1 - rfistartphase2 - rfistartphase1)

# And write out a little script ready to do the various subtractions
gatescale = (gateendphase - gatestartphase) / (rfiendphase2 + rfiendphase1 - rfistartphase2 - rfistartphase1)
with open("dosubtractions.sh", "w") as subout:
    subout.write("uvsubScaled.py FRB_GATE.FITS FRB_RFI.FITS %.9f\n" % (gatescale))
    for i in range(numbins):
        subout.write("uvsubScaled.py FRB_BIN%02d.FITS FRB_RFI.FITS %.9f\n" % (i, binscale))
    for i in range(numfinderbins):
        subout.write("uvsubScaled.py FRB_FINDERBIN%02d.FITS FRB_RFI.FITS %.9f\n" % (i, finderbinscale))
    subout.close()

# Instead cover a 150ms range with bins of size 5ms
#with open("craftfrb.binconfig", "w") as binconfout:
#    binconfout.write("NUM POLYCO FILES:   1\n")
#    binconfout.write("POLYCO FILE 0:      craftfrb.polyco\n")
#    binconfout.write("NUM PULSAR BINS:    30\n")
#    binconfout.write("SCRUNCH OUTPUT:     FALSE\n")
#    for i in range(30):
#        phasestr = ("BIN PHASE END %d:" % i).ljust(20)
#        weightstr = ("BIN WEIGHT %d:" % i).ljust(20)
#        binconfout.write("%s%.9f\n" % (phasestr, startphase + (i-14)*0.0005))
#        binconfout.write("%s1.0\n" % (weightstr))
#    binconfout.close()
