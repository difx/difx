#!/usr/bin/python
import os,sys,argparse

parser = argparse.ArgumentParser(description='Turn a snoopy log into a binconfig and polyco for DiFX.')
parser.add_argument('snoopylog', metavar='S', help='The snoopy log file')
parser.add_argument("-f", "--freq", type=float, default=1152.0, help="The reference freq at which snoopy DM was calculated")
args = parser.parse_args()
inttime = 10 # seconds

with open(args.snoopylog) as snoopyin:
    snoopylines = snoopyin.readlines()
    splitline = snoopylines[-1].split()
    pulsewidth = float(splitline[3])
    dm = float(splitline[5])
    mjd = float(splitline[7])
    
nearestmjdstartsecond = int((mjd - int(mjd))*86400)
fractionalsecond = (mjd - int(mjd))*86400 - nearestmjdstartsecond
startphase = (fractionalsecond - pulsewidth/2000.0)/inttime # pulse width is in samples, which are 1ms??
endphase = (fractionalsecond + pulsewidth/2000.0)/inttime # pulse width is in samples, which are 1ms??

print nearestmjdstartsecond, fractionalsecond
hh = nearestmjdstartsecond/3600
mm = (nearestmjdstartsecond - hh*3600)/60
ss = nearestmjdstartsecond - (hh*3600 + mm*60)
polycostartmjd = int(mjd) + float(nearestmjdstartsecond)/86400.0

# Will use this if/when we figure out the 50-70ms offset...
#with open("craftfrb.binconfig", "w") as binconfout:
#    binconfout.write("NUM POLYCO FILES:   1\n")
#    binconfout.write("POLYCO FILE 0:      craftfrb.polyco\n")
#    binconfout.write("NUM PULSAR BINS:    2\n")
#    binconfout.write("SCRUNCH OUTPUT:     TRUE\n")
#    binconfout.write("BIN PHASE END 0:    %.9f\n" % startphase)
#    binconfout.write("BIN WEIGHT 0:       0.0\n")
#    binconfout.write("BIN PHASE END 1:    %.9f\n" % endphase)
#    binconfout.write("BIN WEIGHT 1:       1.0\n")
#    binconfout.close()

# Instead cover a 100ms range with bins of size 5ms
with open("craftfrb.binconfig", "w") as binconfout:
    binconfout.write("NUM POLYCO FILES:   1\n")
    binconfout.write("POLYCO FILE 0:      craftfrb.polyco\n")
    binconfout.write("NUM PULSAR BINS:    20\n")
    binconfout.write("SCRUNCH OUTPUT:     TRUE\n")
    for i in range(20):
        binconfout.write("BIN PHASE END %02d:   %.9f\n" % (i, startphase))
        binconfout.write("BIN WEIGHT %02d:       1.0\n" % i)
    binconfout.close()



with open("craftfrb.polyco", "w") as polycoout:
    polycoout.write("fake+fake DD-MMM-12 %02d%02d%.2f %.15f %.4f 0.0 0.0\n" % (hh,mm,ss,polycostartmjd, dm))
    polycoout.write("0.0 %.3f 0 100 3 %.3f\n" % (1.0/float(inttime), args.freq))
    polycoout.write("0.00000000000000000E-99 0.00000000000000000E-99 0.00000000000000000E-99\n")
    polycoout.close()
