#!/usr/bin/env python
from __future__ import absolute_import
from __future__ import print_function
import math, sys, glob, os
import astropy.units as u
from astropy.time import Time
from astropy.coordinates import SkyCoord, EarthLocation, AltAz

askaplat = -26.697
askaplon = 116.631
askapheight = 361 # metres
askapradius = 6374217 # metres from geocentre
c = 299792458.0

if not len(sys.argv) == 3:
    print("Usage: %s <rawdata directory> <snoopy version (v1 or v2)>" % sys.argv[0])
    print("Raw data directory should contain snoopy.log or snoopyv2.cand and akXX/beamXX/*.vcraft.hdr files")
    sys.exit()

# Grab the snoopy file
if sys.argv[2] == 'v1':
    snoopyfile = sys.argv[1] + "/snoopy.log"
elif sys.argv[2] == 'v2':
    snoopyfile = sys.argv[1] + "/snoopyv2.cand"
if not os.path.exists(snoopyfile):
    print(snoopyfile, "doesn't exist")
    sys.exit()

# Parse the snoopy file
with open(snoopyfile) as snoopyin:
    snoopylines = snoopyin.readlines()
nocommentlines = []
for line in snoopylines:
    print(line)
    if len(line) > 1 and not line[0] =="#":
        nocommentlines.append(line)
        print("Snoopy info", nocommentlines)
if len(nocommentlines) != 1:
    print("ERROR: No information found")
    sys.exit()
splitline = nocommentlines[0].split()
mjd = float(splitline[7])


# Grab all the headers and find the lowest frequency and the rough FRB direction
antennas = sorted(glob.glob(sys.argv[1] + '/ak??/'))
if len(antennas) == 0:
    print("No antennas found!")
    sys.exit()
beams = glob.glob(antennas[0] + '/beam??/')
if len(beams) == 0:
    print("No beams found in", antennas[0], "(in /beam??/)")
    sys.exit()
hdrfiles = glob.glob(beams[0] + '/*hdr')
if len(hdrfiles) == 0:
    print("No vcraft header files found!")
    sys.exit()
lowestfreq = 99999999
for hf in hdrfiles:
    with open(hf) as headerin:
        lines = headerin.readlines()
        for line in lines:
            if "FREQ" in line:
                freqs = line.split()[1].split(',')
                for f in freqs:
                    if float(f) < lowestfreq:
                        lowestfreq = float(f)
            if "BEAM_RA" in line:
                beamra = float(line.split()[1])
            if "BEAM_DEC" in line:
                beamdec = float(line.split()[1])
            if "TRIGGER_MJD" in line:
                triggermjd = float(line.split()[1])
            if "SAMP_RATE" in line:
                samprate = float(line.split()[1])
            if "NSAMPS_REQUEST" in line:
                nsamps = int(line.split()[1])

            #if "ANT_EL" in line:
            #    ant_el_rad = float(line.split()[1])*math.pi/180.0

frb = SkyCoord(beamra, beamdec, unit='deg')
askap = EarthLocation(lon=askaplon*u.deg, lat=askaplat*u.deg, height=askapheight*u.m)
t = Time(triggermjd, format='mjd')
altaz = frb.transform_to(AltAz(obstime=t,location=askap))
geocentricdelay = math.sin(altaz.alt.radian) * askapradius / c

# Would be much simpler to just get the elevation from the vcraft file!!
# But this is for the centre of the PAF, so could be a bit off vs the elevation of the actual beam
# (which is itself already a bit off vs the actual FRB, but within half a degree which is good enough)
#geocentricdelay = math.sin(ant_el_rad) * askapradius / c

# Now figure out what the correlation start time will be:
corrstartmjd = math.floor(triggermjd*86400 - nsamps/samprate)/86400.0

print("Geocentric delay is", geocentricdelay)
print("Lowest frequency is", lowestfreq)

print("snoopylog2frbgate.py -f %.3f --timediff %.3f --corrstartmjd %.9f %s" % (lowestfreq, geocentricdelay*1e3, corrstartmjd, snoopyfile))
