#!/usr/bin/python
import os,sys,glob,math

# Convenience function
def posradians2string(rarad, decrad):
    rah = rarad * 12 / math.pi
    rhh = int(rah)
    rmm = int(60*(rah - rhh))
    rss = 3600*rah - (3600*rhh + 60*rmm)
    decd = decrad * 180 / math.pi
    decformat = "+%02d:%02d:%010.7f"
    if decd < 0:
        decd = -decd
        decformat = '-' + decformat[1:]
    ddd = int(decd)
    dmm = int(60*(decd - ddd))
    dss = 3600*decd - (3600*ddd + 60*dmm)
    rastring  = "%02d:%02d:%011.8f" % (rhh,rmm,rss)
    decstring = decformat % (ddd, dmm, dss)
    return rastring, decstring

# Check instantiation
if not len(sys.argv) == 3:
    print "Usage: %s <example vcraft header> <glob pattern for vcraft files>" % sys.argv[0]
    sys.exit()

vcraftheader = sys.argv[1]
vcraftglobpattern = sys.argv[2]

if not os.path.exists(vcraftheader):
    print vcraftheader, "doesn't exist!"
    sys.exit()

freqs = []
beamra = -99
beamdec = -99
startmjd = -99
vcraftheaderlines = open(vcraftheader).readlines()
for line in vcraftheaderlines:
    if line.split()[0] == "FREQS":
        freqs = line.split()[1].split(',')
    if line.split()[0] == "BEAM_RA":
        beamra = float(line.split()[1])
    if line.split()[0] == "BEAM_DEC":
        beamdec = float(line.split()[1])
    if line.split()[0] == "START_WRITE_MJD":
        startmjd = float(line.split()[1]) + 37./86400 # Right for the current amount of leap seconds!

if beamra < -90 or beamdec < -90 or startmjd < -90:
    print "Didn't find all info in", vcraftheader
    sys.exit()

vcraftfiles = glob.glob(vcraftglobpattern)
if len(vcraftfiles) == 0:
    print "Didn't find any vcraft files!"
    sys.exit()

# Write the obs.txt file
rastring, decstring = posradians2string(beamra*math.pi/180, beamdec*math.pi/180)
output = open("obs.txt", "w")
output.write("startmjd    = %.9f\n" % startmjd)
output.write("stopmjd     = %.9f\n" % (startmjd + 10./86400))
output.write("srcname     = CRAFTSRC\n")
output.write("srcra       = %s\n" % rastring)
output.write("srcdec      = %s\n" % decstring)
output.close()

# Write the chandefs file
output = open("chandefs.txt", "w")
for f in freqs:
    output.write("%s L 1.185185185185185185\n" % f)
output.close()

# Run the converter for each vcraft file
for f in vcraftfiles:
    antname = ""
    for line in open(f).readlines():
        if line.split()[0] == "ANT":
             antname = line.split()[1].lower()
    if antname == "":
        print "Didn't find the antenna name in the header!"
        sys.exit()
    print "CRAFTConverter %s %s.codif" % (f, antname)
    os.system("CRAFTConverter %s %s.codif" % (f, antname))
