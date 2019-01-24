#!/usr/bin/python
import os 

imagesize = 128
numchannels = 2015
averagechannels = 160
pols = ["YY"]

for pol in pols:
    casaout = open("imagescript.py","w")
    imagebase = "TARGET.cube.%s" % (pol)
    imagename = imagebase + ".image"

    casaout = open("imagescript.py","w")
    for i in range(numchannels/averagechannels):
        casaimage = "%s.slice%03d.image" % (imagebase, i)
        fitsimage = "%s.slice%03d.fits" % (imagebase, i)
        os.system("rm -f %s" % fitsimage)
        os.system("rm -rf %s" % casaimage)
        casaout.write('imcollapse(imagename="%s.image",function="mean",axes="frequency",outfile="%s",box="",region="",chans="%d")\n' % (imagebase, casaimage, i))
        casaout.write('exportfits(imagename="%s",fitsimage="%s")\n' % (casaimage, fitsimage))
    casaout.close()
    os.system("casa -c imagescript.py")
    for i in range(numchannels/averagechannels):
        locstring = "%d,%d,%d,%d" % (imagesize/2-12, imagesize/2-12, imagesize/2+12, imagesize/2+12)
        os.system("jmfitfromfile.py %s.slice%03d.fits %s.slice%03d.jmfit.stats %s" % (imagebase, i, imagebase, i, locstring))
