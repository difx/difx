#!/usr/bin/python
"""make_empty_binconfig.py [-p|--polyco <infile>] [-n|--numbins <N>]

Produces a DiFX .binconfig file with equal-weighted bins across the
entire pulse period. This is largely equivalent to running profile2binconfig.py
without specifying a profile file."""
from optparse import OptionParser

parser = OptionParser(__doc__)
parser.add_option("-p", "--polyco", dest="polyco", default="polyco_new.dat",
                  help="Filename of the polyco file (a single file)")
parser.add_option("-n", "--numbins", dest="numbins", default="40",
                  help="Number of bins in the output binconfig file")
(opt, junk) = parser.parse_args()
numbins = int(opt.numbins)

Wline = 20  # DiFX line width
bcfile = opt.polyco.split('.')[0] + '.binconfig'
binconfout = open(bcfile, 'w')
binconfout.write('NUM POLYCO FILES:'.ljust(Wline))
binconfout.write('1\n')
binconfout.write('POLYCO FILE 0:'.ljust(Wline))
binconfout.write('%s\n' % (opt.polyco))
binconfout.write('NUM PULSAR BINS:'.ljust(Wline))
binconfout.write('%d\n' % (numbins))
binconfout.write('SCRUNCH OUTPUT:'.ljust(Wline))
binconfout.write('FALSE\n')
for i in range(numbins):
	binconfout.write(("BIN PHASE END %i:"%(i)).ljust(Wline))
	binconfout.write("%f\n" % (float(2*i + 1)/float(2*numbins)))
	binconfout.write(("BIN WEIGHT %i:"%(i)).ljust(Wline))
	binconfout.write("1.0\n")
binconfout.close()
print('Wrote %s with %d bins across pulse period' %(bcfile,numbins))
