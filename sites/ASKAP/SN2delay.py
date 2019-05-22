#!/usr/bin/env ParselTongue

################################################################################
# AIPS imports
################################################################################
from AIPS import AIPS
from AIPSData import AIPSUVData

################################################################################
# General python imports
################################################################################
import argparse, sys, os, math

################################################################################
# Set up AIPS stuff
################################################################################
try:
    aipsver = os.environ['PSRPIAIPSVER']
except KeyError:
    aipsver = '31DEC18'
AIPS.userno = 702
snversion = 1

parser = argparse.ArgumentParser()
parser.add_argument('-u', '--user', help="AIPS user number", type=int)
parser.add_argument('-s', '--sn', help="SN table version", type=int)
parser.add_argument("-a", "-av", "--av", default=False, action="store_true", help="Average IFs")
parser.add_argument("-r", "--rate", default=False, action="store_true", help="Do rate, not delay")
parser.add_argument("-c", "--clip", help="Ignore values with magnitude greater than this in average", type=float)
parser.add_argument('aipsfile', help="AIPS  file ")
args = parser.parse_args()

if args.user is not None: AIPS.userno = args.user
if args.sn is not None: snversion = args.sn
avgIf = args.av
rate = args.rate
clip = args.clip

if clip is not None: clip /= 1e9

aipsDisk = 1

aipsfile = args.aipsfile

(name, aipsclass, seq) = aipsfile.split('.')

uvdata = AIPSUVData(name, aipsclass, aipsDisk, int(seq))

if not uvdata.exists():
    print aipsfile, "does not exists! Aborting"
    sys.exit()

# Make a list of antennas
maxanid = 0
antable = uvdata.table('AN', 1)
for row in antable:
    if row.nosta > maxanid:
        maxanid = row.nosta
annames = []
for i in range(maxanid):
    annames.append("")
for row in antable:
    annames[row.nosta-1] = row.anname.strip()

# Read the AIPS delays and phases
delays1 = {}
delays2 = {}
delaysN = {}

sntable = uvdata.table('SN', snversion)
num_if = sntable.keywords['NO_IF']
num_pol = sntable.keywords['NO_POL']
num_snterms = num_if*num_pol

for row in sntable:
    ant = annames[row.antenna_no-1]
    if not ant in delays1:
        delays1[ant] = [0.0]*num_if
        if num_pol>1: delays2[ant] = [0.0]*num_if
        delaysN[ant] = 0

    if num_if==1:
        if rate:
            rowDelay1 = [row.rate_1]
        else:
            rowDelay1 = [row.delay_1]
        if num_pol>1:
            if rate:
                rowDelay2 = [row.rate_2]
            else:
                rowDelay2 = [row.delay_2]
    else:
        if rate:
            rowDelay1 = row.rate_1
        else:
            rowDelay1 = row.delay_1
        if num_pol>1:
            if rate:
                rowDelay2 = row.rate_2
            else:
                rowDelay2 = row.delay_2
    for i in range(num_if):
        if abs(rowDelay1[i])>1 or (num_pol > 1 and abs(rowDelay2[i])>1): continue
        delays1[ant][i] += rowDelay1[i]
        if num_pol > 1: delays2[ant][i] += rowDelay2[i]
    delaysN[ant] += 1


def strFlt(x, precision=None):
    if precision is None:
        return str(x)
    else:
        return "{:.2f}".format(x)


p = 2

if avgIf:
    for ant in delays1:
        N = 0
        delay = 0
        #delaysN[ant] *= num_if
        for i in range(num_if):
            if clip is None or abs(delays1[ant][i])<clip:
                delay += delays1[ant][i]
                N += 1
        if num_pol>1: 
            for i in range(num_if):
                if clip is None or abs(delays2[ant][i])<clip:
                    delay += delays2[ant][i]
                    N += 1
        if N == 0:
            delays1[ant][0] = 0
        else:
            delays1[ant][0] = delay/N*1e9
            
    for ant in sorted(delays1):
        print ant, delays1[ant][0]

else:

    for ant in sorted(delays1):
        if delaysN[ant]==0:
            print ant, 0
        else:
            if num_pol>1: 
                print ant, ','.join([strFlt(delays1[ant][i]/delaysN[ant]*1e9,p)+','+strFlt(delays2[ant][i]/delaysN[ant]*1e9,p) for i in range(num_if)])
            else:
                print ant, ','.join([str(delays1[ant][i]/delaysN[ant]*1e9) for i in range(num_if)])
