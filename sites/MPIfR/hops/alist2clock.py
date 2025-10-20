#!/usr/bin/env python3

from eat.io import hops
import pandas as pd
import numpy as np

import argparse
import datetime
import re
import sys

parser = argparse.ArgumentParser(description='Determine delays and rates by least-square fit to alist.')

parser.add_argument('-r', '--ref', required=True,help='The one-letter code of the reference station')
parser.add_argument('-s', '--scode', help='Filename of table mapping 1-letter to 2-letter station codes with lines of the form: X Xx')
parser.add_argument('-v', '--vexfile', help='Name of vex file. Will produce a valid vex CLOCK-section')
parser.add_argument('-do', '--ref-delay-offset', type=float, default=0.0, help='The delay offset of the reference station')
parser.add_argument('-ro', '--ref-rate-offset', type=float, default=0.0, help='The rate offset of the reference station')
parser.add_argument('-nr', '--no-rate', action='store_true', help='Do not revise the drift rate')
parser.add_argument('-n', '--navg', type=int, default=2, help='Minimum nr of scans over which to baseline-average sbdly, rate, snr')
parser.add_argument('-S', '--minsnr', type=float, default=8.0, help='Minimum fringe SNR to consider as a detection')

parser.add_argument('alist', help='The alist file')

def replaceClockSolutions(clockentry, delay, rate, skipRate=False):
    '''
    Parses VEX clock_early entry and adds the provided
    residual delay (in usec) and rate (sec/sec/) to it
    '''

    # Example clockentry lines:
    # clock_early = 2021y276d14h00m00s  : -21.172 usec : 2021y276d14h00m00s  :  1.02e-13 ;
    # clock_early = 2021y276d14h00m00s:-21.172 usec:2021y276d14h00m00s:1.02e-13;
    # clock_early = 2021y276d14h00m00s  : -21.172e-6   sec:2021y276d14h00m00s:1.02e-13;

    #redelay= re.compile("(.*\d+\.\d+)(.*)")  # match non sci notation float, i.e. will not match -21.172e-6
    redelay= re.compile("(.*\d+\.*\d*[eE]*[+-]*\d*)\s+(.*)")  # match also optional exponential notation

    result = ""
    fields = clockentry.split(":")

    delaymatch = redelay.match(fields[1])
    rateField = fields[3].strip().split(";")
    if skipRate:
        newrate = float(rateField[0].strip())
    else:
        newrate = float(rateField[0].strip()) + rate

    result = fields[0].strip() + " : "
    if delaymatch:
        originaldelay = float(delaymatch.group(1))
        originaldelayunits = delaymatch.group(2).strip()
        if originaldelayunits == 'sec':
            originaldelay = originaldelay * 1e6
        newdelay = originaldelay + delay
        newdelayunits = 'usec'
        if abs(newrate) < 1e-11:
            result += "{:.4f} {:s} : {} : {:.4e} ; ".format(newdelay, newdelayunits, fields[2].strip(), newrate, comment)
        else:
            result += "{:.4f} {:s} : {} : {:.8e} ; ".format(newdelay, newdelayunits, fields[2].strip(), newrate, comment)
    else:
        sys.exit("Cannot parse clock entry: {}".format(clockentry))

    return(result)


def read_stationMap(filename):

    pandasargs = dict(delim_whitespace=True, comment='#', names=["oneletter", "twoletter"], index_col=False)
    table = pd.read_csv(filename, **pandasargs)
    #print(table)

    return table


def parse_vextime_str(tvex):
    '''Parse a VEX timestamp such as 2015y016d07h30m00s'''

    return datetime.datetime.strptime(tvex.strip(), '%Yy%jd%Hh%Mm%Ss')


def datetime_to_vextime(dtime):
   '''Return a Datetime time in VEX format such as 2015y016d07h30m00s'''

   return dtime.strftime('%Yy%jd%Hh%Mm%Ss')


def parse_atime_str(atime, ayear):
    '''Parse an alist doy-hhmmss timestamp such as 099-035500 paired with a year'''

    return datetime.datetime.strptime((str(ayear) + " " + str(atime)) .strip(), '%Y %j-%H%M%S')


def parse_delay_str(delaystr):
    '''Parse a VEX $CLOCK delay entry (value with optional unit)'''

    redelay = re.compile("\s*(.*\d+\.*\d*[eE]*[+-]*\d*)\s*(.*)")  # delay value plus unit, rarely in exponential notation
    delaymatch = redelay.match(delaystr)
    if not delaymatch:
        return 0

    delay = float(delaymatch.group(1))
    delayunits = delaymatch.group(2).strip()

    if delayunits == 'usec':
        delay = delay * 1e-6
    elif delayunits == 'nsec':
        delay = delay * 1e-9

    return delay


def parse_rate_str(ratestr):
    '''Parse a VEX $CLOCK rate entry (value with optional unit)'''

    rerate = re.compile("\s*(.*\d+\.*\d*[eE]*[+-]*\d*)\s*(.*)")  # rate value possibly with unit, usually in exponential notation
    ratematch = rerate.match(ratestr)
    if not ratematch:
        return 0

    rate = float(ratematch.group(1))
    rateunits = ratematch.group(2).strip()

    if rateunits == 'usec/sec':
        rate = rate * 1e-6
    elif rateunits == 'nsec/sec':
        rate = rate * 1e-9
    elif rateunits == 'psec/sec':
        rate = rate * 1e-12

    return rate


def read_vex_clocks(vexfile):

    clocks = {}
    clock_order = []

    redef = re.compile("def\s+(..)\s*;")
    reenddef = re.compile("enddef\s*;")
    #reclock = re.compile(".*clock_early\s*=\s*(\d{4}y\d{3}d\d{2}h\d{2}m\d{2}s)\s*:\s*(.*\.\d+(e-?\d+)?)\s*;") # regex match groups: grp[1]:refEp grp[2]:'dly usec:refep:rate' with rate in sci notation 5e-14
    #reclock = re.compile("clock_early\s*=\s*(\d{4}y)")
    #reclock = re.compile("clock_early\s*=\s*({2}s)")
    reclock = re.compile(".*clock_early\s*=\s*([^;]*);")

    start = False
    with open(vexfile) as file:
        for line in file:
            line = line.strip()

            if (line.startswith("$CLOCK;")):
                start = True
            if start == False:
                continue
            if line.startswith("*"):
                continue

            startdef = redef.match(line)
            enddef = reenddef.match(line)
            clock = reclock.match(line)

            if startdef:
                station = startdef.group(1)
                # print ('startdef ', startdef.group(1))
            if enddef:
                # print ('enddef ', station)
                pass
            if clock:
                fields = clock.group(1).split(":")
                delayep = parse_vextime_str(fields[0])
                delay = parse_delay_str(fields[1])
                rateep = parse_vextime_str(fields[2])
                rate = parse_rate_str(fields[3])
                entry = { 'vexstring':line, 'delay_s':delay, 'epoch':delayep, 'rate_s': rate, 'rateepoch':rateep }
                clocks[station] = entry
                if station not in clock_order:
                    clock_order.append(station)
                #print ('%s  ref time %s  data string %s' % (station,clock.group(1),clock.group(2)))

    file.close()
    #print('read_vex_clocks() : ', clock_order, clocks)

    return (clocks,clock_order)


args = parser.parse_args()
if (args.navg < 1):
    args.navg = 2

alist = hops.read_alist(args.alist)
# print(alist.columns)

if (args.scode):
    stMap = read_stationMap(args.scode)
    #print (stMap)

clocks, clock_ordering = None, None
if (args.vexfile):
    vexclocks, vexclock_ordering = read_vex_clocks(args.vexfile)
    #print (vexclocks)

# Filter the data: select interesting data columns, discard autocorrelations, discard low-SNR entries
fringedata = alist.filter(['scan_id','baseline','snr','sbdelay','quality','polarization', 'delay_rate', 'timetag','year'])
fringedata = fringedata[fringedata.baseline.apply(lambda x: x[0] != x[1])]
fringedata = fringedata[fringedata['snr'] >= args.minsnr]
grouped = fringedata.groupby(['baseline'])

# Get a list of 1-letter station IDs of all baselines
stations = sorted(list(set(''.join(grouped.groups.keys()))))

### For each scan/baselinegroup: determine non-weighted mean SNR, delay, rate - based on 2 highest SNR results only

# loop over all scan/baseline groups
# determine stations and calculate average SNR, delay and rate for each scan/baseline group
valid = pd.DataFrame()
for groupname, groupdata in grouped:
    caldata = groupdata.nlargest(columns='snr', n=args.navg)
    calmean = caldata.mean()
    validRow = {
        'scan_id': caldata['scan_id'].values[0],
        'baseline': groupname,
        'avg_sbdelay': calmean['sbdelay'],
        'avg_rate': calmean['delay_rate'],
        'avg_snr': calmean['snr'],
        'navg': len(caldata),
        'scan_time': parse_atime_str(min(caldata['timetag'].values), min(caldata['year'].values)) # fixme?
    }
    valid = valid.append(validRow, ignore_index=True)

### Summarize

print ("Averaged fringe detections:")
print ("{:8s} {:2s} {:>8s} {:>10s} {:>10s} {:4s}".format("scan", "bl", "snr", "delay[us]", "rate[ps/s]", "navg"))
for index, validrow in valid.iterrows():
    print ("{:8s} {:2s} {:8.2f} {:+10.6f} {:+10.6f} {:2.0f}".format(validrow['scan_id'], validrow['baseline'], validrow["avg_snr"], validrow["avg_sbdelay"], validrow["avg_rate"], validrow["navg"] ))

### New approach with linking of sub-arrays

Nstations = len(stations)
stationIndices = { stn: stations.index(stn) for stn in stations }

Dnn = np.zeros((Nstations, Nstations))
Rnn = np.zeros((Nstations, Nstations))
Tnn = np.array([0]*Nstations, dtype='datetime64[s]')
linked = np.array(np.eye(Nstations), dtype=bool)

for index, validrow in valid.iterrows():
    i1 = stationIndices[validrow['baseline'][0]]
    i2 = stationIndices[validrow['baseline'][1]]
    Dnn[i1,i2] = +validrow['avg_sbdelay']
    Dnn[i2,i1] = -validrow['avg_sbdelay']
    Rnn[i1,i2] = +validrow['avg_rate']
    Rnn[i2,i1] = -validrow['avg_rate']
    Tnn[i1] = validrow['scan_time']
    Tnn[i2] = validrow['scan_time']
    linked[i1,i2] = True
    linked[i2,i1] = True

# print(Dnn)
# print(linked)
print('')

ref = stations.index(args.ref)
linking_attempts = 0
Naugmented = 0
while np.count_nonzero(linked) < Nstations*Nstations and linking_attempts < Nstations:
    linking_attempts += 1
    for rem in range(Nstations):
        if ref == rem:
           continue
        if linked[ref,rem]:
           continue
        print('No good fringes linking ref %d to station %d (%c to %c)' % (ref,rem,stations[ref],stations[rem]))
        for rem2 in range(Nstations):
            if (ref == rem2) or (rem == rem2):
                continue
            if linked[rem,rem2] and linked[ref,rem2]:
                dly = Dnn[ref,rem2] + Dnn[rem2,rem]
                rate = Rnn[ref,rem2] + Rnn[rem2,rem]
                Dnn[ref,rem] = dly
                Dnn[rem,ref] = -dly
                Rnn[ref,rem] = rate
                Rnn[rem,ref] = -rate
                linked[ref,rem] = True
                linked[rem,ref] = True
                Naugmented += 1
                #print('   filled using %d-%d (%c-%c) and %d-%d (%c-%c) : derived dly=%.6f rate=%.6f' % (ref,rem2,stations[ref],stations[rem2], rem,rem2,stations[rem],stations[rem2], -dly, -rate))
                print('   filled using %d-%d (%c-%c) dly=%.6f and %d-%d (%c-%c) dly=%.6f : derived dly=%.6f rate=%.6f'
                    % ( ref,rem2,stations[ref],stations[rem2], Dnn[ref,rem2],
                        rem,rem2,stations[rem],stations[rem2], Dnn[rem2,rem],  dly, rate) )
                break

#if Naugmented > 0:
#     print(Dnn)
#     print(linked)

print('')
print("Residuals relative to reference station '%s':" % (args.ref))
for rem in range(Nstations):
    stname1 = stations[rem]
    stname2 = "--"
    if args.scode:
        idx = stMap.index[stMap['oneletter'] == stname1]
        if idx.empty:
            sys.exit("No mapping for one-letter code {} found in {}". format(stname1, args.scode))
        else:
            stname2 = stMap["twoletter"].iloc[idx[0]]
    if stname2 != '--':
        print("%s %s   %+2.6f usec   %+.6f ps/s   VEX priors %+9.4f usec %+.6f ps/s" % (stname1,stname2,Dnn[ref,rem],Rnn[ref,rem],vexclocks[stname2]['delay_s']*1e6,vexclocks[stname2]['rate_s']*1e12))
    else:
        print("%s %s   %+.6f usec   %+.6f ps/s" % (stname1,stname2,Dnn[ref,rem],Rnn[ref,rem]))

if args.scode:

    print('')
    print('$CLOCK;')

    for remname2 in vexclock_ordering:

        vclk = vexclocks[remname2]
        dly = vclk['delay_s']
        dep = datetime_to_vextime(vclk['epoch'])
        rate = vclk['rate_s']
        rep = datetime_to_vextime(vclk['rateepoch'])

        idx = stMap.index[stMap['twoletter'] == remname2]
        if idx.empty:
            print("  * warn: no reverse mapping for {} found in {}". format(remname2, args.scode))
            remname1 = ''
        else:
            remname1 = stMap["oneletter"].iloc[idx[0]]

        if remname1 == args.ref:
            print('def %s; clock_early = %s : %+9.4f usec : %s : %+.6fe-12; enddef; * REF' % (remname2, dep, dly*1e6, rep, rate*1e12))
        elif remname1 not in stations:
            print('def %s; clock_early = %s : %+9.4f usec : %s : %+.6fe-12; enddef; * unchanged' % (remname2, dep, dly*1e6, rep, rate*1e12))
        else:
            rem = stations.index(remname1)
            dly = dly + Dnn[ref,rem]*1e-6
            if not args.no_rate:
                # dt = (vclk['epoch'] - vclk['rateepoch']).total_seconds()
                dt = -(Tnn[rem] - np.datetime64(vclk['epoch'])).item().total_seconds()
                dly = dly + Rnn[ref,rem] * 1e-12 * dt
                rate = rate + Rnn[ref,rem]*1e-12
            print('def %s; clock_early = %s : %+9.4f usec : %s : %+.6fe-12; enddef; * adjusted' % (remname2, dep, dly*1e6, rep, rate*1e12))

