#!/usr/bin/env python3
'''
A script to plot A-list fringe SNRs in all polarizations.
Useful to catch polarization swaps and misbehaving receivers.
'''

import afiob  # From HOPS of DiFX 2.6.x+

import argparse
import datetime
import numpy
from collections import defaultdict
from operator import itemgetter

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.pyplot import cm
import sys


def getAlistData(alistname, refStation, remStation=None, verbose=True, relabelLinears=True):

    af = afiob.alist(alistname)
    N = af.nfringe

    timeseries = {}  # timeseries[key:<unix timestamp>] = [key:<baseline>][key<polpair>:snr]

    # for frec in af.fringedata: # -> segfault, loops past end of array
    for n in range(N):

        frec = af.fringedata[n]
        baseline = str(frec.baseline, "utf-8")
        scanname = str(frec.scan_id, "utf-8")
        polpair = str(frec.polarization, "utf-8")

        if frec.baseline[0] == frec.baseline[1]:
            continue
        if refStation not in baseline:
            continue
        if remStation and remStation not in baseline:
            continue

        # Change X,Y labels into R,L to simplify plotting
        if relabelLinears:
            polpair = polpair.replace('X','R')
            polpair = polpair.replace('Y','L')

        # Convert afiob timestamp from seconds-since-1980 into unix seconds-since-1970
        time_tag_tunix = frec.time_tag + 315532800
        T = datetime.datetime.utcfromtimestamp(time_tag_tunix)

        # Antennas of baseline, ref.ant. always first
        ant1 = refStation
        ant2 = baseline[0] if baseline[0]!=refStation else baseline[1]
        baseline_std = ant1 + ant2

        # Store time etc in a multi-level dict
        if T not in timeseries:
            timeseries[T] = {'antennas': [refStation]}
        if baseline_std not in timeseries[T]:
            timeseries[T][baseline_std] = {}
        if ant2 not in timeseries[T]['antennas']:
            timeseries[T]['antennas'].append(ant2)
        if polpair in timeseries[T][baseline_std]:
            print("Warning: time %s or scan %s already has an entry for baseline %s polnz %s" % (T.strftime('%j-%H%M%S'), scanname, baseline_std, polpair))
        else:
            timeseries[T][baseline_std][polpair] = float(frec.snr)

        if verbose:
            print(T.strftime('%j-%H%M%S'), scanname, ant1, ant2, baseline, polpair, frec.snr)

    return timeseries


def extractPolTimeseries(baseline, polpairs, timeseries):

    snrs = {}
    times = [t for t in timeseries.keys() if baseline in timeseries[t].keys() and all(p in timeseries[t][baseline] for p in polpairs)]
    times = sorted(times)
    for p in polpairs:
        snrs[p] = [timeseries[t][baseline][p] for t in times]

    return (times, snrs)


def plot_onpick(event):
    '''When a datapoint gets selected in a figure print out its details.'''
    print('Station %s, data index %s' % (event.artist.get_gid(), str(event.ind)))


def plotAlistData(timeseries, refAnt, min_snr=None, verbose=False):

    Nant = 0
    remAnts = set()
    scantimes = timeseries.keys()
    for t in scantimes:
        remAnts.update(timeseries[t]['antennas'])
    remAnts = sorted(list(remAnts - {refAnt}))
    Nant = len(remAnts)

    if Nant <= 8:
        # colormap Dark2 has 8 colors only
        color=iter(cm.Dark2(numpy.linspace(0,1,Nant)))
    else:
        # use a wider palette to avoid duplicate colors for 9+ stations
        color=iter(cm.rainbow(numpy.linspace(0,1,Nant)))

    fig = plt.figure(figsize=(12,8))
    fig.canvas.mpl_connect('pick_event', plot_onpick)

    ax1 = fig.add_subplot(211)
    ax1.set_ylabel('SNR')
    ax1.set_title("Fringe SNRs on baselines to '%s'" % (refAnt))

    ax2 = fig.add_subplot(212)
    ax2.set_ylabel('SNR Ratio (LL+RR)/(LR+RL)')
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%d-%m-%Y %H:%M:%S'))

    if len(timeseries)>0:
        ax2.plot([min(timeseries.keys()), max(timeseries.keys())], [1.0, 1.0], label='', color='red', picker=5, gid='ref_level', alpha=0.4)

    all_polpairs = ['LL','RR','LR','RL']
    plot_alpha = 0.7

    for remAnt in remAnts:

        baseline = '%s-%s' % (refAnt,remAnt)
        b = refAnt + remAnt
        c = next(color)

        (times,snr) = extractPolTimeseries(b, all_polpairs, timeseries)
        ratios = [(snr['LL'][n] + snr['RR'][n]) / (snr['LR'][n] + snr['RL'][n]) for n in range(len(times))]

        if min_snr:
            indices = [i for i in range(len(times)) if any(snr[pp][i] >= min_snr for pp in all_polpairs)]
            if len(indices) > 1:
                ratios = list(itemgetter(*indices)(ratios))
                times = list(itemgetter(*indices)(times))
                for pp in all_polpairs:
                    snr[pp] = list(itemgetter(*indices)(snr[pp]))
            elif len(indices) == 1:
                # ratios = list(itemgetter(*indices)(ratios))
                # --> "TypeError: 'float' object is not iterable", how to fix?
                continue
            else:
                continue

        ax1.plot(times, snr['LL'], marker="o", label=baseline+':LL', color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax1.plot(times, snr['RR'], marker="o", label=baseline+':RR', color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax1.plot(times, snr['LR'], marker="x", label=baseline+':LR', color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax1.plot(times, snr['RL'], marker="x", label=baseline+':RL', color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax1.legend(loc='upper right', ncol=Nant)

        ax2.plot(times, ratios, marker="o", label=baseline, color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax2.legend(loc='upper right', ncol=Nant)

        ax2.set_xlabel('Scan Time (UT)')
        plt.draw()

        plt.gcf().autofmt_xdate()

    plt.tight_layout()


def scatter_onpick(event):
    '''When a datapoint gets selected in a figure print out its details.'''
    print('Station %s, data index %s' % (event.artist.get_gid(), str(event.ind)))


def scatterplotAlistData(timeseries, refAnt, verbose=False):

    Nant = 0
    remAnts = set()
    scantimes = timeseries.keys()
    for t in scantimes:
        remAnts.update(timeseries[t]['antennas'])
    remAnts = sorted(list(remAnts - {refAnt}))
    Nant = len(remAnts)

    if Nant <= 8:
        # colormap Dark2 has 8 colors only
        color=iter(cm.Dark2(numpy.linspace(0,1,Nant)))
    else:
        # use a wider palette to avoid duplicate colors for 9+ stations
        color=iter(cm.rainbow(numpy.linspace(0,1,Nant)))

    fig = plt.figure(figsize=(12,8))
    fig.canvas.mpl_connect('pick_event', scatter_onpick)

    ax = {}
    ax[0] = fig.add_subplot(221)
    ax[1] = fig.add_subplot(222)
    ax[2] = fig.add_subplot(223)
    ax[3] = fig.add_subplot(224)
    plot_alpha = 0.7

    for remAnt in remAnts:

        baseline = '%s-%s' % (refAnt,remAnt)
        b = refAnt + remAnt
        c = next(color)
 
        (t,snr) = extractPolTimeseries(b, ['LL','RL'], timeseries)
        ax[0].scatter(snr['LL'], snr['RL'], marker="x", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)

        (t,snr) = extractPolTimeseries(b, ['LL','RR'], timeseries)
        ax[1].scatter(snr['LL'], snr['RR'], marker="o", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)

        (t,snr) = extractPolTimeseries(b, ['RR','RL'], timeseries)
        ax[2].scatter(snr['RR'], snr['RL'], marker="x", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)

        (t,snr) = extractPolTimeseries(b, ['LR','RL'], timeseries)
        ax[3].scatter(snr['LR'], snr['RL'], marker="x", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)

    ax[0].set_xlabel('LL SNR'); ax[0].set_ylabel('LR SNR'); ax[0].set_title('Parallel against Cross - reference %s' % (refAnt))
    ax[1].set_xlabel('LL SNR'); ax[1].set_ylabel('RR SNR'); ax[1].set_title('Parallel against Parallel')
    ax[2].set_xlabel('RR SNR'); ax[2].set_ylabel('RL SNR'); ax[2].set_title('Parallel against Cross')
    ax[3].set_xlabel('LR SNR'); ax[3].set_ylabel('RL SNR'); ax[3].set_title('Cross against Cross')

    for k in ax:
        # ax[k].set_xlim(0, 30)
        # ax[k].set_ylim(0, 30)
        xmin, xmax = ax[k].get_xlim()
        ymin, ymax = ax[k].get_ylim()
        ax[k].set_xlim(min(xmin,ymin), max(xmax,ymax))
        ax[k].set_ylim(min(xmin,ymin), max(xmax,ymax))
        ax[k].grid(True)
        ax[k].legend(loc='upper right', ncol=min(6,Nant))

    plt.tight_layout()


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("-s", "--min-snr", dest="minSnr", type=int, default=12, help="min. SNR needed in at least one pol-pair (default: 12)")
parser.add_argument("-v", dest="verbose", action='store_true',   help="verbose output")
parser.add_argument("refRemSt", metavar="reference-station-code", help="Reference station 1-letter ID, or two letters for a specific baseline.")
parser.add_argument("alistFile", nargs='+', metavar="Alist-file", help="The input alist file(s). Note: Each file should be produced with alist -v 6.")
args = parser.parse_args()

for alistname in args.alistFile:

    print ("File: %s" % (str(alistname)))

    refSt, remSt = args.refRemSt[0], None
    if len(args.refRemSt) >= 2: remSt = args.refRemSt[1]

    timeseries = getAlistData(alistname, refSt, remSt, verbose=args.verbose)

    plotAlistData(timeseries, refSt, min_snr=args.minSnr, verbose=args.verbose)

    scatterplotAlistData(timeseries, refSt, verbose=args.verbose)

    plt.show()
