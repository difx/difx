#!/usr/bin/env python3
'''
A script to plot delay and rate residuals based on alist (v6) output.
'''

import afiob  # From HOPS of DiFX 2.6.x+

import argparse
import datetime
import numpy
from collections import defaultdict
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.pyplot import cm
import sys


def getAlistData(alistname, refStation, remStation=None, verbose=True, relabelLinears=True):

    af = afiob.alist(alistname)
    N = af.nfringe

    times = {}  # snrs[<remAntenna 1-letter ID>] = [unixT0 unixT1 ...]
    snrs = {}   # snrs[<remAntenna 1-letter ID>][<polzn pair>] = [snr@T0 snr@t1 ...]

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
        bline_sign = +1 if baseline[0]!=refStation else -1

        # Store time and SNR data point
        if ant2 not in times: times[ant2] = []
        if T not in times[ant2]: times[ant2].append(T)

        if ant2 not in snrs: snrs[ant2] = {}
        if polpair not in snrs[ant2]: snrs[ant2][polpair] = []
        snrs[ant2][polpair].append(frec.snr)

        if verbose:
            print(T.strftime('%j-%H%M%S'), scanname, ant1, ant2, baseline, polpair, frec.snr)

    return (times, snrs)


def plotAlistData(times, snrs, refAnt, verbose=True):

    Nant = len(snrs)

    if Nant <= 8:
        # colormap Dark2 has 8 colors only
        color=iter(cm.Dark2(numpy.linspace(0,1,Nant)))
    else:
        # use a wider palette to avoid duplicate colors for 9+ stations
        color=iter(cm.rainbow(numpy.linspace(0,1,Nant)))

    fig = plt.figure(figsize=(12,8))

    ax1 = fig.add_subplot(211)
    ax1.set_ylabel('SNR')
    ax1.set_title("Fringe SNRs on baselines to '%s'" % (refAnt))

    ax2 = fig.add_subplot(212)
    ax2.set_ylabel('SNR Ratio (LL+RR)/(LR+LR)')
    plt.gca().xaxis.set_major_formatter( mdates.DateFormatter('%d-%m-%Y %H:%M:%S'))

    for remAnt in snrs:

        polSNRs = snrs[remAnt]
        pols = [key for key in snrs[remAnt]]
        Ndata = len(polSNRs[pols[0]])

        ratios = []
        if len(pols) == 4:
            for n in range(Ndata):
                r = polSNRs['LL'][n] + polSNRs['RR'][n]
                r = r / (polSNRs['LR'][n] + polSNRs['RL'][n])
                ratios.append(r)
        else:
            print('  too few polarization data products on baseline to %s' % (remAnt))
            ratios = [0.0 for n in range(Ndata)]


        print(remAnt, pols, len(times[remAnt]), Ndata, 'ratios', ratios)

        c = next(color)
        ax1.plot(times[remAnt], polSNRs['LL'], marker="o", label=remAnt, color='red', picker=5, gid=remAnt)
        ax1.plot(times[remAnt], polSNRs['RR'], marker="o", label=remAnt, color='green', picker=5, gid=remAnt)
        ax1.plot(times[remAnt], polSNRs['LR'], marker="x", label=remAnt, color='black', picker=5, gid=remAnt)
        ax1.plot(times[remAnt], polSNRs['RL'], marker="x", label=remAnt, color='grey', picker=5, gid=remAnt)
        ax2.plot(times[remAnt], ratios, marker="o", label=remAnt, color=c, picker=5, gid=remAnt)
        ax2.legend(loc='upper right', ncol=min(6,Nant))

        plt.gcf().autofmt_xdate()

    plt.tight_layout()
    plt.show()


def scatter_onpick(event):
    '''When a datapoint gets selected in a figure print out its details.'''
    print('Station %s, data index %s' % (event.artist.get_gid(), str(event.ind)))


def scatterplotAlistData(times, snrs, refAnt, verbose=True):

    Nant = len(snrs)

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

    for remAnt in snrs:

        polSNRs = snrs[remAnt]
        pols = [key for key in snrs[remAnt]]
        if len(pols) != 4:
            print('  too few polarization data products on baseline %s-%s' % (refAnt,remAnt))
            continue

        c = next(color)
        baseline = '%s-%s' % (refAnt,remAnt)

        ax[0].scatter(polSNRs['LL'], polSNRs['LR'], marker="x", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax[1].scatter(polSNRs['LL'], polSNRs['RR'], marker="o", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax[2].scatter(polSNRs['RR'], polSNRs['RL'], marker="x", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)
        ax[3].scatter(polSNRs['LR'], polSNRs['RL'], marker="x", label=remAnt, color=c, picker=5, gid=remAnt, alpha=plot_alpha)

    ax[0].set_xlabel('LL SNR'); ax[0].set_ylabel('LR SNR'); ax[0].set_title('Parallel against Cross - reference %s' % (refAnt))
    ax[1].set_xlabel('LL SNR'); ax[1].set_ylabel('RR SNR'); ax[1].set_title('Parallel against Parallel')
    ax[2].set_xlabel('RR SNR'); ax[2].set_ylabel('RL SNR'); ax[2].set_title('Parallel against Cross')
    ax[3].set_xlabel('LR SNR'); ax[3].set_ylabel('RL SNR'); ax[3].set_title('Cross against Cross')

    for k in ax:
        # ax[k].set_xlim(0, 30)
        # ax[k].set_ylim(0, 30)
        ax[k].grid(True)
        ax[k].legend(loc='upper right', ncol=min(6,Nant))

    plt.tight_layout()
    plt.show()



parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("-s", "--min-snr", dest="minSnr", type=int, default=7, help="min. SNR to consider (default: 7)")
parser.add_argument("-S", "--src", dest="src", action='append',  help="source to evaluate (when none specified: all)")
parser.add_argument("refRemSt", metavar="reference-station-code", help="Reference station 1-letter ID, or two letters for a specific baseline.")
parser.add_argument("alistFile", nargs='+', metavar="Alist-file", help="The input alist file(s). Note: Each file should be produced with alist -v 6.")
args = parser.parse_args()

for alistname in args.alistFile:
    print ("File: %s" % (str(alistname)))

    refSt, remSt = args.refRemSt[0], None
    if len(args.refRemSt) >= 2: remSt = args.refRemSt[1]

    (times, stationPolSNRs) = getAlistData(alistname, refSt, remSt, verbose=False)
    # plotAlistData(times, stationPolSNRs, refSt)
    scatterplotAlistData(times, stationPolSNRs, refSt)
