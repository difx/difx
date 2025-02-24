#!/usr/bin/env python3
'''
A script to plot delay and rate residuals based on alist (v6) output.
'''

import argparse
import datetime
import numpy 
from collections import defaultdict
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.pyplot import cm 
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("-n","--no-plot", dest="noPlot",action='store_true',  help="do not do graphical output")
parser.add_argument("-q", "--min-quality", dest="minQual", type=int, choices=range(1,10),default=5, help="min. fringe quality to consider (default: 5)")
parser.add_argument("-s", "--min-snr", dest="minSnr", type=int, default=7, help="min. SNR to consider (default: 7)")
parser.add_argument("-m", "--max-fr", dest="maxFr", type=float, default=100, help="max. fringe rate in mHz. Any fringe rate larger than this value will be reported. (default: 100mHz)")
parser.add_argument("-f", "--plot-fr", dest="plotFR", action='store_true', help="plot fringe rate instead of delay rate")
parser.add_argument("-p", "--pol", dest="pol", action='append',  help="polarization pair to evaluate. Can be given multiple times if more than one pair is wanted. (default: LL and RR)") 
parser.add_argument("-S", "--src", dest="src", action='append',  help="source to evaluate (when none specified: all)") 
parser.add_argument("-xS", "--exclude-src", dest="excl_src", action='append',  help="source to exclude.") 
parser.add_argument("refRemSt", metavar="reference-station-code", help="One letter code of the station to serve as delay & rate reference. Two letters indicate a specific baseline.")
parser.add_argument("alistFile", nargs='+', type=argparse.FileType('r'), metavar="Alist-file", help="The input alist file(s). Note: Each file should be produced with alist -v 6.")

def onpick(event):
	'''When a datapoint gets selected in a figure print out its details.'''
	#    for curve in ax1.get_lines():
	#        if mcurve.contains(event)[0]:
	#           print ("over %s" % curve.get_gid())
	thisline = event.artist
	xdata = thisline.get_xdata()
	ydata = thisline.get_ydata()
	ind = event.ind
	print ('X=%s Y=%f bline=%s scan=%s source=%s' % (numpy.take(xdata, ind)[0], numpy.take(ydata, ind)[0], args.refRemSt[0]+event.artist.get_gid(), scans[numpy.take(xdata, ind)[0]], scansources[numpy.take(xdata, ind)[0]]))
	# print ('X='+str(numpy.take(xdata, ind)[0])) # Print X point
	# print ('Y='+str(numpy.take(ydata, ind)[0])) # Print Y point
	# print ('BL='+args.refRemSt[0]+event.artist.get_gid())

args = parser.parse_args()

# set default pol pairs
if not args.pol:
  args.pol = ['RR', 'LL', 'XX', 'YY', 'XR', 'YL']

dates = {}
times = {}
rates = defaultdict(list)
fringerates = defaultdict(list)
delays = defaultdict(list)
scans = defaultdict(str)
scansources = defaultdict(str)

refRemSt = args.refRemSt

for alist in args.alistFile:

	print ("File: %s" % (str(alist)))

	for line in alist:
		line = line.strip()	
		if line.startswith("*"):
			continue
		field = line.split()
		st1 = field[14][0]
		st2 = field[14][1]
		q = int(field[15][0])
		snr = float(field[20])
		pol = field[17]
		srcname = field[13]

		# discard autocorrelations
		if st1 == st2:
			continue

		# discard non-detections
		if q  < args.minQual or snr < args.minSnr:
			continue

		# only consider values to the reference station
		if st1 != refRemSt[0] and st2 != refRemSt[0]:
			continue
		if len(refRemSt) > 1 and st1 != refRemSt[1] and st2 != refRemSt[1]:
			continue

		# only consider selected polarizations
		if pol not in args.pol:
			continue

		# only consider specific source(s) if desired
		if (args.excl_src != None) and (srcname in args.excl_src):
			continue
		if (args.src != None) and (srcname not in args.src):
			continue


		scan = field[8]
		doy,time = field[11].split("-")
		hour = time[0:2]
		minute = time[2:4]
		second = time[4:6]
		delay = float(field[24])
		rate = float(field[27])
		freq = float(field[36])

		if st1 == refRemSt[0]:
			rate = -rate
			delay = -delay

		date = datetime.datetime.strptime(field[10]+"-"+field[11], "%Y-%j-%H%M%S")

		scans[date] = scan
		scansources[date] = srcname

		fringerate = rate*freq*1e-3
		#print (date, st1, st2, scan, rate, fringerate)
		if abs(fringerate) > abs(args.maxFr):
			print ("Warning: abs. fringe rate > %e found: FR=%e bline=%s%s at %s " % (args.maxFr, fringerate, st1, st2, date))

		if st1 in rates.keys():
			rates[st1] = numpy.append(rates[st1], rate)
			fringerates[st1] = numpy.append(fringerates[st1], fringerate)

			delays[st1] = numpy.append(delays[st1], delay)
			dates[st1].append(date)
			times[st1].append(time)
		else:
			rates[st1]= numpy.array([rate])
			fringerates[st1]= numpy.array([fringerate])
			delays[st1] = numpy.array([delay])
			dates[st1] = [date]
			times[st1] = [time]

		if st2 in rates.keys():
			rates[st2] = numpy.append(rates[st2], rate)
			fringerates[st2] = numpy.append(fringerates[st2], fringerate)
			delays[st2] = numpy.append(delays[st2], delay)
			dates[st2].append(date)
			times[st2].append(time)
		else:
			rates[st2]= numpy.array([rate])
			fringerates[st2]= numpy.array([fringerate])
			delays[st2] = numpy.array([delay])
			dates[st2] = [date]
			times[st2] = [time]

print ("Reference station: ", refRemSt[0])
print ("Polarizations selected: ", args.pol)
#print ("{0:2s} {1:3s}  {2:4s}   {3:8s}".format("st", "num", "mean", "std dev."))

if len(dates) < 1:
	sys.exit(0)

fig = plt.figure(figsize=(12,8))
fig.canvas.mpl_connect('pick_event', onpick)
fig.subplots_adjust(right=0.65)
ax1 = fig.add_subplot(211)
ax1.set_ylabel('delay [us]')
ax2 = fig.add_subplot(212)

delayStat = "Delays statistics\n{0:2s} {1:4s} {2:7s} {3:4s}\n".format("st", "#pts", "mean", "std. dev")
rateStat = "Delay rate statistics\n{0:2s} {1:4s} {2:7s} {3:4s}\n".format("st", "#pts", "mean", "std. dev")
fringeRateStat = "Fringe rate statistics\n{0:2s} {1:4s} {2:7s} {3:4s}\n".format("st", "#pts", "mean", "std. dev")
refTxt = "Reference station: " + refRemSt[0]
polTxt = "Pol: " + ",".join(args.pol)

if len(rates.keys()) <= 8:
	# colormap Dark2 has 8 colors only
	color=iter(cm.Dark2(numpy.linspace(0,1,len(rates.keys()))))
else:
	# use a wider palette to avoid duplicate colors for 9+ stations
	color=iter(cm.rainbow(numpy.linspace(0,1,len(rates.keys()))))

#print (scans)
for st in rates.keys():
	if st == refRemSt[0]:
		continue
	plt.gca().xaxis.set_major_formatter( mdates.DateFormatter('%d-%m-%Y %H:%M:%S'))
	isorted = numpy.argsort(dates[st])
	stTimes = [times[st][i] for i in isorted]
	stDates = [dates[st][i] for i in isorted]
	stDelays = [delays[st][i] for i in isorted]
	stRates = [rates[st][i] for i in isorted]
	stFringerates = [fringerates[st][i] for i in isorted]
	delayStat += "{0:2s} {1:002d}   {2:+2.4f} {3:=4f}".format(st, len(stDelays), numpy.mean(stDelays), numpy.std(stDelays))+"\n"
	rateStat  += "{0:2s} {1:002d}   {2:+2.4f} {3:=4f}".format(st, len(stRates), numpy.mean(stRates), numpy.std(stRates))+"\n"
	fringeRateStat += "{0:2s} {1:002d}   {2:+2.4f} {3:=4f}".format(st, len(stFringerates), numpy.mean(stFringerates), numpy.std(stFringerates))+"\n"
	#print ("{0:2s} {1:002d} {2:+2.4f} {3:=4f}".format(st, len(stRates), numpy.mean(stRates), numpy.std(stRates)))
	c = next(color)
	ax1.plot(stDates, stDelays, marker="o", label=st, color=c, picker=5, gid=st)
	if args.plotFR:
		ax2.plot(stDates, stFringerates, marker="o", label=st, color=c, picker=5, gid=st)
	else:
		ax2.plot(stDates, stRates,  marker="o", label=st, color=c, picker=5, gid=st)
	#ax3.plot (times[st], stDelays, marker="o", label=st)
	#ax4.plot (times[st], stRates, marker="o", label=st)

#plt.plot (dates["J"], rates["J"])
ax1.set_title("delay")
ax1.legend(loc='center left', bbox_to_anchor=(1, 0.0), fancybox=True)
#ax2.legend(loc='center left', bbox_to_anchor=(1, 0.5), fancybox=True)
fig.text(0.75, 0.55, delayStat, family='courier', bbox={'facecolor':'white', 'alpha':0.5, 'pad':10})
print (delayStat)

ylims = ax1.get_ylim()
if ylims[0] > -0.15:
    ylims = (-0.15, ylims[1])
if ylims[1] < 0.15:
    ylims = (ylims[0], 0.15)
ax1.set_ylim(ylims)

if args.plotFR:
    ax2.set_title("fringe rate")
    ax2.set_ylabel('fringe rate [mHz]')
    fig.text(0.75, 0.09, fringeRateStat,  family='courier', bbox={'facecolor':'white', 'alpha':0.5, 'pad':10})
    print (fringeRateStat)

    ylims = ax2.get_ylim()
    if ylims[0] > -args.maxFr/2:
        ylims = (-args.maxFr/2, ylims[1])
    if ylims[1] < args.maxFr/2:
        ylims = (ylims[0], args.maxFr/2)
    ax2.set_ylim(ylims)

else:
    ax2.set_title("delay rate")
    ax2.set_ylabel('delay rate [ps/s]')
    fig.text(0.75, 0.09, rateStat,  family='courier', bbox={'facecolor':'white', 'alpha':0.5, 'pad':10})
    print (rateStat)

fig.text(0.01, 0.01, refTxt,  family='courier', bbox={'facecolor':'white', 'alpha':0.5, 'pad':10})
fig.text(0.20, 0.01, polTxt,  family='courier', bbox={'facecolor':'white', 'alpha':0.5, 'pad':10})

plt.gcf().autofmt_xdate()
if args.noPlot == False:
    plt.show()
