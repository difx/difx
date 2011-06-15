/***************************************************************************
 *   Copyright (C) 2009-2011 by Walter Brisken & Adam Deller               *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <vector>
#include <set>
#include <sstream>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <algorithm>
#include <sys/time.h>
#include <sys/stat.h>
#include <difxio/difx_input.h>
#include <difxmessage.h>
#include "vextables.h"
#include "corrparams.h"
#include "vexload.h"
#include "../config.h"

const string version(VERSION);
const string program("vex2difx");
const string verdate("20110612");
const string author("Walter Brisken/Adam Deller");

const int defaultMaxNSBetweenACAvg = 2000000;	// 2ms, good default for use with transient detection

static double current_mjd()
{
	struct timeval t;

	gettimeofday(&t, 0);

	return MJD_UNIX0 + t.tv_sec/SEC_DAY + t.tv_usec/MUSEC_DAY;
}

int calcDecimation(int overSamp)
{
#warning "FIXME: handle non 2^n overSamp here"
	if(overSamp > 2)
	{
		return overSamp / 2;
	}
	else
	{
		return 1;
	}
}

int calculateWorstcaseGuardNS(double samplerate, int subintNS)
{
	double sampleTimeNS = 1000000000.0/samplerate;
	double nsaccumulate = sampleTimeNS;
	while(fabs(nsaccumulate - static_cast<int>(nsaccumulate)) > 0.000000000001)
	{
		nsaccumulate += sampleTimeNS;
	}

	return static_cast<int>(nsaccumulate + 1600.0*subintNS/1000000000.0 + 0.99);
}

// A is assumed to be the first scan in time order
static bool areScansCompatible(const VexScan *A, const VexScan *B, const CorrParams *P)
{
	if(((B->mjdStart < A->mjdStop) && (fabs(B->mjdStart-A->mjdStop)>0.00000001)) ||
	   (B->mjdStart > A->mjdStop + P->maxGap))
	{
		return false;
	}
	if(A->overlap(*B) >= -0.1/86400.0)
	{
		return false;
	}
	if(P->singleScan)
	{
		return false;
	}
	if(P->singleSetup && A->modeDefName != B->modeDefName)
	{
		return false;
	}
	
	return true;
}

// Divides scans into different groups where each group contains scans that can be correlated at the same time.
// This does not pay attention to media or clock breaks
static void genJobGroups(vector<VexJobGroup> &JGs, const VexData *V, const CorrParams *P, int verbose)
{
	list<string> scans;
	V->getScanList(scans);

	while(!scans.empty())
	{
		JGs.push_back(VexJobGroup());
		VexJobGroup &JG = JGs.back();
		JG.scans.push_back(scans.front());
		JG.setTimeRange( *(V->getScanByDefName(scans.front())) );
		scans.pop_front();

		const VexScan *scan1 = V->getScanByDefName(JG.scans.back());
		const CorrSetup *corrSetup1 = P->getCorrSetup(scan1->corrSetupName);

		for(list<string>::iterator it = scans.begin(); it != scans.end();)
		{
			const VexScan *scan2 = V->getScanByDefName(*it);
			const CorrSetup *corrSetup2 = P->getCorrSetup(scan2->corrSetupName);

			// Skip any scans that don't overlap with .v2d mjdStart and mjdStop
			if(P->overlap(*scan2) <= 0.0)
			{
				it++;
				continue;
			}

#warning "FIXME: verify modes are compatible"
			if(areCorrSetupsCompatible(corrSetup1, corrSetup2, P) &&
			   areScansCompatible(scan1, scan2, P))
			{
				JG.logicalOr(*scan2);	// expand jobGroup time to include this scan
				JG.scans.push_back(*it);
				it = scans.erase(it);
				scan1 = scan2;
				corrSetup1 = corrSetup2;
			}
			else
			{	
				it++;
			}
		}
	}

	const list<VexEvent> *events = V->getEvents();
	for(vector<VexJobGroup>::iterator jg = JGs.begin(); jg != JGs.end(); jg++)
	{
		jg->genEvents(*events);
		jg->logicalAnd(*P);		// possibly shrink job group to requested range
	}
}

class MediaChange : public VexInterval
{
public:
	MediaChange(string A, double start, double stop) : VexInterval(start, stop), ant(A) {}

	string ant;
};

ostream& operator << (ostream& os, const MediaChange& x)
{
	int p = os.precision();

	os.precision(12);
	os << "MediaChange(" << x.ant << ", " << x.mjdStart << ", " << x.mjdStop << ")";
	os.precision(p);

	return os;
}

static int nGap(const list<MediaChange> &m, double mjd)
{
	list<MediaChange>::const_iterator it;
	int n = 0;

	for(it = m.begin(); it != m.end(); it++)
	{
		if(mjd >= it->mjdStart && mjd <= it->mjdStop)
		{
			n++;
		}
	}

	return n;
}

static void genJobs(vector<VexJob> &Js, const VexJobGroup &JG, VexData *V, const CorrParams *P, int verbose)
{
	list<VexEvent>::const_iterator e;
	list<double>::const_iterator t;
	list<MediaChange>::iterator c;
	map<string,double> recordStop;
	map<double,int> usage;
	map<double,int> clockBreaks;
	int nClockBreaks = 0;
	list<MediaChange> changes;
	list<double> times;
	list<double> breaks;
	double mjdLast = -1.0;
	int score, scoreBest;
	double mjdBest = 0.0;
	double start;
	int nAnt;
	int nLoop = 0;
	int nEvent;
	VexInterval scanRange;

	nEvent = JG.events.size();

	// first initialize recordStop and usage
	for(e = JG.events.begin(); e != JG.events.end(); e++)
	{
		if(e->eventType == VexEvent::RECORD_START)
		{
			recordStop[e->name] = -1.0;
		}
		if(e->eventType == VexEvent::SCAN_START && (scanRange.mjdStart < 1.0 || e->mjd < scanRange.mjdStart))
		{
			scanRange.mjdStart = e->mjd;
		}
		if(e->eventType == VexEvent::SCAN_START && (scanRange.mjdStart < 1.0 || e->mjd > scanRange.mjdStop))
		{
			scanRange.mjdStop = e->mjd;
		}

		usage[e->mjd] = 0;
		clockBreaks[e->mjd] = 0;
	}
	nAnt = recordStop.size();

	scanRange.logicalAnd(*P);	// Shrink time range to v2d start / stop interval

	// populate changes, times, and usage
	for(e = JG.events.begin(); e != JG.events.end(); e++)
	{
		if(mjdLast > 0.0 && e->mjd > mjdLast)
		{
			usage[e->mjd] = usage[mjdLast];
			mjdLast = e->mjd;
			if(JG.containsAbsolutely(e->mjd))
			{
				times.push_back(e->mjd);
			}
		}
		else if(mjdLast < 0.0)
		{
			usage[e->mjd] = 0;
			mjdLast = e->mjd;
			if(JG.containsAbsolutely(e->mjd))
			{
				times.push_back(e->mjd);
			}
		}

		if(e->eventType == VexEvent::RECORD_START)
		{
			if(recordStop[e->name] > 0.0)
			{
				if(JG.containsAbsolutely(recordStop[e->name]) &&
				   JG.containsAbsolutely(e->mjd) &&
				   scanRange.containsAbsolutely(e->mjd))
				{
					changes.push_back(MediaChange(e->name, recordStop[e->name], e->mjd));
					if(verbose > 0)
					{
						cout << "Media change: " << e->name << " " << 
							(VexInterval)(changes.back()) << endl;
					}
				}
			}
		}
		else if(e->eventType == VexEvent::RECORD_STOP)
		{
			recordStop[e->name] = e->mjd;
		}
		else if(e->eventType == VexEvent::ANT_SCAN_START)
		{
			usage[e->mjd]++;
		}
		else if(e->eventType == VexEvent::ANT_SCAN_STOP)
		{
			usage[e->mjd]--;
		}
		else if(e->eventType == VexEvent::CLOCK_BREAK ||
			e->eventType == VexEvent::LEAP_SECOND ||
			e->eventType == VexEvent::MANUAL_BREAK)
		{
			if(JG.containsAbsolutely(e->mjd))
			{
				clockBreaks[e->mjd]++;
				nClockBreaks++;
			}
		}
	}

	// now go through and set breakpoints
	while(!changes.empty() || nClockBreaks > 0)
	{
		nLoop++;
		if(nLoop > nEvent+3) // There is clearly a problem converging!
		{
			cerr << "Developer error: jobs not converging after " << nLoop << " tries.\n" << endl;

			cerr << "Events:" << endl;
			list<VexEvent>::const_iterator iter;
			for(iter = JG.events.begin(); iter != JG.events.end(); iter++)
			{
				cerr << "   " << *iter << endl;
			}

			cerr << "nClockBreaks = " << nClockBreaks << endl;

			cerr << "Media Changes remaining were:" << endl;
			list<MediaChange>::const_iterator it;
			for(it = changes.begin(); it != changes.end(); it++)
			{
				cerr << "   " << *it << endl;
			}

			exit(0);
		}

		// look for break with highest score
		// Try as hard as possible to minimize number of breaks
		scoreBest = -1;
		for(t = times.begin(); t != times.end(); t++)
		{
			score = nGap(changes, *t) * (nAnt-usage[*t]+1) + 100*clockBreaks[*t];
			if(score > scoreBest)
			{
				scoreBest = score;
				mjdBest = *t;
			}
		}

		breaks.push_back(mjdBest);
		nClockBreaks -= clockBreaks[mjdBest];
		clockBreaks[mjdBest] = 0;

		// find modules that change in the new gap
		for(c = changes.begin(); c != changes.end();)
		{
			if(c->mjdStart <= mjdBest && c->mjdStop >= mjdBest)
			{
				c = changes.erase(c);
			}
			else
			{
				c++;
			}
		}
	}
	breaks.sort();

	// Add a break at end so num breaks = num jobs
	breaks.push_back(JG.mjdStop);

	// form jobs
	start = JG.mjdStart;
	for(t = breaks.begin(); t != breaks.end(); t++)
	{
		VexInterval jobTimeRange(start, *t);
		if(jobTimeRange.duration() > P->minLength)
		{
			JG.createJobs(Js, jobTimeRange, V, P->maxLength, P->maxSize);
		}
		else
		{
			cerr << "Warning: skipping short job of " << (jobTimeRange.duration()*86400.0) << " seconds duration." << endl;
		}
		start = *t;
	}
}

static void makeJobs(vector<VexJob>& J, VexData *V, const CorrParams *P, int verbose)
{
	vector<VexJobGroup> JG;
	vector<VexJob>::iterator j;
	int k;

	// Do splitting of jobs
	genJobGroups(JG, V, P, verbose);

	if(verbose > 0)
	{
		cout << JG.size() << " job groups created:" << endl;
		for(unsigned int i = 0; i < JG.size(); i++)
		{
			cout << "  " << JG[i];
		}
	}

	for(unsigned int i = 0; i < JG.size(); i++)
	{
		genJobs(J, JG[i], V, P, verbose);
	}

	// Finalize all the new job structures
	for(j = J.begin(), k = P->startSeries; j != J.end(); j++, k++)
	{
		ostringstream name;
		j->jobSeries = P->jobSeries;
		j->jobId = k;

		// note -- this is an internal name only, not the job prefix that 
		// becomes part of the filenames
		name << j->jobSeries << "_" << j->jobId;

		V->addEvent(j->mjdStart, VexEvent::JOB_START, name.str());
		V->addEvent(j->mjdStop,  VexEvent::JOB_STOP,  name.str());
		j->assignVSNs(*V);
	}
	V->sortEvents();
}

static DifxJob *makeDifxJob(string directory, const VexJob& J, int nAntenna, const string& obsCode, int *n, int nDigit, char ext, const string &vexFile, const string &threadsFile)
{
	DifxJob *job;
	const char *difxVer;
	char fileBase[DIFXIO_FILENAME_LENGTH];
	int v;

	*n = 1;
	job = newDifxJobArray(*n);
	difxVer = getenv("DIFX_VERSION");
	if(difxVer)
	{
		snprintf(job->difxVersion, DIFXIO_VERSION_LENGTH, "%s", difxVer);
	}
	else
	{
		snprintf(job->difxVersion, DIFXIO_VERSION_LENGTH, "%s", "Unknown");
	}
	snprintf(job->vexFile, DIFXIO_FILENAME_LENGTH, "%s", vexFile.c_str());
	job->jobStart = J.mjdStart;
	job->jobStop  = J.mjdStop;
	job->mjdStart = J.mjdStart;
	job->duration = trunc((J.mjdStop - J.mjdStart) * 86400.0 + 0.001);
	job->jobId    = J.jobId;
	job->subarrayId = 0;
	snprintf(job->obsCode, DIFXIO_OBSCODE_LENGTH, "%s", obsCode.c_str());
	job->obsCode[7] = 0;
	snprintf(job->taperFunction, DIFXIO_TAPER_LENGTH, "%s", "UNIFORM");
	job->polyOrder = 5;
	job->polyInterval = 120;
	job->aberCorr = AberCorrExact;
	job->activeBaselines = nAntenna;
	job->activeBaselines = nAntenna*(nAntenna-1)/2;
	job->dutyCycle = J.dutyCycle;

	// The following line defines the format of the job filenames

	if(J.jobId > 0)
	{
		const int FormatLength = 20;
		char format[FormatLength];

		v = snprintf(format, FormatLength, "%%s/%%s_%%0%dd%%c", nDigit);
		if(v >= FormatLength)
		{
			cerr << "Developer error: makeDifxJob: format being truncated.  Needed " << v << " bytes." << endl;

			exit(0);
		}
		v = snprintf(fileBase, DIFXIO_FILENAME_LENGTH, format, directory.c_str(), J.jobSeries.c_str(), J.jobId, ext);
	}
	else
	{
		if(ext != 0)
		{
			cerr << "Warning: makeDifxJob: ext!=0 and making job names without extensions!" << endl;

			exit(0);
		}
		v = snprintf(fileBase, DIFXIO_FILENAME_LENGTH, "%s/%s", directory.c_str(), J.jobSeries.c_str());
	}
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		cerr << "Developer error: makeDifxJob: fileBase needed " << v << " bytes." << endl;

		exit(0);
	}

	snprintf(job->inputFile,   DIFXIO_FILENAME_LENGTH, "%s.input", fileBase);
	snprintf(job->calcFile,    DIFXIO_FILENAME_LENGTH, "%s.calc",  fileBase);
	snprintf(job->flagFile,    DIFXIO_FILENAME_LENGTH, "%s.flag",  fileBase);
	snprintf(job->imFile,      DIFXIO_FILENAME_LENGTH, "%s.im",    fileBase);
	snprintf(job->outputFile,  DIFXIO_FILENAME_LENGTH, "%s.difx",  fileBase);
	if(threadsFile != "")
	{
		v = snprintf(job->threadsFile, DIFXIO_FILENAME_LENGTH, "%s", threadsFile.c_str());
	}
	else
	{
		v = snprintf(job->threadsFile, DIFXIO_FILENAME_LENGTH, "%s.threads", fileBase);
	}
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		cerr << "Developer error: makeDifxJob: threadsFile wanted " << v << " bytes, not " << DIFXIO_FILENAME_LENGTH << " .  Truncating."  << endl;
	}

	return job;
}

static DifxAntenna *makeDifxAntennas(const VexJob& J, const VexData *V, const CorrParams *P, int *n, vector<string>& antList)
{
	const VexAntenna *ant;
	DifxAntenna *A;
	double clockrefmjd, mjd;
	map<string,string>::const_iterator a;
	int i;

	mjd = 0.5*(V->obsStart() + V->obsStop());

	*n = J.vsns.size();

	antList.clear();

	A = newDifxAntennaArray(*n);

	for(i = 0, a = J.vsns.begin(); a != J.vsns.end(); i++, a++)
	{
		ant = V->getAntenna(a->first);
		snprintf(A[i].name, DIFXIO_NAME_LENGTH, "%s", a->first.c_str());
		A[i].X = ant->x + ant->dx*(mjd-ant->posEpoch)*86400.0;
		A[i].Y = ant->y + ant->dy*(mjd-ant->posEpoch)*86400.0;
		A[i].Z = ant->z + ant->dz*(mjd-ant->posEpoch)*86400.0;
		A[i].mount = stringToMountType(ant->axisType.c_str());
		clockrefmjd = ant->getVexClocks(J.mjdStart, A[i].clockcoeff);
		if(clockrefmjd < 0.0)
		{
			cerr << "WARNING: Job " << J.jobSeries << " " << J.jobId << ": no clock offsets being applied to antenna " << a->first << endl;
			cerr << "          Unless this is intentional, your results will suffer!" << endl;
		}
		A[i].clockrefmjd = clockrefmjd;
		A[i].clockorder = 1;
		A[i].clockcoeff[0] *= 1.0e6;	// convert to us from sec
		A[i].clockcoeff[1] *= 1.0e6;	// convert to us/sec from sec/sec
		A[i].offset[0] = ant->axisOffset;
		A[i].offset[1] = 0.0;
		A[i].offset[2] = 0.0;

		/* override with antenna setup values? */
		const AntennaSetup *antSetup = P->getAntennaSetup(a->first);
		if(antSetup)
		{
			if(fabs(antSetup->X) > 0.1)
			{
				A[i].X = antSetup->X;
			}
			if(fabs(antSetup->Y) > 0.1)
			{
				A[i].Y = antSetup->Y;
			}
			if(fabs(antSetup->Z) > 0.1)
			{
				A[i].Z = antSetup->Z;
			}
			if(antSetup->difxName.size() > 0)
			{
				snprintf(A[i].name, DIFXIO_NAME_LENGTH, "%s", antSetup->difxName.c_str());
			}
			A[i].clockcoeff[0] += antSetup->deltaClock*1.0e6;	// convert to us from sec
			A[i].clockcoeff[1] += antSetup->deltaClockRate*1.0e6;	// convert to us/sec from sec/sec
			A[i].clockorder  = antSetup->clockorder;
			switch(A[i].clockorder) {
				case 5: A[i].clockcoeff[5] = antSetup->clock5*1.0e6; // convert to us/sec^5 from sec/sec^5
				case 4: A[i].clockcoeff[4] = antSetup->clock4*1.0e6; // convert to us/sec^4 from sec/sec^4
				case 3: A[i].clockcoeff[3] = antSetup->clock3*1.0e6; // convert to us/sec^3 from sec/sec^3
				case 2: A[i].clockcoeff[2] = antSetup->clock2*1.0e6; // convert to us/sec^2 from sec/sec^2
				case 1: break;
				default: cerr << "Crazy clock order " << A[i].clockorder << "!" << endl;
			}
		}

		antList.push_back(a->first);
		snprintf(A[i].shelf, DIFXIO_SHELF_LENGTH, "%s", P->getShelf(a->second));
	}

	return A;
}

static DifxDatastream *makeDifxDatastreams(const VexJob& J, const VexData *V, const CorrParams *P, int nSet)
{
	DifxDatastream *datastreams;
	DifxDatastream *dd;
	map<string,string>::const_iterator a;
	const VexAntenna *ant;
	int nDatastream;
	
	nDatastream = J.vsns.size() * nSet;
	a = J.vsns.begin();
	datastreams = newDifxDatastreamArray(nDatastream);
	for(int i = 0; i < nDatastream; i++)
	{
		dd = datastreams + i;

		dd->antennaId = i % J.vsns.size();
		dd->tSys = 0.0;

		ant = V->getAntenna(a->first);
		dd->dataSource = ant->dataSource;

		const AntennaSetup *antennaSetup = P->getAntennaSetup(ant->name);
		if(antennaSetup)
		{
			if(ant->dataSource == DataSourceNetwork)
			{
				dd->windowSize = antennaSetup->windowSize;
				dd->networkPort = antennaSetup->networkPort;
			}

			if(antennaSetup->dataSampling < NumSamplingTypes)
			{
				dd->dataSampling = antennaSetup->dataSampling;
			}
		}

		int nFile = ant->basebandFiles.size();
		if(ant->dataSource == DataSourceFile)
		{
			int count = 0;

			for(int j = 0; j < nFile; j++)
			{
				if(J.overlap(ant->basebandFiles[j]) > 0.0)
				{
					count++;
				}
			}

			DifxDatastreamAllocFiles(dd, count);

			count = 0;

			for(int j = 0; j < nFile; j++)
			{
				if(J.overlap(ant->basebandFiles[j]) > 0.0)
				{
					dd->file[count] = strdup(ant->basebandFiles[j].filename.c_str());
					count++;
				}
			}
		}
		else if(ant->dataSource == DataSourceModule)
		{
			DifxDatastreamAllocFiles(dd, 1);

			dd->file[0] = strdup(a->second.c_str());
		}

		a++;
		// Keep recycling through...
		if(a == J.vsns.end())
		{
			a = J.vsns.begin();
		}

	}

	return datastreams;
}

// round up to the next power of two
// There must be a more elegant solution!
int next2(int x)
{
	int n=0; 
	int m=0;
	
	for(int i=0; i < 31; i++)
	{
		if(x & (1 << i))
		{
			n++;
			m = i;
		}
	}

	if(n < 2)
	{
		return x;
	}
	else
	{
		return 2<<m;
	}
}

class freq
{
public:
	freq(double f=0.0, double b=0.0, char s=' ', int nI=0, int nO=0, int os=0, int d=0, int iz=0, unsigned int t=0) 
		: fq(f), bw(b), sideBand(s), nInputChan(nI), nOutputChan(nO), overSamp(os), decimation(d), isZoomFreq(iz), toneSetId(t) {};
	double fq;
	double bw;
	char sideBand;
	int nInputChan;	// number of input channels (channels in FFT)
	int nOutputChan; // number of channels to give the astronomer
	int overSamp;
	int decimation;
	int isZoomFreq;
	unsigned int toneSetId;

	int specAvg() const { return nInputChan/nOutputChan; }
};

static int getFreqId(vector<freq>& freqs, double fq, double bw, char sb, int nI,
		int nO, int os, int d, int iz, unsigned int t)
{
	for(unsigned int i = 0; i < freqs.size(); i++)
	{
		if(fq == freqs[i].fq &&
		   bw == freqs[i].bw &&
		   sb == freqs[i].sideBand &&
		   nI == freqs[i].nInputChan &&
		   nO == freqs[i].nOutputChan &&
		   os == freqs[i].overSamp &&
		   d  == freqs[i].decimation &&
		   iz == freqs[i].isZoomFreq &&
		   t  == freqs[i].toneSetId)
		{
			return i;
		}
	}

	freqs.push_back(freq(fq, bw, sb, nI, nO, os, d, iz, t));

	return freqs.size() - 1;
}

static int getBand(vector<pair<int,int> >& bandMap, int fqId)
{
	vector<pair<int,int> >::iterator it;
	int i;

	for(i = 0, it = bandMap.begin(); it != bandMap.end(); i++, it++)
	{
		if(it->first == fqId)
		{
			it->second++;

			return i;
		}
	}

	bandMap.push_back(pair<int,int>(fqId, 1));

	return bandMap.size() - 1;
}

static int getToneSetId(vector<vector<int> > &toneSets, const vector<int> &tones)
{
	for(unsigned int i = 0; i < toneSets.size(); i++)
	{
		if(toneSets[i] == tones)
		{
			return i;
		}
	}

	toneSets.push_back(tones);

	return toneSets.size() - 1;
}
	
static int setFormat(DifxInput *D, int dsId, vector<freq>& freqs, vector<vector<int> >& toneSets, const VexMode *mode, 
			const string &antName, const CorrSetup *corrSetup, enum V2D_Mode v2dMode)
{
	vector<pair<int,int> > bandMap;
	int overSamp, decimation;

	int antId = D->datastream[dsId].antennaId;
	if(antId < 0 || antId >= D->nAntenna)
	{
		cerr << "Error: setFormat: antId=" << antId << " while nAntenna=" << D->nAntenna << endl;
		
		exit(0);
	}
	const VexFormat* format = mode->getFormat(antName);
	const VexSetup* setup = mode->getSetup(antName);
	int n2 = next2(format->nRecordChan);

	if(format == 0)
	{
		cerr << "Developer error: setFormat(ant=" << antName << ", mode=" << mode->defName << ") -> format=0" << endl;

		exit(0);
	}

	if(setup == 0)
	{
		cerr << "Developer error: setFormat(ant=" << antName << ", mode=" << mode->defName << ") -> setup=0" << endl;

		exit(0);
	}

	overSamp = mode->getOversampleFactor();
	decimation = calcDecimation(overSamp);

	if(format->format == string("VLBA1_1"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBA");
		D->datastream[dsId].dataFrameSize = 2520*format->nBit*n2;
	}
	else if(format->format == string("VLBA1_2"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBA");
		D->datastream[dsId].dataFrameSize = 5040*format->nBit*n2;
	}
	else if(format->format == string("VLBA1_4"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBA");
		D->datastream[dsId].dataFrameSize = 10080*format->nBit*n2;
	}
	else if(format->format == string("VLBN1_1"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBN");
		D->datastream[dsId].dataFrameSize = 2520*format->nBit*n2;
	}
	else if(format->format == string("VLBN1_2"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBN");
		D->datastream[dsId].dataFrameSize = 5040*format->nBit*n2;
	}
	else if(format->format == string("VLBN1_4"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBN");
		D->datastream[dsId].dataFrameSize = 10080*format->nBit*n2;
	}
	else if(format->format == string("MKIV1_1"))
	{
		strcpy(D->datastream[dsId].dataFormat, "MKIV");
		D->datastream[dsId].dataFrameSize = 2500*format->nBit*n2;
	}
	else if(format->format == string("MKIV1_2"))
	{
		strcpy(D->datastream[dsId].dataFormat, "MKIV");
		D->datastream[dsId].dataFrameSize = 5000*format->nBit*n2;
	}
	else if(format->format == string("MKIV1_4"))
	{
		strcpy(D->datastream[dsId].dataFormat, "MKIV");
		D->datastream[dsId].dataFrameSize = 10000*format->nBit*n2;
	}
	else if(format->format == string("MARK5B"))
	{
		strcpy(D->datastream[dsId].dataFormat, "MARK5B");
		D->datastream[dsId].dataFrameSize = 10016;
	}
	else if(format->format == string("VDIF"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VDIF");
		D->datastream[dsId].dataFrameSize = 1032;
	}
	else if(format->format.substr(0,4) == string("VDIF"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VDIF");
		D->datastream[dsId].dataFrameSize = atoi(format->format.substr(5).c_str());
	}
	else if(format->format.substr(0,14) == string("INTERLACEDVDIF"))
	{
		strcpy(D->datastream[dsId].dataFormat, format->format.substr(0,format->format.find_last_of('/')).c_str());
		D->datastream[dsId].dataFrameSize = atoi(format->format.substr(format->format.find_last_of('/')+1).c_str());
	}
	else if(format->format == string("S2"))
	{
		strcpy(D->datastream[dsId].dataFormat, "LBAVSOP");
		D->datastream[dsId].dataFrameSize = 4096 + 10*format->nBit*n2*static_cast<int>(mode->sampRate+0.5)/8;
		cerr << "Warning: S2 data can be in LBAVSOP or LBASTD format - defaulting to LBAVSOP!!" << endl;
	}
	else if(format->format == string("LBAVSOP"))
	{
		strcpy(D->datastream[dsId].dataFormat, "LBAVSOP");
		D->datastream[dsId].dataFrameSize = 4096 + 10*format->nBit*n2*static_cast<int>(mode->sampRate+0.5)/8;
	}
	else if(format->format == string("LBASTD"))
	{
		strcpy(D->datastream[dsId].dataFormat, "LBASTD");
		D->datastream[dsId].dataFrameSize = 4096 + 10*format->nBit*n2*static_cast<int>(mode->sampRate+0.5)/8;
	}
	else
	{
		cerr << "Error: setFormat: format " << format->format << " not currently supported.  Mode=" << mode->defName << ", ant=" << antName << "." << endl;

		return 0;
	}

	D->datastream[dsId].quantBits = format->nBit;
	DifxDatastreamAllocBands(D->datastream + dsId, n2);

	for(vector<VexChannel>::const_iterator i = format->channels.begin(); i != format->channels.end(); i++)
	{
		if(i->subbandId < 0 || i->subbandId >= static_cast<int>(mode->subbands.size()))
		{
			cerr << "Error: setFormat: index to subband=" << i->subbandId << " is out of range" << endl;

			exit(0);
		}

		int r = i->recordChan;
		unsigned int toneSetId, fqId;
		const VexSubband& subband = mode->subbands[i->subbandId];
		
		if(v2dMode == V2D_MODE_PROFILE || setup->phaseCalIntervalMHz() == 0)
		{
			// In profile mode don't extract any tones
			toneSetId = 0;
		}
		else
		{
			toneSetId = getToneSetId(toneSets, i->tones);
		}
		
		fqId = getFreqId(freqs, subband.freq, subband.bandwidth, subband.sideBand,
				corrSetup->nInputChan(), corrSetup->nOutputChan, overSamp, decimation, 0, toneSetId);	// 0 means not zoom band
		
		if(r < 0 || r >= D->datastream[dsId].nRecBand)
		{
			cerr << "Error: setFormat: index to record channel = " << r << " is out of range" << endl;

			exit(0);
		}
		D->datastream[dsId].recBandFreqId[r] = getBand(bandMap, fqId);
		D->datastream[dsId].recBandPolName[r] = subband.pol;
	}
	DifxDatastreamAllocFreqs(D->datastream + dsId, bandMap.size());
	for(unsigned int j = 0; j < bandMap.size(); j++)
	{
		D->datastream[dsId].recFreqId[j] = bandMap[j].first;
		D->datastream[dsId].nRecPol[j]   = bandMap[j].second;
	}

	return n2;
}

void populateRuleTable(DifxInput *D, const CorrParams *P)
{
	list<string>::const_iterator s;

	D->nRule = P->rules.size();
	D->rule = newDifxRuleArray(D->nRule);
	for(int i = 0; i < D->nRule; i++)
	{
		if(P->rules[i].scanName.size() > 0)
		{
			for(s = (P->rules[i].scanName).begin(); s != (P->rules[i].scanName).end(); s++)
			{
				DifxStringArrayadd(&D->rule[i].scanId, s->c_str(), 0);
			}
		}
		if(P->rules[i].sourceName.size() > 0)
		{
			for(s = (P->rules[i].sourceName).begin(); s != (P->rules[i].sourceName).end(); s++)
			{
				DifxStringArrayadd(&D->rule[i].sourceName, s->c_str(), 0);
			}
		}
		if(P->rules[i].modeName.size() > 0)
		{
			// Note: this is not a problem as the mode selection is being done by vex2difx
			//cerr << "Cannot rule on modeName at this time; ignoring." << endl;
		}
		if(P->rules[i].calCode.size() > 0)
		{
			if(P->rules[i].calCode.size() > 1)
			{
				cerr << "Cannot handle rules for more than one calCode simultaneously." << endl;

				exit(0);
			}
			D->rule[i].calCode[0] = P->rules[i].calCode.front();
			D->rule[i].calCode[1] = 0;
		}
		if(P->rules[i].qualifier.size() > 0)
		{
			if(P->rules[i].qualifier.size() > 1)
			{
				cerr << "Cannot handle rules for more than one qualifier simultaneously." << endl;
				
				exit(0);
			}
			D->rule[i].qual = P->rules[i].qualifier.front();
		}
		snprintf(D->rule[i].configName, DIFXIO_NAME_LENGTH, "%s", P->rules[i].corrSetupName.c_str());
	}
}

static void populateFreqTable(DifxInput *D, const vector<freq>& freqs, const vector<vector<int> > &toneSets)
{
	DifxFreq *df;

	D->nFreq = freqs.size();
	D->freq = newDifxFreqArray(D->nFreq);
	for(unsigned int f = 0; f < freqs.size(); f++)
	{
		df = D->freq + f;

		df->freq = freqs[f].fq/1.0e6;
		df->bw   = freqs[f].bw/1.0e6;
		df->sideband = freqs[f].sideBand;
		df->nChan = freqs[f].nInputChan;	// df->nChan is the number of pre-averaged channels
		df->specAvg = freqs[f].specAvg();
		df->overSamp = freqs[f].overSamp;
		df->decimation = freqs[f].decimation;

		// This is to correct for the fact that mpifxcorr does not know about oversampling
		if(df->overSamp > df->decimation)
		{
			if(freqs[f].isZoomFreq == 0) // Don't correct zoom bands as the bandwidth is already correct
			{
				df->bw *= df->overSamp/df->decimation;
			}
			else	// Instead, correct the number of channels
			{
				df->nChan = df->nChan*df->decimation/df->overSamp;
			}
			df->overSamp = df->decimation;
		}

		if(freqs[f].toneSetId < 0 || freqs[f].toneSetId >= toneSets.size())
		{
			cerr << "Developer error: populateFreqTable: toneSetId=" << freqs[f].toneSetId << " nToneSet=" << toneSets.size() << endl;
		}
		const vector<int> &tones = toneSets[freqs[f].toneSetId];

		if(tones.size() > 0)
		{
			DifxFreqAllocTones(df, tones.size());
		
			for(unsigned int t = 0; t < tones.size(); t++)
			{
				df->tone[t] = tones[t];
			}
		}
	}
}

// warning: assumes same number of datastreams == antennas for each config
void populateBaselineTable(DifxInput *D, const CorrParams *P, const CorrSetup *corrSetup, vector<set <int> > blockedfreqids)
{	
	int n1, n2;
	int npol;
	int a1c[2], a2c[2];
	char a1p[2], a2p[2];
	int nFreq;
	DifxBaseline *bl;
	DifxConfig *config;
	int freqId, altFreqId, blId, configId;
	double lowedgefreq, altlowedgefreq;

	// Calculate maximum number of possible baselines based on list of configs
	D->nBaseline = 0;

#warning "FIXME: below assumes nAntenna == nDatastream!"
	if(P->v2dMode == V2D_MODE_PROFILE)
	{
		// Here use nAntenna as nBaseline
		for(configId = 0; configId < D->nConfig; configId++)
		{
			int nD = D->config[configId].nDatastream;

			D->nBaseline += nD;
		}
	}
	else
	{
		// This is the normal configuration, assume n*(n-1)/2
		for(configId = 0; configId < D->nConfig; configId++)
		{
			int nD = D->config[configId].nDatastream;
			
			D->nBaseline += nD*(nD-1)/2;
		}
	}
	
	D->baseline = newDifxBaselineArray(D->nBaseline);

	bl = D->baseline;
	blId = 0;	// baseline table index

	for(configId = 0; configId < D->nConfig; configId++)
	{
		config = D->config + configId;

		config->nBaseline = 0;

		// Note: these should loop over antennaIds
		if(P->v2dMode == V2D_MODE_PROFILE)
		{
			// Disable writing of standard autocorrelations
			config->doAutoCorr = 0;

			// Instead, make autocorrlations from scratch
			for(int a1 = 0; a1 < config->nDatastream; a1++)
			{
				bl->dsA = config->datastreamId[a1];
				bl->dsB = config->datastreamId[a1];

				DifxBaselineAllocFreqs(bl, D->datastream[a1].nRecFreq);

				nFreq = 0; // this counts the actual number of freqs

				// Note: here we need to loop over all datastreams associated with this antenna!
				for(int f = 0; f < D->datastream[a1].nRecFreq; f++)
				{
					freqId = D->datastream[a1].recFreqId[f];

					if(!corrSetup->correlateFreqId(freqId))
					{
						continue;
					}

					DifxBaselineAllocPolProds(bl, nFreq, 4);

					n1 = DifxDatastreamGetRecBands(D->datastream+a1, freqId, a1p, a1c);

					npol = 0;
					for(int u = 0; u < n1; u++)
					{
						if(corrSetup->doPolar)
						{
							bl->bandA[nFreq][npol] = a1c[u];
							bl->bandB[nFreq][npol] = a1c[u];
							npol++;
						}
					}
					bl->nPolProd[nFreq] = npol;

					if(npol == 0)
					{
						// This deallocates
						DifxBaselineAllocPolProds(bl, nFreq, 0);

						continue;
					}

					nFreq++;
				}

				bl->nFreq = nFreq;

				if(bl->nFreq > 0)
				{
					config->baselineId[config->nBaseline] = blId;
					config->nBaseline++;
					bl++;
					blId++;
				}
			}
		}
		else
		{
			for(int a1 = 0; a1 < config->nDatastream-1; a1++)
			{
				for(int a2 = a1+1; a2 < config->nDatastream; a2++)
				{
					bl->dsA = config->datastreamId[a1];
					bl->dsB = config->datastreamId[a2];

					if(!P->useBaseline(D->antenna[a1].name, D->antenna[a2].name))
					{
						continue;
					}

					DifxBaselineAllocFreqs(bl, D->datastream[a1].nRecFreq + D->datastream[a1].nZoomFreq);

					nFreq = 0; // this counts the actual number of freqs

					// Note: here we need to loop over all datastreams associated with this antenna!
					for(int f = 0; f < D->datastream[a1].nRecFreq; f++)
					{
						freqId = D->datastream[a1].recFreqId[f];

						if(!corrSetup->correlateFreqId(freqId))
						{
							continue;
						}
						if(blockedfreqids[a1].size() > 0 && blockedfreqids[a1].find(freqId) != blockedfreqids[a1].end())
						{
							continue;
						}
						if(blockedfreqids[a2].size() > 0 && blockedfreqids[a2].find(freqId) != blockedfreqids[a2].end())
						{
							continue;
						}

						DifxBaselineAllocPolProds(bl, nFreq, 4);

						n1 = DifxDatastreamGetRecBands(D->datastream+a1, freqId, a1p, a1c);
						n2 = DifxDatastreamGetRecBands(D->datastream+a2, freqId, a2p, a2c);
						if(n2 == 0)
						{
							//look for another freqId which matches band but is opposite sideband
							lowedgefreq = D->freq[freqId].freq;
							if(D->freq[freqId].sideband == 'L')
							{
								lowedgefreq -= D->freq[freqId].bw;
							}
							for(int f2 = 0; f2 < D->datastream[a2].nRecFreq; f2++)
							{
								altFreqId = D->datastream[a2].recFreqId[f2];
								altlowedgefreq = D->freq[altFreqId].freq;
								if(D->freq[altFreqId].sideband == 'L')
								{
									altlowedgefreq -= D->freq[altFreqId].bw;
								}
								if(altlowedgefreq == lowedgefreq)
								{
									n2 = DifxDatastreamGetRecBands(D->datastream+a2, altFreqId, a2p, a2c);
								}
							}

							if(n2 == 0)
							{
								//still no dice? Try the zoom bands of datastream 2
								for(int f2 = 0; f2 < D->datastream[a2].nZoomFreq; f2++)
								{
									altFreqId = D->datastream[a2].zoomFreqId[f2];
									altlowedgefreq = D->freq[altFreqId].freq;
									if(D->freq[altFreqId].sideband == 'L')
									{
										altlowedgefreq -= D->freq[altFreqId].bw;
									}
									if(altlowedgefreq == lowedgefreq)
									{
										n2 = DifxDatastreamGetZoomBands(D->datastream+a2, altFreqId, a2p, a2c);
									}
								}
							}
						}

						npol = 0;
						for(int u = 0; u < n1; u++)
						{
							for(int v = 0; v < n2; v++)
							{
								if(corrSetup->doPolar || a1p[u] == a2p[v])
								{
									bl->bandA[nFreq][npol] = a1c[u];
									bl->bandB[nFreq][npol] = a2c[v];
									npol++;
								}
							}
						}
						bl->nPolProd[nFreq] = npol;

						if(npol == 0)
						{
							// This deallocates
							DifxBaselineAllocPolProds(bl, nFreq, 0);

							continue;
						}

						nFreq++;
					}

					for(int f = 0; f < D->datastream[a1].nZoomFreq; f++)
					{
						freqId = D->datastream[a1].zoomFreqId[f];

						// Unlike for recbands, don't query corrSetup->correlateFreqId as all defined zoom bands should be correlated

						DifxBaselineAllocPolProds(bl, nFreq, 4);

						n1 = DifxDatastreamGetZoomBands(D->datastream+a1, freqId, a1p, a1c);
						n2 = DifxDatastreamGetZoomBands(D->datastream+a2, freqId, a2p, a2c);
						if(n2 == 0)
						{
							//look for another freqId which matches band but is opposite sideband
							lowedgefreq = D->freq[freqId].freq;
							if(D->freq[freqId].sideband == 'L')
							{
								lowedgefreq -= D->freq[freqId].bw;
							}
							for(int f2 = 0; f2 < D->datastream[a2].nZoomFreq; f2++)
							{
								altFreqId = D->datastream[a2].zoomFreqId[f2];
								altlowedgefreq = D->freq[altFreqId].freq;
								if(D->freq[altFreqId].sideband == 'L')
								{
									altlowedgefreq -= D->freq[altFreqId].bw;
								}
								if(altlowedgefreq == lowedgefreq)
								{
									n2 = DifxDatastreamGetZoomBands(D->datastream+a2, altFreqId, a2p, a2c);
								}
							}

							if(n2 == 0)
							{
								//still no dice? Try recorded bands from datastream 1
								for(int f2 = 0; f2 < D->datastream[a2].nRecFreq; f2++)
								{
									altFreqId = D->datastream[a2].recFreqId[f2];
									altlowedgefreq = D->freq[altFreqId].freq;
									if(D->freq[altFreqId].sideband == 'L')
									{
										altlowedgefreq -= D->freq[altFreqId].bw;
									}
									if(altlowedgefreq == lowedgefreq)
									{
										n2 = DifxDatastreamGetRecBands(D->datastream+a2, altFreqId, a2p, a2c);
									}
								}
							}
						}

						npol = 0;
						for(int u = 0; u < n1; u++)
						{
							for(int v = 0; v < n2; v++)
							{
								if(corrSetup->doPolar || a1p[u] == a2p[v])
								{
									bl->bandA[nFreq][npol] = D->datastream[a1].nRecBand + a1c[u];
									bl->bandB[nFreq][npol] = D->datastream[a2].nRecBand + a2c[v];
									npol++;
								}
							}
						}
						bl->nPolProd[nFreq] = npol;

						if(npol == 0)
						{
							// This deallocates
							DifxBaselineAllocPolProds(bl, nFreq, 0);

							continue;
						}

						nFreq++;
					}
	
					bl->nFreq = nFreq;
	
					if(bl->nFreq > 0)
					{
						config->baselineId[config->nBaseline] = blId;
						config->nBaseline++;
						bl++;
						blId++;
					}
				}
			}
		}
		config->baselineId[config->nBaseline] = -1;
	}

	// set actual number of baselines
	D->nBaseline = blId;
}

static void populateEOPTable(DifxInput *D, const vector<VexEOP>& E)
{
	int nEOP;

	nEOP = E.size();
	D->nEOP = nEOP;
	D->eop = newDifxEOPArray(D->nEOP);
	for(int e = 0; e < nEOP; e++)
	{
		D->eop[e].mjd = static_cast<int>(E[e].mjd);
		D->eop[e].tai_utc = static_cast<int>(E[e].tai_utc);
		D->eop[e].ut1_utc = E[e].ut1_utc;
		D->eop[e].xPole = E[e].xPole*180.0*3600.0/M_PI;
		D->eop[e].yPole = E[e].yPole*180.0*3600.0/M_PI;
	}
}

static int getConfigIndex(vector<pair<string,string> >& configs, DifxInput *D, const VexData *V, const CorrParams *P, const VexScan *S)
{
	int c, fftDurNS;
	DifxConfig *config;
	const CorrSetup *corrSetup;
	const VexMode *mode;
	string configName;
	double minBW;		// [Hz]
	double readTimeNS;
	int f;

	corrSetup = P->getCorrSetup(S->corrSetupName);
	if(corrSetup == 0)
	{
		cerr << "Error: correlator setup[" << S->corrSetupName << "] == 0" << endl;
		
		exit(0);
	}

	mode = V->getModeByDefName(S->modeDefName);
	if(mode == 0)
	{
		cerr << "Error: mode[" << S->modeDefName << "] == 0" << endl;
		
		exit(0);
	}

	for(unsigned int i = 0; i < configs.size(); i++)
	{
		if(configs[i].first  == S->modeDefName &&
		   configs[i].second == S->corrSetupName)
		{
			return i;
		}
	}

	configName = S->modeDefName + string("_") + S->corrSetupName;

	c = configs.size();
	configs.push_back(pair<string,string>(S->modeDefName, S->corrSetupName));
	config = D->config + c;
	snprintf(config->name, DIFXIO_NAME_LENGTH, "%s", configName.c_str());
	for(int i = 0; i < D->nRule; i++)
	{
		if(strcmp(D->rule[i].configName, S->corrSetupName.c_str()) == 0)
		{
			snprintf(D->rule[i].configName, DIFXIO_NAME_LENGTH, "%s", configName.c_str());
		}
	}
	config->tInt = corrSetup->tInt;
	minBW = mode->sampRate/2.0;
	fftDurNS = static_cast<int>(corrSetup->fftSize()*(0.5/minBW)*1000000000.0 + 0.5);
	if(corrSetup->subintNS > 0)
	{
		config->subintNS = corrSetup->subintNS;
		if(config->subintNS % fftDurNS != 0)
		{
			cerr << "Error: The provided subintNS (" << config->subintNS << ") is not an integer multiple of the FFT duration (" << fftDurNS << ")" << endl;

			exit(0);
		}
	}
	else
	{
		int divisors[] = {1, 2, 5, 10, 25, 50, 125, 250, 625, 0};	// Must be 0 terminated
		bool ok = false;

		for(int d = 0; divisors[d]; d++)
		{
			double msgSize, dataRate, readSize;

			if(corrSetup->tInt/divisors[d] > 2.14)
			{
				// This would make a subint > 2^31 seconds for any value of dataBufferSize
				continue;
			}

			config->subintNS = static_cast<int>((corrSetup->tInt/divisors[d])*1000000000.0 + 0.5);
			dataRate = (mode->sampRate*mode->getBits()*mode->subbands.size());
			msgSize = (config->subintNS*1.0e-9)*dataRate/8.0;
			readSize = msgSize*D->dataBufferFactor/D->nDataSegments;

			ok = true;

			// stop when readSize falls below specifed maximum (default to 25MB)
			if(readSize < P->readSize)
			{
				break;
			}
		}

		if(ok == false)
		{
			cerr << "Error: Integration time (" << corrSetup->tInt << " s) is too long for automatic subintNS determination.  You must manually set it in the .v2d file." << endl;

			exit(0);
		}

		if(config->subintNS % fftDurNS != 0)
		{
			int origSubintNS = config->subintNS;
			config->subintNS -= (config->subintNS % fftDurNS);
			if(config->subintNS <= 0)
			{
				config->subintNS = fftDurNS;
			}
			cout << "Adjusting subintNS(" << origSubintNS << ") to " << config->subintNS << " since it was a non-integer multiple of fftDurNS (" << fftDurNS << ")" << endl;
		}
	}

	// change dataBufferFactor if needed to get send sizes under 2^31 nanoseconds
	readTimeNS = static_cast<double>(config->subintNS)*D->dataBufferFactor/D->nDataSegments;
	if(readTimeNS > 2140000000.0)
	{
		f = static_cast<int>(2140000000.0/config->subintNS);
		if(f < 1)
		{
			cerr << "Error: There is no way to change dataBufferFactor to keep send sizes below 2^31 seconds" << endl;

			exit(0);
		}
		cout << "Changing dataBufferFactor from " << D->dataBufferFactor << " to " << (f*D->nDataSegments) << " in order to keep data send sizes below 2.14 seconds" << endl;
		D->dataBufferFactor = f*D->nDataSegments;
	}
		
	config->guardNS = corrSetup->guardNS;
	config->fringeRotOrder = corrSetup->fringeRotOrder;
	config->strideLength = corrSetup->strideLength;
	config->xmacLength = corrSetup->xmacLength;
	config->numBufferedFFTs = corrSetup->numBufferedFFTs;

#warning "FIXME: is setting pulsarId = -1 correct"
	config->pulsarId = -1;
	config->doPolar = corrSetup->doPolar;
	config->doAutoCorr = 1;
	config->nAntenna = D->nAntenna;
	config->nDatastream = D->nAntenna;
	config->nBaseline = D->nAntenna*(D->nAntenna-1)/2;

	//if guardNS was not set explicitly, change it to the right amount to allow for
	//adjustment to get to an integer NS + geometric rate slippage (assumes Earth-based antenna)
	if(!corrSetup->explicitGuardNS)
	{
		config->guardNS = calculateWorstcaseGuardNS(mode->sampRate, config->subintNS);
	}
	//config->overSamp = static_cast<int>(mode->sampRate/(2.0*mode->subbands[0].bandwidth) + 0.001);
	//if(config->overSamp <= 0)
	//{
	//	cerr << "Error: configName=" << configName << " overSamp=" << config->overSamp << endl;
	//	cerr << "samprate=" << mode->sampRate << " bw=" << 
	//		mode->subbands[0].bandwidth << endl;
	//	exit(0);
	//}
	// try to get a good balance of oversampling and decim
	//while(config->overSamp % 4 == 0)
	//{
	//	config->overSamp /= 2;
	//	config->decimation *= 2;
	//}
#if 0
	config->overSamp = static_cast<int>(mode->sampRate/(2.0*mode->subbands[0].bandwidth) + 0.001);
	cout << "OS=" << config->overSamp << endl;
	if(config->overSamp <= 0)
	{
		cerr << "Error: configName=" << configName << " overSamp=" << config->overSamp << endl;
		cerr << "samprate=" << mode->sampRate << " bw=" << mode->subbands[0].bandwidth << endl;

		exit(0);
	}

	if(config->overSamp > 2)
	{
		config->decimation = config->oversamp/2;
	}
	cout << "Deci=" << config->decimation << endl;
#endif
	// try to get a good balance of oversampling and decim
	//while(config->overSamp % 4 == 0)
	//{
	//	config->overSamp /= 2;
	//	config->decimation *= 2;
	//}
	
	DifxConfigAllocDatastreamIds(config, config->nDatastream, c*config->nDatastream);
	DifxConfigAllocBaselineIds(config, config->nBaseline, c*config->nBaseline);

	config->nPol = mode->getPols(config->pol);
	config->quantBits = mode->getBits();


	return c;
}

static bool matchingFreq(const ZoomFreq &zoomfreq, const DifxDatastream *dd, int dfreqindex, const vector<freq> freqs)
{
	double channeloffset;
	const freq &f = freqs[dd->recFreqId[dfreqindex]];

	if(f.sideBand == 'L')
	{
		if(zoomfreq.frequency > f.fq)
		{
			return false;
		}
		if(zoomfreq.frequency - zoomfreq.bandwidth < f.fq - f.bw)
		{
			return false;
		}
		if(zoomfreq.spectralaverage > 0 && zoomfreq.spectralaverage != f.specAvg()) //0 means default to parent
		{
			return false;
		}
		channeloffset = ((f.fq - zoomfreq.frequency)/f.bw)*f.nInputChan;
		if(fabs(channeloffset - static_cast<int>(channeloffset+0.5)) > 0.000001)
		{
			return false;
		}

		return true;
	}
	else
	{
		if(zoomfreq.frequency < f.fq)
		{
			return false;
		}
		if(zoomfreq.frequency + zoomfreq.bandwidth > f.fq + f.bw)
		{
			return false;
		}
		if(zoomfreq.spectralaverage > 0 && zoomfreq.spectralaverage != f.specAvg()) //0 means default to parent
		{
			return false;
		}
		channeloffset = ((zoomfreq.frequency - f.fq)/f.bw)*f.nInputChan;
		if(fabs(channeloffset - static_cast<int>(channeloffset+0.5)) > 0.000001)
		{
			return false;
		}

		return true;
	}

	return false;
}

int writeJob(const VexJob& J, const VexData *V, const CorrParams *P, int os, int verbose, ofstream *of, int nDigit, char ext, int strict)
{
	DifxInput *D;
	DifxScan *scan;
	string corrSetupName;
	const CorrSetup *corrSetup;
	const SourceSetup *sourceSetup;
	const PhaseCentre *phaseCentre;
	const PhaseCentre * pointingCentre;
	const AntennaSetup *antennaSetup;
	const VexMode *mode;
	const VexSetup* setup;
	const VexScan *S;
	set<string> configSet;
	set<string> spacecraftSet;
	vector<pair<string,string> > configs;
	vector<string> antList;
	vector<freq> freqs;
	vector<vector<int> > toneSets;
	int nPulsar=0;
	int nTotalPhaseCentres, nbin, maxPulsarBins, maxScanPhaseCentres, fftDurNS;
	double srcra, srcdec;
	int pointingSrcIndex, foundSrcIndex, atSource;
	int nZoomBands, fqId, zoomsrc, polcount, zoomChans, minChans;
	int overSamp, decimation, worstcaseguardns;
	DifxDatastream *dd;
	vector<set <int> > blockedfreqids;

	// Initialize toneSets with the trivial case, which is used for all zoom bands
	vector<int> noTones;
	toneSets.push_back(noTones);

	// Assume same correlator setup for all scans
	if(J.scans.size() == 0)
	{
		cerr << "Developer error: writeJob(): J.scans.size() = 0" << endl;

		exit(0);
	}
	
	S = V->getScanByDefName(J.scans.front());
	if(!S)
	{
		cerr << "Developer error: writeJob() top: scan[" << J.scans.front() << "] = 0" << endl;

		exit(0);
	}
	corrSetupName = S->corrSetupName;
	corrSetup = P->getCorrSetup(corrSetupName);
	if(!corrSetup)
	{
		cerr << "Error: writeJob(): correlator setup " << corrSetupName << ": Not found!" << endl;

		exit(0);
	}

	// make set of unique config names
	for(vector<string>::const_iterator si = J.scans.begin(); si != J.scans.end(); si++)
	{
		string configName;

		S = V->getScanByDefName(*si);
		if(!S)
		{
			cerr << "Developer error: writeJob() loop: scan[" << *si << "] = 0" << endl;

			exit(0);
		}
		configName = S->modeDefName + string("_") + S->corrSetupName;
		configSet.insert(configName);
	}

	D = newDifxInput();

	D->mjdStart = J.mjdStart;
	D->mjdStop  = J.mjdStop;
	D->visBufferLength = P->visBufferLength;
	D->dataBufferFactor = P->dataBufferFactor;
	D->outputFormat = P->outputFormat;
	D->nDataSegments = P->nDataSegments;

	D->antenna = makeDifxAntennas(J, V, P, &(D->nAntenna), antList);
	D->job = makeDifxJob(V->getDirectory(), J, D->nAntenna, V->getExper()->name, &(D->nJob), nDigit, ext, P->vexFile, P->threadsFile);
	
	D->nScan = J.scans.size();
	D->scan = newDifxScanArray(D->nScan);
	D->nConfig = configSet.size();
	D->config = newDifxConfigArray(D->nConfig);

	blockedfreqids.resize(D->nAntenna);

	// Allocate space for the source table - first work out how many sources we'll need
	maxPulsarBins = 0;
	maxScanPhaseCentres = 0;
	nTotalPhaseCentres = 0;
	for(vector<SourceSetup>::const_iterator ss=P->sourceSetups.begin(); ss != P->sourceSetups.end(); ss++)
	{
		nTotalPhaseCentres += ss->phaseCentres.size()+1;
		pointingCentre = &(ss->pointingCentre);
		if((pointingCentre->difxName.compare(PhaseCentre::DEFAULT_NAME) != 0) ||
		   (pointingCentre->ra > PhaseCentre::DEFAULT_RA) ||
		   (pointingCentre->dec > PhaseCentre::DEFAULT_DEC))
		{
			nTotalPhaseCentres++;
		}
	}
	allocateSourceTable(D, nTotalPhaseCentres);

	// Make rule table
	populateRuleTable(D, P);

	// now run through all scans, populating things as we go
	scan = D->scan;
	for(vector<string>::const_iterator si = J.scans.begin(); si != J.scans.end(); si++, scan++)
	{
		S = V->getScanByDefName(*si);
		if(!S)
		{
			cerr << "Developer error: source[" << *si << "] not found!  This cannot be!" << endl;
			
			exit(0);
		}

		const VexSource *src = V->getSourceByDefName(S->sourceDefName);

		// Determine interval where scan and job overlap
		VexInterval scanInterval(*S);
		scanInterval.logicalAnd(J);

		corrSetup = P->getCorrSetup(S->corrSetupName);
		sourceSetup = P->getSourceSetup(src->sourceNames);
		if(!sourceSetup)
		{
			cerr << "Error: no source setup for " << S->sourceDefName << ".  Aborting!" << endl;

			exit(0);
		}
		pointingCentre = &(sourceSetup->pointingCentre);
		scan->nPhaseCentres = sourceSetup->phaseCentres.size();
		if(sourceSetup->doPointingCentre)
		{
			scan->nPhaseCentres++;
		}
		if(scan->nPhaseCentres > maxScanPhaseCentres)
		{
			maxScanPhaseCentres = scan->nPhaseCentres;
		}
		atSource = 0;
		pointingSrcIndex = -1;
		srcra = src->ra;
		srcdec = src->dec;
		if(pointingCentre->ra > PhaseCentre::DEFAULT_RA)
		{
			srcra = pointingCentre->ra;
		}
		if(pointingCentre->dec > PhaseCentre::DEFAULT_DEC)
		{
			srcdec = pointingCentre->dec;
		}
		for(int i = 0; i < D->nSource; i++)
		{
			if(D->source[i].ra == srcra && D->source[i].dec == srcdec &&
			   D->source[i].calCode[0] == src->calCode &&
			   D->source[i].qual == src->qualifier)
			 {
			 	if(pointingCentre->difxName.compare(PhaseCentre::DEFAULT_NAME) != 0)
				{
					if(strcmp(D->source[i].name, pointingCentre->difxName.c_str()) == 0)
					{
						pointingSrcIndex = i;
						break;
					}
				}
				else
				{
					if(strcmp(D->source[i].name, src->defName.c_str()) == 0)
					{
						pointingSrcIndex = i;
						break;
					}
				}
			}
#warning "FIXME: There might be something fishy in the source name comparison above."
			// What happens if the source is renamed?  A better infrastructure for this is needed.
			// Also, code now compares against the def name in case that is the name basis
		}
		if(pointingSrcIndex == -1)
		{
			pointingSrcIndex = D->nSource;
			// below we take the first source name index by default
			snprintf(D->source[pointingSrcIndex].name, DIFXIO_NAME_LENGTH, "%s", src->sourceNames[0].c_str());
			D->source[pointingSrcIndex].ra = src->ra;
			D->source[pointingSrcIndex].dec = src->dec;
			D->source[pointingSrcIndex].calCode[0] = src->calCode;
			D->source[pointingSrcIndex].qual = src->qualifier;
			//overwrite with stuff from the source setup if it exists
			if(pointingCentre->difxName.compare(PhaseCentre::DEFAULT_NAME) != 0)
			{
				snprintf(D->source[pointingSrcIndex].name, DIFXIO_NAME_LENGTH, "%s", pointingCentre->difxName.c_str());
			}
			if(pointingCentre->ra > PhaseCentre::DEFAULT_RA)
			{
				D->source[pointingSrcIndex].ra = pointingCentre->ra;
			}
			if(pointingCentre->dec > PhaseCentre::DEFAULT_DEC)
			{
				D->source[pointingSrcIndex].dec = pointingCentre->dec;
			}
			D->nSource++;
		}
		scan->pointingCentreSrc = pointingSrcIndex;
		if(sourceSetup->doPointingCentre)
		{
			scan->phsCentreSrcs[atSource++] = pointingSrcIndex;
		}
		for(vector<PhaseCentre>::const_iterator p=sourceSetup->phaseCentres.begin(); p != sourceSetup->phaseCentres.end(); p++)
		{
			foundSrcIndex = -1;
			for(int i = 0; i < D->nSource; i++)
			{
				if(D->source[i].ra == p->ra && D->source[i].dec == p->dec &&
					D->source[i].calCode[0] == p->calCode &&
					D->source[i].qual == p->qualifier     &&
					strcmp(D->source[i].name, p->difxName.c_str()) == 0)
				{
					foundSrcIndex = i;
					break;
				}
			}
			if(foundSrcIndex == -1)
			{
				foundSrcIndex = D->nSource;
				snprintf(D->source[foundSrcIndex].name, DIFXIO_NAME_LENGTH, "%s", p->difxName.c_str());
				D->source[foundSrcIndex].ra = p->ra;
				D->source[foundSrcIndex].dec = p->dec;
				D->source[foundSrcIndex].calCode[0] = p->calCode;
				D->source[foundSrcIndex].qual = p->qualifier;
				D->nSource++;
			}
			scan->phsCentreSrcs[atSource++] = foundSrcIndex; 
		}

		scan->mjdStart = scanInterval.mjdStart;
		scan->mjdEnd = scanInterval.mjdStop;
		scan->startSeconds = static_cast<int>((scanInterval.mjdStart - J.mjdStart)*86400.0 + 0.01);
		scan->durSeconds = static_cast<int>(scanInterval.duration_seconds() + 0.01);
		if(scan->durSeconds == 0)
		{
			scan->durSeconds = 1;
		}
		scan->configId = getConfigIndex(configs, D, V, P, S);
		scan->maxNSBetweenUVShifts = corrSetup->maxNSBetweenUVShifts;
		mode = V->getModeByDefName(configs[scan->configId].first);
		fftDurNS = static_cast<int>(corrSetup->fftSize()*1000000000.0/mode->sampRate + 0.5);
		if(corrSetup->maxNSBetweenACAvg > 0)
		{
			scan->maxNSBetweenACAvg = corrSetup->maxNSBetweenACAvg;
		}
		else
		{
			scan->maxNSBetweenACAvg = defaultMaxNSBetweenACAvg;
		}
		if(corrSetup->numBufferedFFTs*fftDurNS > scan->maxNSBetweenACAvg)
		{
			if(corrSetup->maxNSBetweenACAvg != 0)	// Only print warning if explicitly overriding user value
			{
				cout << "Adjusting maxNSBetweenACAvg since the number of buffered FFTs (";
				cout << corrSetup->numBufferedFFTs << ") gives a duration of ";
				cout << corrSetup->numBufferedFFTs*fftDurNS << ", longer that that specified (";
				cout << corrSetup->maxNSBetweenACAvg << ")" << endl;
			}
			scan->maxNSBetweenACAvg = corrSetup->numBufferedFFTs*fftDurNS;
		}
		if(corrSetup->numBufferedFFTs*fftDurNS > corrSetup->maxNSBetweenUVShifts)
		{
			cout << "The number of buffered FFTs (" << corrSetup->numBufferedFFTs;
			cout << ") gives a duration of " << corrSetup->numBufferedFFTs*fftDurNS;
			cout << ", longer that that specified for the UV shift interval (";
			cout << corrSetup->maxNSBetweenUVShifts;
			cout << "). Reduce FFT buffering or increase allowed interval!" << endl;

			exit(0);
		}

		snprintf(scan->identifier, DIFXIO_NAME_LENGTH, "%s", S->defName.c_str());
		snprintf(scan->obsModeName, DIFXIO_NAME_LENGTH, "%s", S->modeDefName.c_str());

		if(sourceSetup->pointingCentre.ephemFile.size() > 0)
		{
			spacecraftSet.insert(sourceSetup->pointingCentre.difxName);
		}
		for(vector<PhaseCentre>::const_iterator p=sourceSetup->phaseCentres.begin(); p != sourceSetup->phaseCentres.end(); p++)
		{
			if(p->ephemFile.size() > 0)
			{
				spacecraftSet.insert(p->difxName);
			}
		}
	}

	for(int c = 0; c < D->nConfig; c++)
	{
		corrSetup = P->getCorrSetup(configs[c].second);
		if(corrSetup->binConfigFile.size() > 0)
		{
			nPulsar++;
		}
	}
	if(nPulsar > 0)
	{
		D->pulsar = newDifxPulsarArray(nPulsar);
	}

	// configure datastreams
	D->datastream = makeDifxDatastreams(J, V, P, D->nConfig);
	D->nDatastream = 0;
	for(int c = 0; c < D->nConfig; c++)
	{
		mode = V->getModeByDefName(configs[c].first);
		if(mode == 0)
		{
			cerr << "Developer error: writeJob: mode[" << configs[c].first << "] is null" << endl;

			exit(0);
		}

		if(os < 0)
		{
			overSamp = mode->getOversampleFactor();
		}
		else
		{
			overSamp = os;
		}
		decimation = calcDecimation(overSamp);

		corrSetup = P->getCorrSetup(configs[c].second);
		if(corrSetup == 0)
		{
			cerr << "Developer error: writeJob: correlator setup[" << configs[c].second << "] is null" << endl;

			exit(0);
		}

		if(corrSetup->binConfigFile.size() > 0)
		{
			D->config[c].pulsarId = D->nPulsar;
			loadPulsarConfigFile(D, corrSetup->binConfigFile.c_str());
			nbin = D->pulsar[D->nPulsar-1].nBin;
			if(D->pulsar[D->nPulsar-1].scrunch > 0)
			{
				nbin = 1;
			}
			if(nbin > maxPulsarBins)
			{
				maxPulsarBins = nbin;
			}
		}

		if(corrSetup->phasedArrayConfigFile.size() > 0)
		{
			D->config[c].phasedArrayId = D->nPhasedArray;
			snprintf(D->phasedarray[D->nPhasedArray].fileName, DIFXIO_FILENAME_LENGTH, "%s",
				corrSetup->phasedArrayConfigFile.c_str());
			D->nPhasedArray++;
		}

		int d = 0;

		minChans = corrSetup->nInputChan();
		for(int a = 0; a < D->nAntenna; a++)
		{
			string antName = antList[a];
			int v = setFormat(D, D->nDatastream, freqs, toneSets, mode, antName, corrSetup, P->v2dMode);
			if(v)
			{
				setup = mode->getSetup(antName);
				antennaSetup = P->getAntennaSetup(antName);
				dd = D->datastream + D->nDatastream;
				dd->phaseCalIntervalMHz = setup->phaseCalIntervalMHz();

				if(antennaSetup)
				{
					if(antennaSetup->tcalFrequency >= 0)
					{
						// use .v2d value
						dd->tcalFrequency = antennaSetup->tcalFrequency;
					}
					if(antennaSetup->phaseCalIntervalMHz >= 0)
					{
						// Override with the .v2d value
						dd->phaseCalIntervalMHz = antennaSetup->phaseCalIntervalMHz;
					}
					nZoomBands = 0;
					
					int nZoomFreqs = antennaSetup->zoomFreqs.size();
					if(nZoomFreqs > 0)
					{
						int *parentFreqIndices = new int[nZoomFreqs];

						DifxDatastreamAllocZoomFreqs(dd, nZoomFreqs);
						
						for(int i = 0; i < nZoomFreqs; i++)
						{
							const ZoomFreq &zf = antennaSetup->zoomFreqs[i];

							parentFreqIndices[i] = -1;
							for(int j = 0; j < dd->nRecFreq; j++)
							{
								if(matchingFreq(zf, dd, j, freqs))
								{
									parentFreqIndices[i] = j;
								}
							}
							if(parentFreqIndices[i] < 0)
							{
								cerr << "Error: Cannot find a parent freq for zoom band " << i << " of datastream " << a << endl;
							
								exit(0);
							}
							zoomChans = static_cast<int>(corrSetup->nInputChan()*zf.bandwidth/freqs[dd->recFreqId[parentFreqIndices[i]]].bw);
							fqId = getFreqId(freqs, zf.frequency, zf.bandwidth,
									freqs[dd->recFreqId[parentFreqIndices[i]]].sideBand,
									zoomChans, zoomChans*corrSetup->nOutputChan/corrSetup->nFFTChan, overSamp, decimation, 1, 0);	// final zero points to the noTone pulse cal setup.
							if(zoomChans < minChans)
							{
								minChans = zoomChans;
							}
							dd->zoomFreqId[i] = fqId;
							dd->nZoomPol[i] = dd->nRecPol[parentFreqIndices[i]];
							nZoomBands += dd->nRecPol[parentFreqIndices[i]];
							if(!zf.correlateparent)
							{
								blockedfreqids[a].insert(dd->recFreqId[parentFreqIndices[i]]);
							}
						}
						DifxDatastreamAllocZoomBands(dd, nZoomBands);
						
						nZoomBands = 0;
						for(int i = 0; i < nZoomFreqs; i++)
						{
							zoomsrc = 0;
							polcount = 0;
							for(int j = 0; j < dd->nZoomPol[i]; j++)
							{
								dd->zoomBandFreqId[nZoomBands] = i;
								for(int k = zoomsrc; k < dd->nRecBand; k++)
								{
									if(dd->recBandFreqId[k] == parentFreqIndices[i])
									{
										dd->zoomBandPolName[nZoomBands] = dd->recBandPolName[k];
										zoomsrc = k + 1;
										polcount++;

										break;
									}
								}
								nZoomBands++;
							}
							if(polcount != dd->nZoomPol[i])
							{
								cout << "Developer error: didn't find all zoom pols (was looking for " << dd->nZoomPol[i] << ", only found " << polcount << ")!!" << endl;
								
								exit(0);
							}
						}
						delete [] parentFreqIndices;
					}

					int nFreqClockOffsets = antennaSetup->freqClockOffs.size();
					if(nFreqClockOffsets > 0)
					{
						if(D->datastream[D->nDatastream].nRecFreq != nFreqClockOffsets)
						{
							cerr << "Error: AntennaSetup for " << antName << " has only " << nFreqClockOffsets << 
								" freqClockOffsets specified but " << dd->nRecFreq << " recorded frequencies" << endl;

							exit(0);
						}
						if(antennaSetup->freqClockOffs.front() != 0.0)
						{
							cerr << "Error: AntennaSetup for " << antName << " has a non-zero clock offset for the first" << 
								" frequency offset. This is not allowed for model " << "accountability reasons." << endl;
							
							exit(0);
						}
						for(int i = 0; i < nFreqClockOffsets; i++)
						{
							D->datastream[D->nDatastream].clockOffset[i] = 
								antennaSetup->freqClockOffs.at(i);
						}
					}

					int nLoOffsets = antennaSetup->loOffsets.size();
					if(nLoOffsets > 0)
					{
						if(D->datastream[D->nDatastream].nRecFreq != nLoOffsets)
						{
							cerr << "Error: AntennaSetup for " << antName << " has only " << nLoOffsets <<
								" loOffsets specified but " << dd->nRecFreq << " recorded frequencies" << endl;

							exit(0);
						}
						for(int i = 0; i < nLoOffsets; i++)
						{
							D->datastream[D->nDatastream].freqOffset[i] = 
								antennaSetup->loOffsets.at(i);
						}
					}
				}
				D->config[c].datastreamId[d] = D->nDatastream;
				D->nDatastream++;
				d++;
			}
		}
		if(corrSetup->xmacLength > minChans)
		{
			if(corrSetup->explicitXmacLength)
			{
				cerr << "Error: xmacLength set explicitly to " << corrSetup->xmacLength << ", but minChans (from a zoom freq) was " << minChans << endl;
				exit(1);
			}
			else
			{
				D->config[c].xmacLength = minChans;
			}
		}
		worstcaseguardns = calculateWorstcaseGuardNS(mode->sampRate, D->config[c].subintNS);
		if(corrSetup->guardNS < worstcaseguardns)
		{
			cerr << "vex2difx calculates the worst-case guardNS as " << worstcaseguardns << ", but you have explicitly set " << corrSetup->guardNS << ". It is possible that mpifxcorr will refuse to run! Unless you know what you are doing, you should probably set guardNS to " << worstcaseguardns << " or above, or just leave it unset!" << endl;
			if(strict)
			{
				cerr << "\nExiting since strict mode was enabled" << endl;
				exit(1);
			}
			else
			{
				cerr << "\nContinuing since --force was specified" << endl;
			}
		}
	}

	if(nPulsar != D->nPulsar)
	{
		cerr << "Error: nPulsar=" << nPulsar << " != D->nPulsar=" << D->nPulsar << endl;
		
		exit(0);
	}

	// Populate spacecraft table
	if(!spacecraftSet.empty())
	{
		DifxSpacecraft *ds;
		double fracday0, deltat;
		int mjdint, n0, nPoint, v;
		double mjd0;

		D->spacecraft = newDifxSpacecraftArray(spacecraftSet.size());
		D->nSpacecraft = spacecraftSet.size();
		
		ds = D->spacecraft;

		for(set<string>::const_iterator s = spacecraftSet.begin(); s != spacecraftSet.end(); s++, ds++)
		{
			phaseCentre = P->getPhaseCentre(*s);
			if(!phaseCentre)
			{
				cerr << "Developer error: couldn't find " << *s << " in the spacecraft table, aborting!)" << endl;
				
				exit(0);
			}
			mjdint = static_cast<int>(J.mjdStart);
			fracday0 = J.mjdStart-mjdint;
			deltat = phaseCentre->ephemDeltaT/86400.0;	// convert from seconds to days
			n0 = static_cast<int>(fracday0/deltat - 2);	// start ephmemeris at least 2 points early
			mjd0 = mjdint + n0*deltat;			// always start an integer number of increments into day
			nPoint = static_cast<int>(J.duration()/deltat) + 6; // make sure to extend beyond the end of the job
			if(verbose > 0)
			{
				cout << "Computing ephemeris:" << endl;
				cout << "  source name = " << phaseCentre->difxName << endl;
				cout << "  ephem object name = " << phaseCentre->ephemObject << endl;
				cout << "  mjd = " << mjdint << "  deltat = " << deltat << endl;
				cout << "  startPoint = " << n0 << "  nPoint = " << nPoint << endl;
				cout << "  ephemFile = " << phaseCentre->ephemFile << endl;
				cout << "  naifFile = " << phaseCentre->naifFile << endl;
			}
			v = computeDifxSpacecraftEphemeris(ds, mjd0, deltat, nPoint, 
			phaseCentre->ephemObject.c_str(),
			phaseCentre->naifFile.c_str(),
			phaseCentre->ephemFile.c_str());
			if(v != 0)
			{
				cerr << "Error: ephemeris calculation failed.  Must stop." << endl;
				
				exit(0);
			}

			// give the spacecraft table the right name so it can be linked to the source
			snprintf(ds->name, DIFXIO_NAME_LENGTH, "%s", phaseCentre->difxName.c_str());
		}

		//Fill in the spacecraft IDs in the DifxInput object
		for(int s = 0; s < D->nSource; s++)
		{
			for(int sc = 0; sc < D->nSpacecraft; sc++)
			{
				if(strcmp(D->spacecraft[sc].name, D->source[s].name) == 0)
				{
					D->source[s].spacecraftId = sc;
					
					break;
				}
			}
			if(D->source[s].spacecraftId < 0)
			{
				cerr << "Developer error: For source=" << D->source[s].name << " spacecraftId=" << D->source[s].spacecraftId << endl; 
			}
		}
	}

	// Make frequency table
	populateFreqTable(D, freqs, toneSets);

	// Make baseline table
	populateBaselineTable(D, P, corrSetup, blockedfreqids);

	// Make EOP table
	populateEOPTable(D, V->getEOPs());

	// Merge identical table entries
	simplifyDifxFreqs(D);
	simplifyDifxDatastreams(D);
	simplifyDifxBaselines(D);
	simplifyDifxConfigs(D);

	//delete any unused rules
	simplifyDifxRules(D);

	//delete the "blocked freq" array
	blockedfreqids.clear();

	if(P->simFXCORR)
	{
		// nudge integration times and start times to match those of the VLBA HW correlator
		DifxInputSimFXCORR(D);
	}

	//All averaging will always be in correlator by default, not difx2fits
	D->specAvg  = 1;

	if(D->nBaseline > 0 || P->minSubarraySize == 1)
	{
		// write input file
		writeDifxInput(D);

		// write calc file
		writeDifxCalc(D);

		// write threads file if requested
		if(P->nCore > 0 && P->nThread > 0)
		{
			DifxInputAllocThreads(D, P->nCore);
			DifxInputSetThreads(D, P->nThread);

#warning "FIXME: ultimately move this to writeDifxInput()"
			DifxInputWriteThreads(D);
		}

		// write flag file
		J.generateFlagFile(*V, D->job->flagFile, P->invalidMask);

		if(verbose > 2)
		{
			printDifxInput(D);
		}

		if(of)
		{
			char fileBase[DIFXIO_FILENAME_LENGTH];
			double tops;    // Trillion operations
			int p;

			generateDifxJobFileBase(D->job, fileBase);

			tops = J.calcOps(V, corrSetup->fftSize(), corrSetup->doPolar) * 1.0e-12;

			*of << fileBase << " " << J.mjdStart << " " << J.mjdStop << " " << D->nAntenna << " ";
			*of << maxPulsarBins << " " << maxScanPhaseCentres << " ";
			p = of->precision();
			of->precision(4);
			*of << tops << " ";
			*of << (J.dataSize/1000000) << "  #";
			of->precision(p);


			for(vector<string>::const_iterator ai = antList.begin(); ai != antList.end(); ai++)
			{
				*of << " " << *ai;
			}
			*of << endl;
		}
	}
	else
	{
		char fileBase[DIFXIO_FILENAME_LENGTH];

		generateDifxJobFileBase(D->job, fileBase);

		cerr << "Warning: job " << fileBase << " not written since it correlates no data" << endl;
		cerr << "This is often due to media not being specified or all frequency Ids being unselected." << endl;
		cerr << "It is also possible that the vex file is faulty and missing e.g. a $IF section, leading " << endl;
		cerr << "to missing polarisation information." << endl;

		cerr << "nBaseline=" << D->nBaseline << "  minSubarraySize=" << P->minSubarraySize << endl;
	}

	if(D->nBaseline > 0 || P->minSubarraySize == 1)
	{
		// clean up and return that job was created
		deleteDifxInput(D);

		return 1;
	}
	else
	{
		// clean up and return that job was not created
		deleteDifxInput(D);

		return 0;
	}
}

static bool illegalSourceName(const string &name)
{
	if(name.size() > VexSource::MAX_SRCNAME_LENGTH)
	{
		return true;
	}
	else if(name.find_first_of("/") != string::npos)
	{
		return true;
	}
	else
	{
		return false;
	}
}

static int sanityCheckConsistency(const VexData *V, const CorrParams *P)
{
	vector<SourceSetup>::const_iterator s;
	vector<AntennaSetup>::const_iterator a;
	vector<CorrRule>::const_iterator r;
	list<string>::const_iterator l;

	int nWarn = 0;

	for(s = P->sourceSetups.begin(); s != P->sourceSetups.end(); s++)
	{
		if(V->getSourceBySourceName(s->vexName) == 0)
		{
			cerr << "Warning: source " << s->vexName << " referenced in .v2d file but is not in vex file" << endl;
			nWarn++;
		}
	}

	for(a = P->antennaSetups.begin(); a != P->antennaSetups.end(); a++)
	{
		if(a->vexName == "DEFAULT")
		{
			cout << "FYI: Using a default antenna setup." << endl;
		}
	}

	for(r = P->rules.begin(); r != P->rules.end(); r++)
	{
		for(l = r->scanName.begin(); l != r->scanName.end(); l++)
		{
			if(V->getScanByDefName(*l) == 0)
			{
				cerr << "Warning: scan " << *l << " referenced in RULE " << r->ruleName << " in .v2d file but is not in vex file" << endl;
				nWarn++;
			}
		}
		for(l = r->sourceName.begin(); l != r->sourceName.end(); l++)
		{
			if(V->getSourceBySourceName(*l) == 0)
			{
				cerr << "Warning: source " << *l << " referenced in RULE " << r->ruleName << " in .v2d file but is not in vex file" << endl;
				nWarn++;
			}
		}
		for(l = r->modeName.begin(); l != r->modeName.end(); l++)
		{
			if(V->getModeByDefName(*l) == 0)
			{
				cerr << "Warning: mode " << *l << " referenced in RULE " << r->ruleName << " in .v2d file but is not in vex file" << endl;
				nWarn++;
			}
		}
	}

	// Verify that final source names are legal
	for(unsigned int s = 0; s < V->nSource(); s++)
	{
		const VexSource *S = V->getSource(s);

		if(S->sourceNames.size() == 0)
		{
			cout << "Warning: vex source def block " << S->defName << " has no source names!" << endl;
			nWarn++;
			continue;
		}
		else if(S->sourceNames.size() > 1)
		{
			cout << "Warning: vex source def block " << S->defName << " has more than 1 source names.  Only the first is being considered!" << endl;
			nWarn++;
		}

		const SourceSetup *sourceSetup=P->getSourceSetup(S->sourceNames);
		if(sourceSetup && sourceSetup->pointingCentre.difxName.size() > 0)
		{
			if(illegalSourceName(sourceSetup->pointingCentre.difxName))
			{
				cerr << "Warning: illegal source name (" << sourceSetup->pointingCentre.difxName << ") provided in SOURCE section for source " << S->defName << endl;
				nWarn++;
			}
		}
		else
		{
			if(illegalSourceName(S->sourceNames[0]))
			{
				cerr << "Warning: illegal source name (" << S->sourceNames[0] << ") in vex file.  Please correct by renaming in the SOURCE section of the .v2d file" << endl;
				nWarn++;
			}
		}
#warning "FIXME: check phase center names for legality"
	}

	// warn on two VexSources with the same sourceNames[] entries
	if(V->nSource() > 1)
	{
		for(unsigned int s2 = 1; s2 < V->nSource(); s2++) 
		{
			const VexSource *S2 = V->getSource(s2);
			for(unsigned int s1 = 0; s1 < s2; s1++)
			{
				const VexSource *S1 = V->getSource(s1);
				for(unsigned int n2 = 0; n2 < S2->sourceNames.size(); n2++)
				{
					for(unsigned int n1 = 0; n1 < S1->sourceNames.size(); n1++)
					{
						if(S1->sourceNames[n1] == S2->sourceNames[n2])
						{
							cerr << "Warning: two sources with the same name:" << endl;
							cerr << "  " << S1->defName << "[" << n1 << "] = " << S1->sourceNames[n1] << endl;
							cerr << "  " << S2->defName << "[" << n2 << "] = " << S2->sourceNames[n2] << endl;
							nWarn++;
						}
					}
				}
			}
		}
	}

	return nWarn;
}

int usage(int argc, char **argv)
{
	cout << endl;
	cout << program << " version " << version << "  " << author << " " << verdate << endl;
	cout << endl;
	cout << "Usage:  " << argv[0] << " [<options>] <v2d file>" << endl;
	cout << endl;
	cout << "  options can include:" << endl;
	cout << "     -h" << endl;
	cout << "     --help        display this information and quit." << endl;
	cout << endl;
	cout << "     -v" << endl;
	cout << "     --verbose     increase the verbosity of the output; -v -v for more." << endl;
	cout << endl;
	cout << "     -o" << endl;
	cout << "     --output      create a v2d file with all defaults populated." << endl;
	cout << endl;
	cout << "     -d" << endl;
	cout << "     --delete-old  delete all jobs in this series before running." << endl;
	cout << endl;
	cout << "     -f" << endl;
	cout << "     --force       continue desipte warnings." << endl;
	cout << endl;
	cout << "     -s" << endl;
	cout << "     --strict      treat some warnings as errors and quit [default]." << endl;
	cout << endl;
	cout << "  the v2d file is the vex2difx configuration file to process." << endl;
	cout << endl;
	cout << "See http://cira.ivec.org/dokuwiki/doku.php/difx/vex2difx for more information" << endl;
	cout << endl;

	return 0;
}

void runCommand(const char *cmd, int verbose)
{
	if(verbose > 0)
	{
		cout << "Executing: " << cmd << endl;
	}
	int s = system(cmd);
	if(s == -1)
	{
		cerr << "Error executing: " << cmd << endl;

		exit(0);
	}
}

int main(int argc, char **argv)
{
	VexData *V;
	const VexScan * S;
	CorrParams *P;
	const SourceSetup * sourceSetup;
	vector<VexJob> J;
	ifstream is;
	string shelfFile;
	int verbose = 0;
	string v2dFile;
	bool writeParams = 0;
	bool deleteOld = 0;
	bool strict = 1;
	int nWarn = 0;
	int nDigit;
	int nJob = 0;
	int nMulti = 0;

	if(argc < 2)
	{
		return usage(argc, argv);
	}

	// force program to work in Univeral Time
	setenv("TZ", "", 1);
	tzset();


	for(int a = 1; a < argc; a++)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				return usage(argc, argv);
			}
			else if(strcmp(argv[a], "-v") == 0 ||
			        strcmp(argv[a], "--verbose") == 0)
			{
				verbose++;
			}
			else if(strcmp(argv[a], "-o") == 0 ||
			        strcmp(argv[a], "--output") == 0)
			{
				writeParams = 1;
			}
			else if(strcmp(argv[a], "-d") == 0 ||
				strcmp(argv[a], "--delete-old") == 0)
			{
				deleteOld = 1;
			}
			else if(strcmp(argv[a], "-f") == 0 ||
				strcmp(argv[a], "--force") == 0)
			{
				strict = 0;
			}
			else if(strcmp(argv[a], "-s") == 0 ||
				strcmp(argv[a], "--strict") == 0)
			{
				strict = 1;
			}
			else
			{
				cerr << "Error: unknown option " << argv[a] << endl;
				cerr << "Run with -h for help information." << endl;
				
				exit(0);
			}
		}
		else
		{
			if(v2dFile.size() > 0)
			{
				cerr << "Error: multiple configuration files provided, only one expected." << endl;
				cerr << "Run with -h for help information." << endl;
				
				exit(0);
			}
			v2dFile = argv[a];
		}
	}

	if(v2dFile.size() > DIFX_MESSAGE_PARAM_LENGTH-2)
	{
		//job numbers into the tens of thousands will be truncated in difxmessage.  Better warn the user.
		cout << "Filename " << v2dFile << " is too long - its job name might be truncated by difxmessage!" << endl;
		cout << "You are strongly suggested to choose a shorter .v2d name (root shorter than 26 characters)" << endl;
	}

	if(v2dFile.size() == 0)
	{
		cerr << "Error: configuration (.v2d) file expected." << endl;
		cerr << "Run with -h for help information." << endl;
		
		exit(0);
	}

	if(v2dFile.find("_") != string::npos)
	{
		cerr << "Error: you cannot have an underscore (_) in the filename!" << endl;
		cerr << "Please rename it and run again." << endl;
		
		exit(0);
	}

	if(!isalpha(v2dFile[0]))
	{
		cerr << "Error: pass name (.v2d file name) must start with a letter!" << endl;
		cerr << "Please rename it and run again." << endl;
		
		exit(0);
	}

	P = new CorrParams(v2dFile);
	if(P->vexFile.size() == 0)
	{
		cerr << "Error: vex file parameter (vex) not found in file." << endl;
		
		exit(0);
	}

	nWarn = P->parseWarnings;

	umask(02);

	shelfFile = P->vexFile.substr(0, P->vexFile.find_last_of('.'));
	shelfFile += string(".shelf");
	nWarn += P->loadShelves(shelfFile);

	V = loadVexFile(*P, &nWarn);

	if(!V)
	{
		cerr << "Error: cannot load vex file: " << P->vexFile << endl;
		
		exit(0);
	}

	nWarn += P->sanityCheck();
	nWarn += V->sanityCheck();
	nWarn += sanityCheckConsistency(V, P);
	if(strict && nWarn > 0)
	{
		cerr << "Quitting since " << nWarn <<
			" warnings were found and strict mode was enabled." << endl;
		
		exit(0);
	}
	else if(nWarn > 0)
	{
		cout << "FYI: Proceeding even though there were " << nWarn << " warnings." << endl;
	}

	//run through all the scans once, creating source setups for any sources
	//that don't have one
	for(unsigned int i = 0; i < V->nScan(); i++)
	{
		SourceSetup * added;
		S = V->getScan(i);
		sourceSetup = P->getSourceSetup(S->sourceDefName);
		if(!sourceSetup)
		{
			const VexSource *src = V->getSourceByDefName(S->sourceDefName);
			
			added = new SourceSetup(S->sourceDefName);
			added->doPointingCentre = true;
			added->pointingCentre = PhaseCentre(src->ra, src->dec, src->sourceNames[0]);
			added->pointingCentre.calCode = src->calCode;
			added->pointingCentre.qualifier = src->qualifier;
			P->addSourceSetup(*added);
		}
	}

	makeJobs(J, V, P, verbose);

	if(verbose > 1)
	{
		cout << *V << endl;
		cout << *P << endl;
	}

	if(deleteOld)
	{
		const int CommandSize = 512;
		char cmd[CommandSize];
		int v;

		v = snprintf(cmd, CommandSize, "rm -f %s.params %s_*.{input,calc,flag}", v2dFile.c_str(), P->jobSeries.c_str());
		if(v < CommandSize)
		{
			if(verbose > 1)
			{
				cerr << "About to execute: " << cmd << endl;
			}
			runCommand(cmd, verbose);
		}
		else
		{
			cerr << "Developer warning: deletion of old files failed due to string length: " << v << " >= " << CommandSize << endl;
		}
	}

	if(writeParams)
	{
		ofstream of;
		string paramsFile = v2dFile + ".params";

		of.open(paramsFile.c_str());
		of << *P << endl;
		of.close();
	}

	ofstream of;
	string jobListFile = P->jobSeries + ".joblist";
	string difxVersion;
	const char *dvstr;

	dvstr = getenv("DIFX_VERSION");
	if(dvstr)
	{
		difxVersion = dvstr;
	}
	else
	{
		cout << endl;
		cout << "Warning: env. variable DIFX_VERSION is not set.  Setting to 'Unknown'" << endl;
		cout << "This means that your version accountability is being compromised!" << endl;
		cout << endl;
		difxVersion = "Unknown";
	}
	of.open(jobListFile.c_str());
	of.precision(12);
	of << "exper=" << V->getExper()->name << "  v2d=" << v2dFile <<"  pass=" << P->jobSeries << "  mjd=" << current_mjd() << "  DiFX=" << difxVersion << "  vex2difx=" << version << endl;
	
	nDigit=0;
	for(int l = J.size()+P->startSeries-1; l > 0; l /= 10)
	{
		nDigit++;
	}
	
	for(vector<VexJob>::iterator j = J.begin(); j != J.end(); j++)
	{
		if(verbose > 0)
		{
			cout << *j;
		}
		//const VexScan *S = V->getScanByDefName(j->scans[0]);
		//const VexMode *M = V->getModeByDefName(S->modeDefName);
		//int n = 0;
		//for(list<int>::const_iterator k = M->overSamp.begin(); k != M->overSamp.end(); k++)
		//{
		//	char ext=0;
		//	if(M->overSamp.size() > 1)
		//	{
		//		ext='a'+n;
		//	}
		//	nJob += writeJob(*j, V, P, *k, verbose, &of, nDigit, ext);
		//	n++;
		//}
		//if(M->overSamp.size() > 1)
		//{
		//	nMulti++;
		//}
		nJob += writeJob(*j, V, P, -1, verbose, &of, nDigit, 0, strict);
	}
	of.close();

	cout << endl;
	cout << nJob << " job(s) created." << endl;

	if(nMulti > 0)
	{
		cout << endl;
		cout << "Notice!  " << nMulti << " jobs were replicated multiple times and have a letter suffix" << endl;
		cout << "after the job number.  This is probably due to mixed amounts of oversampling" << endl;
		cout << "at the same time within one or more observing modes. In cases like this the" << endl;
		cout << "PI might want different processing to be done on each IF (such as number of" << endl;
		cout << "spectral lines or integration times).  Consider explicitly making multiple" << endl;
		cout << ".v2d files, one for each oversample factor, that operate only on the" << endl;
		cout << "relavant baseband channels." << endl;
	}

	delete V;
	delete P;

	cout << endl;

	return 0;
}
