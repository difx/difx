/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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
 * $LastChangedRevision:$
 * $Author:$
 * $LastChangedDate:$
 *
 *==========================================================================*/

#include <vector>
#include <set>
#include <sstream>
#include <difxio/difx_input.h>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include "vextables.h"
#include "corrparams.h"
#include "vexload.h"

const string program("vex2difx");
const string version("0.1");
const string verdate("20090109");
const string author("Walter Brisken");

// A is assumed to be the first scan in time order
bool areScansCompatible(const VexScan *A, const VexScan *B, const CorrParams *P)
{
	if((B->timeRange.mjdStart < A->timeRange.mjdStop) ||
	   (B->timeRange.mjdStart > A->timeRange.mjdStop + P->maxGap))
	{
		return false;
	}
	if(P->singleScan)
	{
		return false;
	}
	if(P->singleSetup && A->modeName != B->modeName)
	{
		return false;
	}
	
	return true;
}

void genJobGroups(vector<VexJobGroup> &JGs, const VexData *V, const CorrParams *P, int verbose)
{
	list<string> scans;
	list<string>::iterator it;
	vector<VexJobGroup>::iterator v;
	V->getScanList(scans);

	while(!scans.empty())
	{
		JGs.push_back(VexJobGroup());
		VexJobGroup &JG = JGs.back();
		JG.scans.push_back(scans.front());
		scans.pop_front();

		const VexScan *scan1 = V->getScan(JG.scans.back());
		const CorrSetup *setup1 = P->getCorrSetup(scan1->setupName);

		for(it = scans.begin(); it != scans.end();)
		{
			const VexScan *scan2 = V->getScan(*it);
			const CorrSetup *setup2 = P->getCorrSetup(scan2->setupName);

			// FIXME -- verify modes are compatible
			if(areCorrSetupsCompatible(setup1, setup2, P) &&
			   areScansCompatible(scan1, scan2, P))
			{
				JG.scans.push_back(*it);
				it = scans.erase(it);
				scan1 = scan2;
				setup1 = setup2;
			}
			else
			{	
				it++;
			}
		}
	}

	const list<VexEvent> *events = V->getEvents();
	for(v = JGs.begin(); v != JGs.end(); v++)
	{
		v->genEvents(*events);
	}
}

class MediaChange
{
public:
	MediaChange(string A, double start, double stop) : ant(A), mjdStart(start), mjdStop(stop) {}

	string ant;
	double mjdStart;
	double mjdStop;
};

int nGap(const list<MediaChange> &m, double mjd)
{
	list<MediaChange>::const_iterator it;
	int n=0;

	for(it = m.begin(); it != m.end(); it++)
	{
		if(mjd >= it->mjdStart && mjd <= it->mjdStop)
		{
			n++;
		}
	}

	return n;
}

void genJobs(vector<VexJob> &Js, const VexJobGroup &JG, VexData *V, const CorrParams *P, int verbose)
{
	list<VexEvent>::const_iterator e;
	list<double>::const_iterator t;
	list<MediaChange>::iterator c;
	map<string,double> recordStop;
	map<double,int> usage;
	map<double,int> clockBreaks;
	list<MediaChange> changes;
	list<double> times;
	list<double> breaks;
	double mjdLast = -1.0;
	int score, scoreBest;
	double mjdBest = 0.0;
	int nAnt;

	// first initialize recordStop and usage
	for(e = JG.events.begin(); e != JG.events.end(); e++)
	{
		if(e->eventType == VexEvent::RECORD_START)
		{
			recordStop[e->name] = -1.0;
		}

		usage[e->mjd] = 0;
		clockBreaks[e->mjd] = 0;
	}
	nAnt = recordStop.size();

	// populate changes, times, and usage
	for(e = JG.events.begin(); e != JG.events.end(); e++)
	{
		if(mjdLast > 0.0 && e->mjd > mjdLast)
		{
			usage[e->mjd] = usage[mjdLast];
			mjdLast = e->mjd;
			times.push_back(e->mjd);
		}
		else if(mjdLast < 0.0)
		{
			usage[e->mjd] = 0;
			mjdLast = e->mjd;
			times.push_back(e->mjd);
		}

		if(e->eventType == VexEvent::RECORD_START)
		{
			if(recordStop[e->name] > 0.0)
			{
				changes.push_back(MediaChange(e->name, recordStop[e->name], e->mjd));
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
		else if(e->eventType == VexEvent::CLOCK_BREAK)
		{
			clockBreaks[e->mjd]++;
		}
	}

	// now go through and set breakpoints
	while(!changes.empty())
	{
		// look for break with highest score
		scoreBest = -1;
		for(t = times.begin(); t != times.end(); t++)
		{
			score = nGap(changes, *t) * (nAnt-usage[*t]) + 100*clockBreaks[*t];
			if(score > scoreBest)
			{
				scoreBest = score;
				mjdBest = *t;
			}
		}

		breaks.push_back(mjdBest);

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
	breaks.sort();	// should be a no-op

	// form jobs
	double start = V->obsStart();
	for(t = breaks.begin(); t != breaks.end(); t++)
	{
		JG.createJob(Js, start, *t);
		start = *t;
	}

	JG.createJob(Js, start, V->obsStop());
}

void makeJobs(vector<VexJob>& J, VexData *V, const CorrParams *P, int verbose)
{
	// FIXME -- no maxLength constraint yet

	vector<VexJobGroup> JG;
	vector<VexJob>::iterator j;
	int k;

	// Do splitting of jobs
	genJobGroups(JG, V, P, verbose);
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
		name << j->jobSeries << "." << j->jobId;
		V->addEvent(j->mjdStart, VexEvent::JOB_START, name.str());
		V->addEvent(j->mjdStop,  VexEvent::JOB_STOP,  name.str());
		j->assignVSNs(*V);
	}
}

DifxJob *makeDifxJob(string directory, const VexJob& J, int nAntenna, const string& obsCode, int *n)
{
	DifxJob *job;
	const char *difxVer;

	*n = 1;
	job = newDifxJobArray(*n);
	difxVer = getenv("DIFX_VERSION");
	if(difxVer)
	{
		strcpy(job->difxVersion, difxVer);
	}
	job->jobStart = J.mjdStart;
	job->jobStop  = J.mjdStop;
	job->mjdStart = J.mjdStart;
	job->duration = trunc((J.mjdStop - J.mjdStart) * 86400.0 + 0.001);
	job->modelInc = 1;
	job->jobId    = J.jobId;
	job->subarrayId = 0;
	strncpy(job->obsCode, obsCode.c_str(), 8);
	job->obsCode[7] = 0;
	strcpy(job->taperFunction, "UNIFORM");
	job->polyOrder = 5;
	job->polyInterval = 120;
	job->aberCorr = AberCorrExact;
	job->activeBaselines = nAntenna;
	job->activeBaselines = nAntenna*(nAntenna-1)/2;
	job->dutyCycle = J.dutyCycle;

	sprintf(job->fileBase, "%s/%s%d", directory.c_str(), J.jobSeries.c_str(), J.jobId);

	return job;
}

DifxAntenna *makeDifxAntennas(const VexJob& J, const VexData *V, const CorrParams *P, int *n, vector<string>& antList)
{
	const VexAntenna *ant;
	DifxAntenna *A;
	int i;
	double offset, rate, mjd;
	map<string,string>::const_iterator a;

	mjd = 0.5*(V->obsStart() + V->obsStop());

	*n = J.vsns.size();

	antList.clear();

	A = newDifxAntennaArray(*n);
	for(i = 0, a = J.vsns.begin(); a != J.vsns.end(); i++, a++)
	{
		ant = V->getAntenna(a->first);
		strcpy(A[i].name, a->first.c_str());
		strcpy(A[i].vsn, a->second.c_str());
		A[i].X = ant->x + ant->dx*(mjd-ant->posEpoch)*86400.0;
		A[i].Y = ant->y + ant->dy*(mjd-ant->posEpoch)*86400.0;
		A[i].Z = ant->z + ant->dz*(mjd-ant->posEpoch)*86400.0;
		strcpy(A[i].mount, ant->axisType.c_str());
		ant->getClock(J.mjdStart, offset, rate);
		A[i].delay = offset*1.0e6;	// convert to us from sec
		A[i].rate  = rate*1.0e6;	// convert to us/sec from sec/sec
		A[i].offset[0] = ant->axisOffset;
		A[i].offset[1] = 0.0;
		A[i].offset[2] = 0.0;

		antList.push_back(a->first);
		strcpy(A[i].shelf, P->getShelf(a->second));
	}

	return A;
}

DifxDatastream *makeDifxDatastreams(const VexJob& J, const VexData *V, int nSet, int *n)
{
	DifxDatastream *D;
	int i;
	
	*n = J.vsns.size() * nSet;
	D = newDifxDatastreamArray(*n);
	for(i = 0; i < *n; i++)
	{
		D[i].antennaId = i % J.vsns.size();
		D[i].tSys = 0.0;
	}

	return D;
}

// round up to the next power of two
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
	freq(double f=0.0, double b=0.0, char s=' ') : fq(f), bw(b), sideBand(s) {};
	double fq;
	double bw;
	char sideBand;
};

int getFreqId(vector<freq>& freqs, double fq, double bw, char sb)
{
	for(unsigned int i = 0; i < freqs.size(); i++)
	{
		if(fq == freqs[i].fq &&
		   bw == freqs[i].bw &&
		   sb == freqs[i].sideBand)
		{
			return i;
		}
	}

	freqs.push_back(freq(fq, bw, sb));

	return freqs.size() - 1;
}

int getBand(vector<pair<int,int> >& bandMap, int fqId)
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
	
int setFormat(DifxInput *D, int dsId, vector<freq>& freqs, const VexMode *mode)
{
	int antId = D->datastream[dsId].antennaId;
	if(antId < 0 || antId >= D->nAntenna)
	{
		cerr << "Error: setFormat: antId=" << antId << " while nAntenna=" << D->nAntenna << endl;
		exit(0);
	}
	string antName(D->antenna[antId].name);
	const VexFormat &format = mode->getFormat(antName);
	int n2 = next2(format.nRecordChan);

	if(format.format == string("VLBA1_1"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBA");
		D->datastream[dsId].dataFrameSize = 2520*format.nBit*n2;
	}
	else if(format.format == string("VLBA1_2"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBA");
		D->datastream[dsId].dataFrameSize = 5040*format.nBit*n2;
	}
	else if(format.format == string("VLBA1_4"))
	{
		strcpy(D->datastream[dsId].dataFormat, "VLBA");
		D->datastream[dsId].dataFrameSize = 10080*format.nBit*n2;
	}
	else if(format.format == string("MKIV1_1"))
	{
		strcpy(D->datastream[dsId].dataFormat, "MKIV");
		D->datastream[dsId].dataFrameSize = 2500*format.nBit*n2;
	}
	else if(format.format == string("MKIV1_2"))
	{
		strcpy(D->datastream[dsId].dataFormat, "MKIV");
		D->datastream[dsId].dataFrameSize = 5000*format.nBit*n2;
	}
	else if(format.format == string("MKIV1_4"))
	{
		strcpy(D->datastream[dsId].dataFormat, "MKIV");
		D->datastream[dsId].dataFrameSize = 10000*format.nBit*n2;
	}
	else if(format.format == string("S2"))
	{
		strcpy(D->datastream[dsId].dataFormat, "LBA");
		D->datastream[dsId].dataFrameSize = 4096 + 10*format.nBit*n2*(int)(mode->sampRate+0.5)/8;
	}
	else
	{
		cerr << "Error: format " << format.format << " not currently supported.  Mode=" << mode->name << ", ant=" << D->antenna[antId].name << "." << endl;
		return 0;
	}

	strcpy(D->datastream[dsId].dataSource, "MODULE");
	D->datastream[dsId].quantBits = format.nBit;
	DifxDatastreamAllocRecChans(D->datastream + dsId, n2);

	vector<pair<int,int> > bandMap;
	for(vector<VexIF>::const_iterator i = format.ifs.begin(); i != format.ifs.end(); i++)
	{
		if(i->subbandId < 0 || i->subbandId >= mode->subbands.size())
		{
			cerr << "Error: index to subband=" << i->subbandId << " is out of range" << endl;
			exit(0);
		}
		int r = i->recordChan;
		const VexSubband& subband = mode->subbands[i->subbandId];
		int fqId = getFreqId(freqs, subband.freq, subband.bandwidth, subband.sideBand);
		
		if(r < 0 || r >= D->datastream[dsId].nRecChan)
		{
			cerr << "Error: index to RC = " << r << " is out of range" << endl;
			exit(0);
		}
		D->datastream[dsId].RCfreqId[r] = getBand(bandMap, fqId);
		D->datastream[dsId].RCpolName[r] = subband.pol;
	}
	DifxDatastreamAllocFreqs(D->datastream + dsId, bandMap.size());
	for(unsigned int j = 0; j < bandMap.size(); j++)
	{
		D->datastream[dsId].freqId[j] = bandMap[j].first;
		D->datastream[dsId].nPol[j] = bandMap[j].second;
	}

	return n2;
}

void populateFreqTable(DifxInput *D, const vector<freq>& freqs)
{
	D->nFreq = freqs.size();
	D->freq = newDifxFreqArray(D->nFreq);
	for(unsigned int f = 0; f < freqs.size(); f++)
	{
		D->freq[f].freq = freqs[f].fq/1.0e6;
		D->freq[f].bw   = freqs[f].bw/1.0e6;
		D->freq[f].sideband = freqs[f].sideBand;
	}
}

// warning: assumes same number of datastreams == antennas for each config
void populateBaselineTable(DifxInput *D, int doPolar)
{	
	int a1, a2, c, f, g, n1, n2, u, v;
	int npol;
	int a1c[2], a2c[2];
	char a1p[2], a2p[2];
	DifxBaseline *bl;

	D->nBaseline = 0;

	for(c = 0; c < D->nConfig; c++)
	{
		int nD = D->config[c].nDatastream;
		D->nBaseline += nD*(nD-1)/2;
	}
	
	D->baseline = newDifxBaselineArray(D->nBaseline);

	bl = D->baseline;
	for(c = 0; c < D->nConfig; c++)
	{
		for(a2 = 1; a2 < D->config[c].nDatastream; a2++)
		{
			for(a1 = 0; a1 < a2; a1++)
			{
				bl->dsA = D->config[c].datastreamId[a1];
				bl->dsB = D->config[c].datastreamId[a2];

				DifxBaselineAllocFreqs(bl, D->datastream[a1].nFreq);

				for(f = 0; f < D->datastream[a1].nFreq; f++)
				{
					npol = 0;
					DifxBaselineAllocPolProds(bl, f, 4);
					g = D->datastream[a1].freqId[f];

					n1 = DifxDatastreamGetRecChans(D->datastream+a1, g, a1p, a1c);
					n2 = DifxDatastreamGetRecChans(D->datastream+a2, g, a2p, a2c);

					for(u = 0; u < n1; u++)
					{
						for(v = 0; v < n2; v++)
						{
							if(a1p[u] == a2p[v])
							{
								bl->recChanA[f][npol] = a1c[u];
								bl->recChanB[f][npol] = a2c[v];
								npol++;
							}
						}
					}

					if(npol == 2 && doPolar)
					{
						// configure cross hands here
						bl->recChanA[f][2] = bl->recChanA[f][0];
						bl->recChanB[f][2] = bl->recChanB[f][1];
						bl->recChanA[f][3] = bl->recChanA[f][1];
						bl->recChanB[f][3] = bl->recChanB[f][0];
					}
					else
					{
						// Not all 4 products used: reduce count
						bl->nPolProd[f] = npol;
					}
				}

				bl++;
			}
		}
	}
}

void populateEOPTable(DifxInput *D, const vector<VexEOP>& E)
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

int getConfigIndex(vector<pair<string,string> >& configs, DifxInput *D, const VexData *V, const CorrParams *P, const VexScan *S)
{
	int c;
	DifxConfig *config;
	const CorrSetup *setup;
	const VexMode *mode;
	string configName;
	double sendLength;
	double minBW;

	setup = P->getCorrSetup(S->setupName);
	if(setup == 0)
	{
		cerr << "Error: setup[" << S->setupName << "] == 0" << endl;
		exit(0);
	}

	mode = V->getMode(S->modeName);
	if(mode == 0)
	{
		cerr << "Error: mode[" << S->modeName << "] == 0" << endl;
		exit(0);
	}

	for(unsigned int i = 0; i < configs.size(); i++)
	{
		if(configs[i].first  == S->modeName &&
		   configs[i].second == S->setupName)
		{
			return i;
		}
	}

	sendLength = P->sendLength;

	configName = S->modeName + string("_") + S->setupName;

	c = configs.size();
	configs.push_back(pair<string,string>(S->modeName, S->setupName));
	config = D->config + c;
	strcpy(config->name, configName.c_str());
	config->tInt = setup->tInt;
	if(setup->specAvg == 0)
	{
		config->nChan = setup->nChan;
		config->specAvg = 1;	
		if(setup->nChan < 128)
		{
			config->specAvg = 128/setup->nChan;
			config->nChan = 128;
		}
	}
	else
	{
		config->nChan = setup->nChan*setup->specAvg;
		config->specAvg = setup->specAvg;
	}
	config->guardBlocks = 1;
	config->postFFringe = setup->postFFringe;
	config->quadDelayInterp = 1;
	config->pulsarId = -1;		// FIXME -- from setup
	config->doPolar = setup->doPolar;
	config->nAntenna = D->nAntenna;
	config->nDatastream = D->nAntenna;
	config->nBaseline = D->nAntenna*(D->nAntenna-1)/2;
	config->overSamp = static_cast<int>(mode->sampRate/(2.0*mode->subbands[0].bandwidth) + 0.001);
	config->decimation = 1;
	if(config->overSamp <= 0)
	{
		cerr << "Error: configName=" << configName << " overSamp=" << config->overSamp << endl;
		cerr << "samprate=" << mode->sampRate << " bw=" << 
			mode->subbands[0].bandwidth << endl;
		exit(0);
	}
	// try to get a good balance of oversampling and decim
	while(config->overSamp % 4 == 0)
	{
		config->overSamp /= 2;
		config->decimation *= 2;
	}
	minBW = mode->sampRate/(2.0*config->decimation);
	DifxConfigAllocDatastreamIds(config, config->nDatastream, c*config->nDatastream);
	DifxConfigAllocBaselineIds(config, config->nBaseline, c*config->nBaseline);

	config->nPol = mode->getPols(config->pol);
	config->quantBits = mode->getBits();
	if(setup->blocksPerSend > 0)
	{
		config->blocksPerSend = setup->blocksPerSend;
	}
	else
	{
		config->blocksPerSend = (int)(sendLength*minBW*D->nDataSegments/(config->decimation*config->nChan*D->dataBufferFactor));
	}

	// FIXME -- reset sendLength based on blockspersend, then readjust tInt, perhaps

	return c;
}

void writeJob(const VexJob& J, const VexData *V, const CorrParams *P)
{
	DifxInput *D;
	DifxScan *scan;
	string setupName;
	const CorrSetup *setup;
	const SourceSetup *sourceSetup;
	const VexMode *mode;
	const VexScan *S;
	set<string> configSet;
	vector<pair<string,string> > configs;
	vector<string> antList;
	vector<freq> freqs;
	int nPulsar=0;

	setupName = V->getScan(J.scans.front())->setupName;
	setup = P->getCorrSetup(setupName);
	if(!setup)
	{
		cerr << "Error: setup " << setupName << "Not found!" << endl;
		exit(0);
	}

	// make set of unique config names
	for(vector<string>::const_iterator si = J.scans.begin(); si != J.scans.end(); si++)
	{
		string configName;

		S = V->getScan(*si);
		configName = S->modeName + string("_") + S->setupName;
		configSet.insert(configName);
	}


	D = newDifxInput();

	D->mjdStart = J.mjdStart;
	D->mjdStop  = J.mjdStop;
	D->startChan = setup->startChan;
	D->dataBufferFactor = P->dataBufferFactor;
	D->nDataSegments = P->nDataSegments;

	D->antenna = makeDifxAntennas(J, V, P, &(D->nAntenna), antList);
	D->job = makeDifxJob(V->getDirectory(), J, D->nAntenna, V->getExper()->name, &(D->nJob));
	
	D->nScan = J.scans.size();
	D->scan = newDifxScanArray(D->nScan);
	D->nConfig = configSet.size();
	D->config = newDifxConfigArray(D->nConfig);

	// now run through all scans, populating things as we go
	scan = D->scan;
	for(vector<string>::const_iterator si = J.scans.begin(); si != J.scans.end(); si++, scan++)
	{
		S = V->getScan(*si);
		if(!S)
		{
			cerr << "Error: source[" << *si << "] not found!  This cannot be!" << endl;
			exit(0);
		}

		const VexSource *src = V->getSource(S->sourceName);

		setup = P->getCorrSetup(S->setupName);
		sourceSetup = P->getSourceSetup(S->sourceName.c_str());

		scan->mjdStart = S->timeRange.mjdStart;
		scan->mjdEnd = S->timeRange.mjdStop;
		scan->startPoint = static_cast<int>((S->timeRange.mjdStart - J.mjdStart)*86400.0/D->job->modelInc + 0.01);
		scan->nPoint = static_cast<int>((S->timeRange.mjdStop - S->timeRange.mjdStart)*86400.0/D->job->modelInc + 0.01);
		scan->configId = getConfigIndex(configs, D, V, P, S);

		scan->ra = src->ra;
		scan->dec = src->dec;
		strcpy(scan->name, S->sourceName.c_str());
		// FIXME qual and calcode

		if(sourceSetup)
		{
			if(sourceSetup->ra > -990)
			{
				scan->ra = sourceSetup->ra;
			}
			if(sourceSetup->dec > -990)
			{
				scan->dec = sourceSetup->dec;
			}
			if(sourceSetup->difxName.size() > 0)
			{
				strcpy(scan->name, sourceSetup->difxName.c_str());
			}
		}
	}

	for(int c = 0; c < D->nConfig; c++)
	{
		setup = P->getCorrSetup(configs[c].second);
		if(setup->binConfigFile.size() > 0)
		{
			nPulsar++;
		}
	}
	if(nPulsar > 0)
	{
		D->pulsar = newDifxPulsarArray(nPulsar);
	}

	// configure datastreams
	D->datastream = makeDifxDatastreams(J, V, D->nConfig, &(D->nDatastream));
	D->nDatastream = 0;
	for(int c = 0; c < D->nConfig; c++)
	{
		mode = V->getMode(configs[c].first);
		if(mode == 0)
		{
			cerr << "Error: mode[" << configs[c].first << "] is null" << endl;
			exit(0);
		}

		setup = P->getCorrSetup(configs[c].second);
		if(setup == 0)
		{
			cerr << "Error: setup[" << configs[c].second << "] is null" << endl;
			exit(0);
		}

		if(setup->binConfigFile.size() > 0)
		{
			D->config[c].pulsarId = D->nPulsar;
			strcpy(D->pulsar[D->nPulsar].fileName, setup->binConfigFile.c_str());
			D->nPulsar++;
		}

		int d = 0;
		for(int a = 0; a < D->nAntenna; a++)
		{
			int v = setFormat(D, D->nDatastream, freqs, mode);
			if(v)
			{
				D->config[c].datastreamId[d] = D->nDatastream;
				D->nDatastream++;
				d++;
			}
		}
	}

	if(nPulsar != D->nPulsar)
	{
		cerr << "Error: nPulsar=" << nPulsar << " != D->nPulsar=" << D->nPulsar << endl;
		exit(0);
	}

	// Make frequency table
	populateFreqTable(D, freqs);

	// Make baseline table
	populateBaselineTable(D, setup->doPolar);

	// Make EOP table
	populateEOPTable(D, V->getEOPs());

	// complete a few DifxInput structures
	deriveSourceTable(D);
	//printDifxInput(D);

	// Merge identical table entries
	simplifyDifxFreqs(D);
	simplifyDifxDatastreams(D);
	simplifyDifxBaselines(D);
	simplifyDifxConfigs(D);

	// fix a few last parameters
	if(setup->specAvg == 0)
	{
		D->specAvg  = D->config[0].specAvg;
	}
	else
	{
		D->specAvg = setup->specAvg;
	}

	// write input file
	ostringstream inputName;
	inputName << D->job->fileBase << ".input";
	writeDifxInput(D, inputName.str().c_str());

	// write calc file
	ostringstream calcName;
	calcName << D->job->fileBase << ".calc";
	writeDifxCalc(D, calcName.str().c_str());

	// clean up
	deleteDifxInput(D);
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
	cout << "     --help      display this information and quit." << endl;
	cout << endl;
	cout << "     -v" << endl;
	cout << "     --verbose   increase the verbosity of the output; -v -v for more detail." << endl;
	cout << endl;
	cout << "     -o" << endl;
	cout << "     --output    create a v2d file with all defaults populated." << endl;
	cout << endl;
	cout << "  the v2d file is the vex2difx configuration file to process." << endl;
	cout << endl;
	cout << "See http://cira.ivec.org/dokuwiki/doku.php/difx/vex2difx for more information" << endl;
	cout << endl;

	return 0;
}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	vector<VexJob> J;
	ifstream is;
	string shelfFile;
	int verbose = 0;
	string v2dFile;
	bool writeParams = 0;

	if(argc < 2)
	{
		return usage(argc, argv);
	}

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
				writeParams=1;
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
				cerr << "Error: multiple configuration files provides, one expected." << endl;
				cerr << "Run with -h for help information." << endl;
				exit(0);
			}
			v2dFile = argv[a];
		}
	}

	if(v2dFile.size() == 0)
	{
		cerr << "Error: configuration file expected." << endl;
		cerr << "Run with -h for help information." << endl;
		exit(0);
	}

	P = new CorrParams(v2dFile);
	if(P->vexFile.size() == 0)
	{
		cerr << "Error: vex file parameter (vex) not found in file." << endl;
		exit(0);
	}

	shelfFile = P->vexFile.substr(0, P->vexFile.find_last_of('.'));
	shelfFile += string(".shelf");
	P->loadShelves(shelfFile);

	V = loadVexFile(*P);

	makeJobs(J, V, P, verbose);

	if(verbose > 1)
	{
		cout << *V << endl;
		cout << *P << endl;
	}

	if(writeParams)
	{
		ofstream of;
		string paramsFile = v2dFile + ".params";

		of.open(paramsFile.c_str());
		of << *P << endl;
		of.close();
	}

	for(vector<VexJob>::iterator j = J.begin(); j != J.end(); j++)
	{
		if(verbose > 0)
		{
			cout << *j;
		}
		writeJob(*j, V, P);
	}
	cout << J.size() << " job(s) created." << endl;

	delete V;
	delete P;

	return 0;
}
