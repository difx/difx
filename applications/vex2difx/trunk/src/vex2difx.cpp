#include <vector>
#include <sstream>
#include <difxio/difx_input.h>
#include <cmath>
#include <cstdlib>
#include "vextables.h"
#include "corrparams.h"
#include "vexload.h"

// FIXME : where to put this function?

// A is assumed to be the first scan in time order
bool areScansCompatible(const VexScan *A, const VexScan *B, const CorrParams *P)
{
	if(A->timeRange.mjdStop + P->maxGap < B->timeRange.mjdStart)
	{
		return false;
	}
	if(P->singleScan)
	{
		return false;
	}
	if(P->singleSetup)
	{
		if(A->modeName != B->modeName)
		{
			return false;
		}
		else
		{
			return true;
		}
	}
	
	return true;
}

void genJobGroups(vector<VexJobGroup> &JGs, const VexData *V, const CorrParams *P)
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

		cout << JG.scans.back() << endl;
		
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
				cout << "adding  " << *it << endl;
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

		// dog and pony show
		cout << "Job Group Events " << v->events.size() << endl;
		list<VexEvent>::const_iterator e;
		for(e = v->events.begin(); e != v->events.end(); e++)
		{
			cout << *e << endl;
		}
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
		if(mjd > it->mjdStart && mjd < it->mjdStop)
		{
			n++;
		}
	}

	return n;
}

void genJobs(vector<VexJob> &Js, const VexJobGroup &JG, VexData *V, const CorrParams *P)
{
	list<VexEvent>::const_iterator e;
	list<double>::const_iterator t;
	list<MediaChange>::iterator c;
	map<string,double> recordStop;
	map<double,int> usage;
	list<MediaChange> changes;
	list<double> times;
	list<double> breaks;
	double mjdLast = -1.0;
	int score, scoreBest;
	double mjdBest;
	int nAnt;

	// first initialize recordStop and usage
	for(e = JG.events.begin(); e != JG.events.end(); e++)
	{
		if(e->eventType == VexEvent::RECORD_START)
		{
			recordStop[e->name] = -1.0;
		}

		usage[e->mjd] = 0;
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
	}

	// now go through and set breakpoints
	while(!changes.empty())
	{
		// look for break with highest score
		scoreBest = -1;
		for(t = times.begin(); t != times.end(); t++)
		{
			score = nGap(changes, *t) * (nAnt-usage[*t]);
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

void makeJobs(vector<VexJob>& J, VexData *V, const CorrParams *P)
{
	// FIXME -- no maxLength constraint yet

	vector<VexJobGroup> JG;
	vector<VexJob>::iterator j;
	int k;

	// Do splitting of jobs
	genJobGroups(JG, V, P);
	for(int i = 0; i < JG.size(); i++)
	{
		genJobs(J, JG[i], V, P);
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

DifxJob *makeDifxJob(const VexJob& J, int nAntenna, const string& obsCode, int *n)
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

	sprintf(job->fileBase, "%s.%d", J.jobSeries.c_str(), J.jobId);

	return job;
}

DifxAntenna *makeDifxAntennas(const VexJob& J, const VexData *V, int *n, vector<string>& antList)
{
	const VexAntenna *ant;
	DifxAntenna *A;
	int i;
	map<string,string>::const_iterator a;

	*n = J.vsns.size();

	antList.clear();

	A = newDifxAntennaArray(*n);
	for(i = 0, a = J.vsns.begin(); a != J.vsns.end(); i++, a++)
	{
		ant = V->getAntenna(a->first);
		strcpy(A[i].name, a->first.c_str());
		strcpy(A[i].vsn, a->second.c_str());
		A[i].X = ant->x;
		A[i].Y = ant->y;
		A[i].Z = ant->z;
		strcpy(A[i].mount, ant->axisType.c_str());
		A[i].delay = ant->clockOffset;
		A[i].rate  = ant->clockRate;
		antList.push_back(a->first);
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
	for(int i = 0; i < freqs.size(); i++)
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

void writeJob(const VexJob& J, const VexData *V, const CorrParams *P)
{
	DifxInput *D;
	DifxScan *scan;
	string setupName;
	string configName;
	const CorrSetup *setup;
	const VexMode *mode;
	const VexScan *S;
	const VexSource *src;
	map<string,int> configIndex;
	vector<string>::const_iterator si;
	vector<pair<string,string> > configs;
	vector<string> antList;
	int n2, a, b, i, c, dsId;
	int nConfig = 0;
	vector<freq> freqs;

	setupName = V->getScan(J.scans.front())->setupName;
	setup = P->getCorrSetup(setupName);
	if(!setup)
	{
		cerr << "Setup " << setupName << "Not found!" << endl;
		return;
	}

	// initialize configIndex
	for(si = J.scans.begin(); si != J.scans.end(); si++)
	{
		S = V->getScan(*si);
		configName = S->modeName + string("_") + S->setupName;
		configIndex[configName] = -1;
	}


	D = newDifxInput();

	D->mjdStart = J.mjdStart;
	D->mjdStop  = J.mjdStop;
	D->specAvg  = setup->specAvg;
	D->startChan = setup->startChan;

	// FIXME -- next two: expose to params file
	D->dataBufferFactor = 32;
	D->nDataSegments = 8;

	D->antenna = makeDifxAntennas(J, V, &(D->nAntenna), antList);
	D->job = makeDifxJob(J, D->nAntenna, V->getExper()->name, &(D->nJob));
	
	// now run through all scans, populating things as we go
	D->nScan = J.scans.size();
	D->scan = newDifxScanArray(D->nScan);
	D->nConfig = configIndex.size();
	D->config = newDifxConfigArray(D->nConfig);
	scan = D->scan;
	for(si = J.scans.begin(); si != J.scans.end(); si++, scan++)
	{
		S = V->getScan(*si);
		cout << *S << endl;
		configName = S->modeName + string("_") + S->setupName;
		setup = P->getCorrSetup(S->setupName);
		mode = V->getMode(S->modeName);
		c = configIndex[configName];
		if(c < 0)
		{
			DifxConfig *config;
			
			c = configIndex[configName] = nConfig;
			nConfig++;
			configs.push_back(pair<string,string>(S->modeName, S->setupName));
			cout << configName << " -> " << c << endl;
			config = D->config + c;
			strcpy(config->name, configName.c_str());
			config->tInt = setup->tInt;
			config->nChan = setup->nChan;
			config->blocksPerSend = 100;	// FIXME
			config->specAvg = 1;		// FIXME
			config->guardBlocks = 2;
			config->postFFringe = setup->postFFringe;
			config->quadDelayInterp = 1;
			config->pulsarId = -1;		// FIXME -- from setup
			config->doPolar = setup->doPolar;
			config->nAntenna = D->nAntenna;
			config->nDatastream = D->nAntenna;
			config->nBaseline = D->nAntenna*(D->nAntenna-1)/2;
			config->overSamp = static_cast<int>(mode->sampRate/(2.0*mode->subbands[0].bandwidth) + 0.001);
			config->decimation = 1;
			// try to get a good balance of oversampling and decim
			while(config->overSamp % 4 == 0)
			{
				config->overSamp /= 2;
				config->decimation *= 2;
			}
			DifxConfigSetDatastreamIds(config, config->nDatastream, c*config->nDatastream);
			DifxConfigSetBaselineIds(config, config->nBaseline, c*config->nBaseline);

			// FIXME -- use mode info
			config->nPol = mode->getPols(config->pol);
			config->quantBits = mode->getBits();
		}
		scan->configId = c;
		src = V->getSource(S->sourceName);
		scan->ra = src->ra;
		scan->dec = src->dec;
		scan->mjdStart = S->timeRange.mjdStart;
		scan->mjdEnd = S->timeRange.mjdStop;
		scan->startPoint = static_cast<int>((S->timeRange.mjdStart - J.mjdStart)*86400.0/D->job->modelInc + 0.01);
		scan->nPoint = static_cast<int>((S->timeRange.mjdStop - S->timeRange.mjdStart)*86400.0/D->job->modelInc + 0.01);
		strcpy(scan->name, S->sourceName.c_str());
		// qual and calcode
	}

	// configure datastreams
	if(nConfig != configIndex.size())
	{
		cerr << "Error : nConfig != configIndex.size())" << endl;
	}
	D->datastream = makeDifxDatastreams(J, V, nConfig, &(D->nDatastream));
	for(c = 0; c < nConfig; c++)
	{
		mode  = V->getMode(configs[c].first);
		setup = P->getCorrSetup(configs[c].second);
		cout << "Setting up " << mode->name << " " << setup->setupName << endl;

		for(a = 0; a < antList.size(); a++)
		{
			vector<pair<int,int> > bandMap;

			dsId = c*antList.size() + a;
			const VexFormat format = mode->getFormat(antList[a]);
			n2 = next2(format.nRecordChan);
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
			else
			{
				cerr << "Format " << format.format << " not currently supported" << endl;
			}
			strcpy(D->datastream[dsId].dataSource, "MODULE");
			D->datastream[dsId].quantBits = format.nBit;
			DifxDatastreamAllocRecChans(D->datastream + dsId, n2);

			for(vector<VexIF>::const_iterator i = format.ifs.begin(); i != format.ifs.end(); i++)
			{
				int r = i->recordChan;
				const VexSubband& subband = mode->subbands[i->subbandId];
				int fqId = getFreqId(freqs, subband.freq, subband.bandwidth, subband.sideBand);
				
				D->datastream[dsId].RCfreqId[r] = getBand(bandMap, fqId);
				D->datastream[dsId].RCpolName[r] = subband.pol;
			}
			DifxDatastreamAllocFreqs(D->datastream + dsId, bandMap.size());
			for(int i = 0; i < bandMap.size(); i++)
			{
				D->datastream[dsId].freqId[i] = bandMap[i].first;
				D->datastream[dsId].nPol[i] = bandMap[i].second;
			}
		}
	}

	// Make frequency table
	D->nFreq = freqs.size();
	D->freq = newDifxFreqArray(D->nFreq);
	for(int f = 0; f < freqs.size(); f++)
	{
		D->freq[f].freq = freqs[f].fq/1.0e6;
		D->freq[f].bw   = freqs[f].bw/1.0e6;
		D->freq[f].sideband = freqs[f].sideBand;
	}

	// Make baseline table
	D->nBaseline = nConfig*D->nAntenna*(D->nAntenna-1)/2;
	D->baseline = newDifxBaselineArray(D->nBaseline);
	b = 0;
	for(c = 0; c < nConfig; c++)
	{
		for(int a2 = 1; a2 < D->nAntenna; a2++)
		{
			for(int a1 = 0; a1 < a2; a1++)
			{
				DifxBaseline *bl = D->baseline + b;
	
				bl->dsA = a1 + c*D->nAntenna;
				bl->dsB = a2 + c*D->nAntenna;
				DifxBaselineAllocFreqs(bl, D->datastream[a1].nFreq);

				for(int f = 0; f < D->datastream[a1].nFreq; f++)
				{
					int npol = 0;
					int n1, n2, g;
					int a1c[2], a2c[2];
					char a1p[2], a2p[2];
					
					DifxBaselineAllocPolProds(bl, f, 4);
					g = D->datastream[a1].freqId[f];

					n1 = DifxDatastreamGetRecChans(D->datastream+a1, g, a1p, a1c);
					n2 = DifxDatastreamGetRecChans(D->datastream+a2, g, a2p, a2c);

					for(int u = 0; u < n1; u++)
					{
						for(int v = 0; v < n2; v++)
						{
							if(a1p[u] == a2p[v])
							{
								bl->recChanA[f][npol] = a1c[u];
								bl->recChanB[f][npol] = a2c[v];
								npol++;
							}
						}
					}

					if(npol == 2 && setup->doPolar)
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

				b++;
			}
		}
	}

	deriveSourceTable(D);

	printDifxInput(D);

	ostringstream inputName;
	inputName << D->job->jobId << ".input";
	writeDifxInput(D, inputName.str().c_str());

	ostringstream calcName;
	calcName << D->job->jobId << ".calc";
	writeDifxCalc(D, calcName.str().c_str());

	deleteDifxInput(D);
}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	vector<VexJob> J;

	P = new CorrParams();

//	P->antennaList.push_back("Ar");
//	P->antennaList.push_back("Gb");
//	P->antennaList.push_back("Br");
//	P->antennaList.push_back("Fd");
//	P->antennaList.push_back("Mk");
//	P->antennaList.push_back("Sc");

	P->example();
	 
	if(argc < 2)
	{
		cout << "need filename" << endl;
		return 0;
	}

	V = loadVexFile(argv[1], *P);

	makeJobs(J, V, P);

	cout << *V << endl;
	cout << *P << endl;

	vector<VexJob>::iterator j;
	for(j = J.begin(); j != J.end(); j++)
	{
		cout << *j;
	}

	for(j = J.begin(); j != J.end(); j++)
	{
		writeJob(*j, V, P);
	}

	delete V;
	delete P;

	return 0;
}
