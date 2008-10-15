#include <vector>
#include <sstream>
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

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	vector<VexJobGroup> JG;
	vector<VexJob> J;
	vector<VexJob>::iterator j;
	int k;

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

	cout << *V << endl;
	cout << *P << endl;

	for(j = J.begin(), k = P->startSeries; j != J.end(); j++, k++) cout << *j;

	delete V;
	delete P;

	return 0;
}
