#include <vector>
#include "vextables.h"
#include "corrparams.h"
#include "vexload.h"

// FIXME : where to put this function?

// A is assumed to be the first scan in time order
bool areScansCompatible(const VexScan *A, const VexScan *B, const CorrParams *P)
{
	if(A->timeRange.mjdEnd + P->maxGap < B->timeRange.mjdStart)
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
	list<VexEvent>::const_iterator e;
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
		cout << "Job Group Events " << v->events.size() << endl;

		for(e = v->events.begin(); e != v->events.end(); e++)
		{
			cout << *e << endl;
		}
	}
}

void genJobs(vector<VexJob> &Js, const VexJobGroup &JG, const VexData *V, const CorrParams *P)
{
	list<VexEvent> events;
}


int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	vector<VexJobGroup> JG;

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

	cout << *V << endl;

	cout << *P << endl;

	genJobGroups(JG, V, P);

	delete V;
	delete P;

	return 0;
}
