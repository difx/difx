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

void genJobGroups(vector<VexJobGroup> &Js, const VexData *V, const CorrParams *P)
{
	list<string> scans;
	list<string>::iterator it;

	V->getScanList(scans);

	while(!scans.empty())
	{
		Js.push_back(VexJobGroup());
		VexJobGroup &J = Js.back();
		J.scans.push_back(scans.front());
		scans.pop_front();

		cout << J.scans.back() << endl;
		
		const VexScan *scan1 = V->getScan(J.scans.back());
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
				J.scans.push_back(*it);
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
