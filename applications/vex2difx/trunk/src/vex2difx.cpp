#include <vector>
#include "vextables.h"
#include "corrparams.h"
#include "vexload.h"

//void genJobGroups(vector<JobGroup> &J, const VexData *V, const CorrParams *P)
//{
//}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
//	vector<JobGroup> jobGroups;

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

//	genJobGroups(jobGroups, V, P);

	delete V;
	delete P;

	return 0;
}
