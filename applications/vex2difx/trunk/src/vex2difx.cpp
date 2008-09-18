#include "vextables.h"
#include "corrparams.h"
#include "vexload.h"

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams params;

	params.antennaList.push_back("Ar");
	params.antennaList.push_back("Gb");
	params.antennaList.push_back("Br");
	params.antennaList.push_back("Fd");
	params.antennaList.push_back("Mk");
	params.antennaList.push_back("Sc");
	 
	if(argc < 2)
	{
		cout << "need filename" << endl;
		return 0;
	}

	V = loadVexFile(argv[1], params);

	cout << *V << endl;

	delete V;

	return 0;
}
