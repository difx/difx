#include "vextables.h"
#include "corrparams.h"
#include "vexload.h"

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams params;

	params.antennaList.push_back("Fd");
	params.antennaList.push_back("MK");
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
