#include "vextables.h"
#include "corrparams.h"
#include "../vex/vex.h"


main()
{
	VexInterval v(51000, 52000);
	VexScan *S;
	VexData V;

	S = V.newScan();

	S->name = "xyz";
	S->timeRange.mjdStart = 10322;
	S->timeRange.mjdEnd = 10323;
	S->modeName = "abc";
	S->sourceName = "12+12";
	S->stations["FD"] = VexInterval(10322.5, 10322.6);
	S->stations["BR"] = VexInterval(10322.5, 10322.6);


	cout << v << endl;

	cout << *S << endl;

	cout << V << endl;
}
