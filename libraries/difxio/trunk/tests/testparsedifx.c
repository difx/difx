#include <stdio.h>
#include "parsedifx.h"

int main(int argc, char **argv)
{
	DifxParameters *dp;
	int i;

	if(argc < 2)
	{
		fprintf(stderr, "Usage : %s <input file>\n", argv[0]);
		return 0;
	}
	
	/*** First demonstrate parsing on an input file ***/

	printf("Loading DiFX Parameters from file\n");
	
	/* Load file */
	dp = newDifxParametersfromfile(argv[1]);

	/* Print to stdout */
	printDifxParameters(dp);

	/* Look for simple key, print if found */
	i = DifxParametersfind(dp, 0, "OUTPUT FORMAT");
	printf("i = %d\n", i); 
	if(i >= 0)
	    printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	else
	    printf("key not found\n");
	
	/* Look for key with one index, print if found */
	i = DifxParametersfind1(dp, 0, "DATASTREAM %d INDEX", 3);
	printf("i = %d\n", i); 
	if(i >= 0)
	    printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	else
	    printf("key not found\n");
	
	/* free from memory */
	deleteDifxParameters(dp);

	/*** Next, demonstrate buiding DifxParameters object from a set of
	     strings as you might do if constructing from stdin ***/
	
	printf("Making new DifxParameters structure by hand\n");

	/* first create empty parameter structure */
	dp = newDifxParameters();

	/* then start adding some rows */
	DifxParametersaddrow(dp, "MJD:                54140");
	DifxParametersaddrow(dp, "U (METRES):         123456.89");

	/* then demonstrate finding a row */
	i = DifxParametersfind(dp, 0, "MJD");
	printf("i = %d\n", i);
	if(i >= 0)
	    printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	else
	    printf("key not found\n");

	/* searching for an non-existant parameter */
	i = DifxParametersfind(dp, 0, "NOT HERE");
	if(i >= 0)
	    printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	else
	    printf("key not found\n");

	
	/* free from memory */
	deleteDifxParameters(dp);

	
	return 0;
}
