#ifndef __OPTIONS_H__
#define __OPTIONS_H__

#define DEFAULT_FLUX_DENSITY		100	/* [Jy] */
#define DEFAULT_SEFD			500	/* [Jy] */
#define DEFAULT_FILTER_TRANSITION	0.1	/* band fraction for full transition from zero to 100% */

typedef struct
{
	int verbose;
	int usage;
	int testMode;
	int force;		/* if true, allow file overwrite */
	int debug;		/* if true, produce some additional debug output */
	const char *program;
	const char *version;
	int nInputFile;
	const char **inputFile;
	const char *configFile;	/* contains description of source signaling.  Default is to use broadband point source */
	double SEFD;		/* [Jy] default antenna SEFD */
	double fluxDensity;	/* [Jy] default point source flux density; resets to zero if configFile is provided */
	double filterTransition;/* fractional bandwidth of band edge transition zone */
	int nProcess;		/* fed by MPI_Comm_size() */
	int useDifxMessage;	/* if true, multicast status */
	int randSeed;		/* random number seed */
} CommandLineOptions;

void usage(const CommandLineOptions *opts);

void resetCommandLineOptions(CommandLineOptions *opts);

CommandLineOptions *newCommandLineOptions(int argc, char **argv);

void deleteCommandLineOptions(CommandLineOptions *opts);

void printCommandLineOptions(const CommandLineOptions *opts);

#endif
