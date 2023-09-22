#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <mpi.h>
#include <difxmessage.h>
#include "options.h"
#include "sim.h"

int main(int argc, char **argv)
{
	CommandLineOptions *opts;
	int rv = 0;

	MPI_Init(&argc, &argv);

	opts = newCommandLineOptions(argc, argv);
	if(!opts)
	{
		return EXIT_FAILURE;
	}

	if(opts->usage)
	{
		usage(opts);
		deleteCommandLineOptions(opts);

		return EXIT_SUCCESS;
	}

	if(opts->verbose > 1)
	{
		printCommandLineOptions(opts);
	}

	/* set to UTC */
	setenv("TZ", "", 1);
	tzset();

	if(opts->useDifxMessage)
	{
		int mpiRank;

		MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
		difxMessageInit(mpiRank, "vdifsim");
	}

	printf("Starting program: %s ver %s\n", opts->program, opts->version);

	simulate(opts);

	printf("Ending program: %s ver %s\n", opts->program, opts->version);

	deleteCommandLineOptions(opts);

	MPI_Finalize();

	if(rv == 0)
	{
		return EXIT_SUCCESS;
	}
	else
	{
		return EXIT_FAILURE;
	}
}
