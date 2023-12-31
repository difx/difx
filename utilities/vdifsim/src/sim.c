#include <stdio.h>
#include <string.h>
#include <difxio/difx_input.h>
#include <pthread.h>
#include <mpi.h>
#include <difxmessage.h>
#include "sim.h"
#include "common.h"
#include "datastream.h"
#include "options.h"
#include "configuration.h"


typedef struct
{
	const DifxInput *D;
	const CommonSignal *C;
	Datastream *d;
} DatastreamInfo;

void *datastreamProcessWrapper(void *data)
{
	DatastreamInfo *di = (DatastreamInfo *)data;

	datastreamProcess(di->D, di->C, di->d);

	return 0;
}

static int work(const DifxInput *D, const CommandLineOptions *opts, const Configuration *config)
{
	CommonSignal *C;
	Datastream **ds;
	int mjd, sec;
	int t, dur;	/* [sec] */
	int i;
	pthread_t *threads;
	DatastreamInfo *info;
	int mpiRank;

	MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);

	/* 1. Configure working arrays */
	C = newCommonSignal(D, opts, config);
	ds = newDatastreams(D, C, opts, config);
	threads = (pthread_t *)calloc(D->nDatastream, sizeof(pthread_t));
	info = (DatastreamInfo *)calloc(D->nDatastream, sizeof(DatastreamInfo));
	for(i = 0; i < D->nDatastream; ++i)
	{
		info[i].D = D;
		info[i].C = C;
		info[i].d = ds[i];

		printDatastream(ds[i]);
	}

	/* 2. Loop over time; 1 second of common signal + buffers on both sides */
	mjd = (int)(D->mjdStart);
	sec = (int)(86400 * (D->mjdStart - mjd) + 0.5);
	if(opts->testMode)
	{
		dur = 5;
	}
	else
	{
		dur = (int)(86400 * (D->mjdStop - D->mjdStart) + 1.1);
	}
	if(opts->verbose > 0)
	{
		printf("Rank %d : Starting loop over time: mjd=%d sec=%d dur=%d\n", mpiRank, mjd, sec, dur);
	}
	for(t = 0; t < dur; ++t)
	{
		if(opts->verbose > 0)
		{
			printf("Rank %d : Starting second %d / %d\n", mpiRank, t, dur);
		}
		if(opts->useDifxMessage)
		{
			const int MessageSize = 128;
			char message[MessageSize];

			snprintf(message, MessageSize, "Starting second %d/%d %5.2f%% complete.", t, dur, (100.0*t)/dur);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			printf("Sending %s\n", message);
		}

		/* 2.1. Generate common signal */
		generateCommonSignal(C, mjd, sec);
		if(opts->verbose > 1)
		{
			printCommonSignal(C);
		}

		/* 2.2. Perform processing */
		for(i = 0; i < D->nDatastream; ++i)
		{
			pthread_create(threads + i, 0, datastreamProcessWrapper, info + i);
		}
		for(i = 0; i < D->nDatastream; ++i)
		{
			void *tptr;
			pthread_join(threads[i], &tptr);
		}

		/* 2.3. Get ready for next second */
		++sec;
		if(sec >= 86400)
		{
			sec = 0;
			++mjd;
		}
	}

	/* 3. Clean up */
	free(threads);
	free(info);
	deleteDatastreams(ds);
	deleteCommonSignal(C);

	return 1;
}

/* return 0 if requirements are not met */
int complianceCheck(const DifxInput *D)
{
	int d;
	int rv = 1;

	/* single job */
	if(D->nJob != 1)
	{
		fprintf(stderr, "! More than one job!\n");
		rv = 0;
	}

	/* single scan */
	if(D->nScan != 1)
	{
		fprintf(stderr, "! More than one scan!\n");
		rv = 0;
	}


	/* VDIF, bits, single file per datastream */
	for(d = 0; d < D->nDatastream; ++d)
	{
		int bits;

		if(strncmp(D->datastream[d].dataFormat, "INTERLACEDVDIF", 14) != 0)
		{
			fprintf(stderr, "! datastream %d is not VDIF format.  It reports %s\n", d, D->datastream[d].dataFormat);
			rv = 0;
		}

		bits = D->datastream[d].quantBits;
		if(bits != 1 && bits != 2 && bits != 4 && bits != 8)
		{
			fprintf(stderr, "! datastream %d does not have one of: 1,2,4,8 bits per sample.  It reports %d\n", d, bits);
			rv = 0;
		}

		if(D->datastream->nFile != 1)
		{
			fprintf(stderr, "! datastream %d does not have exactly 1 file.  It reports %d\n", d, D->datastream->nFile);
			rv = 0;
		}
	}

	return rv;
}

int simulate(const CommandLineOptions *opts)
{
	int N = 0;
	int i;
	Configuration *config = 0;
	int mpiRank;

	MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);

	if(opts->verbose > 0)
	{
		printf("Rank %d / %d is running\n", mpiRank, opts->nProcess);
	}

	if(opts->configFile)
	{
		config = loadConfigration(opts->configFile);

		if(!config)
		{
			fprintf(stderr, "Error: loading of configuration file '%s' failed.\n", opts->configFile);

			return 0;
		}

		printConfiguration(config);
	}

	for(i = 0; i < opts->nInputFile; ++i)
	{
		DifxInput *D;

		if(i % opts->nProcess != mpiRank)
		{
			/* a different MPI process will handle this file */

			continue;
		}

		if(opts->useDifxMessage)
		{
			difxMessageSetInputFilename(opts->inputFile[i]);
		}

		printf("Rank %d running on input file %d/%d = %s\n", mpiRank, i+1, opts->nInputFile, opts->inputFile[i]);

		D = loadDifxInput(opts->inputFile[i]);
		if(!D)
		{
			fprintf(stderr, "Rank %d : error loading %s\n", mpiRank, opts->inputFile[i]);

			break;
		}
		D = updateDifxInput(D, 0);
		if(!D)
		{
			fprintf(stderr, "Rank %d : update failed: D == 0.\n", mpiRank);

			break;
		}

		if(opts->verbose > 2)
		{
			printDifxInput(D);
		}

		if(!complianceCheck(D))
		{
			fprintf(stderr, "Rank %d : error: %s doesn't meet the requirements of this program.\n\n", mpiRank, opts->inputFile[i]);
		}
		else
		{
			N += work(D, opts, config);
		}

		deleteDifxInput(D);
	}

	if(config)
	{
		deleteConfiguration(config);
	}

	return N;
}
