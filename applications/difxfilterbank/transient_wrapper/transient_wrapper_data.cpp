#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include "transient_wrapper_data.h"

#define min(x,y) (((x) < (y)) ? (x) : (y))
#define max(x,y) (((x) > (y)) ? (x) : (y))

/* default parameters to use if no config file is found */
const int default_enable = 1;
const char default_vfastrHost[] = "boom";
const char default_outputPath[] = "/home/boom/TESTDATA/CAPTURES";
const double default_minFreeMB = 1000000.0;	/* don't copy unless there are this many MB free in the above path */
const double default_maxCopyOverhead = 0.04;	/* 4% */
const double default_recorr_tInt = 0.000256;
const int default_recorr_nChan = 256;
const int default_recorr_specAvg = 2;
const double default_recorr2_tInt = 0.000256;
const int default_recorr2_nChan = 2048;
const int default_recorr2_specAvg = 1;

TransientWrapperData *newTransientWrapperData(const TransientWrapperConf *conf)
{
	TransientWrapperData *T;
	const char *rankStr;

	T = (TransientWrapperData *)calloc(1, sizeof(TransientWrapperData));

	T->difxState = DIFX_STATE_SPAWNING;
	T->conf = conf;

	rankStr = getenv("OMPI_COMM_WORLD_RANK");
	if(rankStr)
	{
		T->rank = atoi(rankStr);
	}
	else
	{
		T->rank = -1;
	}

	printf("This is MPI rank %d\n", T->rank);
	fflush(stdout);

	return T;
}

void deleteTransientWrapperData(TransientWrapperData *T)
{
	if(T)
	{
		if(T->D)
		{
			deleteDifxInput(T->D);
		}

		if(T->filePrefix)
		{
			free(T->filePrefix);
		}
	}
}

void printTransientWrapperData(const TransientWrapperData *T)
{
	int e;

	printf("TransientWrapperData [%p]\n", T);
	if(T)
	{
		printf("  identifier = %s\n", T->identifier);
		printf("  rank = %d\n", T->rank);
		printf("  DifxState = %s [%d]\n", DifxStateStrings[T->difxState], T->difxState);
		printf("  filePrefix = %s\n", T->filePrefix);
		printf("  monitorThreadDie = %d\n", T->monitorThreadDie);
		printf("  verbose = %d\n", T->verbose);
		printf("  doCopy = %d\n", T->doCopy);
		printf("  executeTime = %d\n", T->executeTime);
		printf("  nTransient = %d\n", T->nTransient);
		printf("  nMerged = %d\n", T->nMerged);
		printf("  nEvent = %d\n", T->nEvent);
		for(e = 0; e < T->nEvent; ++e)
		{
			printf("    event[%d] = [%12.6f,%12.6f] P=%f DM=%f\n", e, 
				T->event[e].startMJD, T->event[e].stopMJD,
				T->event[e].priority, T->event[e].dm);
		}
	}
}

/* Note this sorts on priority only and puts the _highest_ priority events first */
static int eventCompare(const void *p1, const void *p2)
{
	const TransientEvent *e1, *e2;

	e1 = (TransientEvent *)p1;
	e2 = (TransientEvent *)p2;

	if(e1->priority > e2->priority)
	{
		return -1;
	}
	else if(e1->priority == e2->priority)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

void sortEvents(TransientWrapperData *T)
{
	if(T->nEvent > 1)
	{
		qsort(T->event, T->nEvent, sizeof(TransientEvent), eventCompare);

		/* trim down list to maximum allowed if needed */
		if(T->nEvent > MAX_EVENTS)
		{
			T->nEvent = MAX_EVENTS;
		}
	}
}

void addEvent(TransientWrapperData *T, const DifxMessageTransient *transient)
{
	int merged = 0;

	++T->nTransient;

	if(transient->startMJD > T->D->job->jobStop || transient->stopMJD < T->D->job->jobStart)
	{
		char message[DIFX_MESSAGE_LENGTH];
		
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"Transient received out of job time range ([%12.6f,%12.6f] not in [%12.6f,%12.6f])",
			transient->startMJD, transient->stopMJD,
			T->D->job->jobStart, T->D->job->jobStop);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
	}
	else
	{
#if 0
		/* not really sensible to do this as DMs may not match */

		if(T->nEvent > 0)
		{
			printf("Cool.  Merging two events!\n");
			if(min(T->event[T->nEvent-1].stopMJD,  transient->stopMJD) >= 
			   max(T->event[T->nEvent-1].startMJD, transient->startMJD))
			{
				T->event[T->nEvent-1].startMJD = min(T->event[T->nEvent-1].startMJD, transient->startMJD);
				T->event[T->nEvent-1].stopMJD  = max(T->event[T->nEvent-1].stopMJD,  transient->stopMJD);
				T->event[T->nEvent-1].priority = sqrt(T->event[T->nEvent-1].priority*T->event[T->nEvent-1].priority +
					transient->priority*transient->priority);
			}
			merged = 1;
		}
#endif
		if(merged == 0)
		{
			if(T->nEvent >= MAX_EVENTS + EXTRA_EVENTS)
			{
				sortEvents(T);
			}

			T->event[T->nEvent].startMJD = transient->startMJD;
			T->event[T->nEvent].stopMJD  = transient->stopMJD;
			T->event[T->nEvent].priority = transient->priority;
			T->event[T->nEvent].dm       = transient->dm;
			++T->nEvent;
		}
	}

	T->nMerged += merged;
}

static void genDifxFiles(const TransientWrapperData *T, int eventId)
{
	const int MaxCommandLength = 512;
	DifxInput *newD;
	int i, l, v;
	FILE *out;
	double mjd;
	double mjd1, mjd2;
	char command[MaxCommandLength];
	char outDir[DIFXIO_FILENAME_LENGTH];
	char origDir[DIFXIO_FILENAME_LENGTH];
	char baseNameFB[DIFXIO_FILENAME_LENGTH];
	char baseNameIm[DIFXIO_FILENAME_LENGTH];
	char fileName[DIFXIO_FILENAME_LENGTH];
	DifxScan *S;
	int scanId, configId;

	if(T->verbose > 0)
	{
		printf("Generating DiFX files for eventId=%d\n", eventId);
		fflush(stdout);
	}

	/* use center of range for scan id */
	mjd = (T->event[eventId].startMJD + T->event[eventId].stopMJD)/2.0;

	mjd1 = T->event[eventId].startMJD;
	mjd2 = T->event[eventId].stopMJD;

	mjd1 = floor(mjd1*86400.0)/86400.0;
	mjd2 = ceil(mjd2*86400.0)/86400.0;

	scanId = DifxInputGetScanIdByJobId(T->D, mjd, 0);
	if(scanId < 0 || scanId >= T->D->nScan)
	{
		fprintf(stderr, "Error: mjd=%f not claimed by any scan\n", mjd);
		
		return;
	}
	configId = T->D->scan[scanId].configId;
	if(configId < 0 || configId >= T->D->nConfig)
	{
		fprintf(stderr, "Error: configId=%d for scanId=%d\n", configId, scanId);

		return;
	}

	v = snprintf(outDir, DIFXIO_FILENAME_LENGTH, "%s/%s%s/%s",
		T->conf->path, T->D->job->obsCode, T->D->job->obsSession, T->identifier);

	snprintf(baseNameFB, DIFXIO_FILENAME_LENGTH, "%s/transientfb_%03d", outDir, eventId+1);
	snprintf(baseNameIm, DIFXIO_FILENAME_LENGTH, "%s/transientim_%03d", outDir, eventId+1);

	snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s.event", baseNameFB);
	out = fopen(fileName, "w");
	fprintf(out, "job %s.input\n", baseNameFB);
	fprintf(out, "imagejob %s.input\n", baseNameIm);
	fprintf(out, "mjdStart %14.8f\n", T->event[eventId].startMJD);
	fprintf(out, "mjdEnd %14.8f\n", T->event[eventId].stopMJD);
	fprintf(out, "priority %8.6f\n", T->event[eventId].priority);
	fprintf(out, "DM %8.6f\n", T->event[eventId].dm);
	fclose(out);

	newD = loadDifxInput(T->filePrefix);

	strcpy(origDir, T->filePrefix);
	l = strlen(origDir);
	for(i = l-1; i > 0; --i)
	{
		if(origDir[i] == '/')
		{
			origDir[i+1] = 0;
			break;
		}
	}

	/* summarize the original job for record */
	snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s.difxio.orig", baseNameFB);
	out = fopen(fileName, "w");
	fprintDifxInput(out, newD);
	fclose(out);

	/* MODIFY THE CONTENTS TO MAKE A NEW JOB */

	/* First change the name of the job and all of the paths */
	snprintf(newD->job->inputFile,   DIFXIO_FILENAME_LENGTH, "%s.input", baseNameFB);
	snprintf(newD->job->calcFile,    DIFXIO_FILENAME_LENGTH, "%s.calc", baseNameFB);
	snprintf(newD->job->imFile,      DIFXIO_FILENAME_LENGTH, "%s.im", baseNameFB);
	snprintf(newD->job->flagFile,    DIFXIO_FILENAME_LENGTH, "%s.flag", baseNameFB);
	snprintf(newD->job->threadsFile, DIFXIO_FILENAME_LENGTH, "%s.threads", baseNameFB);
	snprintf(newD->job->outputFile,  DIFXIO_FILENAME_LENGTH, "%s.difx", baseNameFB);

	/* Then select the appropriate scan and reduce its timerange */
	S = newDifxScanArray(1);
	copyDifxScan(S, newD->scan+scanId, 0, 0, 0, 0);
	deleteDifxScanArray(newD->scan, newD->nScan);
	newD->scan = S;
	newD->nScan = 1;
	newD->mjdStart = newD->scan->mjdStart = newD->job->jobStart = newD->job->mjdStart = mjd1;
	newD->mjdStop  = newD->scan->mjdEnd   = newD->job->jobStop  = mjd2;
	newD->job->duration = (int)(86400.0*(mjd2-mjd1) + 0.00001);
	newD->scan->startSeconds = 0;
	newD->scan->durSeconds = (int)ceil(newD->job->duration);

	if(newD->nPulsar > 0)
	{
		deleteDifxPulsarArray(newD->pulsar, newD->nPulsar);
		newD->nPulsar = 0;
		newD->pulsar = 0;
	}
	for(int c = 0; c < newD->nConfig; ++c)
	{
		newD->config[c].pulsarId = -1;
	}

	/* Then change all data sources to FILE and point to those files */
	for(int dsId = 0; dsId < newD->nDatastream; ++dsId)
	{
		if(newD->datastream[dsId].dataSource == DataSourceModule)
		{
			newD->datastream[dsId].dataSource = DataSourceFile;
			if(newD->datastream[dsId].nFile == 1)
			{
				snprintf(fileName, DIFXIO_FILENAME_LENGTH,
					"%s/%d/%s_%12.6f_%12.6f_0", outDir, dsId+1, 
					newD->datastream[dsId].file[0],
					T->event[eventId].startMJD,
					T->event[eventId].stopMJD);
				free(newD->datastream[dsId].file[0]);
				newD->datastream[dsId].file[0] = strdup(fileName);
			}
		}
	}

	/* Finally change correlation parameters */
	newD->config[configId].tInt = T->conf->recorr_tInt;
	newD->config[configId].subintNS = (int)(T->conf->recorr_tInt*1.0e9 + 0.5);
	for(int freqId = 0; freqId <= newD->nFreq; ++freqId)
	{
		newD->freq[freqId].nChan = T->conf->recorr_nChan;
		newD->freq[freqId].specAvg = T->conf->recorr_specAvg;
	}
	DifxInputAllocThreads(newD, 2);
	DifxInputSetThreads(newD, 1);

	/* And write it! */
	writeDifxInput(newD);
	writeDifxCalc(newD);
	writeDifxIM(newD);
	DifxInputWriteThreads(newD);

	snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s.machines", baseNameFB);
	out = fopen(fileName, "w");
	for(i = 0; i < newD->nDatastream+3; ++i)
	{
		fprintf(out, "%s\n", T->conf->vfastrHost);
	}
	fclose(out);


	/* AGAIN MODIFY THE CONTENTS TO MAKE A NEW JOB; this one for imaging */

	/* First change the name of the job and all of the paths */
	snprintf(newD->job->inputFile,   DIFXIO_FILENAME_LENGTH, "%s.input", baseNameIm);
	snprintf(newD->job->calcFile,    DIFXIO_FILENAME_LENGTH, "%s.calc", baseNameIm);
	snprintf(newD->job->imFile,      DIFXIO_FILENAME_LENGTH, "%s.im", baseNameIm);
	snprintf(newD->job->flagFile,    DIFXIO_FILENAME_LENGTH, "%s.flag", baseNameIm);
	snprintf(newD->job->threadsFile, DIFXIO_FILENAME_LENGTH, "%s.threads", baseNameIm);
	snprintf(newD->job->outputFile,  DIFXIO_FILENAME_LENGTH, "%s.difx", baseNameIm);
	newD->config[configId].tInt = T->conf->recorr2_tInt;
	newD->config[configId].subintNS = (int)(T->conf->recorr2_tInt*1.0e9 + 0.5);
	for(int freqId = 0; freqId <= newD->nFreq; ++freqId)
	{
		newD->freq[freqId].nChan = T->conf->recorr2_nChan;
		newD->freq[freqId].specAvg = T->conf->recorr2_specAvg;
	}

	/* use pulsar gate to selectively correlate */
	newD->pulsar = newDifxPulsarArray(1);
	newD->pulsar[0].nPolyco = 0;
	newD->pulsar[0].polyco = 0;
	newD->pulsar[0].nBin = 2;
	newD->pulsar[0].binEnd = (double *)calloc(newD->pulsar[0].nBin, sizeof(double));
	newD->pulsar[0].binWeight = (double *)calloc(newD->pulsar[0].nBin, sizeof(double));
	newD->pulsar[0].scrunch = 1;
	snprintf(newD->pulsar[0].fileName, DIFXIO_FILENAME_LENGTH, "%s.binconfig", baseNameIm);
	for(int c = 0; c < newD->nConfig; ++c)
	{
		newD->config[c].pulsarId = 0;
	}
		
	writeDifxInput(newD);
	writeDifxCalc(newD);
	writeDifxIM(newD);
	DifxInputWriteThreads(newD);
	snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s.machines", baseNameIm);
	out = fopen(fileName, "w");
	for(i = 0; i < newD->nDatastream+3; ++i)
	{
		fprintf(out, "%s\n", T->conf->vfastrHost);
	}
	fclose(out);

	// FIXME: this causes a crash
	//deleteDifxInput(newD);

	/* copy calibration data as well */
	snprintf(command, MaxCommandLength, "cp %s/{flag,tsys,weather,pcal} %s", origDir, outDir);
	if(T->verbose)
	{
		logExecute(command);
	}
	system(command);

	/* now that the jobs are ready to run, send a message to transient_daemon */
	difxMessageSendDifxParameterTo("queuerecorr", baseNameFB, T->conf->vfastrHost);
	if(T->verbose)
	{
		printf("Sent queuerecorr to %s for %s\n", T->conf->vfastrHost, baseNameFB);
		fflush(stdout);
	}
}

int copyBasebandData(const TransientWrapperData *T)
{
	const unsigned int MaxCommandLength = 1024;
	char command[MaxCommandLength];
	char message[DIFX_MESSAGE_LENGTH];
	time_t t1, t2;
	char outDir[DIFXIO_FILENAME_LENGTH];
	int v, e;

	v = snprintf(outDir, DIFXIO_FILENAME_LENGTH, 
		"%s/%s%s/%s/%d",
		T->conf->path, T->D->job->obsCode, T->D->job->obsSession, T->identifier, T->rank);
	
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "Error: pathname is too long (%d > %d)\n", v, DIFXIO_FILENAME_LENGTH-1);
		
		return 0;
	}

	snprintf(command, MaxCommandLength, "mkdir -p %s", outDir);
	if(T->verbose)
	{
		logExecute(command);
	}
	v = system(command);
	if(v == -1)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Error: cannot execute %s\n", command);
		fprintf(stderr, "%s\n", message);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		return 0;
	}

	t1 = t2 = time(0);
	for(e = 0; e < T->nEvent; ++e)
	{
		if(t2-t1 > T->executeTime*T->conf->maxCopyOverhead)
		{
			break;
		}

		if(T->D->datastream[T->rank-1].dataSource == DataSourceModule)
		{
			snprintf(command, MaxCommandLength, 
				"mk5cp Active %12.6f_%12.6f %s", 
				T->event[e].startMJD, T->event[e].stopMJD,
				outDir);

			snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			if(T->verbose)
			{
				logExecute(command);
			}
			v = system(command);
		}

		if(T->rank == 1) /* only generate files once */
		{
			genDifxFiles(T, e);
		}
		
		if(v == -1)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Error: cannot execute %s\n", command);
			fprintf(stderr, "%s\n", message);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

			return e;
		}

		t2 = time(0);
	}
	
	if(T->rank == 1) /* only set perms once */
	{
		/* finally, chgrp the output directory, indirectly */
		snprintf(command, MaxCommandLength, "mk5control reown_vfastr%s/%s%s/%s %s", 
			T->conf->path, T->D->job->obsCode, T->D->job->obsSession, T->identifier, 
			T->conf->vfastrHost);
		if(T->verbose)
		{
			logExecute(command);
		}
		system(command);
	}


	return e;
}

TransientWrapperConf *newTransientWrapperConf()
{
	TransientWrapperConf *conf;

	conf = (TransientWrapperConf *)calloc(1, sizeof(TransientWrapperConf));

	if(!conf)
	{
		fprintf(stderr, "Error: cannot allocate a Transient Wrapper Configuration Object\n");

		exit(EXIT_FAILURE);
	}
	conf->enable = default_enable;
	snprintf(conf->vfastrHost, DIFX_MESSAGE_PARAM_LENGTH, "%s", default_vfastrHost);

	conf->minFreeDiskMB = default_minFreeMB;
	conf->maxCopyOverhead = default_maxCopyOverhead;
	snprintf(conf->path, DIFX_MESSAGE_FILENAME_LENGTH, "%s", default_outputPath);

	conf->recorr_tInt = default_recorr_tInt;
	conf->recorr_nChan = default_recorr_nChan;
	conf->recorr_specAvg = default_recorr_specAvg;

	conf->recorr2_tInt = default_recorr2_tInt;
	conf->recorr2_nChan = default_recorr2_nChan;
	conf->recorr2_specAvg = default_recorr2_specAvg;
	
	return conf;
}

void deleteTransientWrapperConf(TransientWrapperConf *conf)
{
	free(conf);
}

int loadTransientWrapperConf(TransientWrapperConf *conf, const char *filename)
{
	FILE *in;
	int n;
	char line[DIFX_MESSAGE_COMMENT_LENGTH];
	char A[DIFX_MESSAGE_COMMENT_LENGTH];
	char B[DIFX_MESSAGE_COMMENT_LENGTH];
	char C[DIFX_MESSAGE_COMMENT_LENGTH];

	in = fopen(filename, "r");
	if(!in)
	{
		return -1;
	}

	for(int l = 1;; ++l)
	{
		fgets(line, DIFX_MESSAGE_COMMENT_LENGTH-1, in);
		if(feof(in))
		{
			break;
		}
		for(int i = 0; line[i]; ++i)
		{
			if(line[i] == '#')	/* break at a comment charcter */
			{
				line[i] = 0;
				break;
			}
			if(line[i] == '=')	/* turn equal signs into spaces */
			{
				line[i] = ' ';
			}
		}
		n = sscanf(line, "%s %s %s", A, B, C);
		if(n <= 0)
		{
			continue;
		}
		if(n != 2)
		{
			fprintf(stderr, "Config file %s : parse error line %d n=%d\n", filename, l, n);
		}

		/* transient_wrapper specific code here */
		if(strcmp(A, "vfastr_enable") == 0)
		{
			conf->enable = atoi(B);
		}
		else if(strcmp(A, "vfastr_host") == 0)
		{
			strncpy(conf->vfastrHost, B, DIFX_MESSAGE_FILENAME_LENGTH-1);
		}
		else if(strcmp(A, "baseband_copy_overhead") == 0)
		{
			conf->maxCopyOverhead = atof(B);
		}
		else if(strcmp(A, "baseband_copy_min_space") == 0)
		{
			conf->minFreeDiskMB = atof(B);
		}
		else if(strcmp(A, "baseband_copy_path") == 0)
		{
			strncpy(conf->path, B, DIFX_MESSAGE_FILENAME_LENGTH-1);
		}
		else if(strcmp(A, "recorr_int_time") == 0)
		{
			conf->recorr_tInt = atof(B);
		}
		else if(strcmp(A, "recorr_n_chan") == 0)
		{
			conf->recorr_nChan = atoi(B);
		}
		else if(strcmp(A, "recorr_chan_avg") == 0)
		{
			conf->recorr_specAvg = atoi(B);
		}
		else if(strcmp(A, "image_recorr_int_time") == 0)
		{
			conf->recorr2_tInt = atof(B);
		}
		else if(strcmp(A, "image_recorr_n_chan") == 0)
		{
			conf->recorr2_nChan = atoi(B);
		}
		else if(strcmp(A, "image_recorr_chan_avg") == 0)
		{
			conf->recorr2_specAvg = atoi(B);
		}
	}

	fclose(in);

	return 0;
}

void printTransientWrapperConf(const TransientWrapperConf *conf)
{
	printf("TransientWrapperConf [%p]\n", conf);
	printf("  enable = %d\n", conf->enable);
	printf("  maxCopyOverhead = %f\n", conf->maxCopyOverhead);
	printf("  minFreeDiskMB = %f\n", conf->minFreeDiskMB);
	printf("  path = %s\n", conf->path);
	printf("  hires recorrelation int time (s) = %f\n", conf->recorr_tInt);
	printf("  hires recorrelation num chans = %d\n", conf->recorr_nChan);
	printf("  hires recorrelation specAvg = %d\n", conf->recorr_specAvg);
	printf("  image recorrelation int time (s) = %f\n", conf->recorr2_tInt);
	printf("  image recorrelation num chans = %d\n", conf->recorr2_nChan);
	printf("  image recorrelation specAvg = %d\n", conf->recorr2_specAvg);
}
