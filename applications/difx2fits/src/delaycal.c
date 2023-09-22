#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "delaycal.h"

void resetDelayCalCache(DelayCalCache *dcc)
{
	memset(dcc, 0, sizeof(DelayCalCache));
}

void printDelayCalCache(const DelayCalCache *dcc)
{
	printf("DelayCalCache: lastMJD=%14.8f index0=%d index1=%d weight0=%5.3f weight1=%5.3f\n", dcc->lastMJD, dcc->index0, dcc->index1, dcc->weight0, dcc->weight1);
}


DelayCal *loadDelayCal(const char *filename, const DifxInput *D)
{
	DelayCal *dc;
	FILE *in;
	char line[1024];
	double lastMJD = 0;	/* [day] */
	double mjd;		/* [day] */
	int day;		/* [day] */
	double hour;		/* [hr] */
	int srcId;
	char src[32];
	int antId;
	char ant[32];
	int p, n, nbbc;
	int slot;

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "\nError: cannot open %s for read\n", filename);

		return 0;
	}

	dc = (DelayCal *)calloc(1, sizeof(DelayCal));
	dc->nPol = D->nPol;
	for(p = 0; p < D->nPol; ++p)
	{
		dc->pols[p] = D->polPair[p];
	}
	dc->pols[p] = 0;
	dc->nAntenna = D->nAntenna;
	while(1)
	{
		char *rv;

		rv = fgets(line, 1023, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		n = sscanf(line, "%d%lf%d%31s%d%31s%d", &day, &hour, &srcId, src, &antId, ant, &nbbc);
		if(n == 7)
		{
			if(nbbc > dc->nBBC)
			{
				dc->nBBC = nbbc;
			}
			mjd = day + hour/24.0;
			if(mjd != lastMJD)
			{
				lastMJD = mjd;
				++dc->nSlot;
			}
		}
	}

	dc->data = (DelaySolution *)calloc(dc->nSlot, sizeof(DelaySolution));
	for(slot = 0; slot < dc->nSlot; ++slot)
	{
		int a;

		dc->data[slot].phase = (double **)calloc(dc->nAntenna, sizeof(double *));
		dc->data[slot].delay = (double **)calloc(dc->nAntenna, sizeof(double *));
		for(a = 0; a < dc->nAntenna; ++a)
		{
			dc->data[slot].phase[a] = (double *)calloc(dc->nBBC, sizeof(double));
			dc->data[slot].delay[a] = (double *)calloc(dc->nBBC, sizeof(double));
		}
	}
	
	rewind(in);
	slot = -1;
	while(1)
	{
		char *rv;
		int p, b;
		char *l;

		rv = fgets(line, 1023, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		n = sscanf(line, "%d%lf%d%s%d%s%d%n", &day, &hour, &srcId, src, &antId, ant, &nbbc, &p);
		if(n == 7)
		{
			mjd = day + hour/24.0;
			if(mjd != lastMJD)
			{
				lastMJD = mjd;
				++slot;
				dc->data[slot].mjd = mjd;
			}

			/* get antenna id */
			for(antId = 0; antId < dc->nAntenna; ++antId)
			{
				if(strcasecmp(D->antenna[antId].name, ant) == 0)
				{
					break;
				}
			}
			if(antId >= dc->nAntenna)
			{
				/* No antenna match; ignore */
				continue;
			}

			l = line + p;
			for(b = 0; b < nbbc; ++b)
			{
				double delay;	/* [ns] native units in aapd file */
				double phase;	/* [deg] native units in aapd file */
				double rate;	/* [Hz] native units in aapd file */
				n = sscanf(l, "%lf%lf%lf%n", &delay, &phase, &rate, &p);
				if(n == 3)
				{
					l += p;
					dc->data[slot].delay[antId][b] = delay*0.001;		/* convert to [us] from [ns] */
					dc->data[slot].phase[antId][b] = phase*M_PI/180.0;	/* convert to [rad] from [deg] */
				}
				else
				{
					break;
				}
			}
		}
	}

	fclose(in);

	return dc;
}

void deleteDelayCal(DelayCal *dc)
{
	if(dc)
	{
		free(dc);
	}
}

void printDelayCal(const DelayCal *dc)
{
	int s;

	printf("DelayCal [%p]\n", dc);
	if(!dc)
	{
		fprintf(stderr, "Developer error: printDelayCal() called with Null argument.\n");
		
		return;
	}

	printf("  nAntenna = %d\n", dc->nAntenna);
	printf("  nBBC = %d\n", dc->nBBC);
	printf("  nSlot = %d\n", dc->nSlot);
	printf("  nPol = %d  [%s]\n", dc->nPol, dc->pols);
	for(s = 0; s < dc->nSlot; ++s)
	{
		int a;

		printf("    slot[%d]\n", s);
		printf("      mjd = %14.8f\n", dc->data[s].mjd);
		for(a = 0; a < dc->nAntenna; ++a)
		{
			int b;

			printf("      Ant[%d] delay =", a);
			for(b = 0; b < dc->nBBC; ++b)
			{
				if(b != 0)
				{
					printf(",");
				}
				printf(" %4.2f", dc->data[s].delay[a][b]*1000.0);
			}
			printf(" ns\n");
		}
	}
}

/* Inputs:

   deltaFreq [MHz] 
   mjd [day]   
   
*/
int applyDelayCal(float complex *spectrum, int nChan, double deltaFreq, int freqId, int antId1, char pol1, int antId2, char pol2, double mjd, const DelayCal *dc, DelayCalCache *dcc)
{
	double d1, d2;	/* [us] */
	int c;
	int bbc1;
	int bbc2;

	if(antId1 == antId2 && pol1 == pol2)
	{
		return 0;
	}

	/* When MJD changes, update indexes into the DelayCal data array and interpolation weighting */
	if(mjd != dcc->lastMJD)
	{
		int highestSlot;

		highestSlot = dc->nSlot - 1;
		dcc->lastMJD = mjd;

		if(mjd <= dc->data[0].mjd)
		{
			dcc->index0 = 0;
			dcc->index1 = 0;
			dcc->weight0 = 1.0;
			dcc->weight1 = 0.0;
		}
		else if(mjd >= dc->data[highestSlot].mjd)
		{
			dcc->index0 = highestSlot;
			dcc->index1 = highestSlot;
			dcc->weight0 = 0.0;
			dcc->weight1 = 1.0;
		}
		else
		{
			double deltaMJD;

			if(mjd < dc->data[dcc->index0].mjd)	/* this should only happen when peeking at files to determine their times */
			{
				dcc->index1 = 1;
			}
			while(mjd > dc->data[dcc->index1].mjd)
			{
				++dcc->index1;
			}
			dcc->index0 = dcc->index1 - 1;

			deltaMJD = dc->data[dcc->index1].mjd - dc->data[dcc->index0].mjd;
			dcc->weight0 = (dc->data[dcc->index1].mjd - mjd)/deltaMJD;
			dcc->weight1 = (mjd - dc->data[dcc->index0].mjd)/deltaMJD;
		}
	}

	bbc1 = freqId * dc->nPol + ((pol1 == dc->pols[0]) ? 0 : 1);
	bbc2 = freqId * dc->nPol + ((pol2 == dc->pols[0]) ? 0 : 1);

	d1 = dc->data[dcc->index0].delay[antId1][bbc1]*dcc->weight0 + dc->data[dcc->index1].phase[antId1][bbc1]*dcc->weight1;
	d2 = dc->data[dcc->index0].delay[antId2][bbc2]*dcc->weight0 + dc->data[dcc->index1].phase[antId1][bbc2]*dcc->weight1;

	for(c = 0; c < nChan; ++c)
	{
		double phi;
		complex phasor;

		phi = 2*M_PI*c*deltaFreq*(d2-d1);

		phasor = cos(phi) + 1.0I*sin(phi);
		spectrum[c] *= phasor;
	}

	return 0;
}
