/***************************************************************************
 *   Copyright (C) 2011-2013 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/parsevis.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include <time.h>
#include <ctype.h>
#include "difx_tcal.h"

/* Note! Keep this in sync with enum DifxTcalType in difx_tcal.h */
const char difxTcalTypeString[][MAX_DIFX_TCAL_TYPE_LENGTH] =
{
	"None",
	"Constant",
	"VLBA",
	"DIFX"
};

/* Note! Keep this in sync with enum DifxTcalInterpolation in difx_tcal.h */
const char difxTcalInterpolationString[][MAX_DIFX_TCAL_TYPE_LENGTH] =
{
	"Nearest",
	"Linear"
};

static int loadDifxTcalVLBA(DifxTcal *dt, const char *antenna, const char *receiver);
static int loadDifxTcalDIFX(DifxTcal *dt);

DifxTcal *newDifxTcal()
{
	DifxTcal *dt;

	dt = (DifxTcal *)calloc(1, sizeof(DifxTcal));
	if(!dt)
	{
		return 0;
	}

	/* some defaults */
	dt->_nGroupAlloc = 16;

	/* allocate initial space for Tcal groups */
	dt->group = (DifxTcalGroup *)malloc(dt->_nGroupAlloc*sizeof(DifxTcalGroup));
	if(!dt->group)
	{
		free(dt);

		return 0;
	}

	dt->verbose = 1;

	return dt;
}

void deleteDifxTcal(DifxTcal *dt)
{
	if(dt)
	{
		if(dt->group)
		{
			int g;

			for(g = 0; g < dt->nGroup; ++g)
			{
				if(dt->group[g].value)
				{
					if(dt->group[g]._freqRangeExceeded)
					{
						printf("Warning: Tcals for antena %s receiver %s did not span entire freq range.\n", dt->group[g].antenna, dt->group[g].receiver);
					}
					free(dt->group[g].value);
					dt->group[g].value = 0;
					dt->group[g].nValue = 0;
					dt->group[g]._nValueAlloc = 0;
				}
			}
			free(dt->group);
			dt->group = 0;
			dt->nGroup = 0;
			dt->_nGroupAlloc = 0;
		}
		free(dt);
	}
}

int getDifxTcalGroupIndex(const DifxTcal *dt, double mjd, const char *antenna, const char *receiver)
{
	int best = -1;
	float deltamjd = 1e10;
	
	/* a zero MJD implies take latest */
	if(mjd < 1.0)
	{
		mjd = 1e9;
	}

	if(dt && dt->nGroup > 0)
	{
		int g;

		for(g = 0; g < dt->nGroup; ++g)
		{
			int ma, mr;

			if(mjd < dt->group[g].mjdStart)
			{
				continue;
			}

			ma = strcasecmp(antenna,  dt->group[g].antenna);
			mr = strcasecmp(receiver, dt->group[g].receiver);

			if(ma == 0 && mr == 0)
			{
				if(mjd - dt->group[g].mjdStart < deltamjd)
				{
					deltamjd = mjd - dt->group[g].mjdStart;
					best = g;
				}
			}
			else if(best < 0)	/* look for partial matches */
			{
				if( (ma == 0 && dt->group[g].receiver[0] == 0) ||
				    (mr == 0 && dt->group[g].antenna[0] == 0) ||
				    (dt->group[g].receiver[0] == 0 && dt->group[g].antenna[0] == 0) )
				{
					best = g;
				}
			}
		}
	}

	return best;
}

int addDifxTcalGroup(DifxTcal *dt)
{
	if(dt)
	{
		int n;

		n = dt->nGroup;

		++dt->nGroup;
		if(dt->nGroup > dt->_nGroupAlloc)
		{
			DifxTcalGroup *tmp;
			int oldNAlloc;

			oldNAlloc = dt->_nGroupAlloc;
			dt->_nGroupAlloc *= 2;
			tmp = (DifxTcalGroup *)realloc(dt->group, dt->_nGroupAlloc*sizeof(DifxTcalGroup));
			if(!tmp)
			{
				dt->_nGroupAlloc = oldNAlloc;
				dt->nGroup = n;

				return -2;
			}
			else
			{
				dt->group = tmp;
			}
		}
		dt->group[n].antenna[0] = 0;
		dt->group[n].receiver[0] = 0;
		dt->group[n].mjdStart = 0.0;
		dt->group[n].serial = -1;
		dt->group[n].interpolation = DifxTcalInterpolationLinear;	/* a good default, probably */
		dt->group[n].nValue = 0;
		dt->group[n]._nValueAlloc = 16;
		dt->group[n]._freqRangeExceeded = 0;
		dt->group[n].value = (DifxTcalValue *)malloc(dt->group[n]._nValueAlloc*sizeof(DifxTcalValue));
		if(!dt->group[n].value)
		{
			dt->nGroup = n;

			return -3;
		}

		return dt->nGroup - 1;
	}
	else
	{
		return -1;
	}
}

static float getDifxTcalNearest(const DifxTcalGroup *group, char pol, float freq)
{
	float tcal = 0.0;

	if(group)
	{
		int v;
		float best = 1e12;

		for(v = 0; v < group->nValue; ++v)
		{
			int p;
			
			for(p = 0; p < 2; ++p)
			{
				if(group->value[v].pol[p] == pol)
				{
					float df;

					df = fabs(group->value[v].freq - freq);
					if(df < best)
					{
						best = df;
						tcal = group->value[v].tcal[p];
					}
				}
			}
		}
	}

	return tcal;
}

static float getDifxTcalLinear(DifxTcalGroup *group, char pol, float freq)
{
	float tcalLow = 0.0;
	float tcalHigh = 0.0;
	float freqLow = 0.0;
	float freqHigh = 1e12;

	if(group)
	{
		int v;

		for(v = 0; v < group->nValue; ++v)
		{
			int p;

			for(p = 0; p < 2; ++p)
			{
				if(group->value[v].pol[p] == pol)
				{
					float df;

					df = group->value[v].freq - freq;
					if(df < 0)
					{
						if(-df < freq-freqLow)
						{
							/* a better low freq option */
							freqLow = group->value[v].freq;
							tcalLow = group->value[v].tcal[p];
						}
					}
					else if(df > 0)
					{
						if(df < freqHigh-freq)
						{
							/* a better high freq option */
							freqHigh = group->value[v].freq;
							tcalHigh = group->value[v].tcal[p];
						}
					}
					else
					{
						freqLow = freqHigh = group->value[v].freq;
						tcalLow = tcalHigh = group->value[v].tcal[p];
					}
				}
			}
		}
	}

	if(tcalLow > 0.0 && tcalHigh > 0.0)
	{
		float f = 1.0;

		if(freqHigh != freqLow)
		{
			f = (freq - freqLow)/(freqHigh - freqLow);
		}

		return f*tcalHigh + (1.0-f)*tcalLow;
	}
	else if(tcalLow > 0.0)
	{
		++group->_freqRangeExceeded;

		return tcalLow;
	}
	else if(tcalHigh > 0.0)
	{
		++group->_freqRangeExceeded;

		return tcalHigh;
	}
	else
	{
		return 0;
	}
}

float getDifxTcal(DifxTcal *dt, double mjd, const char *antenna, const char *receiver, char pol, float freq)
{
	float tcal = 0.0;

	if(dt)
	{
		int g;

		if(dt->type == DifxTcalTypeVLBA && receiver[0] == 0)
		{
			receiver = defaultVLBAReceiver(freq);
		}

		g = getDifxTcalGroupIndex(dt, mjd, antenna, receiver);
		if(g < 0)
		{
			switch(dt->type)
			{
			case DifxTcalTypeVLBA:
				loadDifxTcalVLBA(dt, antenna, receiver);
				break;
			default:
				break;
			}
			g = getDifxTcalGroupIndex(dt, mjd, antenna, receiver);
		}
		if(g >= 0)
		{
			switch(dt->group[g].interpolation)
			{
			case DifxTcalInterpolationNearest:
				tcal = getDifxTcalNearest(dt->group+g, pol, freq);
				break;
			case DifxTcalInterpolationLinear:
				tcal = getDifxTcalLinear(dt->group+g, pol, freq);
				break;
			default:
				break;
			}
		}
	}

	return tcal;
}

int addDifxTcalValue(DifxTcalGroup* group, float freq, float tcal1, char pol1, float tcal2, char pol2)
{
	if(group)
	{
		int n;

		n = group->nValue;

		++group->nValue;
		if(group->nValue > group->_nValueAlloc)
		{
			DifxTcalValue *tmp;
			int oldNAlloc;

			oldNAlloc = group->_nValueAlloc;
			group->_nValueAlloc *= 2;
			tmp = (DifxTcalValue *)realloc(group->value, group->_nValueAlloc*sizeof(DifxTcalValue));
			if(!tmp)
			{
				group->_nValueAlloc = oldNAlloc;
				group->nValue = n;

				return -2;
			}
			else
			{
				group->value = tmp;
			}
		}
		group->value[n].freq = freq;
		group->value[n].tcal[0] = tcal1;
		group->value[n].tcal[1] = tcal2;
		group->value[n].pol[0] = pol1;
		group->value[n].pol[1] = pol2;

		return group->nValue;
	}
	else
	{
		return -1;
	}
}

int setDifxTcalInterpolationType(DifxTcalGroup *group, enum DifxTcalInterpolation interpolation)
{
	if(group)
	{
		group->interpolation = interpolation;

		return 0;
	}
	else
	{
		return -1;
	}
}

static void fprintDifxTcalCore(FILE *out, const DifxTcal *dt, int summary)
{
	fprintf(out, "DifxTcal [%p]:\n", dt);
	
	if(dt)
	{
		int g;
		
		fprintf(out, "  type = %d (%s)\n", dt->type, difxTcalTypeString[dt->type]);
		fprintf(out, "  path = %s\n", dt->path);
		fprintf(out, "  nGroup = %d\n", dt->nGroup);

		for(g = 0; g < dt->nGroup; ++g)
		{
			fprintf(out, "  Group %d:\n", g);
			fprintf(out, "    Antenna = %s\n", dt->group[g].antenna);
			fprintf(out, "    Receiver = %s\n", dt->group[g].receiver);
			fprintf(out, "    Rx serial # = %d\n", dt->group[g].serial);
			fprintf(out, "    MJD Start = %14.8f\n", dt->group[g].mjdStart);
			fprintf(out, "    Interpolation = %d (%s)\n", dt->group[g].interpolation, difxTcalInterpolationString[dt->group[g].interpolation]);
			if(summary)
			{
				fprintf(out, "    # values = %d\n", dt->group[g].nValue);
			}
			else
			{
				int v;

				fprintf(out, "    value = %p\n", dt->group[g].value);
				for(v = 0; v < dt->group[g].nValue; ++v)
				{
					fprintf(out, "      value %3d  freq=%f MHz  Tcal(%c)=%f  Tcal(%c)=%f\n", v, 
						dt->group[g].value[v].freq,
						dt->group[g].value[v].pol[0],
						dt->group[g].value[v].tcal[0],
						dt->group[g].value[v].pol[1],
						dt->group[g].value[v].tcal[1]);
				}
			}
		}
	}
}

void printDifxTcal(const DifxTcal *dt)
{
	fprintDifxTcalCore(stdout, dt, 0);
}

void summarizeDifxTcal(const DifxTcal *dt)
{
	fprintDifxTcalCore(stdout, dt, 1);
}

void fprintDifxTcal(FILE *out, const DifxTcal *dt)
{
	fprintDifxTcalCore(out, dt, 0);
}

void fsummarizeDifxTcal(FILE *out, const DifxTcal *dt)
{
	fprintDifxTcalCore(out, dt, 1);
}

/* below here are site-specific functions */

/* 1. For a constant Tcal value over all frquencies */

int setDifxTcalConstant(DifxTcal *dt, float tcal1, char pol1, float tcal2, char pol2)
{
	if(dt)
	{
		int g;

		if(dt->type != DifxTcalNone)
		{
			return -2;
		}

		g = addDifxTcalGroup(dt);
		
		if(g < 0)
		{
			return -3;
		}
		addDifxTcalValue(dt->group+g, 1.0, tcal1, pol1, tcal2, pol2);
		dt->type = DifxTcalTypeConstant;
		setDifxTcalInterpolationType(dt->group+g, DifxTcalInterpolationNearest);
	}
	else
	{
		return -1;
	}

	return 0;
}

/* 2. For VLBA: */

int setDifxTcalVLBA(DifxTcal *dt, const char *tcalPath)
{
	if(dt)
	{
		int v;

		if(dt->type != DifxTcalNone)
		{
			return -2;
		}

		v = snprintf(dt->path, DIFX_TCAL_FILENAME_LENGTH, "%s", tcalPath);
		if(v >= DIFX_TCAL_FILENAME_LENGTH)
		{
			fprintf(stderr, "Developer error: setDifxTcalVLBA: DIFX_TCAL_FILENAME_LENGTH is too short (%d); needs to be %d or more\n", DIFX_TCAL_FILENAME_LENGTH, v+1);
			

			return -3;
		}
		dt->type = DifxTcalTypeVLBA;
	}
	else
	{
		return -1;
	}

	return 0;
}

const char *defaultVLBAReceiver(float freq)		/* freq in MHz */
{
	if(     freq < 1000.0)  { return "90cm"; }
	else if(freq < 2000.0)  { return "20cm"; }
	else if(freq < 3800.0)  { return "13cm"; }
	else if(freq < 7900.0)  { return "6cm"; }
	else if(freq < 10000.0) { return "4cm"; }
	else if(freq < 18000.0) { return "2cm"; }
	else if(freq < 28000.0) { return "1cm"; }
	else if(freq < 55000.0) { return "7mm"; }
	else                    { return "3mm"; }
}

const char *defaultVLBAReceiverStrict(float freq)	/* freq in MHz */
{
	if(     freq > 290.0   && freq < 700.0  ) { return "90cm"; }
	else if(freq > 1100.0  && freq < 1900.0 ) { return "20cm"; }
	else if(freq > 2000.0  && freq < 3000.0 ) { return "13cm"; }
	else if(freq > 3800.0  && freq < 7900.0 ) { return "6cm"; }
	else if(freq > 8100.0  && freq < 9000.0 ) { return "4cm"; }
	else if(freq > 11000.0 && freq < 16000.0) { return "2cm"; }
	else if(freq > 20000.0 && freq < 25000.0) { return "1cm"; }
	else if(freq > 40000.0 && freq < 46000.0) { return "7mm"; }
	else if(freq > 79000.0 && freq < 91000.0) { return "3mm"; }
	else                                      { return "?"; }
}

static double tcalDate2mjd(double tcalDate)
{
	int t;
	struct tm tm;
	time_t unixTime;

	t = (int)(tcalDate + 0.001);
	memset(&tm, 0, sizeof(tm));
	tm.tm_year = t / 10000 - 1900;		/* first 4 digits */
	tm.tm_mon = (t % 10000) / 100 - 1;	/* next 2 digits */
	tm.tm_mday = t % 100;			/* last 2 digits */

	unixTime = mktime(&tm);

	return 40587.0 + unixTime/86400.0;
}

static int loadDifxTcalVLBA(DifxTcal *dt, const char *antenna, const char *receiver)
{
	if(dt)
	{
		FILE *in;
		char fileName[DIFX_TCAL_FILENAME_LENGTH];
		int v;
		int lineNum;
		int g = -1;
		char lowercaseAntenna[MAX_DIFX_TCAL_ANTENNA_LENGTH];

		for(v = 0; v < MAX_DIFX_TCAL_ANTENNA_LENGTH && antenna[v]; ++v)
		{
			lowercaseAntenna[v] = tolower(antenna[v]);
		}
		lowercaseAntenna[v] = 0;

		if(dt->type != DifxTcalTypeVLBA)
		{
			fprintf(stderr, "Error: loadDifxTcalVLBA: dt->type != DifxTcalTypeVLBA\n");

			return -2;
		}

		if(getDifxTcalGroupIndex(dt, 0.0, antenna, receiver) >= 0)	/* already loaded this file! */
		{
			return -5;
		}

		v = snprintf(fileName, DIFX_TCAL_FILENAME_LENGTH, "%s/%s.%s", dt->path, receiver, lowercaseAntenna);
		if(v >= DIFX_TCAL_FILENAME_LENGTH)
		{
			fprintf(stderr, "Developer error: loadDifxTcalVLBA: DIFX_TCAL_FILENAME_LENGTH is too short (%d); needs to be %d or more\n", DIFX_TCAL_FILENAME_LENGTH, v+1);

			return -3;
		}

		in = fopen(fileName, "r");
		if(!in)
		{
			return -4;
		}

		for(lineNum = 1;; lineNum++)
		{
			const int MaxVLBATcalLineLength = 100;
			char line[MaxVLBATcalLineLength];
			char *rv;
	
			rv = fgets(line, MaxVLBATcalLineLength-1, in);
			if(!rv)
			{
				break;
			}
			line[MaxVLBATcalLineLength-1] = 0;

			if(line[0] == '!')	/* comment */
			{
				continue;
			}
			else if(strncmp(line, "RECEIVER", 8) == 0)	/* start a new group */
			{
				double tcalDate, mjd;
				char serialName[10];
				int n;

				n = sscanf(line+9, "%s%lf", serialName, &tcalDate);
				if(n != 2)
				{
					fprintf(stderr, "Error: loadDifxTcalVLBA: cannot parse line %d of %s\n", lineNum, fileName);

					fclose(in);

					return -7;
				}

				mjd = tcalDate2mjd(tcalDate);

				g = addDifxTcalGroup(dt);
				if(g < 0)
				{
					fclose(in);

					return -6;
				}
				strcpy(dt->group[g].antenna, antenna);
				strcpy(dt->group[g].receiver, receiver);
				if(strcasecmp(receiver, "50cm") == 0 || strcasecmp(receiver, "90cm") == 0)	/* special case */
				{
					dt->group[g].interpolation = DifxTcalInterpolationNearest;
				}
				else
				{
					dt->group[g].interpolation = DifxTcalInterpolationLinear;
				}
				dt->group[g].serial = atoi(serialName);
				dt->group[g].mjdStart = mjd;
			}
			else
			{
				float freq;	/* MHz */
				float tcal1;	/* K */
				float tcal2;	/* K */
				char pol1 = '.';
				char pol2 = '.';
				int n;
				
				n = sscanf(line, "%f%f%f", &freq, &tcal1, &tcal2);
				if(n == 3)
				{
					if(g < 0)
					{
						fprintf(stderr, "Error: loadDifxTcalVLBA: data line %d before a RECEIVER line in %s (g = %d)\n", lineNum, fileName, g);

						fclose(in);

						return -8;
					}

					if(tcal1 > 0.0 && tcal1 < 999.0)
					{
						pol1 = 'L';
					}
					if(tcal2 > 0.0 && tcal2 < 999.0)
					{
						pol2 = 'R';
					}
					v = addDifxTcalValue(dt->group+g, freq, tcal1, pol1, tcal2, pol2);
					if(v < 0)
					{
						fclose(in);

						return -9;
					}
				}
			}
		}
		fclose(in);
		
		if(dt->verbose > 0)
		{
			printf("Loaded Tcal file: %s\n", fileName);
		}

		return 0;
	}
	else
	{
		return -1;
	}
}


/* 3. For DIFX format: */

int setDifxTcalDIFX(DifxTcal *dt, const char *tcalFile)
{
	if(dt)
	{
		int v;

		if(dt->type != DifxTcalNone)
		{
			return -2;
		}

		v = snprintf(dt->path, DIFX_TCAL_FILENAME_LENGTH, "%s", tcalFile);
		if(v >= DIFX_TCAL_FILENAME_LENGTH)
		{
			fprintf(stderr, "Developer error: setDifxTcalVLBA: DIFX_TCAL_FILENAME_LENGTH is too short (%d); needs to be %d or more\n", DIFX_TCAL_FILENAME_LENGTH, v+1);
			

			return -3;
		}

		v = loadDifxTcalDIFX(dt);
		if(v == 0)	/* success */
		{
			dt->type = DifxTcalTypeDIFX;
		}

		return v;
	}
	else
	{
		return -1;
	}
}

static int loadDifxTcalDIFX(DifxTcal *dt)
{
	FILE *in;
	char lastAntenna[MAX_DIFX_TCAL_ANTENNA_LENGTH] = "";
	char lastReceiver[MAX_DIFX_TCAL_RECEIVER_LENGTH] = "";
	int g = -1;
	int gAnt = -1;

	if(!dt)
	{
		return -1;
	}

	in = fopen(dt->path, "r");
	if(!in)
	{
		return -2;
	}

	for(;;)
	{
		const int MaxVLBATcalLineLength = 100;
		char line[MaxVLBATcalLineLength];
		char *rv;
		char antenna[MAX_DIFX_TCAL_ANTENNA_LENGTH] = "";
		char receiver[MAX_DIFX_TCAL_RECEIVER_LENGTH] = "";
		int n;
		float freq, tcal1, tcal2;
		char pol1, pol2;

		rv = fgets(line, MaxVLBATcalLineLength-1, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		pol1 = 'R';
		pol2 = 'L';
		n = sscanf(line, "%s%s%f%f%f%c%c", antenna, receiver, &freq, &tcal1, &tcal2, &pol1, &pol2);
		if(n == 5 || n == 7)
		{
			int newGroup = 0;
			int v;

			if(strncmp(antenna, lastAntenna, MAX_DIFX_TCAL_ANTENNA_LENGTH) != 0)
			{
				strncpy(lastAntenna, antenna, MAX_DIFX_TCAL_ANTENNA_LENGTH);
				newGroup = 1;

				/* also make a new group for just the antenna */
				/* this results in each entry being duplicated: one indexed w/ receiver and one without */
				/* which will make it practical for look-ups based on receiver name or not */
				gAnt = addDifxTcalGroup(dt);
				if(gAnt < 0)
				{
					fclose(in);

					return -6;
				}
				strncpy(dt->group[gAnt].antenna, antenna, MAX_DIFX_TCAL_ANTENNA_LENGTH);
				dt->group[gAnt].receiver[0] = 0;
				dt->group[gAnt].serial = 0;
				dt->group[gAnt].mjdStart = 0;
			}

			if(strncmp(receiver, lastReceiver, MAX_DIFX_TCAL_RECEIVER_LENGTH) != 0)
			{
				strncpy(lastReceiver, receiver, MAX_DIFX_TCAL_RECEIVER_LENGTH);
				newGroup = 1;
			}

			if(newGroup)
			{
				g = addDifxTcalGroup(dt);
				if(g < 0)
				{
					fclose(in);

					return -6;
				}
				strncpy(dt->group[g].antenna, antenna, MAX_DIFX_TCAL_ANTENNA_LENGTH);
				strncpy(dt->group[g].receiver, receiver, MAX_DIFX_TCAL_RECEIVER_LENGTH);
				dt->group[g].serial = 0;
				dt->group[g].mjdStart = 0;
			}

			v = addDifxTcalValue(dt->group+g, freq, tcal1, pol1, tcal2, pol2);
			if(v < 0)
			{
				fclose(in);

				return -9;
			}

			/* here is where the duplicate tcal value is actually added */
			v = addDifxTcalValue(dt->group+gAnt, freq, tcal1, pol1, tcal2, pol2);
			if(v < 0)
			{
				fclose(in);

				return -9;
			}
		}

	}
	fclose(in);

	return 0;
}

