#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <difxio.h>
#include "externaldelay.h"

/* Assumes delays are in filename and atmosphere is in filename+".atm" */
ExternalDelay *newExternalDelay(const char *filename)
{
	const int MaxLineSize=1023;
	char line[MaxLineSize+1];
	FILE *in, *in2;
	ExternalDelay *ed;
	char atmfilename[DIFXIO_FILENAME_LENGTH];
	char *rv;
	int n, lineNum, nLines=0;
	double delay, dry, wet, jd;
	int nData = 0;

	n = snprintf(atmfilename, DIFXIO_FILENAME_LENGTH, "%s.atm", filename);
	if(n >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "Developer error: newExternalDelay: DIFXIO_FILENAME_LENGTH too short (%d < %d)\n", DIFXIO_FILENAME_LENGTH, n+1);

		return 0;
	}

	in = fopen(filename, "r");
	if(!in)
	{
		return 0;
	}

	in2 = fopen(atmfilename, "r");
	if(!in2)
	{
		fprintf(stderr, "Warning: Not using external delay file %s because corresponding atm file %s is not found\n", filename, atmfilename);

		fclose(in);

		return 0;
	}

	ed = (ExternalDelay *)calloc(1, sizeof(ExternalDelay));
	if(!ed)
	{
		fprintf(stderr, "Error: newExternalDelay: cannot allocate %d bytes for ExternalDelay\n", (int)(sizeof(ExternalDelay)));

		fclose(in);
		fclose(in2);

		return 0;
	}

	/* determine array size needed */
	for(;;)
	{
		rv = fgets(line, MaxLineSize, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		n = sscanf(line, "%d %*s %*s %lf %*s %*s %*s %*s %lf %*f", &lineNum, &jd, &delay);
		if(n == 3)
		{
			if(lineNum > nLines)
			{
				nLines = lineNum;
			}
			++nData;
		}
	}
	rewind(in);

	if(nData != nLines)
	{
		fprintf(stderr, "Error: The max row number (%d) does not equal the number of data lines (%d) in %s\n", nLines, nData, filename);

		fclose(in);
		fclose(in2);
		free(ed);

		return 0;
	}

	if(nLines == 0 || nLines > 1000000)
	{
		fprintf(stderr, "Number of delay lines in %s is %d.  That is ridiculous.\n", filename, nLines);

		fclose(in);
		fclose(in2);
		free(ed);

		return 0;
	}

	ed->nRow = nLines;
	ed->rows = (DelayRow *)calloc(nLines, sizeof(DelayRow));

	/* read delays */
	for(;;)
	{
		rv = fgets(line, MaxLineSize, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		n = sscanf(line, "%d %*s %*s %lf %*s %*s %*s %*s %lf %*f", &lineNum, &jd, &delay);
		if(n == 3)
		{
			if(lineNum < 1 || lineNum > nData)
			{
				fprintf(stderr, "Error: row number in %s is out of range (%d)\n", filename, lineNum);

				fclose(in);
				fclose(in2);
				deleteExternalDelay(ed);

				return 0;
			}
			ed->rows[lineNum-1].mjd = jd-2400000.5;
			ed->rows[lineNum-1].delay = delay;
		}
	}

	/* read atmosphere */
	for(;;)
	{
		rv = fgets(line, MaxLineSize, in2);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		n = sscanf(line, "%d %*s %*s %lf %*s %*s %*s %*s %lf %*f %lf %*f", &lineNum, &jd, &dry, &wet);
		if(n == 4)
		{
			if(lineNum < 1 || lineNum > nData)
			{
				fprintf(stderr, "Error: row number in %s is out of range (%d)\n", atmfilename, lineNum);

				fclose(in);
				fclose(in2);
				deleteExternalDelay(ed);

				return 0;
			}
			if(ed->rows[lineNum-1].mjd != jd-2400000.5)
			{
				fprintf(stderr, "Error: mismatch in timestamps in files %s and %s, value %d\n", atmfilename, atmfilename, lineNum);

				fclose(in);
				fclose(in2);
				deleteExternalDelay(ed);

				return 0;
			}
			ed->rows[lineNum-1].dry = dry;
			ed->rows[lineNum-1].wet = wet;
			ed->rows[lineNum-1].delay -= (dry + wet);
		}
	}

	fclose(in);
	fclose(in2);

	printf("%d delay rows read from %s\n", nData, filename);

	return ed;
}

void deleteExternalDelay(ExternalDelay *ed)
{
	if(ed)
	{
		if(ed->rows && ed->nRow > 0)
		{
			free(ed->rows);
		}
		ed->nRow = 0;
		ed->rows = 0;
		free(ed);
	}
}

int getExternalDelay(const ExternalDelay *ed, double mjd, double *delay, double *dry, double *wet)
{
	int i;

	if(!ed)
	{
		return -1;
	}

	for(i = 0; i < ed->nRow; ++i)
	{
		if(fabs(ed->rows[i].mjd - mjd) < 1.0/8640000.0)
		{
			if(delay)
			{
				*delay = ed->rows[i].delay;
			}
			if(dry)
			{
				*dry = ed->rows[i].dry;
			}
			if(wet)
			{
				*wet = ed->rows[i].wet;
			}

			return 0;
		}
	}

	return -2;
}

