/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mark5dir.h"
#include "mark5access.h"

/* returns active bank, or -1 if none */
int Mark5BankGet(SSHANDLE xlrDevice)
{
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	int b = -1;

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC == XLR_SUCCESS)
	{
		if(bank_stat.Selected)
		{
			b = 0;
		}
	}
	if(b == -1)
	{
		xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
		if(xlrRC == XLR_SUCCESS)
		{
			if(bank_stat.Selected)
			{
				b = 1;
			}
		}
	}

	return b;
}

/* returns 0 or 1 for bank A or B, or < 0 if module not found */
int Mark5BankSetByVSN(SSHANDLE xlrDevice, const char *vsn)
{
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	int b = -1;

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC == XLR_SUCCESS)
	{
		if(strncmp(bank_stat.Label, vsn, 8) == 0)
		{
			b = 0;
			if(!bank_stat.Selected)
			{
				xlrRC = XLRSelectBank(xlrDevice, BANK_A);
				if(xlrRC != XLR_SUCCESS)
				{
					b = -2;
				}
			}
		}
	}
	if(b == -1)
	{
		xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
		if(xlrRC == XLR_SUCCESS)
		{
			if(strncmp(bank_stat.Label, vsn, 8) == 0)
			{
				b = 1;
				if(!bank_stat.Selected)
				{
					xlrRC = XLRSelectBank(xlrDevice, BANK_B);
					if(xlrRC != XLR_SUCCESS)
					{
						b = -3;
					}
				}
			}
		}
	}

	return b;
}

int getMark5Module(struct Mark5Module *module, SSHANDLE xlrDevice, int mjdref)
{
	XLR_RETURN_CODE xlrRC;
	Mark5Directory m5dir;
	int len, i, n;
	mark5_stream *vs = 0;
	struct mark5_format *mf;
	Mark5Scan *scan;
	unsigned long a, b;
	unsigned long *buffer;
	int bufferlen;

	/* allocate the minimum needed */
	bufferlen = 20160*8*3;

	memset(module, 0, sizeof(struct Mark5Module));

	module->bank = Mark5BankGet(xlrDevice);
	if(module->bank < 0)
	{
		return -1;
	}

	xlrRC = XLRGetLabel(xlrDevice, module->label);
	if(xlrRC != XLR_SUCCESS)
	{
		return -1;
	}

	module->label[8] = 0;

	len = XLRGetUserDirLength(xlrDevice);
	if(len < (signed int)sizeof(struct Mark5Directory))
	{
		return -1;
	}

	xlrRC = XLRGetUserDir(xlrDevice, sizeof(struct Mark5Directory), 
		0, &m5dir);
	if(xlrRC != XLR_SUCCESS)
	{
		return -1;
	}

	buffer = (unsigned long *)malloc(bufferlen);
	
	module->nscans = m5dir.nscans;
	
	for(i = 0; i < module->nscans; i++)
	{
		scan = module->scans + i;

		strncpy(scan->name, m5dir.scanName[i], MAXLENGTH);
		scan->start  = m5dir.start[i];
		scan->length = m5dir.length[i];

		if(scan->start & 4)
		{
			scan->start -= 4;
			scan->length -= 4;
		}

		a = scan->start>>32;
		b = scan->start % (1LL<<32);

		xlrRC = XLRReadData(xlrDevice, buffer, a, b, bufferlen);

		if(xlrRC == XLR_FAIL)
		{
			printf("?");
			fflush(stdout);
			continue;
		}

		mf = new_mark5_format_from_stream(new_mark5_stream_memory(buffer, bufferlen));
	
		if(!mf)
		{
			printf("!");
			fflush(stdout);
			continue;
		}
		
		scan->mjd = mf->mjd;
		scan->sec = mf->sec;
		scan->ns  = mf->ns;
		n = (mjdref - scan->mjd + 500) / 1000;
		scan->mjd += n*1000;
		
		scan->format      = mf->format;
		scan->frameoffset = mf->frameoffset;
		scan->tracks      = mf->ntrack;
		scan->framens     = mf->framens;
		scan->framebytes  = mf->framebytes;
		scan->duration    = (int)((scan->length - scan->frameoffset)
			/ scan->framebytes) * (scan->framens*1.e-9);
		
		delete_mark5_format(mf);

		printf("."); 
		fflush(stdout);
	}

	free(buffer);
	
	printf("\n");

	return 0;
}

void printMark5Module(const struct Mark5Module *module)
{
	int i, n;
	const Mark5Scan *scan;

	if(!module)
	{
		return;
	}
	if(module->bank < 0)
	{
		return;
	}
	
	printf("Module Name = %s   Nscans = %d   Bank = %c\n", 
		module->label, module->nscans, module->bank+'A');

	n = module->nscans;
	for(i = 0; i < n; i++)
	{
		scan = module->scans + i;
	
		printf("%3d %1d %-32s %13Ld %13Ld %5d %2d %5d %5d.%04d %10.4f %6.4f\n",
			i+1,
			scan->format,
			scan->name,
			scan->start,
			scan->start+scan->length,
			scan->frameoffset,
			scan->tracks,
			scan->mjd,
			scan->sec,
			scan->ns/100000,
			scan->duration,
			scan->framens*1.0e-9);
	}

	printf("\n");
}

int loadMark5Module(struct Mark5Module *module, const char *filename)
{
	FILE *in;
	struct Mark5Scan *scan;
	char line[256];
	int i, nscans;
	char bank;
	char label[XLR_LABEL_LENGTH];
	double sec, framesec;

	if(!module)
	{
		return -1;
	}

	module->label[0] = 0;
	module->nscans = 0;
	module->bank = -1;

	in = fopen(filename, "r");
	if(!in)
	{
		return -1;
	}

	fgets(line, 255, in);
	if(feof(in))
	{
		fclose(in);
		return -1;
	}

	if(sscanf(line, "%8s %d %c", 
		label,
		&nscans,
		&bank) != 3)
	{
		fclose(in);
		return -1;
	}

	if(nscans > MAXSCANS || nscans < 0)
	{
		fclose(in);
		return -1;
	}

	strcpy(module->label, label);
	module->nscans = nscans;
	module->bank = bank-'A';

	for(i = 0; i < nscans; i++)
	{
		scan = module->scans + i;

		fgets(line, 255, in);
		if(feof(in))
		{
			module->nscans = i;
			fclose(in);
			return -1;
		}
		
		sscanf(line, "%14Ld %14Ld %5d %10lf %6lf %10lf %6d %6d %2d %1d %64s",
			&scan->start, 
			&scan->length,
			&scan->mjd,
			&sec,
			&framesec,
			&scan->duration,
			&scan->framebytes,
			&scan->frameoffset,
			&scan->tracks,
			&scan->format,
			scan->name);
		scan->sec = (int)sec;
		scan->ns = (int)((sec - scan->sec)*1e4 + 0.5)*100000;
		scan->framens = (int)((framesec*1e4) + 0.5)*100000;
	}

	fclose(in);
	
	return 0;
}

int saveMark5Module(struct Mark5Module *module, const char *filename)
{
	FILE *out;
	struct Mark5Scan *scan;
	int i;
	
	if(!module)
	{
		return -1;
	}

	out = fopen(filename, "w");
	if(!out)
	{
		return -1;
	}

	fprintf(out, "%8s %d %c\n",
		module->label,
		module->nscans,
		module->bank+'A');
	for(i = 0; i < module->nscans; i++)
	{
		scan = module->scans + i;
		
		fprintf(out, "%14Ld %14Ld %5d %d.%04d %6.4f %10.4f %6d %6d %2d %1d %s\n",
			scan->start, 
			scan->length,
			scan->mjd,
			scan->sec,
			scan->ns/100000,
			scan->framens*1.0e-9,
			scan->duration,
			scan->framebytes,
			scan->frameoffset,
			scan->tracks,
			scan->format,
			scan->name);
	}

	fclose(out);

	return 0;
}

/* retrieves directory (either from cache or module) and makes sure
 * desired module is the active one.  On any failure return < 0 
 */
int getCachedMark5Module(struct Mark5Module *module, SSHANDLE xlrDevice, 
	int mjdref, const char *vsn, const char *dir)
{
	char filename[256];
	int v, m1, m2, curbank;
	int nscans = 0;

	curbank = Mark5BankSetByVSN(xlrDevice, vsn);
	if(curbank < 0)
	{
		return -1;
	}
	
	sprintf(filename, "%s/%s.dir", dir, vsn);
	
	v = loadMark5Module(module, filename);

	if(v >= 0)
	{
		nscans = module->nscans;
		if(nscans < 1)
		{
			v = -1;
		}
		else	/* valid dir loaded.  Is date OK? */
		{
			m1 = module->scans[0].mjd;
			m2 = module->scans[nscans-1].mjd;
			if(mjdref < m1-1 || mjdref > m2+1)
			{
				v = -1;
			}
		}
	}

	if(v < 0)	/* No cached dir or cache is old.  Remake. */
	{
		v = getMark5Module(module, xlrDevice, mjdref);
		nscans = module->nscans;
		if(v >= 0 && nscans < 1)
		{
			v = -1;
		}
		if(v >= 0)
		{
			saveMark5Module(module, filename);
		}
	}
	else if(module->bank != curbank)
	{
		module->bank = curbank;
		saveMark5Module(module, filename);
	}

	return v;
}
