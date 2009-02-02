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


#include <iostream>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <mark5access.h>
#include "mark5dir.h"
#include "replaced.h"

using namespace std;

char Mark5DirDescription[][20] =
{
	"Short scan",
	"XLR Read error",
	"Decode error",
	"Decoded",
	"Decoded WR"
};

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
		if(strncasecmp(bank_stat.Label, vsn, 8) == 0)
		{
			b = 0;
			xlrRC = XLRSelectBank(xlrDevice, BANK_A);
			if(xlrRC != XLR_SUCCESS)
			{
				b = -2;
			}
		}
	}
	if(b == -1)
	{
		xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
		if(xlrRC == XLR_SUCCESS)
		{
			if(strncasecmp(bank_stat.Label, vsn, 8) == 0)
			{
				b = 1;
				xlrRC = XLRSelectBank(xlrDevice, BANK_B);
				if(xlrRC != XLR_SUCCESS)
				{
					b = -3;
				}
			}
		}
	}

	return b;
}

static int getMark5Module(struct Mark5Module *module, SSHANDLE xlrDevice, int mjdref, 
	int (*callback)(int, int, int, void *), void *data, float *replacedFrac)
{
	XLR_RETURN_CODE xlrRC;
	Mark5Directory m5dir;
	int len, i, n;
	struct mark5_format *mf;
	Mark5Scan *scan;
	char label[XLR_LABEL_LENGTH];
	int bank;
	unsigned long a, b;
	unsigned long *buffer;
	int bufferlen;
	unsigned int x, signature;
	int die = 0;
	long long wGood, wBad;
	long long wGoodSum=0, wBadSum=0;

	/* allocate a bit more than the minimum needed */
	bufferlen = 20160*8*5;

	bank = Mark5BankGet(xlrDevice);
	if(bank < 0)
	{
		return -1;
	}

	xlrRC = XLRGetLabel(xlrDevice, label);
	if(xlrRC != XLR_SUCCESS)
	{
		return -1;
	}
	label[8] = 0;

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

	/* the adventurous would use md5 here */
	signature = 1;
	for(i = 0; i < sizeof(struct Mark5Directory)/4; i++)
	{
		x = ((unsigned int *)(&m5dir))[i] + 1;
		signature = signature ^ x;
	}

	/* prevent a zero signature */
	if(signature == 0)
	{
		signature = 0x55555555;
	}

	if(module->signature == signature && module->nscans > 0)
	{
		module->bank = bank;
		return 0;
	}

	buffer = (unsigned long *)malloc(bufferlen);
	
	memset(module, 0, sizeof(struct Mark5Module));
	module->nscans = m5dir.nscans;
	module->bank = bank;
	strcpy(module->label, label);
	module->signature = signature;

	for(i = 0; i < module->nscans; i++)
	{
		wGood = wBad = 0;
		scan = module->scans + i;

		strncpy(scan->name, m5dir.scanName[i], MAXLENGTH);
		scan->start  = m5dir.start[i];
		scan->length = m5dir.length[i];
		if(scan->length < bufferlen*10)
		{
			if(callback)
			{
				die = callback(i, module->nscans, MARK5_DIR_SHORT_SCAN, data);
			}
			continue;
		}

		if(die)
		{
			break;
		}

		if(scan->start & 4)
		{
			scan->start -= 4;
			scan->length -= 4;
		}

		a = scan->start>>32;
		b = scan->start % (1LL<<32);

		xlrRC = XLRReadData(xlrDevice, buffer, a, b, bufferlen);

		countReplaced(buffer, bufferlen/4, &wGood, &wBad);

		if(xlrRC == XLR_FAIL)
		{
			if(callback)
			{
				die = callback(i, module->nscans, MARK5_DIR_READ_ERROR, data);
			}
			continue;
		}

		if(die)
		{
			break;
		}

		mf = new_mark5_format_from_stream(new_mark5_stream_memory(buffer, bufferlen));
	
		if(!mf)
		{
			if(callback)
			{
				die = callback(i, module->nscans, MARK5_DIR_DECODE_ERROR, data);
			}
			continue;
		}
		
		if(die)
		{
			break;
		}

		scan->mjd = mf->mjd;
		scan->sec = mf->sec;
		n = (mjdref - scan->mjd + 500) / 1000;
		scan->mjd += n*1000;
		
		scan->format      = mf->format;
		scan->frameoffset = mf->frameoffset;
		scan->tracks      = mf->ntrack;
		scan->framespersecond = int(1000000000.0/mf->framens + 0.5);
		scan->framenuminsecond = int(mf->ns/mf->framens + 0.5);
		scan->framebytes  = mf->framebytes;
		scan->duration    = (int)((scan->length - scan->frameoffset)
			/ scan->framebytes)/(double)(scan->framespersecond);
		
		delete_mark5_format(mf);

		if(callback)
		{
			enum Mark5DirStatus s;

			if(wBad > 8)
			{
				s = MARK5_DIR_DECODE_WITH_REPLACEMENTS;
			}
			else
			{
				s = MARK5_DIR_DECODE_SUCCESS;
			}
			die = callback(i, module->nscans, s, data);
		}

		wGoodSum += wGood;
		wBadSum += wBad;

		if(die)
		{
			break;
		}
	}

	if(replacedFrac)
	{
		*replacedFrac = (double)wBad/(double)wGood;
	}

	free(buffer);

	return -die;
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
	
	printf("Module Name = %s   Nscans = %d   Bank = %c  Sig = %u\n", 
		module->label, module->nscans, module->bank+'A', 
		module->signature);

	n = module->nscans;
	for(i = 0; i < n; i++)
	{
		scan = module->scans + i;
	
		printf("%3d %1d %-32s %13Ld %13Ld %5d %2d %5d %5d+%d/%d %6.4f\n",
			i+1,
			scan->format,
			scan->name,
			scan->start,
			scan->start+scan->length,
			scan->frameoffset,
			scan->tracks,
			scan->mjd,
			scan->sec,
			scan->framenuminsecond,
			scan->framespersecond,
			scan->duration);
	}
}

int loadMark5Module(struct Mark5Module *module, const char *filename)
{
	FILE *in;
	struct Mark5Scan *scan;
	char line[256];
	int i, nscans, n;
	char bank;
	char label[XLR_LABEL_LENGTH];
	unsigned int signature;

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

	n = sscanf(line, "%8s %d %c %u", label, &nscans, &bank, &signature);
	if(n < 3)
	{
		fclose(in);
		return -1;
	}
	if(n == 3)
	{
		signature = ~0;
	}

	if(nscans > MAXSCANS || nscans < 0)
	{
		fclose(in);
		return -1;
	}

	strcpy(module->label, label);
	module->nscans = nscans;
	module->bank = bank-'A';
	module->signature = signature;

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
		
		sscanf(line, "%Ld%Ld%d%d%d%d%lf%d%d%d%d%63s",
			&scan->start, 
			&scan->length,
			&scan->mjd,
			&scan->sec,
			&scan->framenuminsecond,
			&scan->framespersecond,
			&scan->duration,
			&scan->framebytes,
			&scan->frameoffset,
			&scan->tracks,
			&scan->format,
			scan->name);
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

	fprintf(out, "%8s %d %c %u\n",
		module->label,
		module->nscans,
		module->bank+'A',
		module->signature);
	for(i = 0; i < module->nscans; i++)
	{
		scan = module->scans + i;
		
		fprintf(out, "%14Ld %14Ld %5d %d %d %d %12.6f %6d %6d %2d %1d %s\n",
			scan->start, 
			scan->length,
			scan->mjd,
			scan->sec,
			scan->framenuminsecond,
			scan->framespersecond,
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
	int mjdref, const char *vsn, const char *dir,
	int (*callback)(int, int, int, void *), void *data,
	float *replacedFrac)
{
	char filename[256];
	int v, curbank;

	curbank = Mark5BankSetByVSN(xlrDevice, vsn);
	if(curbank < 0)
	{
		return -1;
	}
	
	sprintf(filename, "%s/%s.dir", dir, vsn);
	
	v = loadMark5Module(module, filename);

	v = getMark5Module(module, xlrDevice, mjdref, callback, data, replacedFrac);

	if(v >= 0)
	{
		saveMark5Module(module, filename);
	}

	return v;
}
