/***************************************************************************
 *  Copyright (C) 2013 Walter Brisken                                      *
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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================


#include <string.h>
#include <stdio.h>
#include "vdifio.h"

/* Note: the decoders in this source file are intended for decoding some or all data in a
 * single VDIF frame.  The array presented to the decoding function must begin with
 * the VDIF frame header.
 */


#define OPTIMAL_2BIT_HIGH	3.3359

static const float HiMag = OPTIMAL_2BIT_HIGH;
static const float FourBit1sigma = 2.95;

static float lut1bit[256][8];
static float lut2bit[256][4];
static float lut4bit[256][2];
static float lut8bit[256];
static float zeros[8];

static void initluts()
{
	const float lut2level[2] = {-1.0, 1.0};
	const float lut4level[4] = {-HiMag, -1.0, 1.0, HiMag};
	const float lut16level[16] = {-8/FourBit1sigma,-7/FourBit1sigma,-6/FourBit1sigma,-5/FourBit1sigma,-4/FourBit1sigma,
				      -3/FourBit1sigma,-2/FourBit1sigma,-1/FourBit1sigma,0,1/FourBit1sigma,2/FourBit1sigma,
				      3/FourBit1sigma,4/FourBit1sigma,5/FourBit1sigma,6/FourBit1sigma,7/FourBit1sigma};
	int b, i, l;
	
	for(i = 0; i < 8; i++)
	{
		zeros[i] = 0.0;
	}

	for(b = 0; b < 256; b++)
	{
		/* lut1bit */
		for(i = 0; i < 8; i++)
		{
			l = (b>>i) & 0x01;
			lut1bit[b][i] =  lut2level[l];
		}

		/* lut2bit */
		for(i = 0; i < 4; i++)
		{
			l = (b >> (2*i)) & 0x03;
			lut2bit[b][i] = lut4level[l];
		}

		/* lut4bit */
		for(i = 0; i < 2; i++)
		{
			l = (b >> (4*i)) & 0x0F;
			lut4bit[b][i] = lut16level[l];
		}

		/* lut8bit */
		lut8bit[b] = (b*2-255)/256.0;
	}
}

/************************* decode routines **************************/

static int vdif_decode_1channel_1bit(const unsigned char *vdifFrame, float *samples, int maxSamples)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nsamp;
	int hl;

	hl = getVDIFHeaderBytes( (vdif_header *)vdifFrame );
	buf = vdifFrame + hl;

	nsamp = (getVDIFFrameBytes( (vdif_header *)vdifFrame ) - hl) * 8;
	if(nsamp > maxSamples)
	{
		nsamp = maxSamples;
	}

	for(i = o = 0; o < nsamp; )
	{
		fp = lut1bit[buf[i++]];
		samples[o++] = fp[0];
		samples[o++] = fp[1];
		samples[o++] = fp[2];
		samples[o++] = fp[3];
		samples[o++] = fp[4];
		samples[o++] = fp[5];
		samples[o++] = fp[6];
		samples[o++] = fp[7];
	}

	return nsamp;
}

static int vdif_decode_1channel_2bit(const unsigned char *vdifFrame, float *samples, int maxSamples)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nsamp;
	int hl;

	hl = getVDIFHeaderBytes( (vdif_header *)vdifFrame );
	buf = vdifFrame + hl;

	nsamp = (getVDIFFrameBytes( (vdif_header *)vdifFrame ) - hl) * 4;
	if(nsamp > maxSamples)
	{
		nsamp = maxSamples;
	}

	for(i = o = 0; o < nsamp; )
	{
		fp = lut2bit[buf[i++]];
		samples[o++] = fp[0];
		samples[o++] = fp[1];
		samples[o++] = fp[2];
		samples[o++] = fp[3];
	}

	return nsamp;
}

static int vdif_decode_1channel_4bit(const unsigned char *vdifFrame, float *samples, int maxSamples)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nsamp;
	int hl;

	hl = getVDIFHeaderBytes( (vdif_header *)vdifFrame );
	buf = vdifFrame + hl;

	nsamp = (getVDIFFrameBytes( (vdif_header *)vdifFrame ) - hl) * 2;
	if(nsamp > maxSamples)
	{
		nsamp = maxSamples;
	}

	for(i = o = 0; o < nsamp; )
	{
		fp = lut4bit[buf[i++]];

		samples[o++] = fp[0];
		samples[o++] = fp[1];
	}

	return nsamp;
}

static int vdif_decode_1channel_8bit(const unsigned char *vdifFrame, float *samples, int maxSamples)
{
	const unsigned char *buf;
	int o;
	int nsamp;
	int hl;

	hl = getVDIFHeaderBytes( (vdif_header *)vdifFrame );
	buf = vdifFrame + hl;

	nsamp = (getVDIFFrameBytes( (vdif_header *)vdifFrame ) - hl);
	if(nsamp > maxSamples)
	{
		nsamp = maxSamples;
	}

	for(o = 0; o < nsamp; ++o)
	{
		samples[o] = lut8bit[buf[o]];
	}

	return nsamp;
}

int decodeSingleChannelVDIF(const unsigned char *vdifFrame, float *samples, int maxSamples)
{
	static int first = 1;

	if(first == 1)
	{
		/* on first call, initialize look-up tables */
		initluts();
		first = 0;
	}

	switch(getVDIFBitsPerSample( (vdif_header *)vdifFrame ) )
	{
	case 2:
		return vdif_decode_1channel_2bit(vdifFrame, samples, maxSamples);
	case 1:
		return vdif_decode_1channel_1bit(vdifFrame, samples, maxSamples);
	case 8:
		return vdif_decode_1channel_8bit(vdifFrame, samples, maxSamples);
	case 4:
		return vdif_decode_1channel_4bit(vdifFrame, samples, maxSamples);
	default:
		return 0;
	}
}
