/***************************************************************************
 *   Copyright (C) 2008-2014 by Walter Brisken                             *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: mark5utils.cpp 5453 2013-06-28 02:13:50Z WalterBrisken $
 * $HeadURL: $
 * $LastChangedRevision: 5453 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2013-06-27 20:13:50 -0600 (Thu, 27 Jun 2013) $
 *
 *==========================================================================*/

#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <difxmessage.h>
#include "alert.h"
#include "mark5dir.h"
#include "watchdog.h"

#define XLR_WATERMARK_VALUE	0x0FF00FFFF0005555LL

int dirCallback(int scan, int nscan, int status, void *data)
{
	const int MessageLength=200;
	char message[MessageLength];
	int v;
	DifxMessageMk5Status *mk5status;

	mk5status = reinterpret_cast<DifxMessageMk5Status *>(data);
	mk5status->scanNumber = scan + 1;
	mk5status->position = nscan;
	v = snprintf(mk5status->scanName, MODULE_SCAN_NAME_LENGTH, "%s", Mark5DirDescription[status]);
	if(v >= MessageLength)
	{
		fprintf(stderr, "Warning: dirCallback: scanName: v=%d >= %d\n", v, MODULE_SCAN_NAME_LENGTH);
	}

	difxMessageSendMark5Status(mk5status);

	if(status == MARK5_DIR_READ_ERROR)
	{
		v = snprintf(message, MessageLength, "XLR read error in decoding of scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
	}
	else if(status == MARK5_DIR_DECODE_ERROR)
	{
		v = snprintf(message, MessageLength, "Cannot decode scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
	}

	if(v >= MessageLength)
	{
		fprintf(stderr, "Warning: dirCallback: message: v=%d, >= %d\n", v, MessageLength);
	}

	return 0;
}

bool legalVSN(const char *vsn)
{
	int nSep = 0;

	for(int i = 0; i < 8; ++i)
	{
		if(vsn[i] == '+' || vsn[i] == '-')
		{
			if(nSep > 0 || i == 0 || i == 7)
			{
				return false;
			}
			++nSep;
		}
		else if(isalpha(vsn[i]))
		{
			if(nSep != 0)
			{
				return false;
			}
		}
		else if(isdigit(vsn[i]))
		{
			if(nSep != 1)
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}

	if(nSep != 1)
	{
		return false;
	}

	return true;
}

/* This function reads <bytes> bytes of data from a Mark5 module starting at pointer <readpointer>
 * writing to memory location <dest>.  This operation may be split into multiple XLRRead() function
 * calls to ensure no single read is longer than 20 MB as that is known to trigger failure on
 * some linux kernel versions.
 *
 * This function also looks to see if a large fraction (30/400) of the extracted bytes are all zeros.
 * This can be indicative of certain problems.  Each time this condition is found a second read attempt
 * is made.
 */
XLR_RETURN_CODE difxMark5Read(SSHANDLE xlrDevice, long long readpointer, unsigned char *dest, int bytes, int readDelayMicroseconds)
{
	const int maxReadBytes = 20000000;	// The maximum number of bytes to read in one call of XLRRead()

	XLR_RETURN_CODE xlrRC = XLR_SUCCESS;
	S_READDESC      xlrRD;

	/* divide the read into multiple sections */
	int nRead = bytes/(maxReadBytes-1) + 1;
	int readSize = (bytes / nRead + 7) & 0xFFFFFFF8;

	for(int offset = 0; offset < bytes; offset += readSize)
	{
		long long *watermarkSpot;

		if(offset + readSize > bytes)
		{
			readSize = bytes - offset;
		}

		// set up the XLR info
		xlrRD.AddrHi = static_cast<unsigned long>( (readpointer + offset) >> 32 );
		xlrRD.AddrLo = static_cast<unsigned long>( (readpointer + offset) & 0xFFFFFFF8 ); // enforce 8 byte boundary
		xlrRD.XferLength = readSize;
		xlrRD.BufferAddr = reinterpret_cast<streamstordatatype *>(dest + offset);

		// delay the read if needed
		if(readDelayMicroseconds > 0)
		{
			usleep(readDelayMicroseconds);
		}

		// place watermark at end of buffer
		watermarkSpot = reinterpret_cast<long long *>(dest + offset + readSize) - 1;
		*watermarkSpot = XLR_WATERMARK_VALUE;

		WATCHDOG( xlrRC = XLRReadData(xlrDevice, xlrRD.BufferAddr, xlrRD.AddrHi, xlrRD.AddrLo, xlrRD.XferLength) );
		
		if(readSize > 1000)
		{
			int nZero = 0;
			for(int i = 600; i < 1000; ++i)
			{
				if(dest[offset+i] == 0)
				{
					++nZero;
				}
			}

			if(xlrRC != XLR_SUCCESS)
			{
				XLR_ERROR_CODE xlrError;
				char errString[XLR_ERROR_LENGTH];

				xlrError = XLRGetLastError();
				XLRGetErrorMessage(errString, xlrError);

				cwarn << startl << "XLRReadData resulted in an error: " << errString << endl;
			}

			if(xlrRC == XLR_SUCCESS && nZero > 30)
			{
				static int nHighZeroRate = 0;

				++nHighZeroRate;
				if( (nHighZeroRate & (nHighZeroRate-1)) == 0)
				{
					cwarn << startl << "High zero rate=" << nZero << "/" << 400 <<" in this data.  rereading!  readpointer=" << (readpointer + offset) << " readsize=" << readSize << " N=" << nHighZeroRate << endl;
				}

				usleep(readDelayMicroseconds);
				WATCHDOG( xlrRC = XLRReadData(xlrDevice, xlrRD.BufferAddr, xlrRD.AddrHi, xlrRD.AddrLo, xlrRD.XferLength) );
			}
			else if(*watermarkSpot == XLR_WATERMARK_VALUE)
			{
				static int nWatermarkFound = 0;

				++nWatermarkFound;
				if( (nWatermarkFound & (nWatermarkFound-1)) == 0)
				{
					cwarn << startl << "XLR Read incomplete.  rereading!  readpointer=" << (readpointer + offset) << "  readSize=" << readSize << " N=" << nWatermarkFound << endl;
				}

				usleep(readDelayMicroseconds);
				WATCHDOG( xlrRC = XLRReadData(xlrDevice, xlrRD.BufferAddr, xlrRD.AddrHi, xlrRD.AddrLo, xlrRD.XferLength) );

				if( (nWatermarkFound & (nWatermarkFound-1)) == 0)
				{
					if(*watermarkSpot == XLR_WATERMARK_VALUE)
					{
						cwarn << startl << "Reread did not help!" << endl;
					}
					else
					{
						cinfo << startl << "Reread did help!" << endl;
					}
				}
			}
			else
			{
				xlrRC = XLR_SUCCESS;
			}

			if(xlrRC != XLR_SUCCESS)
			{
				XLR_ERROR_CODE xlrError;
				char errString[XLR_ERROR_LENGTH];

				xlrError = XLRGetLastError();
				XLRGetErrorMessage(errString, xlrError);

				cwarn << startl << "XLRReadData second attempt resulted in an error: " << errString << endl;
			}
		}
	}

	return xlrRC;
}
