/***************************************************************************
 *   Copyright (C) 2008-2016 by Walter Brisken                             *
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

#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <difxmessage.h>
#include "config.h"
#include "alert.h"
#include "watchdog.h"
#include "mark5utils.h"

#if HAVE_MARK5IPC
#include <mark5ipc.h>
#endif

#define XLR_WATERMARK_VALUE	0x0FF00FFFF0005555ULL


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


XLR_RETURN_CODE openMark5(SSHANDLE *xlrDevice)
{
	XLR_RETURN_CODE xlrRC;
	S_XLRSWREV swVersion;
	S_DEVINFO devInfo;

	WATCHDOG( xlrRC = XLROpen(1, xlrDevice) );
  
  	if(xlrRC == XLR_FAIL)
	{
#if HAVE_MARK5IPC
                unlockMark5();
#endif
		WATCHDOG( XLRClose(*xlrDevice) );
		cfatal << startl << "Cannot open Streamstor device.  Either this Mark5 unit has crashed, you do not have read/write permission to /dev/windrvr6, or some other process has full control of the Streamstor device." << endl;
		
		return xlrRC;
	}

	WATCHDOG( xlrRC = XLRGetVersion(*xlrDevice, &swVersion) );
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot get Streamstor software versions" << endl;
	}
	else
	{
		cinfo << startl << "Mark5 Streamstor software version info: ApiVersion=" << swVersion.ApiVersion << " ApiDateCode=" << swVersion.ApiDateCode << " FirmwareVersion=" << swVersion.FirmwareVersion << " FirmDateCode=" << swVersion.FirmDateCode << " MonitorVersion=" << swVersion.MonitorVersion << " XbarVersion=" << swVersion.XbarVersion << " AtaVersion=" << swVersion.AtaVersion << " UAtaVersion=" << swVersion.UAtaVersion << " DriverVersion=" << swVersion.DriverVersion << endl;
	}

	WATCHDOG( xlrRC = XLRGetDeviceInfo(*xlrDevice, &devInfo) );
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot get Streamstor device info" << endl;
	}
	else
	{
		cinfo << startl << "Mark5 Streamstor device info: BoardType=" << devInfo.BoardType << " SerialNum=" << devInfo.SerialNum << endl;
	}

	// FIXME: for non-bank-mode operation, need to look at the modules to determine what to do here.
	WATCHDOG( xlrRC = XLRSetBankMode(*xlrDevice, SS_BANKMODE_NORMAL) );
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot put Mark5 unit in bank mode" << endl;
	}

	WATCHDOG( XLRSetMode(*xlrDevice, SS_MODE_SINGLE_CHANNEL) );
	WATCHDOG( XLRClearChannels(*xlrDevice) );
	WATCHDOG( XLRSelectChannel(*xlrDevice, 0) );
	WATCHDOG( XLRBindOutputChannel(*xlrDevice, 0) );

	return XLR_SUCCESS;
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
XLR_RETURN_CODE difxMark5Read(SSHANDLE xlrDevice, unsigned long long readpointer, unsigned char *dest, unsigned int bytes, unsigned int readDelayMicroseconds)
{
	const unsigned int maxReadBytes = 20000000;	// The maximum number of bytes to read in one call of XLRRead()

	XLR_RETURN_CODE xlrRC = XLR_SUCCESS;
	S_READDESC      xlrRD;

	/* divide the read into multiple sections */
	unsigned int nRead = bytes/(maxReadBytes-1) + 1;
	unsigned int readSize = (bytes / nRead + 7) & 0xFFFFFFF8UL;

	for(unsigned int offset = 0; offset < bytes; offset += readSize)
	{
		unsigned long long *watermarkSpot;

		if(offset + readSize > bytes)
		{
			readSize = bytes - offset;
		}

		// set up the XLR info
		xlrRD.AddrHi = (readpointer + offset) >> 32;
		xlrRD.AddrLo = (readpointer + offset) & 0xFFFFFFF8UL; // enforce 8 byte boundary
		xlrRD.XferLength = readSize;
		xlrRD.BufferAddr = reinterpret_cast<streamstordatatype *>(dest + offset);

		// delay the read if needed
		if(readDelayMicroseconds > 0)
		{
			usleep(readDelayMicroseconds);
		}

		// place watermark at end of buffer
		watermarkSpot = reinterpret_cast<unsigned long long *>(dest + offset + readSize) - 1;
		*watermarkSpot = XLR_WATERMARK_VALUE;

		WATCHDOG( xlrRC = XLRReadData(xlrDevice, xlrRD.BufferAddr, xlrRD.AddrHi, xlrRD.AddrLo, xlrRD.XferLength) );
		
		if(readSize > 2000)
		{
			unsigned int nZero = 0;
			for(unsigned int i = 1600; i < 2000; ++i)
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
				static unsigned int nHighZeroRate = 0;

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
				static unsigned int nWatermarkFound = 0;

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

unsigned int calculateMark5Signature(SSHANDLE xlrDevice)
{
	const int LegacyScanDirectorySize = 81952;
	const int NeoLegacyScanDirectorySize = 5242912;
	unsigned int signature;
	int len;

	signature = 1;

	WATCHDOG( len = XLRGetUserDirLength(xlrDevice) );
	if(len > 128)
	{
		unsigned char *dirData = new unsigned char[len];
		const unsigned char *data;
		int size;

		WATCHDOGTEST( XLRGetUserDir(xlrDevice, len, 0, dirData) );

		if(len % 128 != 0)
		{
			if(len >= NeoLegacyScanDirectorySize)
			{
				len = NeoLegacyScanDirectorySize;
			}
			else
			{
				len = LegacyScanDirectorySize;
			}
			size = len;
			data = dirData;
		}
		else
		{
			size = len - 128;
			data = dirData + 128;
		}

		for(int j = 0; j < size/4; ++j)
		{
			unsigned int x = ((unsigned int *)data)[j] + 1;
			signature = signature ^ x;
		}

		/* prevent a zero signature */
		if(signature == 0)
		{
			signature = 0x55555555;
		}

		delete[] dirData;
	}

	return signature;
}

/* returns 0 or 1 for bank A or B, or < 0 if module not found or on error */
int Mark5BankSetByVSN(SSHANDLE xlrDevice, const char *vsn)
{
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	S_DIR dir;
	int b = -1;
	int bank=-1;

	WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat) );
	if(xlrRC == XLR_SUCCESS)
	{
		if(strncasecmp(bank_stat.Label, vsn, 8) == 0)
		{
			b = 0;
			bank = BANK_A;
		}
	}
	else
	{
		return -2;
	}

	if(b == -1)
	{
		WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat) );
		if(xlrRC == XLR_SUCCESS)
		{
			if(strncasecmp(bank_stat.Label, vsn, 8) == 0)
			{
				b = 1;
				bank = BANK_B;
			}
		}
		else
		{
			return -3;
		}
	}

	if(bank < 0)
	{
		return -1;
	}

	WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, bank, &bank_stat) );
	if(xlrRC != XLR_SUCCESS)
	{
		return -4;
	}

	WATCHDOG( xlrRC = XLRSelectBank(xlrDevice, bank) );
	if(xlrRC != XLR_SUCCESS)
	{
		b = -5 - b;
	}
	else
	{
		for(int i = 0; i < 100; ++i)
		{
			WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, bank, &bank_stat) );
			if(xlrRC != XLR_SUCCESS)
			{
				return -7;
			}
			if(bank_stat.State == STATE_READY && bank_stat.Selected)
			{
				break;
			}
			usleep(100000);
		}

		if(bank_stat.State != STATE_READY || !bank_stat.Selected)
		{
			b = -8;
		}
	}

	/* the following line is essential to work around an apparent streamstor bug */
	WATCHDOG( xlrRC = XLRGetDirectory(xlrDevice, &dir) );
	if(xlrRC != XLR_SUCCESS)
	{
		return -9;
	}

	return b;
}
