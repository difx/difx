/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken and Adam Deller                  *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
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

#include <mpi.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
#include "config.h"
#include "nativemk5.h"
#include "alert.h"

#ifdef WORDS_BIGENDIAN
#define FILL_PATTERN 0x44332211UL
#else
#define FILL_PATTERN 0x11223344UL
#endif

static int dirCallback(int scan, int nscan, int status, void *data)
{
	char message[256];
	DifxMessageMk5Status *mk5status;

	mk5status = (DifxMessageMk5Status *)data;
	mk5status->scanNumber = scan + 1;
	mk5status->position = nscan;
	sprintf(mk5status->scanName, "%s", Mark5DirDescription[status]);
	difxMessageSendMark5Status(mk5status);

	if(status == MARK5_DIR_READ_ERROR)
	{
		sprintf(message, "XLR read error in decoding of scan %d\n", 
			scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
	}
	else if(status == MARK5_DIR_DECODE_ERROR)
	{
		sprintf(message, "cannot decode scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
	}

	return 0;
}

NativeMk5DataStream::NativeMk5DataStream(Configuration * conf, int snum, 
	int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor, 
	numsegments)
{
	XLR_RETURN_CODE xlrRC;

	/* each data buffer segment contains an integer number of frames, 
	 * because thats the way config determines max bytes
	 */

	executeseconds = conf->getExecuteSeconds();

	nError = 0;

	sendMark5Status(MARK5_STATE_OPENING, 0, 0, 0.0, 0.0);

	cinfo << startl << "Opening Streamstor" << endl;
	xlrRC = XLROpen(1, &xlrDevice);
  
  	if(xlrRC == XLR_FAIL)
	{
		XLRClose(xlrDevice);
		cfatal << startl << "Cannot open Streamstor device.  Either this Mark5 unit has crashed, you do not have read/write permission to /dev/windrvr6, or some other process has full control of the Streamstor device." << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	else
	{
		cinfo << startl << "Success opening Streamstor device" << endl;
	}

	// FIXME -- for non-bank-mode operation, need to look at the modules to determine what to do here.
	xlrRC = XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL);
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot put Mark5 unit in bank mode" << endl;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC == XLR_SUCCESS)
	{
		xlrRC = XLRSetOption(xlrDevice, SS_OPT_REALTIMEPLAYBACK);
	}
	if(xlrRC == XLR_SUCCESS)
	{
		xlrRC = XLRSetFillData(xlrDevice, FILL_PATTERN);
	}
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot set Mark5 data replacement mode / fill pattern" << endl;
	}

	module.nscans = -1;
	readpointer = -1;
	scan = 0;
	lastval = 0xFFFFFFFF;
	mark5stream = 0;
	filltime = 0;
	invalidtime = 0;
	invalidstart = 0;
	newscan = 0;
	lastrate = 0.0;
	nomoredata = false;
	nfill = ninvalid = ngood = 0;
	nrate = 0;
	sendMark5Status(MARK5_STATE_OPEN, 0, 0, 0.0, 0.0);
}

static void setDiscModuleState(SSHANDLE xlrDevice, const char *newState)
{
	XLR_RETURN_CODE xlrRC;
	char label[XLR_LABEL_LENGTH];
	int labelLength = 0, rs = 0;
	xlrRC = XLRGetLabel(xlrDevice, label);
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot read the Mark5 module label" << endl;
		return;
	}

	for(labelLength = 0; labelLength < XLR_LABEL_LENGTH; labelLength++)
	{
		if(!label[labelLength])
		{
			break;
		}
	}
	if(labelLength >= XLR_LABEL_LENGTH)
	{
		cwarn << startl << "Module label is not terminated!" << endl;
		return;
	}

	for(rs = 0; rs < labelLength; rs++)
	{
		if(label[rs] == 30)	// ASCII "RS" == "Record separator"
		{
			break;
		}
	}
	if(rs >= labelLength)
	{
		cwarn << startl << "Module label record separator not found!" << endl;
	}

	label[rs] = 0;
	cverbose << startl << "Module extended VSN is " << label << " Previous DMS is " << label+rs+1 << endl;

	if(strcmp(label+rs+1, newState) == 0)
	{
		// Nothing to do here
		return;
	}

	xlrRC = XLRClearWriteProtect(xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot clear Mark5 write protect" << endl;
	}
	cinfo << startl << "Setting module DMS to Played" << endl;
	label[rs] = 30;	// ASCII "RS" == "Record separator"
	strcpy(label+rs+1, newState);
	xlrRC = XLRSetLabel(xlrDevice, label, strlen(label));
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot set the Mark5 module state" << endl;
	}
	XLRSetWriteProtect(xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot set Mark5 write protect" << endl;
	}
}

NativeMk5DataStream::~NativeMk5DataStream()
{
	if(mark5stream)
	{
		delete_mark5_stream(mark5stream);
	}

	if(ngood == 0 && ninvalid + nfill > 0)
	{
		cerror << startl << "All data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << ".  Please consider reading the module directory again and investigating the module health" << endl;
		sendMark5Status(MARK5_STATE_ERROR, 0, 0, 0.0, 0.0);
		nError++;
	}
	else if(ninvalid + nfill > ngood)
	{
		cerror << startl << "Most of the data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" << ngood << ".  Please consider  reading the module directory again and investigating the module health" << endl;
		sendMark5Status(MARK5_STATE_ERROR, 0, 0, 0.0, 0.0);
		nError++;
	}
	else if(9*(ninvalid + nfill) >= ngood)
	{
		int f = 100*(ninvalid + nfill)/(ninvalid + nfill + ngood);
		cwarn << startl << f << " percent of the data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" <<     ngood << "." << endl;
	}
	else
	{
		cinfo << startl << "Data recovery statistics: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" <<     ngood << "." << endl;
	}

	if(nError == 0)
	{
		sendMark5Status(MARK5_STATE_CLOSE, 0, 0, 0.0, 0.0);
		XLRClose(xlrDevice);
	}
}

int NativeMk5DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
	static int last_offsetsec = -1;
	int r;
	
	// call parent class equivalent function and store return value
	r = Mk5DataStream::calculateControlParams(scan, offsetsec, offsetns);

	// check to see if we should send a status update
	if(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] == Mode::INVALID_SUBINT)
	{
		// Every second (of data time) send a reminder to the operator
		if(last_offsetsec > -1 && offsetsec != last_offsetsec)
		{
			double mjd = corrstartday + (corrstartseconds+offsetsec)/86400.0;
			
			// NO DATA implies that things are still good, but there is no data to be sent.
			sendMark5Status(MARK5_STATE_NODATA, 0, 0, mjd, 0.0);
		}
		last_offsetsec = offsetsec;
	}
	else  // set last value to indicate that this interval contained data.
	{
		last_offsetsec = -1;
	}

	// return parent class equivalent function return value
	return r;
}

/* Here "File" is VSN */
void NativeMk5DataStream::initialiseFile(int configindex, int fileindex)
{
	char defaultDirPath[] = ".";
	double startmjd;
	double scanstart, scanend;
	int v, i, fanout;
	int scanns = 0;
	long long n;
	int doUpdate = 0;
	char *mk5dirpath;
	char formatname[64];
	int nbits, nrecordedbands, framebytes;
	Configuration::dataformat format;
	double bw;

	format = config->getDataFormat(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	framebytes = config->getFrameBytes(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);
	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, framebytes, config->getDDecimationFactor(configindex, streamnum), formatname);
        if(fanout < 0)
          MPI_Abort(MPI_COMM_WORLD, 1);

	cinfo << startl << "initialiseFile format=" << formatname << endl;
	if(mark5stream)
	{
		delete_mark5_stream(mark5stream);
	}
	mark5stream = new_mark5_stream(
	  new_mark5_stream_unpacker(0),
	  new_mark5_format_generic_from_string(formatname) );

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(mk5dirpath == 0)
	{
		mk5dirpath = defaultDirPath;
	}

	startmjd = corrstartday + corrstartseconds/86400.0;

	sendMark5Status(MARK5_STATE_GETDIR, 0, 0, startmjd, 0.0);

	if(module.nscans < 0)
	{
		doUpdate = 1;
		cinfo << startl << "getting module info" << endl;
		v = getCachedMark5Module(&module, &xlrDevice, corrstartday, 
			datafilenames[configindex][fileindex].c_str(), 
			mk5dirpath, &dirCallback, &mk5status, 0);

		if(v < 0)
		{
			cerror << startl << "Module " << 
				datafilenames[configindex][fileindex] << 
				" not found in unit - aborting!!!" << endl;
			dataremaining = false;
			return;
		}

		v = sanityCheckModule(&module);
		if(v < 0)
		{
			csevere << startl << "Module " << 
				datafilenames[configindex][fileindex] <<
				" contains undecoded scans - aborting!!!" << endl;
			dataremaining = false;
			return;
		}

		// Set the module state to "Played"
		setDiscModuleState(xlrDevice, "Played");
	}

	sendMark5Status(MARK5_STATE_GOTDIR, 0, 0, startmjd, 0.0);

	// find starting position
  
	if(scan != 0)  /* just continue by reading next valid scan */
	{
		cinfo << startl << "Advancing to next Mark5 module scan" << endl;
		do
		{
			scan++;
		} while(scan-module.scans < module.nscans && scan->duration < 0.1);
		if(scan-module.scans >= module.nscans)
		{
			cwarn << startl << "No more data on module [" << mpiid << "]" << endl;
			scan = 0;
			dataremaining = false;
			keepreading = false;
			nomoredata = true;
			sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0, 0.0, 0.0);
			return;
		}
		scanns = int(1000000000.0*scan->framenuminsecond/scan->framespersecond + 0.1);
		cverbose << startl << "Before scan change: readscan = " << readscan << "  readsec = " << readseconds << "  readns = " << readnanoseconds << endl;
		scanstart = scan->mjd + (scan->sec + scanns*1.e-9)/86400.0;
		scanend = scanstart + scan->duration/86400.0;
		readpointer = scan->start + scan->frameoffset;
		readseconds = (scan->mjd-corrstartday)*86400 + scan->sec - corrstartseconds;
		readnanoseconds = scanns;
                while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
                  readscan++;
                while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
                  readscan--;
                readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);

		cinfo << startl << "After scan change: readscan = " << readscan << " rs = " << readseconds << "  rns = " << readnanoseconds << endl;

		if(readscan == model->getNumScans() - 1 && readseconds >= model->getScanDuration(readscan))
		{
			cwarn << startl << "No more data for project on module [" << mpiid << "]" << endl;
			scan = 0;
			dataremaining = false;
			keepreading = false;
			nomoredata = true;
			sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0, 0.0, 0.0);
			return;
		}
	}
	else	/* first time this project */
	{
		n = 0;
		for(i = 0; i < module.nscans; i++)
		{
			double scanstart, scanend;
			scan = module.scans + i;
			scanns = int(1000000000.0*scan->framenuminsecond/scan->framespersecond + 0.1);
			scanstart = scan->mjd + (scan->sec + scanns*1.e-9)/86400.0;
			scanend = scanstart + scan->duration/86400.0;

 			if(startmjd < scanstart)  /* obs starts before data */
			{
				cinfo << startl << "NM5 : scan found(1) : " << (i+1) << endl;
				readpointer = scan->start + scan->frameoffset;
				readseconds = (scan->mjd-corrstartday)*86400 
					+ scan->sec - corrstartseconds;
				readnanoseconds = scanns;
                                while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
                                  readscan++;
                                while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
                                  readscan--;
                                readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
				break;
			}
			else if(startmjd < scanend) /* obs starts within data */
			{
				cinfo << startl << "NM5 : scan found(2) : " << (i+1) << endl;
				readpointer = scan->start + scan->frameoffset;
				n = (long long)((((corrstartday - scan->mjd)*86400 
			+ (corrstartseconds - (scan->sec + scanns*1.e-9)))
					*config->getFramesPerSecond(configindex, streamnum)) + 0.5);
				readpointer += n*scan->framebytes;
				readseconds = 0;
				readnanoseconds = 0;
                                while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
                                  readscan++;
                                while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
                                  readscan--;
                                readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
				break;
			}
		}
		cinfo << startl << "NativeMk5DataStream " << mpiid << 
			" positioned at byte " << readpointer << 
			" scan = " << readscan << " seconds = " << readseconds <<
                        " ns = " << readnanoseconds << " n = " << n << endl;

		if(i >= module.nscans || scan == 0)
		{
			cerror << startl << "No valid data found.  Stopping playback!" << endl;
			dataremaining = false;
			sendMark5Status(MARK5_STATE_NODATA, 0, 0, 0.0, 0.0);
			return;
		}

		cinfo << startl << "Scan info. start = " << scan->start << " off = " << scan->frameoffset << " size = " << scan->framebytes << endl;
	}

	sendMark5Status(MARK5_STATE_GOTDIR, scan-module.scans+1, readpointer, scan->mjd+(scan->sec+scanns*1.e-9)/86400.0, 0.0);

	newscan = 1;

	cinfo << startl << "The frame start day is " << scan->mjd << 
		", the frame start seconds is " << (scan->sec+scanns*1.e-9)
		<< ", readscan is " << readscan << ", readseconds is " << readseconds << 
		", readnanoseconds is " << readnanoseconds << endl;

	/* update all the configs - to ensure that the nsincs and 
	 * headerbytes are correct
	 */
	if(doUpdate)
	{
		cinfo << startl << "Updating all configs [" << mpiid << "]" << endl;
		for(i = 0; i < numdatasegments; i++)
		{
			updateConfig(i);
		}
	}
	else
	{
		cinfo << startl << "NOT updating all configs [" << mpiid << "]" << endl;
	}

	cinfo << startl << "Scan " << (scan-module.scans) <<" initialised[" << mpiid << "]" << endl;
}

void NativeMk5DataStream::openfile(int configindex, int fileindex)
{
	cinfo << startl << "NativeMk5DataStream " << mpiid << 
		" is about to look at a scan" << endl;

	/* fileindex should never increase for native mark5, but
	 * check just in case. 
	 */
	if(fileindex >= confignumfiles[configindex])
	{
		dataremaining = false;
		keepreading = false;
		cinfo << startl << "NativeMk5DataStream " << mpiid << 
			" is exiting because fileindex is " << fileindex << 
			", while confignumconfigfiles is " << 
			confignumfiles[configindex] << endl;
		return;
	}

	dataremaining = true;

	initialiseFile(configindex, fileindex);
}

void NativeMk5DataStream::moduleToMemory(int buffersegment)
{
	long long start;
	unsigned long *buf, *data;
	unsigned long a, b;
	int i, t;
	S_READDESC      xlrRD;
	XLR_RETURN_CODE xlrRC;
	XLR_ERROR_CODE  xlrEC;
	XLR_READ_STATUS xlrRS=XLR_SUCCESS;
	int bytes;
	char errStr[XLR_ERROR_LENGTH];
	static int now = 0;
	static long long lastpos = 0;
	struct timeval tv;
	int mjd, sec, sec2;
	double ns;

	/* All reads of a module must be 64 bit aligned */
	bytes = readbytes;
	start = readpointer;
	data = buf = (unsigned long *)&databuffer[buffersegment*(bufferbytes/
		numdatasegments)];
	if(start & 4)
	{
		start += 4;
		*buf = lastval;
		buf++;
	}

	// we're starting after the end of the scan.  Just set flags and return
	if(start >= scan->start + scan->length)
	{
		bufferinfo[buffersegment].validbytes = 0;
		dataremaining = false;
		return;
	}

	if(start + bytes > scan->start + scan->length)
	{
		bytes = scan->start + scan->length - start;
		cverbose << startl << "At end of scan: shortening Mark5 read to only " << bytes << " bytes " << "(was " << readbytes << ")" << endl;
	}

	/* always read multiples of 8 bytes */
	bytes &= ~7;

	a = start >> 32;
	b = start & 0xFFFFFFFF; 

	waitForBuffer(buffersegment);

	xlrRD.AddrHi = a;
	xlrRD.AddrLo = b;
	xlrRD.XferLength = bytes;
	xlrRD.BufferAddr = buf;

	for(t = 0; t < 2; t++)
	{
		xlrRC = XLRReadImmed(xlrDevice, &xlrRD);

		if(xlrRC != XLR_SUCCESS)
		{
			xlrEC = XLRGetLastError();
			XLRGetErrorMessage(errStr, xlrEC);
			cerror << startl << "Cannot read data from Mark5 module: [1] position=" << readpointer << ", length=" << bytes << ", error=" << errStr << endl;
			dataremaining = false;
			keepreading = false;
			bufferinfo[buffersegment].validbytes = 0;

			double errorTime = corrstartday + (model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + corrstartseconds + readnanoseconds*1.0e-9)/86400.0;
			sendMark5Status(MARK5_STATE_ERROR, scan-module.scans+1, readpointer, errorTime, 0.0);
			nError++;
			return;
		}

		/* Wait up to 5 seconds for a return */
		for(i = 1; i < 60; i++)
		{
			xlrRS = XLRReadStatus(0);
			if(xlrRS == XLR_READ_COMPLETE)
			{
				break;
			}
			else if(xlrRS == XLR_READ_ERROR)
			{
				xlrEC = XLRGetLastError();
				XLRGetErrorMessage(errStr, xlrEC);
				cerror << startl << "Cannot read data from Mark5 module: [2] position=" << readpointer << ", length=" << bytes << ", error=" << errStr << endl;

				dataremaining = false;
				keepreading = false;
				bufferinfo[buffersegment].validbytes = 0;
				double errorTime = corrstartday + (model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + corrstartseconds + readnanoseconds*1.0e-9)/86400.0;
				sendMark5Status(MARK5_STATE_ERROR, scan-module.scans+1, readpointer, errorTime, 0.0);
				nError++;
				return;
			}
			if(i % 10 == 0 && i > 30)
			{
				cwarn << startl << "Waited " << (i/10) << " sec  state="; 
				if(xlrRS == XLR_READ_WAITING)
				{
					cwarn << "XLR_READ_WAITING" << endl;
				}
				else if(xlrRS == XLR_READ_RUNNING)
				{
					cwarn << "XLR_READ_RUNNING" << endl;
				}
				else
				{
					cwarn << "XLR_READ_OTHER" << endl;
				}
			}
			usleep(100000);
		}
		if(xlrRS == XLR_READ_COMPLETE)
		{
			break;
		}
		else if(t == 0)
		{
			cwarn << startl << "XLRCardReset() being called!" << endl;
			xlrRC = XLRCardReset(1);
			if(xlrRC != XLR_SUCCESS)
			{
				cerror << startl << "XLRCardReset() failed.  Remainder of data from this antenna will not be correlated and a reboot of this Mark5 unit is probably needed." << endl;
				sendMark5Status(MARK5_STATE_ERROR, scan-module.scans+1, readpointer, 0.0, 0.0);
				nError++;
				dataremaining = false;
				keepreading = false;
				bufferinfo[buffersegment].validbytes = 0;
				return;
			}
			else
			{
				cinfo << startl << "XLRCardReset() success!" << endl;
			}

			cinfo << startl << "XLROpen() being called!" << endl;
			xlrRC = XLROpen(1, &xlrDevice);
			if(xlrRC != XLR_SUCCESS)
			{
				cerror << startl << "XLROpen() failed.  Remainder of data from this antenna will not be correlated and a reboot of this Mark5 unit is probably needed." << endl;
				sendMark5Status(MARK5_STATE_ERROR, scan-module.scans+1, readpointer, 0.0, 0.0);
				nError++;
				dataremaining = false;
				keepreading = false;
				bufferinfo[buffersegment].validbytes = 0;
				return;
			}
			else
			{
				cinfo << startl << "XLROpen() success!" << endl;
			}

			xlrRC = XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL);
			if(xlrRC != XLR_SUCCESS)
			{
				cerror << startl << "Cannot put Mark5 unit in bank mode" << endl;
			}

			xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
			if(xlrRC == XLR_SUCCESS)
			{
				xlrRC = XLRSetOption(xlrDevice, SS_OPT_REALTIMEPLAYBACK);
			}
			if(xlrRC == XLR_SUCCESS)
			{
				xlrRC = XLRSetFillData(xlrDevice, FILL_PATTERN);
			}
			if(xlrRC != XLR_SUCCESS)
			{
				cerror << startl << "Cannot set Mark5 data replacement mode / fill pattern" << endl;
			}
		}
	}

	if(xlrRS != XLR_READ_COMPLETE)
	{
		cerror << startl << "Waited 6 seconds for a Mark5 module read and gave up.  position=" << readpointer << " length=" << bytes << endl;
		bufferinfo[buffersegment].validbytes = 0;
		return;
	}

	bufferinfo[buffersegment].validbytes = bytes;
	bufferinfo[buffersegment].readto = true;
	lastval = buf[bytes/4-1];

	// Check for validity
	mark5stream->frame = (unsigned char *)data;
	mark5_stream_get_frame_time(mark5stream, &mjd, &sec, &ns);
	mark5stream->frame = 0;
	sec2 = (model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + corrstartseconds) % 86400;

	if( (data[1] == FILL_PATTERN && data[2] == FILL_PATTERN) ||
	    (data[998] == FILL_PATTERN && data[999] == FILL_PATTERN) )
	{
		filltime++;
		nfill++;
		// use Brian Kernighan's bit counting trick to see if invalidtime is a power of 2 
		if(filltime > 5 && (filltime & (filltime-1)) == 0)
		{
			cwarn << startl << filltime << " consecutive Mark5 data frames replaced with fill patterns at time " << sec2 << "," << readnanoseconds << endl ;
		}

		if(filltime > 1)
		{
			bufferinfo[buffersegment].validbytes = 0;
		}
	}
	else if((sec % 86400) != sec2 || fabs(ns - readnanoseconds) > 0.5)
	{
		invalidtime++;
		ninvalid++;
		invalidstart = readpointer;
		bufferinfo[buffersegment].validbytes = 0;
		// use Brian Kernighan's bit counting trick to see if invalidtime is a power of 2 
		if((invalidtime & (invalidtime-1)) == 0)
		{
			cwarn << startl << invalidtime << " consecutive Mark5 sync errors starting at readpos " << invalidstart << " (" << mjd << "," << sec << "," << ns << ")!=(" << sec2 << "," << readnanoseconds << ")" << " length=" << bytes << endl ;
		}
		if(invalidtime == 100)
		{
			cerror << startl << invalidtime << " consecutive Mark5 sync errors.  Something is probably wrong!" << endl;
		}
		// FIXME -- if invalidtime > threshhold, look for sync again
	}
	else
	{
		ngood++;
		filltime = 0;
		invalidtime = 0;
	}


	gettimeofday(&tv, 0);
	if(tv.tv_sec > now)
	{
		now = tv.tv_sec;
		if(lastpos > 0)
		{
			double rate;
			double fmjd, fmjd2;
			enum Mk5State state;

			fmjd = corrstartday + (corrstartseconds + model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + (double)readnanoseconds/1000000000.0)/86400.0;
			if(newscan > 0)
			{
				newscan = 0;
				state = MARK5_STATE_START;
				rate = 0.0;
				lastrate = 0.0;
				nrate = 0;
				fmjd2 = scan->mjd + (scan->sec + (float)scan->framenuminsecond/scan->framespersecond)/86400.0;
				if(fmjd2 > fmjd)
				{
					fmjd = fmjd2;
				}
			}
			else if(invalidtime == 0)
			{
				state = MARK5_STATE_PLAY;
				rate = (double)(readpointer + bytes - lastpos)*8.0/1000000.0;
				if(nrate > 1)
				{
					rate = (nrate*lastrate + 4*rate)/(nrate+4);
				}
				lastrate = rate;
				nrate++;
			}
			else
			{
				state = MARK5_STATE_PLAYINVALID;
				rate = invalidtime;
				nrate = 0;
			}

			sendMark5Status(state, scan-module.scans+1, readpointer, fmjd, rate);
		}
		lastpos = readpointer + bytes;
	}

	// Update various counters
	readnanoseconds += (bufferinfo[buffersegment].nsinc % 1000000000);
	readseconds += (bufferinfo[buffersegment].nsinc / 1000000000);
	readseconds += readnanoseconds/1000000000;
	readnanoseconds %= 1000000000;
        if(readseconds >= model->getScanDuration(readscan)) {
          if(readscan < model->getNumScans()-1) {
            readscan++;
            readseconds -= model->getScanStartSec(readscan, corrstartday, corrstartseconds) - model->getScanStartSec(readscan-1, corrstartday, corrstartseconds);
            cdebug << startl << "Incrementing scan to " << readscan << ", readseconds is now " << readseconds << endl;
	    if(readseconds < -1)
	    {
	    	int skipseconds = -readseconds-1;
		readpointer += (long long)(skipseconds)*(long long)(scan->framebytes)*(long long)(scan->framespersecond);
		readseconds += skipseconds;
		cdebug << startl << "Skipping " << skipseconds << " seconds at scan boundary." << endl;
	    }
          }
          else
            keepreading = false;
        }
	if(bytes < readbytes)
	{
		dataremaining = false;
	}
	else
	{
		readpointer += bytes;
	}
	if(readpointer >= scan->start + scan->length)
	{
		dataremaining = false;
	}
}

void NativeMk5DataStream::loopfileread()
{
  int perr;
  int numread = 0;

  //lock the outstanding send lock
  perr = pthread_mutex_lock(&outstandingsendlock);
  if(perr != 0)
    csevere << startl << "Error in initial telescope readthread lock of outstandingsendlock!!!" << endl;

  //lock the first section to start reading
  openfile(bufferinfo[0].configindex, 0);
  moduleToMemory(numread++);
  moduleToMemory(numread++);
  perr = pthread_mutex_lock(&(bufferlock[numread]));
  if(perr != 0)
    csevere << startl << "Error in initial telescope readthread lock of first buffer section!!!" << endl;
  readthreadstarted = true;
  perr = pthread_cond_signal(&initcond);
  if(perr != 0)
    csevere << startl << "NativeMk5DataStream readthread " << mpiid << " error trying to signal main thread to wake up!!!" << endl;
  moduleToMemory(numread++);

  lastvalidsegment = (numread-1)%numdatasegments;
  while((bufferinfo[lastvalidsegment].configindex < 0 || filesread[bufferinfo[lastvalidsegment].configindex] <= confignumfiles[bufferinfo[lastvalidsegment].configindex]) && keepreading)
  {
    while(dataremaining && keepreading)
    {
      lastvalidsegment = (lastvalidsegment + 1)%numdatasegments;
      
      //lock the next section
      perr = pthread_mutex_lock(&(bufferlock[lastvalidsegment]));
      if(perr != 0)
        csevere << startl << "Error in telescope readthread lock of buffer section!!!" << lastvalidsegment << endl;

      //unlock the previous section
      perr = pthread_mutex_unlock(&(bufferlock[(lastvalidsegment-1+numdatasegments)% numdatasegments]));
      if(perr != 0)
        csevere << startl << "Error in telescope readthread unlock of buffer section!!!" << (lastvalidsegment-1+numdatasegments)%numdatasegments << endl;

      //do the read
      moduleToMemory(lastvalidsegment);
      numread++;
    }
    if(keepreading)
    {
      //if we need to, change the config
      int nextconfigindex = config->getScanConfigIndex(readscan);
      cinfo << startl << "old config[" << mpiid << "] = " << nextconfigindex << endl;
      while(nextconfigindex < 0 && readscan < model->getNumScans()) {
        readseconds = 0; 
        nextconfigindex = config->getScanConfigIndex(++readscan);
      }
      if(readscan == model->getNumScans())
      {
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scan = model->getNumScans()-1;
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = model->getScanDuration(model->getNumScans()-1);
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
        keepreading = false;
      }
      else
      {
        if(config->getScanConfigIndex(readscan) != bufferinfo[(lastvalidsegment + 1)%numdatasegments].configindex)
          updateConfig((lastvalidsegment + 1)%numdatasegments);
	//if the datastreams for two or more configs are common, they'll all have the same 
        //files.  Therefore work with the lowest one
        int lowestconfigindex = bufferinfo[(lastvalidsegment+1)%numdatasegments].configindex;
        for(int i=config->getNumConfigs()-1;i>=0;i--)
        {
          if(config->getDDataFileNames(i, streamnum) == config->getDDataFileNames(lowestconfigindex, streamnum))
            lowestconfigindex = i;
        }
        openfile(lowestconfigindex, filesread[lowestconfigindex]);
      }
      if(keepreading == false)
      {
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = config->getExecuteSeconds();
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
      }
    }
  }
  perr = pthread_mutex_unlock(&(bufferlock[lastvalidsegment]));
  if(perr != 0)
    csevere << startl << "Error in telescope readthread unlock of buffer section!!!" << lastvalidsegment << endl;

  //unlock the outstanding send lock
  perr = pthread_mutex_unlock(&outstandingsendlock);
  if(perr != 0)
    csevere << startl << "Error in telescope readthread unlock of outstandingsendlock!!!" << endl;

  cinfo << startl << "Readthread is exiting! Filecount was " << filesread[bufferinfo[lastvalidsegment].configindex] << ", confignumfiles was " << confignumfiles[bufferinfo[lastvalidsegment].configindex] << ", dataremaining was " << dataremaining << ", keepreading was " << keepreading << endl;
}

int NativeMk5DataStream::sendMark5Status(enum Mk5State state, int scanNum, long long position, double dataMJD, float rate)
{
	int v = 0;
	S_BANKSTATUS A, B;
	XLR_RETURN_CODE xlrRC;

	// If there really is no more data, override a simple NODATA with a more precise response
	if(nomoredata == true && state == MARK5_STATE_NODATA)
	{
		state = MARK5_STATE_NOMOREDATA;
	}

	mk5status.state = state;
	mk5status.status = 0;
	mk5status.activeBank = ' ';
	mk5status.position = position;
	mk5status.rate = rate;
	mk5status.dataMJD = dataMJD;
	mk5status.scanNumber = scanNum;
	if(scan)
	{
		strcpy(mk5status.scanName, scan->name);
	}
	else
	{
		strcpy(mk5status.scanName, "none");
	}
	if(state != MARK5_STATE_OPENING && state != MARK5_STATE_ERROR && state != MARK5_STATE_IDLE)
	{
		xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &A);
		if(xlrRC == XLR_SUCCESS)
		{
			xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &B);
		}
		if(xlrRC == XLR_SUCCESS)
		{
			strncpy(mk5status.vsnA, A.Label, 8);
			mk5status.vsnA[8] = 0;
			if(strncmp(mk5status.vsnA, "LABEL NO", 8) == 0)
			{
				strcpy(mk5status.vsnA, "none");
			}
			strncpy(mk5status.vsnB, B.Label, 8);
			mk5status.vsnB[8] = 0;
			if(strncmp(mk5status.vsnB, "LABEL NO", 8) == 0)
			{
				strcpy(mk5status.vsnB, "none");
			}
			if(A.Selected)
			{
				mk5status.activeBank = 'A';
				mk5status.status |= 0x100000;
			}
			if(A.State == STATE_READY)
			{
				mk5status.status |= 0x200000;
			}
			if(A.MediaStatus == MEDIASTATUS_FAULTED)
			{
				mk5status.status |= 0x400000;
			}
			if(A.WriteProtected)
			{
				mk5status.status |= 0x800000;
			}
			if(B.Selected)
			{
				mk5status.activeBank = 'B';
				mk5status.status |= 0x1000000;
			}
			if(B.State == STATE_READY)
			{
				mk5status.status |= 0x2000000;
			}
			if(B.MediaStatus == MEDIASTATUS_FAULTED)
			{
				mk5status.status |= 0x4000000;
			}
			if(B.WriteProtected)
			{
				mk5status.status |= 0x8000000;
			}
		}
		if(xlrRC != XLR_SUCCESS)
		{
			mk5status.state = MARK5_STATE_ERROR;
		}
	}
	else
	{
		sprintf(mk5status.vsnA, "???");
		sprintf(mk5status.vsnB, "???");
	}
	switch(mk5status.state)
	{
	case MARK5_STATE_PLAY:
		mk5status.status |= 0x0100;
		break;
	case MARK5_STATE_ERROR:
		mk5status.status |= 0x0002;
		break;
	case MARK5_STATE_IDLE:
		mk5status.status |= 0x0001;
		break;
	default:
		break;
	}

	v = difxMessageSendMark5Status(&mk5status);

	return v;
}
