/***************************************************************************
 *   Copyright (C) 2007-2013 by Walter Brisken and Adam Deller             *
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

#include <cstring>
#include <cstdlib>
#include <ctype.h>
#include <cmath>
#include <sys/time.h>
#include <mpi.h>
#include <unistd.h>
#include <vdifio.h>
#include "config.h"
#include "nativemk5.h"
#include "watchdog.h"
#include "alert.h"
#include "mark5utils.h"

#if HAVE_MARK5IPC
#include <mark5ipc.h>
#endif



int NativeMk5DataStream::resetDriveStats()
{
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	const int defaultStatsRange[] = { 75000, 150000, 300000, 600000, 1200000, 2400000, 4800000, -1 };

	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_DRVSTATS) );
	for(int b = 0; b < XLR_MAXBINS; ++b)
	{
		driveStats[b].range = defaultStatsRange[b];
		driveStats[b].count = 0;
	}
	WATCHDOGTEST( XLRSetDriveStats(xlrDevice, driveStats) );

	return 0;
}

int NativeMk5DataStream::reportDriveStats()
{
	XLR_RETURN_CODE xlrRC;
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	DifxMessageDriveStats driveStatsMessage;

	snprintf(driveStatsMessage.moduleVSN, DIFX_MESSAGE_MARK5_VSN_LENGTH+1, "%s",  datafilenames[0][0].c_str());
	driveStatsMessage.type = DRIVE_STATS_TYPE_READ;

	/* FIXME: for now don't include complete information on drives */
	strcpy(driveStatsMessage.serialNumber, "");
	strcpy(driveStatsMessage.modelNumber, "");
	driveStatsMessage.diskSize = 0;
	driveStatsMessage.startByte = 0;

	for(int d = 0; d < 8; ++d)
	{
		for(int i = 0; i < DIFX_MESSAGE_N_DRIVE_STATS_BINS; ++i)
		{
			driveStatsMessage.bin[i] = -1;
		}
		driveStatsMessage.moduleSlot = d;
		WATCHDOG( xlrRC = XLRGetDriveStats(xlrDevice, d/2, d%2, driveStats) );
		if(xlrRC == XLR_SUCCESS)
		{
			for(int i = 0; i < XLR_MAXBINS; ++i)
			{
				if(i < DIFX_MESSAGE_N_DRIVE_STATS_BINS)
				{
					driveStatsMessage.bin[i] = driveStats[i].count;
				}
			}
		}
		difxMessageSendDriveStats(&driveStatsMessage);
	}

	resetDriveStats();

	return 0;
}

void NativeMk5DataStream::openStreamstor()
{
	XLR_RETURN_CODE xlrRC;

	sendMark5Status(MARK5_STATE_OPENING, 0, 0.0, 0.0);

	cinfo << startl << "Opening Streamstor" << endl;
	WATCHDOG( xlrRC = XLROpen(1, &xlrDevice) );
  
  	if(xlrRC == XLR_FAIL)
	{
#if HAVE_MARK5IPC
                unlockMark5();
#endif
		WATCHDOG( XLRClose(xlrDevice) );
		cfatal << startl << "Cannot open Streamstor device.  Either this Mark5 unit has crashed, you do not have read/write permission to /dev/windrvr6, or some other process has full control of the Streamstor device." << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	else
	{
		cinfo << startl << "Success opening Streamstor device" << endl;
	}

	// FIXME: for non-bank-mode operation, need to look at the modules to determine what to do here.
	WATCHDOG( xlrRC = XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	if(xlrRC != XLR_SUCCESS)
	{
		cerror << startl << "Cannot put Mark5 unit in bank mode" << endl;
	}

	WATCHDOG( XLRSetMode(xlrDevice, SS_MODE_SINGLE_CHANNEL) );
	WATCHDOG( XLRClearChannels(xlrDevice) );
	WATCHDOG( XLRSelectChannel(xlrDevice, 0) );
	WATCHDOG( XLRBindOutputChannel(xlrDevice, 0) );

	sendMark5Status(MARK5_STATE_OPEN, 0, 0.0, 0.0);
}

void NativeMk5DataStream::closeStreamstor()
{
	sendMark5Status(MARK5_STATE_CLOSE, 0, 0.0, 0.0);
	WATCHDOG( XLRClose(xlrDevice) );
}

void NativeMk5DataStream::resetStreamstor()
{
	sendMark5Status(MARK5_STATE_RESETTING, 0, 0.0, 0.0);
	WATCHDOG( XLRReset(xlrDevice) );
}

NativeMk5DataStream::NativeMk5DataStream(const Configuration * conf, int snum, 
	int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor, 
	numsegments)
{
	int perr;

	/* each data buffer segment contains an integer number of frames, 
	 * because thats the way config determines max bytes
	 */

	scanNum = -1;
	readpointer = -1;
	scanPointer = 0;
	lastval = 0xFFFFFFFF;
	mark5stream = 0;
	filltime = 0;
	invalidtime = 0;
	invalidstart = 0;
	newscan = 0;
	lastrate = 0.0;
	noMoreData = false;
	nfill = ninvalid = ngood = 0;
	nrate = 0;
	nError = 0;
	nDMAError = 0;
	readDelayMicroseconds = 0;
	noDataOnModule = false;
	nReads = 0;

#if HAVE_MARK5IPC
        int v = lockMark5(5);
        {
                if(v)
                {
                        sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
                        ++nError;
                        cfatal << startl << "Cannot obtain lock for Streamstor device." << endl;
                        MPI_Abort(MPI_COMM_WORLD, 1);
                }
        }
#endif
	openStreamstor();

        // Start up mark5 watchdog thread
        perr = initWatchdog();
        if(perr != 0)
        {
                csevere << startl << "Cannot create the nativemk5 watchdog thread!" << endl;
                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	resetDriveStats();
}

NativeMk5DataStream::~NativeMk5DataStream()
{
	if(mark5stream)
	{
		if(mark5stream->nvalidatefail > mark5stream->nvalidatepass/49)
		{
			cerror << startl << "There were " << mark5stream->nvalidatefail << "/" << (mark5stream->nvalidatefail + mark5stream->nvalidatepass) << " data validation failures.  This number is high and might indicate a problem with the formatter at the station.  This is unlikely to be a playback problem." << endl;
		}
		else if(mark5stream->nvalidatefail > 0)
		{
			cwarn << startl << "There were " << mark5stream->nvalidatefail << "/" << (mark5stream->nvalidatefail + mark5stream->nvalidatepass) << " data validation failures." << endl;
		}

		delete_mark5_stream(mark5stream);
	}

	if(ngood == 0 && ninvalid + nfill > 0)
	{
		cerror << startl << "All data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << ".  Please consider reading the module directory again and investigating the module health" << endl;
		sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
		++nError;
	}
	else if(ninvalid + nfill > ngood)
	{
		cerror << startl << "Most of the data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" << ngood << ".  Please consider reading the module directory again and investigating the module health" << endl;
		sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
		++nError;
	}
	else if(9*(ninvalid + nfill) > ngood)
	{
		int f = 100*(ninvalid + nfill)/(ninvalid + nfill + ngood);
		cwarn << startl << f <<" percent of the data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" <<     ngood << "." << endl;
	}
	else
	{
		cinfo << startl << "Data recovery statistics: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" << ngood << "." << endl;
	}
	if(readDelayMicroseconds > 0)
	{
		cinfo << startl << "To reduce read rate in RT mode, read delay was set to " << readDelayMicroseconds << " microseconds" << endl;
	}

	reportDriveStats();

	if(nError > 0)
	{
		cwarn << startl << nError << " errors were encountered reading this module" << endl;
	}
	closeStreamstor();
#if HAVE_MARK5IPC
	unlockMark5();
#endif

        // stop watchdog thread
        stopWatchdog();
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
			sendMark5Status(MARK5_STATE_NODATA, 0, mjd, 0.0);
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
	int v, fanout;
	long long n;
	int doUpdate = 0;
	char *mk5dirpath;
	int nbits, nrecordedbands, framebytes, fbytes;
	Configuration::dataformat format;
	double bw;
	XLR_RETURN_CODE xlrRC;

	format = config->getDataFormat(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	framebytes = config->getFrameBytes(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);
	/* mark5stream is only ever used for checking after the data comes into memory (scanPointer is used here in initialiseFile
           accordingly, adjust framebytes for multiplexed data, if multiplexing is being performed (nBands does not change). */
        if(config->isDMuxed(configindex, streamnum)) {
          framebytes = (framebytes-VDIF_HEADER_BYTES)*config->getDNumMuxThreads(configindex, streamnum) + VDIF_HEADER_BYTES;
        }
	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, config->getDSampling(configindex, streamnum), framebytes, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
        if(fanout < 0)
        {
#if HAVE_MARK5IPC
                unlockMark5();
#endif
                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	cinfo << startl << "initialiseFile format=" << formatname << endl;
	if(mark5stream)
	{
		delete_mark5_stream(mark5stream);
	}
	mark5stream = new_mark5_stream_absorb(
	  new_mark5_stream_unpacker(0),
	  new_mark5_format_generic_from_string(formatname) );

	framegranularity = mark5stream->framegranularity;

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(mk5dirpath == 0)
	{
		mk5dirpath = defaultDirPath;
	}

	startmjd = corrstartday + corrstartseconds/86400.0;

	sendMark5Status(MARK5_STATE_GETDIR, 0, startmjd, 0.0);

	if(module.nScans() == 0)
	{
		doUpdate = 1;
		cinfo << startl << "Getting module " << datafilenames[configindex][fileindex] << " directory info." << endl;
		v = module.getCachedDirectory(xlrDevice, corrstartday, 
			datafilenames[configindex][fileindex].c_str(), 
			mk5dirpath, &dirCallback, &mk5status, 0, 0, 0, 1, -1, -1);

		if(v < 0)
		{
                	if(module.error.str().size() > 0)
			{
				cerror << startl << module.error.str() << " (error code=" << v << ")" << endl;
			}
			else
			{
				cerror << startl << "Directory for module " << datafilenames[configindex][fileindex] << " is not up to date.  Error code is " << v << " .  You should have received a more detailed error message than this!" << endl;
			}
			dataremaining = false;
			sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
			++nError;
			WATCHDOG( XLRClose(xlrDevice) );
#if HAVE_MARK5IPC
			unlockMark5();
#endif
			MPI_Abort(MPI_COMM_WORLD, 1);
		}

		v = module.sanityCheck();
		if(v < 0)
		{
			cerror << startl << "Module " << datafilenames[configindex][fileindex] << " contains undecoded scans" << endl;
			dataremaining = false;

			return;
		}

		if(module.mode == MARK5_READ_MODE_RT)
		{
			WATCHDOG( xlrRC = XLRSetFillData(xlrDevice, MARK5_FILL_PATTERN) );
			if(xlrRC == XLR_SUCCESS)
			{
				WATCHDOG( xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
			}
			if(xlrRC != XLR_SUCCESS)
			{
				cerror << startl << "Cannot set Mark5 data replacement mode / fill pattern" << endl;
			}
			// NOTE: removed WATCHDOG( xlrRC = XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
			cwarn << startl << "Enabled realtime playback mode" << endl;
			readDelayMicroseconds = 10000;	// prime the read delay to speed up convergence to best value
		}
	}

	sendMark5Status(MARK5_STATE_GOTDIR, 0, startmjd, 0.0);

	// find starting position

	if(scanPointer && scanNum >= 0)  /* just continue by reading next valid scan */
	{
		cinfo << startl << "Advancing to next Mark5 module scan" << endl;
		do
		{
			++scanNum;
			if(scanNum >= module.nScans())
			{
				break;
			}
			scanPointer = &module.scans[scanNum];
		} while(scanPointer->duration < 0.1);
		if(scanNum >= module.nScans())
		{
			cwarn << startl << "No more data for this job on this module" << endl;
			scanPointer = 0;
			dataremaining = false;
			keepreading = false;
			noMoreData = true;
			sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0.0, 0.0);

			return;
		}
		cverbose << startl << "Before scan change: readscan = " << readscan << "  readsec = " << readseconds << "  readns = " << readnanoseconds << endl;
		readpointer = scanPointer->start + scanPointer->frameoffset;
		readseconds = (scanPointer->mjd-corrstartday)*86400 + scanPointer->sec - corrstartseconds + intclockseconds;
		readnanoseconds = scanPointer->nsStart();
		while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
		{
			++readscan;
		}
		while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
		{
			--readscan;
		}
		readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);

		cinfo << startl << "After scan change: readscan = " << readscan << " rs = " << readseconds << "  rns = " << readnanoseconds << endl;

		if(readscan == model->getNumScans() - 1 && readseconds >= model->getScanDuration(readscan))
		{
			cwarn << startl << "No more data for project on module [" << mpiid << "]" << endl;
			scanPointer = 0;
			scanNum = -1;
			dataremaining = false;
			keepreading = false;
			noMoreData = true;
			sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0.0, 0.0);

			return;
		}
	}
	else	/* first time this project */
	{
		n = 0;
		for(scanNum = 0; scanNum < module.nScans(); ++scanNum)
		{
			double scanstart, scanend;
			scanPointer = &module.scans[scanNum];
			scanstart = scanPointer->mjdStart();
			scanend = scanPointer->mjdEnd();

 			if(startmjd < scanstart)  /* obs starts before data */
			{
				cinfo << startl << "NM5 : scan found(1) : " << (scanNum+1) << endl;
				readpointer = scanPointer->start + scanPointer->frameoffset;
				readseconds = (scanPointer->mjd-corrstartday)*86400 + scanPointer->sec - corrstartseconds + intclockseconds;
				readnanoseconds = scanPointer->nsStart();
				while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
				{
					++readscan;
				}
				while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
				{
					--readscan;
				}
				readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
				break;
			}
			else if(startmjd < scanend) /* obs starts within data */
			{
				cinfo << startl << "NM5 : scan found(2) : " << (scanNum+1) << endl;
				readpointer = scanPointer->start + scanPointer->frameoffset;
				n = static_cast<long long>((
					( ( (corrstartday - scanPointer->mjd)*86400 
					+ corrstartseconds - scanPointer->sec) - scanPointer->nsStart()*1.e-9)
					*scanPointer->framespersecond) + 0.5);
				fbytes = scanPointer->framebytes;
				if(datamuxer)
				{
					fbytes *= scanPointer->tracks;
				}
				readpointer += n*fbytes;
				readseconds = 0;
				readnanoseconds = 0;
				while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
				{
					++readscan;
				}
				while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
				{
					--readscan;
				}
				readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds) + intclockseconds;
				break;
			}
		}
		cinfo << startl << "NativeMk5DataStream " << mpiid << " positioned at byte " << readpointer << " scan = " << readscan << " seconds = " << readseconds << " ns = " << readnanoseconds << " n = " << n << endl;

		if(scanNum >= module.nScans() || scanPointer == 0)
		{
			cwarn << startl << "No valid data found.  Stopping playback!" << endl;
/*
			cerror << startl << "No valid data found.  Stopping playback!" << endl;
			dataremaining = false;
			keepreading = false;
			noMoreData = true;
			sendMark5Status(MARK5_STATE_NODATA, 0, 0.0, 0.0);
			scanNum = -1;
			scanPointer = 0;

			return;
*/
			scanNum = module.nScans()-1;
			scanPointer = &module.scans[scanNum];
			readpointer = scanPointer->start + scanPointer->length - (1<<21);
			if(readpointer < 0)
			{
				readpointer = 0;
			}
/*
			readseconds = (scanPointer->mjd-corrstartday)*86400 + scanPointer->sec - corrstartseconds + intclockseconds;
			readnanoseconds = scanPointer->nsStart();
*/
			readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds) + intclockseconds;
			readnanoseconds = 0;

			noDataOnModule = true;
		}

		cinfo << startl << "Scan info. start = " << scanPointer->start << " off = " << scanPointer->frameoffset << " size = " << scanPointer->framebytes << endl;
	}

        if(readpointer == -1)
        {
		cwarn << startl << "No data for this job on this module" << endl;
		scanPointer = 0;
		dataremaining = false;
		keepreading = false;
		noMoreData = true;
		sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0.0, 0.0);

		return;
        }

	sendMark5Status(MARK5_STATE_GOTDIR, readpointer, scanPointer->mjdStart(), 0.0);

	newscan = 1;

	cinfo << startl << "The frame start day is " << scanPointer->mjd << ", the frame start seconds is " << scanPointer->secStart()
		<< ", readscan is " << readscan << ", readseconds is " << readseconds << ", readnanoseconds is " << readnanoseconds << endl;

	/* update all the configs - to ensure that the nsincs and 
	 * headerbytes are correct
	 */
	if(doUpdate)
	{
		cinfo << startl << "Updating all configs [" << mpiid << "]" << endl;
		for(int i = 0; i < numdatasegments; ++i)
		{
			updateConfig(i);
		}
	}
	else
	{
		cinfo << startl << "NOT updating all configs [" << mpiid << "]" << endl;
	}

	cinfo << startl << "Scan " << scanNum <<" initialised[" << mpiid << "]" << endl;
}

void NativeMk5DataStream::openfile(int configindex, int fileindex)
{
	cinfo << startl << "NativeMk5DataStream " << mpiid << " is about to look at a scan" << endl;

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

int NativeMk5DataStream::readonedemux(bool resetreference, int buffersegment)
{
  long long localreadpointer;
  int fixbytes, rbytes;
  char * readto;
  bool ok;
  int nfix = 0;

  //check that the thread buffer is not getting too full
  if(datamuxer->getMinThreadBufferFree() < 3.0/(2.0*DataMuxer::DEMUX_BUFFER_FACTOR))
  {
    cwarn << startl << "Data muxer thread buffer getting full - skipping one read/deinterlace!" << endl;
    if(datamuxer->getMaxThreadBufferFree() > 0.5)
      cerror << startl << "Thread buffers are getting well out of sync - are one or more threads lagging? Min/max free space is " << datamuxer->getMinThreadBufferFree() << "/" << datamuxer->getMaxThreadBufferFree() << endl;
    return 0; // Note exit here, skipping the read this time!
  }

  //cinfo << startl << "At the beginning of readonedemux, readpointer is " << readpointer << endl;
  rbytes = moduleRead((unsigned long*)datamuxer->getCurrentDemuxBuffer(), datamuxer->getSegmentBytes(), readpointer, buffersegment);
  //the main readpointer will be updated outside of this routine, use localreadpointer for here
  localreadpointer = readpointer + rbytes;
  if(rbytes != datamuxer->getSegmentBytes()) {
    cerror << startl << "Data muxer did not fill demux buffer properly! Read " << rbytes << " bytes, wanted " << datamuxer->getSegmentBytes() << " bytes" << endl;
  }
  fixbytes = datamuxer->datacheck(datamuxer->getCurrentDemuxBuffer(), rbytes, 0);
  while(fixbytes > 0) {
    ++nfix;
    readto = reinterpret_cast<char*>(datamuxer->getCurrentDemuxBuffer()) + rbytes - fixbytes;
    moduleRead(reinterpret_cast<unsigned long*>(readto), fixbytes, localreadpointer, buffersegment);
    readpointer += fixbytes; //but if we need extra reads, then we must add these "extra" values to the readpointer
    localreadpointer += fixbytes; //and to the local one also of course
    fixbytes = datamuxer->datacheck(datamuxer->getCurrentDemuxBuffer(), rbytes, rbytes - fixbytes);
  }
  if(nfix>0)
    cwarn << startl << "Number of extra reads required due to interlopers was " << nfix << endl;
  if(resetreference)
    datamuxer->initialise();
  datamuxer->incrementReadCounter();
  ok = datamuxer->deinterlace(rbytes);
  if(!ok)
    MPI_Abort(MPI_COMM_WORLD, 1);
  //cinfo << startl << "At the end of readonedemux, readpointer will be  " << localreadpointer << endl;
  return rbytes;
}

int NativeMk5DataStream::moduleRead(unsigned long *destination, int nbytes, long long start, int buffersegment)
{
	int bytes = nbytes;
	XLR_RETURN_CODE xlrRC;
	XLR_ERROR_CODE  xlrEC;
	char errStr[XLR_ERROR_LENGTH];

	if(start & 4)
	{
		start += 4;
		*destination = lastval;
		++destination;
	}

	// if we're starting after the end of the scan, then just set flags and return
	if(start >= scanPointer->start + scanPointer->length)
	{
		bufferinfo[buffersegment].validbytes = 0;
		dataremaining = false;

		return 0;
	}

	//if this will be the last read, shorten if necessary
	if(start + bytes > scanPointer->start + scanPointer->length)
	{
		bytes = scanPointer->start + scanPointer->length - start;

		cverbose << startl << "At end of scan: shortening read to only " << bytes << " bytes " << "(was " << nbytes << ")" << endl;
	}

	/* always read multiples of 8 bytes */
	bytes &= ~7;

	// This is where the actual read from the Mark5 unit happens
	xlrRC = difxMark5Read(xlrDevice, start, reinterpret_cast<unsigned char *>(destination), bytes, readDelayMicroseconds);

	if(xlrRC != XLR_SUCCESS)
	{
		xlrEC = XLRGetLastError();
		XLRGetErrorMessage(errStr, xlrEC);

#warning "FIXME: use error code when known"
		if(strncmp(errStr, "DMA Timeout", 11) == 0)	// A potentially recoverable issue 
		{
			++nDMAError;
			cwarn << startl << "Cannot read data from Mark5 module: position=" << readpointer << ", length=" << bytes << ", XLRErrorCode=" << xlrEC << ", error=" << errStr << endl;
			cwarn << startl << "This is DMA error number " << nDMAError << " on this unit in this job." << endl;
			cwarn << startl << "Resetting streamstor card..." << endl;

			// first fill buffer with fill pattern
			for(int w = 0; w < bytes/4; ++w)
			{
				(reinterpret_cast<unsigned int *>(destination))[w] = MARK5_FILL_PATTERN;
			}

			// then try to reset card
			resetStreamstor();
		}
		else
		{
			cerror << startl << "Cannot read data from Mark5 module: position=" << readpointer << ", length=" << bytes << ", XLRErrorCode=" << xlrEC << ", error=" << errStr << endl;
			dataremaining = false;
			keepreading = false;
			bufferinfo[buffersegment].validbytes = 0;

			double errorTime = corrstartday + (readseconds + corrstartseconds + readnanoseconds*1.0e-9)/86400.0;
			sendMark5Status(MARK5_STATE_ERROR, readpointer, errorTime, 0.0);
			++nError;

			return 0;
		}
	}
	++nReads;

	consumedbytes += bytes;
	bufferinfo[buffersegment].validbytes = bytes;
	bufferinfo[buffersegment].readto = true;
	lastval = destination[bytes/4-1];

	return bytes;
}

void NativeMk5DataStream::moduleToMemory(int buffersegment)
{
	unsigned long *buf, *data;
	char *readto;
	int rbytes, obytes;
	double tv_us;
	static double now_us;
	static long long lastpos = 0;
	struct timeval tv;
	int mjd, sec, sec2, fbytes;
	double ns;
	bool hasFilledData;
	double f;
	int n = 0;

	/* All reads of a module must be 64 bit aligned */
	data = buf = reinterpret_cast<unsigned long *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);

	waitForBuffer(buffersegment);

	//deinterlace and mux if needed
	if(datamuxer)
	{
		rbytes = readonedemux(false, buffersegment); //tells you how many bytes were read, used for changing the readpointer
		readto = (char*)&databuffer[buffersegment*(bufferbytes/numdatasegments)];
		bufferinfo[buffersegment].validbytes = datamuxer->multiplex((u8*)readto); //this corrects validbytes, which may have been corrupted by multiple moduleReads
		obytes = bufferinfo[buffersegment].validbytes; //this is the number of bytes relevant for downstream processing
	}
	else
	{
		rbytes = moduleRead(buf, readbytes, readpointer, buffersegment);
		obytes = rbytes;
	}

	//if there was no valid data read, return
	if(obytes == 0)
	{
		return;
	}

	// Check for validity
	mark5stream->frame = reinterpret_cast<unsigned char *>(data);
	mark5_stream_get_frame_time(mark5stream, &mjd, &sec, &ns);
	
	f = ns + 0.001;
	f -= (int)f;
	while(f > 0.002 && bufferinfo[buffersegment].validbytes > 2*mark5stream->framebytes)	// If we got a frame starting at fractional ns, this will wreak havok...
	{
		++n;
		cwarn << startl << "Nudging read because ns=" << ns << " is not integral " << n << endl;

		rbytes -= mark5stream->framebytes;
		bufferinfo[buffersegment].validbytes -= mark5stream->framebytes;
		memmove(reinterpret_cast<char *>(data), reinterpret_cast<char *>(data)+mark5stream->framebytes, rbytes);
		readpointer -= 2*mark5stream->framebytes;	// One to account for reduced value of rbytes, one to get the frame lopped by the memmove

		mark5stream->frame = reinterpret_cast<unsigned char *>(data);
		mark5_stream_get_frame_time(mark5stream, &mjd, &sec, &ns);
		
		f = ns + 0.001;
		f -= (int)f;
	}

	sec += intclockseconds;
	mark5stream->frame = 0;
	sec2 = (model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + corrstartseconds) % 86400;

	hasFilledData = (data[1] == MARK5_FILL_PATTERN && data[2] == MARK5_FILL_PATTERN);
	if(!hasFilledData && obytes > 4000)
	{
		hasFilledData = (data[998] == MARK5_FILL_PATTERN && data[999] == MARK5_FILL_PATTERN);
	}

	if(hasFilledData)
	{
		++filltime;
		++nfill;
		// use Brian Kernighan's bit counting trick to see if invalidtime is a power of 2 
		if(filltime > 5 && (filltime & (filltime-1)) == 0)
		{
			cwarn << startl << filltime << " consecutive fill patterns at time " << sec2 << "," << readnanoseconds << endl ;
		}

		if(filltime > 1)
		{
			bufferinfo[buffersegment].validbytes = 0;
		}

		// insert an extra delay to let a slow drive catch up
		if(module.mode == MARK5_READ_MODE_RT)
		{
			if(readDelayMicroseconds > 0)
			{
				usleep(readDelayMicroseconds);
			}
			else
			{
				usleep(20000);	
			}
		}
	}
	else if((sec % 86400) != sec2 || fabs(ns - readnanoseconds) > 0.5)
	{
		if(abs(sec-sec2) < 5)
		{
			// If offset is small, just nudge it 
			// Data will be invalid this time through, but should be OK next time
			if(!noDataOnModule)
			{
				cwarn << startl << "Nudged time just a bit; sec2 was " << sec2 << " and ns was " << readnanoseconds << ", now sec = " << sec << " and ns = " << ns << endl;
				cwarn << startl << "The difference between ns_old and ns_new was " << ns - readnanoseconds << endl;
			}
			readseconds += (sec-sec2);
			readnanoseconds = (int)(ns + 0.4);
		}
		else if(invalidtime > 3 && mjd == corrstartday)	// if a large time difference persists
		{
			// try correcting the read position, hopefully putting the datastream within 5 seconds of the target
			long long origreadpointer = readpointer;
			fbytes = scanPointer->framebytes;
			if(datamuxer)
			{
				fbytes *= datamuxer->getNumThreads();
			}
			readpointer -= (sec-sec2)*fbytes*scanPointer->framespersecond;
			readpointer -= (readpointer % 4);
			if(readpointer < scanPointer->start)
			{
				readpointer = scanPointer->start;
			}
			long long edge = scanPointer->start+scanPointer->length - 3*readbytes;
			if(readpointer > edge)
			{
				readpointer = edge;
			}
			if(!noDataOnModule)
			{
				cwarn << startl << "Nudged read position by " << (readpointer - origreadpointer) << " bytes." << endl;
			}
		}
		++invalidtime;
		++ninvalid;
		invalidstart = readpointer;
		bufferinfo[buffersegment].validbytes = 0;
		// use Brian Kernighan's bit counting trick to see if invalidtime is a power of 2 
		if((invalidtime & (invalidtime-1)) == 0 && !noDataOnModule)
		{
			cwarn << startl << invalidtime << " consecutive sync errors starting at readpos " << invalidstart << " (" << mjd << "," << sec << "," << ns << ")!=(" << sec2 << "," << readnanoseconds << ")" << " length=" << obytes << endl ;
		}
		// After 5 sync errors try to find the sync word again
		if(invalidtime % 6 == 5)
		{
			struct mark5_format *mf;
			mf = new_mark5_format_from_stream(new_mark5_stream_memory(data, obytes));
			if(mf)
			{
				readpointer += mf->frameoffset;
				if(!noDataOnModule)
				{
					cwarn << startl << "Jumping " << mf->frameoffset << " bytes" << endl;
				}
				invalidtime = 0;
				delete_mark5_format(mf);
			}
		}
		// Call it an error after 25 sync errors
		if(invalidtime == 25 && !noDataOnModule)
		{
			cerror << startl << invalidtime << " consecutive sync errors.  Something is probably wrong!" << endl;
		}
	}
	else
	{
		static int nt = 0;

		++ngood;
		filltime = 0;
		invalidtime = 0;

		++nt;

                // feed switched power detector
		if(switchedpower && (nt % switchedpowerincrement == 0) )
		{
			struct mark5_stream *m5stream = new_mark5_stream_absorb(
				new_mark5_stream_memory(data, obytes),
				new_mark5_format_generic_from_string(formatname) );
			if(m5stream)
			{
				mark5_stream_fix_mjd(m5stream, config->getStartMJD());
				switchedpower->feed(m5stream);
				delete_mark5_stream(m5stream);
			}
                }
	}


	gettimeofday(&tv, 0);
	tv_us = 1.0e6*tv.tv_sec + tv.tv_usec;

	if(tv_us - now_us > 1.5e6 && nReads > 4)
	{
		if(lastpos > 0)
		{
			double rate;
			double fmjd, fmjd2;
			enum Mk5State state;

			fmjd = corrstartday + (corrstartseconds + model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + static_cast<double>(readnanoseconds)/1000000000.0)/86400.0;
			if(newscan > 0)
			{
				newscan = 0;
				state = MARK5_STATE_START;
				rate = 0.0;
				lastrate = 0.0;
				nrate = 0;
				fmjd2 = scanPointer->mjd + (scanPointer->sec + static_cast<double>(scanPointer->framenuminsecond)/scanPointer->framespersecond)/86400.0;
				if(fmjd2 > fmjd)
				{
					fmjd = fmjd2;
				}
			}
			else if(invalidtime == 0)
			{
				const int HighRealTimeRate = 1440;
				const int LowRealTimeRate = 1300;

				state = MARK5_STATE_PLAY;
				rate = (static_cast<double>(readpointer) + static_cast<double>(rbytes) - static_cast<double>(lastpos))*8.0/(tv_us - now_us);
//				if(nrate > 1)
//				{
//					rate = (nrate*lastrate + 4*rate)/(nrate+4);
//				}

				// If in real-time mode, servo playback rate through adjustable inter-read delay
				if(module.mode == MARK5_READ_MODE_RT)
				{
					if(rate > HighRealTimeRate && lastrate > HighRealTimeRate && readDelayMicroseconds < 150000)
					{
						if(readDelayMicroseconds == 0)
						{
							readDelayMicroseconds = 10000;
						}
						else
						{
							readDelayMicroseconds = readDelayMicroseconds*3/2;
						}
						usleep(100000);
					}
					if(rate < LowRealTimeRate && lastrate < LowRealTimeRate)
					{
						readDelayMicroseconds = readDelayMicroseconds*9/10;	// reduce delay by 10%
					}
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

			sendMark5Status(state, readpointer, fmjd, rate);
		}
		lastpos = readpointer + rbytes;
		now_us = tv_us;
	}

	// Update various counters
	if(rbytes < readbytes)
	{
		if(rbytes == 0 && obytes > 0) // can only happen for VDIF muxing, means that data has not run out
		{
			cinfo << startl << "Noting a skipped read while muxing" << endl;
		}
		else
		{
			dataremaining = false;
		}
	}
	else
	{
		readpointer += rbytes;
	}
	if(readpointer >= scanPointer->start + scanPointer->length)
	{
		dataremaining = false;
	}
	readnanoseconds += (bufferinfo[buffersegment].nsinc % 1000000000);
	readseconds += (bufferinfo[buffersegment].nsinc / 1000000000);
	readseconds += readnanoseconds/1000000000;
	readnanoseconds %= 1000000000;
	if(readseconds >= model->getScanDuration(readscan))
	{
		if(readscan < model->getNumScans()-1)
		{
			cinfo << startl << "Going to next scan!" << endl;
			cinfo << startl << "Currently, sec2 is " << (model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + corrstartseconds) % 86400 << endl;
			++readscan;
			readseconds -= model->getScanStartSec(readscan, corrstartday, corrstartseconds) - model->getScanStartSec(readscan-1, corrstartday, corrstartseconds);
			cinfo << startl << "Incrementing scan to " << readscan << ", readseconds is now " << readseconds << endl;
			if(readseconds < -1)
			{
				int skipseconds, windbacksec;
				skipseconds = -readseconds-1;
				windbacksec = 0;
				fbytes = scanPointer->framebytes;
				if(datamuxer)
				{
					fbytes *= datamuxer->getNumThreads();
					datamuxer->addSkipFrames(skipseconds*scanPointer->framespersecond);
					windbacksec = (int)((2.0*bufferinfo[buffersegment].nsinc)/1000000000.0);
					if((2.0*bufferinfo[buffersegment].nsinc)/1000000000.0 - windbacksec > 0)
					{
						++windbacksec;
					}
				}
				if(skipseconds > windbacksec)
				{
					cinfo << startl << "Going to skip " << skipseconds << " (windbacksec " << windbacksec << "), readpointer is currently " << readpointer << endl;
					readpointer += static_cast<long long>(skipseconds)*static_cast<long long>(fbytes)*static_cast<long long>(scanPointer->framespersecond);
					cinfo << startl << "Skipping forward " << static_cast<long long>(skipseconds)*static_cast<long long>(fbytes)*static_cast<long long>(scanPointer->framespersecond) << " bytes - fbytes is " << fbytes << " and framespersecond is " << scanPointer->framespersecond << ", and so now readpointer is " << readpointer << endl;
					readseconds += skipseconds;
					if (datamuxer)
					{
						// Clear the interlacing cache in the datamuxer, so that times will align (requires shuffling the readpointer back first)
						readpointer -= (2*static_cast<long long>(bufferinfo[buffersegment].nsinc)*static_cast<long long>(fbytes)*static_cast<long long>(scanPointer->framespersecond))/1000000000;
						cinfo << startl << "In order to refill buffer, winding back " << (2*static_cast<long long>(bufferinfo[buffersegment].nsinc)*static_cast<long long>(fbytes)*static_cast<long long>(scanPointer->framespersecond))/1000000000 << " bytes" << endl;
						datamuxer->resetcounters(); //needed to clear all of the counters so we get the right demux buffer in the following function
						rbytes = readonedemux(true, buffersegment);
						readpointer += rbytes;
						rbytes = readonedemux(false, buffersegment);
						readpointer += rbytes;
					}
				}
			}
			/*if(readseconds < -1)
			{
				int skipseconds = -readseconds-1;
				fbytes = scanPointer->framebytes;
				if(datamuxer)
				{
					fbytes *= datamuxer->getNumThreads();
					datamuxer->addSkipFrames(skipseconds*scanPointer->framespersecond);
				}
				readpointer += static_cast<long long>(skipseconds)*static_cast<long long>(fbytes)*static_cast<long long>(scanPointer->framespersecond);
				readseconds += skipseconds;
				cinfo << startl << "After skipping " << skipseconds << " seconds, sec2 is now " << (model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + corrstartseconds) % 86400 << endl;
				cinfo << startl << "Corrstartseconds is " << corrstartseconds << ", startseconds for this scan is " << model->getScanStartSec(readscan, corrstartday, corrstartseconds) << ", and readseconds is " << readseconds << endl;
				cinfo << startl << "Skipping " << skipseconds << " seconds at scan boundary, which is " << static_cast<long long>(skipseconds)*static_cast<long long>(fbytes)*static_cast<long long>(scanPointer->framespersecond) << "bytes.  Readpointer was " << readpointer-static_cast<long long>(skipseconds)*static_cast<long long>(fbytes)*static_cast<long long>(scanPointer->framespersecond) << ", is now " << readpointer << endl;
			}*/
		}
		else
		{
			keepreading = false;
		}
	}
}

void NativeMk5DataStream::loopfileread()
{
  int perr, rbytes;
  int numread = 0;

  //lock the outstanding send lock
  perr = pthread_mutex_lock(&outstandingsendlock);
  if(perr != 0)
    csevere << startl << "Error in initial telescope readthread lock of outstandingsendlock!!!" << endl;

  //lock the first section to start reading
  openfile(bufferinfo[0].configindex, 0);
  if(datamuxer) {
    datamuxer->resetcounters();
    rbytes = readonedemux(true, 0);
    readpointer += rbytes;
    rbytes = readonedemux(false, 0);
    readpointer += rbytes;
  }
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
  if(noDataOnModule)
  {
  	dataremaining = false;
	keepreading = false;
  }
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

int NativeMk5DataStream::sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate)
{
	int v = 0;
	S_BANKSTATUS A, B;
	XLR_RETURN_CODE xlrRC;

	// If there really is no more data, override a simple NODATA with a more precise response
	if(noMoreData == true && state == MARK5_STATE_NODATA)
	{
		state = MARK5_STATE_NOMOREDATA;
	}

	mk5status.state = state;
	mk5status.status = 0;
	mk5status.activeBank = ' ';
	mk5status.position = position;
	mk5status.rate = rate;
	mk5status.dataMJD = dataMJD;
	mk5status.scanNumber = scanNum+1;
	if(scanPointer && scanNum >= 0)
	{
      		snprintf(mk5status.scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s", scanPointer->name.c_str());
	}
	else
	{
		strcpy(mk5status.scanName, "none");
	}
	if(state != MARK5_STATE_OPENING && state != MARK5_STATE_ERROR && state != MARK5_STATE_IDLE)
	{
		WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &A) );
		if(xlrRC == XLR_SUCCESS)
		{
			WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &B) );
		}
		if(xlrRC == XLR_SUCCESS)
		{
			strncpy(mk5status.vsnA, A.Label, 8);
			mk5status.vsnA[8] = 0;
			if(strncmp(mk5status.vsnA, "LABEL NO", 8) == 0)
			{
				strcpy(mk5status.vsnA, "none");
			}
			else if(!legalVSN(mk5status.vsnA))
			{
				strcpy(mk5status.vsnA, "badvsn");
			}
			strncpy(mk5status.vsnB, B.Label, 8);
			mk5status.vsnB[8] = 0;
			if(strncmp(mk5status.vsnB, "LABEL NO", 8) == 0)
			{
				strcpy(mk5status.vsnB, "none");
			}
			else if(!legalVSN(mk5status.vsnB))
			{
				strcpy(mk5status.vsnB, "badvsn");
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
