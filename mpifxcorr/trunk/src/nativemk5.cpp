/***************************************************************************
 *   Copyright (C) 2007-2011 by Walter Brisken and Adam Deller             *
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
#include "config.h"
#include "nativemk5.h"
#include "watchdog.h"
#include "alert.h"

#if HAVE_MARK5IPC
#include <mark5ipc.h>
#endif


time_t watchdogTime;
int watchdogVerbose;
char watchdogStatement[256];
pthread_mutex_t watchdogLock;
char watchdogXLRError[XLR_ERROR_LENGTH+1];
int watchdogTimeout;
pthread_t watchdogThread;

void *watchdogFunction(void *data)
{
	NativeMk5DataStream *nativeMk5 = (NativeMk5DataStream *)data;
	int deltat;
	int lastdeltat = 0;

	for(;;)
	{
		usleep(100000);
		pthread_mutex_lock(&watchdogLock);

		if(strcmp(watchdogStatement, "DIE") == 0)
		{
			pthread_mutex_unlock(&watchdogLock);
			
			return 0;
		}
		else if(watchdogTime != 0)
		{
			deltat = time(0) - watchdogTime;
			if(deltat > 60)  // Nothing should take 60 seconds to complete!
			{
				cfatal << startl << "Watchdog caught a hang-up executing: " << watchdogStatement << " Aborting!!!" << endl;
				if(nativeMk5)
				{
					nativeMk5->sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
				}
#if HAVE_MARK5IPC
				unlockMark5();
#endif
				MPI_Abort(MPI_COMM_WORLD, 1);
			}
			else if(deltat != lastdeltat && deltat > 8)
			{
				cwarn << startl << "Waiting " << deltat << " seconds executing: " << watchdogStatement << endl;
				lastdeltat = deltat;
			}
		}
		else
		{
			lastdeltat = 0;
		}

		pthread_mutex_unlock(&watchdogLock);
	}
}

int initWatchdog()
{
	int perr;

	pthread_mutex_init(&watchdogLock, NULL);
	watchdogStatement[0] = 0;
	watchdogXLRError[0] = 0;
	watchdogTime = 0;
	watchdogTimeout = 20;
	perr = pthread_create(&watchdogThread, NULL, watchdogFunction, 0);

	if(perr != 0)
	{
		fprintf(stderr, "Error: could not launch watchdog thread!\n");

		return -1;
	}

	return 0;
}

void stopWatchdog()
{
	pthread_mutex_lock(&watchdogLock);
	strcpy(watchdogStatement, "DIE");
	pthread_mutex_unlock(&watchdogLock);
	pthread_join(watchdogThread, NULL);
}

static int dirCallback(int scan, int nscan, int status, void *data)
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
		v = snprintf(message, MessageLength, "cannot decode scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
	}

	if(v >= MessageLength)
	{
		fprintf(stderr, "Warning: dirCallback: message: v=%d, >= %d\n", v, MessageLength);
	}

	return 0;
}

int NativeMk5DataStream::resetDriveStats()
{
	XLR_RETURN_CODE xlrRC;
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	const int defaultStatsRange[] = { 75000, 150000, 300000, 600000, 1200000, 2400000, 4800000, -1 };

	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_DRVSTATS) );
	for(int b = 0; b < XLR_MAXBINS; b++)
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
	DifxMessageDiskStat driveStatsMessage;

	snprintf(driveStatsMessage.moduleVSN, DIFX_MESSAGE_MARK5_VSN_LENGTH, "%s", vsn);
	driveStatsMessage.type = DRIVE_STATS_TYPE_READ;

	/* FIXME: for now don't include complete information on drives */
	strcpy(driveStatsMessage.serialNumber, "X");
	strcpy(driveStatsMessage,modelNumber, "Y");
	driveStatsMessage.diskSize = 0;

	for(int d = 0; d < 8; d++)
	{
		for(int i = 0; i < DIFX_MESSAGE_N_DRIVE_STATS_BINS; i++)
		{
			driveStatsMessage.bin[i] = -1;
		}
		driveStatsMessage.moduleSlot = d;
		WATCHDOG( xlrRC = XLRGetDriveStats(xlrDevice, d/2, d%2, driveStats) );
		if(xlrRC == XLR_SUCCESS)
		{
			for(int i = 0; i < XLR_MAXBINS; i++)
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
}

NativeMk5DataStream::NativeMk5DataStream(Configuration * conf, int snum, 
	int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor, 
	numsegments)
{
	XLR_RETURN_CODE xlrRC;
	int perr;
        int v;

	/* each data buffer segment contains an integer number of frames, 
	 * because thats the way config determines max bytes
	 */

	executeseconds = conf->getExecuteSeconds();
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
	nomoredata = false;
	nfill = ninvalid = ngood = 0;
	nrate = 0;
	nError = 0;

#if HAVE_MARK5IPC
        v = lockMark5(5);
        {
                if(v)
                {
                        sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
                        nError++;
                        cfatal << startl << "Cannot obtain lock for Streamstor device." << endl;
                        MPI_Abort(MPI_COMM_WORLD, 1);
                }
        }
#endif

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

	sendMark5Status(MARK5_STATE_OPEN, 0, 0.0, 0.0);

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
		nError++;
	}
	else if(ninvalid + nfill > ngood)
	{
		cerror << startl << "Most of the data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" << ngood << ".  Please consider reading the module directory again and investigating the module health" << endl;
		sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
		nError++;
	}
	else if(9*(ninvalid + nfill) >= ngood)
	{
		int f = 100*(ninvalid + nfill)/(ninvalid + nfill + ngood);
		cwarn << startl << f <<" percent of the data from this module was discarded: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" <<     ngood << "." << endl;
	}
	else
	{
		cinfo << startl << "Data recovery statistics: ninvalid=" << ninvalid << " nfill=" << nfill << " ngood=" << ngood << "." << endl;
	}

	if(nError == 0)
	{
#ifdef HAVE_DIFXMESSAGE
		reportDriveStats();
		sendMark5Status(MARK5_STATE_CLOSE, 0, 0.0, 0.0);
#endif
#if HAVE_MARK5IPC
                unlockMark5();
#endif
		WATCHDOG( XLRClose(xlrDevice) );
	}

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
	double scanstart, scanend;
	int v, fanout;
	long long n;
	int doUpdate = 0;
	char *mk5dirpath;
	int nbits, nrecordedbands, framebytes;
	Configuration::dataformat format;
	double bw;
	XLR_RETURN_CODE xlrRC;

	format = config->getDataFormat(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	framebytes = config->getFrameBytes(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);
        if(config->isDMuxed(configindex, streamnum)) {
          framebytes = (framebytes-32)*config->getDNumMuxThreads(configindex, streamnum) + 32;
          nrecordedbands = 1;
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
			nError++;
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
		}
	}

	sendMark5Status(MARK5_STATE_GOTDIR, 0, startmjd, 0.0);

	// find starting position

	if(scanPointer && scanNum >= 0)  /* just continue by reading next valid scan */
	{
		cinfo << startl << "Advancing to next Mark5 module scan" << endl;
		do
		{
			scanNum++;
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
			nomoredata = true;
			sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0.0, 0.0);

			return;
		}
		cverbose << startl << "Before scan change: readscan = " << readscan << "  readsec = " << readseconds << "  readns = " << readnanoseconds << endl;
		scanstart = scanPointer->mjdStart();
		scanend = scanPointer->mjdEnd();
		readpointer = scanPointer->start + scanPointer->frameoffset;
		readseconds = (scanPointer->mjd-corrstartday)*86400 + scanPointer->sec - corrstartseconds + intclockseconds;
		readnanoseconds = scanPointer->nsStart();
                while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
                  readscan++;
                while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
                  readscan--;
                readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);

		cinfo << startl << "After scan change: readscan = " << readscan << " rs = " << readseconds << "  rns = " << readnanoseconds << endl;

		if(readscan == model->getNumScans() - 1 && readseconds >= model->getScanDuration(readscan))
		{
			cwarn << startl << "No more data for project on module [" << mpiid << "]" << endl;
			scanPointer = 0;
			scanNum = -1;
			dataremaining = false;
			keepreading = false;
			nomoredata = true;
			sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0.0, 0.0);

			return;
		}
	}
	else	/* first time this project */
	{
		n = 0;
		for(scanNum = 0; scanNum < module.nScans(); scanNum++)
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
                                  readscan++;
                                while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
                                  readscan--;
                                readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
				break;
			}
			else if(startmjd < scanend) /* obs starts within data */
			{
				cinfo << startl << "NM5 : scan found(2) : " << (scanNum+1) << endl;
				readpointer = scanPointer->start + scanPointer->frameoffset;
				n = (long long)((
					( ( (corrstartday - scanPointer->mjd)*86400 
					+ corrstartseconds - scanPointer->sec) - scanPointer->nsStart()*1.e-9)
					*config->getFramesPerSecond(configindex, streamnum)) + 0.5);
				readpointer += n*scanPointer->framebytes;
				readseconds = 0;
				readnanoseconds = 0;
                                while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
                                  readscan++;
                                while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
                                  readscan--;
                                readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds) + intclockseconds;
				break;
			}
		}
		cinfo << startl << "NativeMk5DataStream " << mpiid << 
			" positioned at byte " << readpointer << 
			" scan = " << readscan << " seconds = " << readseconds <<
                        " ns = " << readnanoseconds << " n = " << n << endl;

		if(scanNum >= module.nScans() || scanPointer == 0)
		{
			cerror << startl << "No valid data found.  Stopping playback!" << endl;
			dataremaining = false;
			sendMark5Status(MARK5_STATE_NODATA, 0, 0.0, 0.0);
			scanNum = -1;
			scanPointer = 0;

			return;
		}

		cinfo << startl << "Scan info. start = " << scanPointer->start << " off = " << scanPointer->frameoffset << " size = " << scanPointer->framebytes << endl;
	}

        if(readpointer == -1)
        {
	  cwarn << startl << "No data for this job on this module" << endl;
	  scanPointer = 0;
	  dataremaining = false;
	  keepreading = false;
	  nomoredata = true;
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
		for(int i = 0; i < numdatasegments; i++)
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

void NativeMk5DataStream::moduleToMemory(int buffersegment)
{
	long long start;
	unsigned long *buf, *data;
	unsigned long a, b;
	S_READDESC      xlrRD;
	XLR_RETURN_CODE xlrRC;
	XLR_ERROR_CODE  xlrEC;
	int bytes;
	char errStr[XLR_ERROR_LENGTH];
	static int now = 0;
	static long long lastpos = 0;
	struct timeval tv;
	int mjd, sec, sec2;
	double ns;
	bool hasFilledData;

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
	if(start >= scanPointer->start + scanPointer->length)
	{
		bufferinfo[buffersegment].validbytes = 0;
		dataremaining = false;

		return;
	}

	if(start + bytes > scanPointer->start + scanPointer->length)
	{
		bytes = scanPointer->start + scanPointer->length - start;

		cverbose << startl << "At end of scan: shortening read to only " << bytes << " bytes " << "(was " << readbytes << ")" << endl;
	}

	/* always read multiples of 8 bytes */
	bytes &= ~7;

	a = start >> 32;
	b = start & 0xFFFFFFFF; 

	waitForBuffer(buffersegment);

	xlrRD.AddrHi = a;
	xlrRD.AddrLo = b;
	xlrRD.XferLength = bytes;
	xlrRD.BufferAddr = (streamstordatatype *)buf;

	//WATCHDOG( xlrRC = XLRRead(xlrDevice, &xlrRD) );
	WATCHDOG( xlrRC = XLRReadData(xlrDevice, xlrRD.BufferAddr, xlrRD.AddrHi, xlrRD.AddrLo, xlrRD.XferLength) );
	if(xlrRC != XLR_SUCCESS)
	{
		xlrEC = XLRGetLastError();
		XLRGetErrorMessage(errStr, xlrEC);
		cerror << startl << "Cannot read data from Mark5 module: position=" << readpointer << ", length=" << bytes << ", error=" << errStr << endl;
		dataremaining = false;
		keepreading = false;
		bufferinfo[buffersegment].validbytes = 0;

		double errorTime = corrstartday + (readseconds + corrstartseconds + readnanoseconds*1.0e-9)/86400.0;
		sendMark5Status(MARK5_STATE_ERROR, readpointer, errorTime, 0.0);
		nError++;

		return;
	}

	bufferinfo[buffersegment].validbytes = bytes;
	bufferinfo[buffersegment].readto = true;
	lastval = buf[bytes/4-1];

	// Check for validity
	mark5stream->frame = (uint8_t *)data;
	mark5_stream_get_frame_time(mark5stream, &mjd, &sec, &ns);
	sec += intclockseconds;
	mark5stream->frame = 0;
	sec2 = (model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + corrstartseconds) % 86400;

	hasFilledData = (data[1] == MARK5_FILL_PATTERN && data[2] == MARK5_FILL_PATTERN);
	if(!hasFilledData && bytes > 4000)
	{
		hasFilledData = (data[998] == MARK5_FILL_PATTERN && data[999] == MARK5_FILL_PATTERN);
	}

	if(hasFilledData)
	{
		filltime++;
		nfill++;
		// use Brian Kernighan's bit counting trick to see if invalidtime is a power of 2 
		if(filltime > 5 && (filltime & (filltime-1)) == 0)
		{
			cwarn << startl << filltime << " consecutive fill patterns at time " << sec2 << "," << readnanoseconds << endl ;
		}

		if(filltime > 1)
		{
			bufferinfo[buffersegment].validbytes = 0;
		}
	}
	else if((sec % 86400) != sec2 || fabs(ns - readnanoseconds) > 0.5)
	{
		// If offset is small, just nudge it 
		// Data will be invalid this time through, but should be OK next time
		if(abs(sec-sec2) < 5)
		{
			readseconds += (sec-sec2);
			readnanoseconds = (int)(ns + 0.4);
			cwarn << startl << "Nudged time just a bit" << endl;
		}
		invalidtime++;
		ninvalid++;
		invalidstart = readpointer;
		bufferinfo[buffersegment].validbytes = 0;
		// use Brian Kernighan's bit counting trick to see if invalidtime is a power of 2 
		if((invalidtime & (invalidtime-1)) == 0)
		{
			cwarn << startl << invalidtime << " consecutive sync errors starting at readpos " << invalidstart << " (" << mjd << "," << sec << "," << ns << ")!=(" << sec2 << "," << readnanoseconds << ")" << " length=" << bytes << endl ;
		}
		// After 10 sync errors try to find the sync word again
		if(invalidtime % 11 == 10)
		{
			struct mark5_format *mf;
			mf = new_mark5_format_from_stream(new_mark5_stream_memory(data, bytes));
			if(mf)
			{
				readpointer += mf->frameoffset;
				cwarn << startl << "Jumping " << mf->frameoffset << " bytes" << endl;
				invalidtime = 0;
				delete_mark5_format(mf);
			}
		}
		// Call it an error after 25 sync errors
		if(invalidtime == 25)
		{
			cerror << startl << invalidtime << " consecutive sync errors.  Something is probably wrong!" << endl;
		}
	}
	else
	{
		static int nt = 0;

		ngood++;
		filltime = 0;
		invalidtime = 0;

		nt++;

                // feed switched power detector
                if(switchedpower && (nt % switchedpowerincrement == 0) )
                {
                  struct mark5_stream *m5stream = new_mark5_stream_absorb(
                    new_mark5_stream_memory(data, bytes),
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
				fmjd2 = scanPointer->mjd + (scanPointer->sec + (float)scanPointer->framenuminsecond/scanPointer->framespersecond)/86400.0;
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

			sendMark5Status(state, readpointer, fmjd, rate);
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
		readpointer += (long long)(skipseconds)*(long long)(scanPointer->framebytes)*(long long)(scanPointer->framespersecond);
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
	if(readpointer >= scanPointer->start + scanPointer->length)
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

static bool legalVSN(const char *vsn)
{
	int nSep = 0;

	for(int i = 0; i < 8; i++)
	{
		if(vsn[i] == '+' || vsn[i] == '-')
		{
			if(nSep > 0 || i == 0 || i == 7)
			{
				return false;
			}
			nSep++;
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

int NativeMk5DataStream::sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate)
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
