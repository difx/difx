/***************************************************************************
 *   Copyright (C) 2007-2016 by Walter Brisken and Adam Deller             *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/nativemk5.cpp $
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
#include "mark5bmark5.h"
#include "watchdog.h"
#include "alert.h"
#include "mark5utils.h"
#include "dirlist/old_dirlist.h"

#if HAVE_MARK5IPC
#include <mark5ipc.h>
#endif

/* TODO
   * Jump over large blocks of data based on requested times
   * Terminate gracefully
     - on signal (need to test)
 */

Mark5BMark5DataStream::Mark5BMark5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		Mark5BDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	int perr;

	/* each data buffer segment contains an integer number of frames, 
	 * because thats the way config determines max bytes
	 */

	scanNum = -1;
	readpointer = -1;
	scanPointer = 0;
	filltime = 0;
	invalidstart = 0;
	newscan = 0;
	lastrate = 0.0;
	noMoreData = false;
	nrate = 0;
	nError = 0;
	nDMAError = 0;
	readDelayMicroseconds = 0;
	noDataOnModule = false;
	nReads = 0;
	jobEndMJD = conf->getStartMJD() + (conf->getStartSeconds() + conf->getExecuteSeconds() + 1)/86400.0;

	readbufferslots = 8;
	readbufferslotsize = (bufferfactor/numsegments)*conf->getMaxDataBytes(streamnum)*21/10;
	readbufferslotsize -= (readbufferslotsize % 8); // make it a multiple of 8 bytes
	readbuffersize = readbufferslots * readbufferslotsize;
	// Note: the read buffer is allocated in mark5bfile.cpp by Mark5BDataStream::initialse()
	// the above values override defaults for file-based Mark5B

#if HAVE_MARK5IPC
	perr = lockMark5(5);
	{
		if(perr)
		{
			sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);
			++nError;
			cfatal << startl << "Cannot obtain lock for Streamstor device." << endl;
			MPI_Abort(MPI_COMM_WORLD, 1);
		}
	}
#endif
	sendMark5Status(MARK5_STATE_OPENING, 0, 0.0, 0.0);
	openStreamstor();
	sendMark5Status(MARK5_STATE_OPEN, 0, 0.0, 0.0);

        // Start up mark5 watchdog thread
        perr = initWatchdog();
        if(perr != 0)
        {
                cfatal << startl << "Cannot create the nativemk5 watchdog thread!" << endl;
                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	resetDriveStats();


	// set up Mark5 module reader thread
	mark5xlrfail = false;
	mark5threadstop = false;
	lockstart = lockend = lastslot = -2;
	endindex = 0;

	perr = pthread_barrier_init(&mark5threadbarrier, 0, 2);
	mark5threadmutex = new pthread_mutex_t[readbufferslots];
	for(int m = 0; m < readbufferslots; ++m)
	{
		if(perr == 0)
		{
			perr = pthread_mutex_init(mark5threadmutex + m, 0);
		}
	}

	if(perr == 0)
	{
		pthread_attr_t attr;
		pthread_attr_init(&attr);
		pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
		perr = pthread_create(&mark5thread, &attr, Mark5BMark5DataStream::launchmark5threadfunction, this);
		pthread_attr_destroy(&attr);
	}

	if(perr)
	{
		cfatal << startl << "Cannot create the Mark5 reader thread!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
}

Mark5BMark5DataStream::~Mark5BMark5DataStream()
{

	/* barriers come in pairs to allow the read thread to always get first lock */
	pthread_barrier_wait(&mark5threadbarrier);
        mark5threadstop = true; // "signal" for exit while thread guaranteed to still be alive
	pthread_barrier_wait(&mark5threadbarrier);

	pthread_join(mark5thread, 0);

	reportDriveStats();
	closeStreamstor();
#if HAVE_MARK5IPC
	unlockMark5();
#endif

	for(int m = 0; m < readbufferslots; ++m)
	{
		pthread_mutex_destroy(mark5threadmutex + m);
	}
	delete [] mark5threadmutex;
	pthread_barrier_destroy(&mark5threadbarrier);

	if(readDelayMicroseconds > 0)
	{
		cinfo << startl << "To reduce read rate in RT mode, read delay was set to " << readDelayMicroseconds << " microseconds" << endl;
	}

	if(nError > 0)
	{
		cwarn << startl << nError << " errors were encountered reading this module" << endl;
	}

        // stop watchdog thread
        stopWatchdog();
}

// this function implements the Mark5 module reader.  It is continuously either filling data into a ring buffer or waiting for a mutex to clear.
// any serious problems are reported by setting mark5xlrfail.  This will call the master thread to shut down.
void Mark5BMark5DataStream::mark5threadfunction()
{
	int lockmod = readbufferslots-1;	// slot 0 and slot readbufferslots-1 share a lock
	int lastlockedslot;

	for(;;)
	{
		// Note two-barrier situation here to allow this thread to have first dibs on locking slot 1
		pthread_barrier_wait(&mark5threadbarrier);

		for(int m = 0; m < lockmod; ++m)
		{
			int v = pthread_mutex_trylock(mark5threadmutex + m);
			if(v == 0)
			{
				pthread_mutex_unlock(mark5threadmutex + m);
			}
			else
			{
				cerror << startl << "Dev error: mutex " << m << " already locked" << endl;
			}
		}

		readbufferwriteslot = 1;	// always start a new reading at slot 1
		pthread_mutex_lock(mark5threadmutex + (readbufferwriteslot % lockmod));
		pthread_barrier_wait(&mark5threadbarrier);

		lastlockedslot = readbufferwriteslot;

		while(!mark5threadstop)
		{
			int bytes;
			XLR_RETURN_CODE xlrRC;
			XLR_ERROR_CODE  xlrEC;
			char errStr[XLR_ERROR_LENGTH];
			bool endofscan = false;

			// Bytes to read.  In most cases the following is desired, but when the end of the scan nears it changes
			bytes = readbufferslotsize;

			// if we're starting, jump out of the loop.  Really this should not be executed
			if(readpointer >= readend)
			{
				cwarn << startl << "Developer error: mark5threadfunction: readpointer=" << readpointer << " >= readend=" << readend << " readbufferwriteslot=" << readbufferwriteslot << endl;

				endofscan = true;
				
				// back up one slot
				readbufferwriteslot = lastlockedslot;
				if(readbufferwriteslot == 0)
				{
					readbufferwriteslot = readbufferslots-1;
				}

				lastslot = readbufferwriteslot;
				endindex = lastslot*readbufferslotsize;

				break;
			}

			// if this will be the last read of the scan, shorten if necessary
			if(readpointer + bytes >= readend)
			{
				int origbytes = bytes;
				bytes = readend - readpointer;
				bytes -= (bytes % 8);		// enforce 8 byte multiple length
				endofscan = true;

				lastslot = readbufferwriteslot;
				endindex = lastslot*readbufferslotsize + bytes;	// No data in this slot from here to end

				cinfo << startl << "At end of scan: shortening Mark5 read to only " << bytes << " bytes " << "(was " << origbytes << ")" << endl;
				cinfo << startl << "endindex=" << endindex << " lastslot=" << lastslot << " readbufferwriteslot=" << readbufferwriteslot << endl;
			}

			// This is where the actual reading from the Mark5 device happens
			xlrRC = difxMark5Read(xlrDevice, readpointer, readbuffer + readbufferwriteslot*readbufferslotsize, bytes, readDelayMicroseconds);

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

					sleep(1);

					// try to reset card
					resetStreamstor();
				}
				else
				{
					cerror << startl << "Cannot read data from Mark5 module: position=" << readpointer << ", length=" << bytes << ", XLRErrorCode=" << xlrEC << ", error=" << errStr << endl;
					dataremaining = false;
					keepreading = false;

					double errorTime = corrstartday + (readseconds + corrstartseconds + readnanoseconds*1.0e-9)/86400.0;
					sendMark5Status(MARK5_STATE_ERROR, readpointer, errorTime, 0.0);
					++nError;
					mark5xlrfail = true;
				}

				return;
			}
			else
			{
				int curslot = readbufferwriteslot;

#ifdef REPORT_FILL_PATTERN
				if(((uint32_t *)(readbuffer + readbufferwriteslot*readbufferslotsize))[0] == MARK5_FILL_PATTERN)
				{
					cwarn << startl << "Fill pattern at start of read" << endl;
				}
				if(((uint32_t *)(readbuffer + readbufferwriteslot*readbufferslotsize))[bytes/4-1] == MARK5_FILL_PATTERN)
				{
					cwarn << startl << "Fill pattern at end of read" << endl;
				}
#endif

				readpointer += bytes;
				++nReads;

				++readbufferwriteslot;
				if(readbufferwriteslot >= readbufferslots)
				{
					// Note: we always save slot 0 for wrap-around
					readbufferwriteslot = 1;
				}
				pthread_mutex_lock(mark5threadmutex + (readbufferwriteslot % lockmod));
				lastlockedslot = readbufferwriteslot;
				pthread_mutex_unlock(mark5threadmutex + (curslot % lockmod));

				if(!dataremaining)
				{
					endofscan = true;
				}

				servoMark5();
			}
			if(endofscan || !keepreading)
			{
				break;
			}
		}
		if(lastlockedslot != readbufferwriteslot)
		{
			cwarn << startl << "Developer error: lastlockedslot=" << lastlockedslot << " != readbufferwriteslot=" << readbufferwriteslot << endl;
		}
		pthread_mutex_unlock(mark5threadmutex + (readbufferwriteslot % lockmod));
		if(mark5threadstop)
		{
			break;
		}

		// No locks shall be set at this point
	} 
}

void *Mark5BMark5DataStream::launchmark5threadfunction(void *self)
{
	Mark5BMark5DataStream *me = (Mark5BMark5DataStream *)self;

	me->mark5threadfunction();

	return 0;
}

int Mark5BMark5DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
	static int last_offsetsec = -1;
	int r;
	
	// call parent class equivalent function and store return value
	r = Mark5BDataStream::calculateControlParams(scan, offsetsec, offsetns);

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
void Mark5BMark5DataStream::initialiseFile(int configindex, int fileindex)
{
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	int rv;

	double startmjd;
	long long n;
	int bank;
	int doUpdate = 0;
	XLR_RETURN_CODE xlrRC;

	format = config->getDataFormat(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	framebytes = config->getFrameBytes(configindex, streamnum);
	framespersecond = config->getFramesPerSecond(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);

	framegranularity = framespersecond/12800;
	if(framegranularity < 1)
	{
		framegranularity = 1;
	}

	startOutputFrameNumber = -1;

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, framebytes, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
        if(fanout != 1)
        {
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
#if HAVE_MARK5IPC
                unlockMark5();
#endif
                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	bank = Mark5BankSetByVSN(xlrDevice, datafilenames[configindex][fileindex].c_str());
	if(bank < 0)
	{
		cerror << startl << "Cannot find module " << datafilenames[configindex][fileindex] << endl;

		dataremaining = false;
		keepreading = false;
		noMoreData = true;
		sendMark5Status(MARK5_STATE_NODATA, 0, 0.0, 0.0);

		return;
	}
	else
	{
		cinfo << startl << "Module " << datafilenames[configindex][fileindex] << " found in bank " << static_cast<char>('A' + bank) << endl;
	}

	cinfo << startl << "Module initialized: format=" << formatname << endl;

	startmjd = corrstartday + corrstartseconds/86400.0;

	sendMark5Status(MARK5_STATE_GETDIR, 0, startmjd, 0.0);

	if(dirlist.empty())
	{
		stringstream err;
		doUpdate = 1;
		cinfo << startl << "Getting module " << datafilenames[configindex][fileindex] << " directory info." << endl;

		rv = mark5LegacyLoad(dirlist, datafilenames[configindex][fileindex].c_str(), err);

		if(rv < 0)
		{
			cerror << err.str() << endl;

			sendMark5Status(MARK5_STATE_ERROR, 0, 0.0, 0.0);

			dataremaining = false;
			keepreading = false;
			++nError;
			WATCHDOG( XLRClose(xlrDevice) );
#if HAVE_MARK5IPC
			unlockMark5();
#endif
			MPI_Abort(MPI_COMM_WORLD, 1);
		}

		rv = dirlist.sanityCheck();
		if(rv < 0)
		{
			cerror << startl << "Module " << datafilenames[configindex][fileindex] << " contains undecoded scans" << endl;
			dataremaining = false;
			keepreading = false;

			return;
		}

		if(dirlist.getConstParameter("class")->getValue() != "mark5")
		{
			cerror << startl << "Module " << datafilenames[configindex][fileindex] << " directory does not seem to describe a mark5 module!" << endl;
			dataremaining = false;
			keepreading = false;

			return;
		}

		const DirListParameter *hash = dirlist.getConstParameter("hash");
		if(hash)
		{
			long long signature = calculateMark5Signature(xlrDevice);
			if(hash->getInt() != signature)
			{
				cerror << startl << "Module " << datafilenames[configindex][fileindex] << " directory is out of date (hash/signature in directory listing does not match that computed from the module.  hash=" << hash->getInt() << " sig=" << signature << endl;
				dataremaining = false;
				keepreading = false;

				return;
			}
		}
		else
		{
			cwarn << startl << "Module " << datafilenames[configindex][fileindex] << " directory does not contain a hash/signature" << endl;
		}

		if(dirlist.isParameterTrue("realtime"))
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
			cwarn << startl << "Enabled realtime playback mode" << endl;
			readDelayMicroseconds = 80000;	// prime the read delay to speed up convergence to best value
		}
	}

	sendMark5Status(MARK5_STATE_GOTDIR, 0, startmjd, 0.0);

	// find starting position

	if(scanPointer && scanNum >= 0)  /* just continue by reading next valid scan */
	{
		int p = cinfo.precision();

		cinfo << startl << "Advancing to next record scan from " << (scanNum+1) << " of " << dirlist.nScan() << endl;
		cinfo.precision(14);
		cinfo << startl << "Scan start at MJD " << scanPointer->getMjdStart() << " job ends at MJD " << jobEndMJD << endl;
		cinfo.precision(p);
		++scanNum;
		scanPointer = dirlist.getMark5Scan(scanNum);
		if(scanPointer == 0 || scanPointer->getMjdStart() > jobEndMJD)
		{
			cwarn << startl << "No more data for this job on this module" << endl;
			scanPointer = 0;
			dataremaining = false;
			keepreading = false;
			mark5threadstop = true;
			noMoreData = true;
			sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0.0, 0.0);

			return;
		}
		cinfo << startl << "Landed on record scan " << (scanNum+1) << endl;
		cinfo << startl << "readscan remains at " << readscan << endl;
		readpointer = scanPointer->getStartPointer();
		readseconds = (scanPointer->getMjdStart()-corrstartday)*86400 + scanPointer->getIntSecStart() - corrstartseconds + intclockseconds;

		readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);

		readnanoseconds = scanPointer->getIntNSStart();
	}
	else	/* first time this project */
	{
		n = 0;
		for(scanNum = 0; scanNum < dirlist.nScan(); ++scanNum)
		{
			double scanstart, scanend;
			scanPointer = dirlist.getMark5Scan(scanNum);
			scanstart = scanPointer->getFullMjdStart();
			scanend = scanPointer->getFullMjdEnd();

 			if(startmjd <= scanstart && scanstart < jobEndMJD)  /* obs starts before data */
			{
				int prec = cinfo.precision();

				cinfo.precision(12);
				cinfo << startl << "NM5 : scan found (1): " << (scanNum+1) << " named " << scanPointer->getName() << "  startmjd=" << startmjd << "  scanstart=" << scanstart << "  scanend=" << scanend << endl;
				cinfo.precision(prec);

				readpointer = scanPointer->getStartPointer();
				readseconds = (scanPointer->getMjdStart()-corrstartday)*86400 + scanPointer->getIntSecStart() - corrstartseconds + intclockseconds;
				readnanoseconds = scanPointer->getIntNSStart();
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
			else if(startmjd < scanend && scanstart < jobEndMJD) /* obs starts within data */
			{
				int fbytes;
				int prec = cinfo.precision();

				cinfo.precision(12);
				cinfo << startl << "NM5 : scan found (2): " << (scanNum+1) << " named " << scanPointer->getName() << "  startmjd=" << startmjd << "  scanstart=" << scanstart << "  scanend=" << scanend << endl;
				cinfo.precision(prec);

				readpointer = scanPointer->getStartPointer();
				n = static_cast<long long>((
					( ( (corrstartday - scanPointer->getMjdStart())*86400 
					+ corrstartseconds - scanPointer->getSecStart()))
					*scanPointer->getFramesPerSecond()) + 0.5);
				fbytes = scanPointer->getFrameBytes();
				readpointer += n*fbytes;
				if(readpointer >= scanPointer->getStart() + scanPointer->getLength())
				{
					cerror << startl << "Scan " << (scanNum+1) << " duration seems misrepresented in the directory listing.  Jumping to next scan to be safe." << endl;

					readpointer = -1;

					continue;
				}
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
		if(scanNum >= dirlist.nScan() || scanPointer == 0)
		{
			readpointer = -1;
		}
	}

	if(readpointer >= 0)
	{
		cinfo << startl << "The frame start day is " << scanPointer->getMjdStart() << ", the frame start seconds is " << scanPointer->getSecStart() << ", readscan is " << readscan << ", readseconds is " << readseconds << ", readnanoseconds is " << readnanoseconds << ", readpointer is " << readpointer << endl;

		if(scanPointer->getFormat() != MK5_FORMAT_MARK5B)
		{
			cerror << startl << "Error! A Mark5B scan was expected (based on the .input file), but the scan for the matching time is not Mark5B formatted!  The actual format number (as found in the .dir file) is: " << scanPointer->getFormat() << endl;

			readpointer = -1;
		}
	}

        if(readpointer <= -1)
        {
		cwarn << startl << "initialiseFile: No data for this job on this module" << endl;
		scanPointer = 0;
		scanNum = 0;
		dataremaining = false;
		keepreading = false;
		noMoreData = true;
		noDataOnModule = true;
		readseconds = 0;
		readnanoseconds = 0;
		sendMark5Status(MARK5_STATE_NOMOREDATA, 0, 0.0, 0.0);
		mark5threadstop = true;

		return;
        }

	sendMark5Status(MARK5_STATE_GOTDIR, readpointer, scanPointer->getFullMjdStart(), 0.0);

	newscan = 1;

	/* update all the configs to ensure that the nsincs and 
	 * headerbytes are correct
	 */
	if(doUpdate)
	{
		cinfo << startl << "Updating all configs" << endl;
		for(int i = 0; i < numdatasegments; ++i)
		{
			updateConfig(i);
		}
	}

	// pointer to first byte after end of current scan
	readend = scanPointer->getStart() + scanPointer->getLength();

	lockstart = lockend = lastslot = -1;

	// cause Mark5 reading thread to go ahead and start filling buffers
	// these barriers come in pairs...

	pthread_barrier_wait(&mark5threadbarrier);
	pthread_barrier_wait(&mark5threadbarrier);
	
	if(readpointer >= 0)
	{
		cinfo << startl << "Scan " << (scanNum+1) <<" initialised" << endl;
	}
	else
	{
		cinfo << startl << "The read thread should die shortly" << endl;
	}
}

void Mark5BMark5DataStream::openfile(int configindex, int fileindex)
{
	/* fileindex should never increase for native mark5, but
	 * check just in case. 
	 */
	if(fileindex >= confignumfiles[configindex])
	{
		dataremaining = false;
		keepreading = false;
		cinfo << startl << "Mark5BMark5DataStream is exiting because fileindex is " << fileindex << ", while confignumconfigfiles is " << confignumfiles[configindex] << endl;

		return;
	}

	dataremaining = true;
	initialiseFile(configindex, fileindex);
}

int Mark5BMark5DataStream::dataRead(int buffersegment)
{
	// Note: here readbytes is actually the length of the buffer segment, i.e., the amount of data wanted to be "read" by calling processes. 
	// In this threaded approach the actual size of reads off Mark5 modules (as implemented in the ring buffer writing thread) is generally larger.

	unsigned char *destination = reinterpret_cast<unsigned char *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);
	int n1, n2;	/* slot number range of data to be processed.  Either n1==n2 or n1+1==n2 */
	unsigned int fixend;
	int bytesvisible;
	int lockmod = readbufferslots - 1;
	int fixReturn;

	if(lockstart < -1)
	{
		csevere << startl << "dataRead lockstart=" << lockstart << endl;
	}

	if(lockstart == -1)
	{
		// first decoding of scan
		fixindex = readbufferslotsize;	// start at beginning of slot 1 (second slot)
		lockstart = lockend = 1;
		pthread_mutex_lock(mark5threadmutex + (lockstart % lockmod));
		if(mark5xlrfail)
		{
			cwarn << startl << "dataRead detected mark5xlrfail. [1] Stopping." << endl;
			dataremaining = false;
			keepreading = false;
			pthread_mutex_unlock(mark5threadmutex + (lockstart % lockmod));
			lockstart = lockend = -2;

			return 0;
		}
	}

	n1 = fixindex / readbufferslotsize;
	if(lastslot >= 0 && fixindex + readbytes > endindex && fixindex < endindex)
	{
		// here we have less than readbytes remaining so make sure n2 gets set properly
		n2 = (endindex - 1) / readbufferslotsize;
	}
	else
	{
		n2 = (fixindex + readbytes - 1) / readbufferslotsize;
	}

	// note: it should be impossible for n2 >= readbufferslots because a previous memmove and slot shuffling should have prevented this.
	if(n2 >= readbufferslots)
	{
		csevere << startl << "dataRead n2=" << n2 << " >= readbufferslots=" << readbufferslots << " fixindex=" << fixindex << " readbufferslotsize=" << readbufferslotsize << " n1=" << n1 << " n2=" << n2 << " endindex=" << endindex << " lastslot=" << lastslot << endl;
	}

	if(n2 > n1 && lockend != n2)
	{
		lockend = n2;
		pthread_mutex_lock(mark5threadmutex + (lockend % lockmod));
		if(mark5xlrfail)
		{
			cwarn << startl << "dataRead detected mark5xlrfail. [2] Stopping." << endl;
			dataremaining = false;
			keepreading = false;
			pthread_mutex_unlock(mark5threadmutex + (lockstart % lockmod));
			pthread_mutex_unlock(mark5threadmutex + (lockend % lockmod));
			lockstart = lockend = -2;

			return 0;
		}
	}
	
	if(lastslot == n2)
	{
		fixend = endindex;
	}
	else
	{
		fixend = (n2+1)*readbufferslotsize;
	}

	if(fixend <= fixindex)
	{
		csevere << startl << "Weird: fixend=" << fixend << " <= fixindex=" << fixindex << ": this should never be!  readbufferslots=" << readbufferslots << " readbufferslotsize=" << readbufferslotsize << " n1=" << n1 << " n2=" << n2 << " endindex=" << endindex << " lastslot=" << lastslot << endl;

		bufferinfo[buffersegment].validbytes = 0;
		bufferinfo[buffersegment].readto = true;

		// Note that this exit strategy likely will hang mpifxcorr

		return 0;
	}

	bytesvisible = fixend - fixindex;

	// "fix" Mark5B data: remove stray packets/bytes and put good frames on a uniform grid
	fixReturn = mark5bfix(destination, readbytes, readbuffer+fixindex, bytesvisible, framespersecond, startOutputFrameNumber, &m5bstats);
	if(fixReturn < 0)
	{
		cwarn << startl << "mark5bfix returned " << fixReturn << endl;
	}
	if(startOutputFrameNumber >= 0 && m5bstats.srcUsed <= 0)
	{
		int lastOFN = startOutputFrameNumber;

		startOutputFrameNumber = -1;

		fixReturn = mark5bfix(destination, readbytes, readbuffer+fixindex, bytesvisible, framespersecond, startOutputFrameNumber, &m5bstats);

		cwarn << startl << "Just used zero bytes in fix operation.  Tried again.  startOutputFrame: " << lastOFN << " -> -1 -> " << (m5bstats.startFrameNumber + m5bstats.destUsed/10016) << endl;
	}
	bufferinfo[buffersegment].validbytes = m5bstats.destUsed;
	bufferinfo[buffersegment].readto = true;
	consumedbytes += m5bstats.srcUsed;
	if(bufferinfo[buffersegment].validbytes > 0)
	{
		// In the case of Mark5B, we can get the time from the data, so use that just in case there was a jump
		bufferinfo[buffersegment].scanns = m5bstats.startFrameNanoseconds;
		// FIXME: warning! here we are assuming no leap seconds since the epoch of the Mark5B stream. FIXME
		// FIXME: below assumes each scan is < 86400/2 seconds long
		bufferinfo[buffersegment].scanseconds = m5bstats.startFrameSeconds + intclockseconds - corrstartseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
		bufferinfo[buffersegment].scan = readscan;
		if(bufferinfo[buffersegment].scanseconds > 86400/2)
		{
			bufferinfo[buffersegment].scanseconds -= 86400;
		}
		else if(bufferinfo[buffersegment].scanseconds < -86400/2)
		{
			bufferinfo[buffersegment].scanseconds += 86400;
		}

		readnanoseconds = bufferinfo[buffersegment].scanns;
		readseconds = bufferinfo[buffersegment].scanseconds;

		if(m5bstats.destUsed == m5bstats.srcUsed)
		{
			startOutputFrameNumber = (m5bstats.startFrameNumber + m5bstats.destUsed/10016) % framespersecond;
		}
		else
		{
			if(m5bstats.srcUsed < m5bstats.destUsed - 10*10016)
			{
				// Warn if more than 10 frames of data are missing
				cwarn << startl << "Data gap of " << (m5bstats.destUsed-m5bstats.srcUsed) << " bytes out of " << m5bstats.destUsed << " bytes found  startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << endl;
			}
			else if(m5bstats.srcUsed > m5bstats.destUsed + 10*10016)
			{
				// Warn if more than 10 frames of unexpected data found
				// Note that 5008 bytes of extra data at scan ends is not uncommon, so specifically don't warn for that.
				cwarn << startl << "Data excess of " << (m5bstats.srcUsed-m5bstats.destUsed) << " bytes out of " << m5bstats.destUsed << " bytes found  startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << endl;
			}
			startOutputFrameNumber = -1;
		}
	}
	else
	{
		startOutputFrameNumber = -1;
	}

	fixindex += m5bstats.srcUsed;

	if(lastslot == n2 && (fixindex+minleftoverdata > endindex || bytesvisible < readbytes / 4) )
	{
		// end of useful data for this scan
		cinfo << startl << "End of data for record scan " << (scanNum+1) << endl;
		dataremaining = false;
		pthread_mutex_unlock(mark5threadmutex + (lockstart % lockmod));
		if(lockstart != lockend)
		{
			pthread_mutex_unlock(mark5threadmutex + (lockend % lockmod));
		}
		lockstart = lockend = -2;
		startOutputFrameNumber = -1;
	}
	else if(fixindex == readbufferslotsize*readbufferslots)	// special case where the buffer was used up exactly
	{
		cinfo << startl << "In special case where full buffer was used up exactly" << endl;

		// start again at the beginning of slot 1
		fixindex = readbufferslotsize;			

		// need to acquire lock for first slot
		pthread_mutex_lock(mark5threadmutex + 1);

		// unlock existing locks
		while(lockstart < readbufferslots)
		{
			pthread_mutex_unlock(mark5threadmutex + (lockstart % lockmod));
			++lockstart;
		}

		// At this point we hold just this one lock.
		lockstart = 1;
		lockend = 1;
	}
	else
	{
		int n3;
		// note:  in all cases n2 = n1 or n1+1, n3 = n1 or n1+1 and n3 = n2 or n2+1
		// i.e., n3 >= n2 >= n1 and n3-n1 <= 1

		n3 = fixindex / readbufferslotsize;

		while(lockstart < n3)
		{
			pthread_mutex_unlock(mark5threadmutex + (lockstart % lockmod));
			++lockstart;
		}

		if(lockstart == readbufferslots - 1 && lastslot != readbufferslots - 1)
		{
			// Here it is time to move the data in the last slot to slot 0
			// Geometry: |  slot 0  |  slot 1  |  slot 2  |  slot 3  |  slot 4  |  slot 5  |
			// Before:   |          |dddddddddd|          |          |          |      dddd|
			// After:    |      dddd|dddddddddd|          |          |          |          |

			// Note! No need change locks here as slot 0 and slot readbufferslots - 1 share a lock

			lockstart = 0;

			int newstart = fixindex % readbufferslotsize;
			memmove(readbuffer + newstart, readbuffer + fixindex, readbuffersize-fixindex);
			fixindex = newstart;

			lockend = 0;
		}
	}

	return 0;
}

void Mark5BMark5DataStream::loopfileread()
{
	int perr;
	int numread = 0;

	//lock the outstanding send lock
	perr = pthread_mutex_lock(&outstandingsendlock);
	if(perr != 0)
	{
		csevere << startl << "Error in initial readthread lock of outstandingsendlock!" << endl;
	}

	openfile(bufferinfo[0].configindex, 0);

	if(keepreading)
	{
		diskToMemory(numread++);
		if(keepreading)
		{
			diskToMemory(numread++);
			perr = pthread_mutex_lock(&(bufferlock[numread]));
			if(perr != 0)
			{
				csevere << startl << "Error in initial readthread lock of first buffer section!" << endl;
			}
		}
		if(keepreading)
		{
			diskToMemory(numread++);
		}
		lastvalidsegment = (numread-1) % numdatasegments;
	}

	readthreadstarted = true;
	perr = pthread_cond_signal(&initcond);
	if(perr != 0)
	{
		csevere << startl << "Datastream readthread error trying to signal main thread to wake up!" << endl;
	}

	if(noDataOnModule)
	{
		dataremaining = false;
		keepreading = false;
	}

	while(keepreading && (bufferinfo[lastvalidsegment].configindex < 0 || filesread[bufferinfo[lastvalidsegment].configindex] <= confignumfiles[bufferinfo[lastvalidsegment].configindex]))
	{
		while(dataremaining && keepreading)
		{
			lastvalidsegment = (lastvalidsegment + 1)%numdatasegments;

			//lock the next section
			perr = pthread_mutex_lock(&(bufferlock[lastvalidsegment]));
			if(perr != 0)
			{
				csevere << startl << "Error in readthread lock of buffer section!" << lastvalidsegment << endl;
			}

			//unlock the previous section
			perr = pthread_mutex_unlock(&(bufferlock[(lastvalidsegment-1+numdatasegments) % numdatasegments]));    
			if(perr != 0)
			{
				csevere << startl << "Error (" << perr << ") in readthread unlock of buffer section!" << (lastvalidsegment-1+numdatasegments)%numdatasegments << endl;
			}

			//do the read
			diskToMemory(lastvalidsegment);
			numread++;
		}
//cverbose << startl << "Out of inner read loop: keepreading=" << keepreading << " dataremaining=" << dataremaining << endl;
		if(keepreading)
		{
			openfile(bufferinfo[0].configindex, 0);
		}
		if(!keepreading)
		{
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = config->getExecuteSeconds();
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
			cinfo << startl << "New record scan -> keepreading=false" << endl;

			dataremaining = false;
		}
	}
	if(lockstart >= 0)
	{
		int v = pthread_mutex_trylock(mark5threadmutex + (lockstart % (readbufferslots - 1)));
		if(v == 0)
		{
			cwarn << startl << "Weird: i was able to lock " << (lockstart % (readbufferslots - 1)) << endl;
		}
		cinfo << startl << "About to unlock thread mutex " << (lockstart % (readbufferslots - 1)) << endl;
		pthread_mutex_unlock(mark5threadmutex + (lockstart % (readbufferslots - 1)));
		cinfo << startl << "Unlocked thread mutex " << (lockstart % (readbufferslots - 1)) << endl;
	}
	if(numread > 0)
	{
		perr = pthread_mutex_unlock(&(bufferlock[lastvalidsegment]));
		if(perr != 0)
		{
			csevere << startl << "Error (" << perr << ") in readthread unlock of buffer section!" << lastvalidsegment << endl;
		}
	}

	//unlock the outstanding send lock
	perr = pthread_mutex_unlock(&outstandingsendlock);
	if(perr != 0)
	{
		csevere << startl << "Error (" << perr << ") in readthread unlock of outstandingsendlock!" << endl;
	}

	cinfo << startl << "Readthread is exiting; dataremaining=" << dataremaining << ", keepreading=" << keepreading << endl;
}

void Mark5BMark5DataStream::servoMark5()
{
	double tv_us;
	static double now_us = 0.0;
	static long long lastpos = 0;
	struct timeval tv;

	gettimeofday(&tv, 0);
	tv_us = 1.0e6*tv.tv_sec + tv.tv_usec;

	if(tv_us - now_us > 1.5e6 && nReads > 4)
	{
		if(lastpos > 0)
		{
			double rate;
			double fmjd;
			enum Mk5State state;

			fmjd = corrstartday + (corrstartseconds + model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + static_cast<double>(readnanoseconds)/1000000000.0)/86400.0;
			if(newscan > 0)
			{
				double fmjd2;

				newscan = 0;
				state = MARK5_STATE_START;
				rate = 0.0;
				lastrate = 0.0;
				nrate = 0;
				fmjd2 = scanPointer->getFullMjdStart();
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
				rate = (static_cast<double>(readpointer) - static_cast<double>(lastpos))*8.0/(tv_us - now_us);

				// If in real-time mode, servo playback rate through adjustable inter-read delay
				if(dirlist.isParameterTrue("realtime"))
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
		lastpos = readpointer;
		now_us = tv_us;
	}
}



int Mark5BMark5DataStream::resetDriveStats()
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

int Mark5BMark5DataStream::reportDriveStats()
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

void Mark5BMark5DataStream::openStreamstor()
{
	XLR_RETURN_CODE xlrRC;

	xlrRC = openMark5(&xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		cfatal << startl << "openMark5 did not return XLR_SUCCESS.  Must abort." << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
}

void Mark5BMark5DataStream::closeStreamstor()
{
	sendMark5Status(MARK5_STATE_CLOSE, 0, 0.0, 0.0);
	WATCHDOG( XLRClose(xlrDevice) );
}

void Mark5BMark5DataStream::resetStreamstor()
{
	sendMark5Status(MARK5_STATE_RESETTING, 0, 0.0, 0.0);
	WATCHDOG( XLRReset(xlrDevice) );
}

int Mark5BMark5DataStream::sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate)
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
      		snprintf(mk5status.scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s", scanPointer->getName().c_str());
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
