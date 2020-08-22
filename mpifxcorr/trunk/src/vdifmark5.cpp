/***************************************************************************
 *   Copyright (C) 2007-2020 by Walter Brisken and Adam Deller             *
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
#include <vdifio.h>
#include "config.h"
#include "vdifmark5.h"
#include "watchdog.h"
#include "alert.h"
#include "mark5utils.h"
#include "dirlist/old_dirlist.h"

#if HAVE_MARK5IPC
#include <mark5ipc.h>
#endif

VDIFMark5DataStream::VDIFMark5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		VDIFDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
}

VDIFMark5DataStream::~VDIFMark5DataStream()
{
}

// this function implements the Mark5 module reader.  It is continuously either filling data into a ring buffer or waiting for a mutex to clear.
// any serious problems are reported by setting readfail.  This will call the master thread to shut down.
void VDIFMark5DataStream::readthreadfunction()
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

	for(;;)
	{
		// Lock for readbufferwriteslot = 1 shall be set at this point by startReaderThread()

		while(keepreading)
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
				cwarn << startl << "Developer error: readthreadfunction: readpointer >= readend" << endl;

				endofscan = true;
				
				// back up one slot
				--readbufferwriteslot;
				if(readbufferwriteslot == 0)
				{
					readbufferwriteslot = readbufferslots-1;
				}
				
				lastslot = readbufferwriteslot;
				endindex = lastslot*readbufferslotsize;
cinfo << startl << "lastslot=" << lastslot << " endindex=" << endindex << endl;

				break;
			}

			// if this will be the last read of the scan, shorten if necessary
			if(readpointer + bytes > readend)
			{
				int origbytes = bytes;
				bytes = readend - readpointer;
				bytes -= (bytes % 8);		// enforce 8 byte multiple length
				endofscan = true;

				lastslot = readbufferwriteslot;
				endindex = lastslot*readbufferslotsize + bytes;	// No data in this slot from here to end

				cverbose << startl << "At end of scan: shortening Mark5 read to only " << bytes << " bytes " << "(was " << origbytes << ")" << endl;
			}

			// This is where the actual read from the Mark5 unit happens
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
					readfail = true;
				}

				return;
			}
			else
			{
				int curslot = readbufferwriteslot;

				readpointer += bytes;
				++nReads;

				++readbufferwriteslot;
				if(readbufferwriteslot >= readbufferslots)
				{
					// Note: we always save slot 0 for wrap-around
					readbufferwriteslot = 1;
				}
				pthread_mutex_lock(readthreadmutex + (readbufferwriteslot % lockmod));
				pthread_mutex_unlock(readthreadmutex + (curslot % lockmod));

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
		pthread_mutex_unlock(readthreadmutex + (readbufferwriteslot % lockmod));
		if(!keepreading)
		{
			break;
		}

		// No locks shall be set at this point
	} 

	reportDriveStats();
	closeStreamstor();
#if HAVE_MARK5IPC
	unlockMark5();
#endif

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

void *VDIFMark5DataStream::launchreadthreadfunction(void *self)
{
	VDIFMark5DataStream *me = (VDIFMark5DataStream *)self;

	me->readthreadfunction();

	return 0;
}

int VDIFMark5DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
	static int last_offsetsec = -1;
	int r;
	
	// call parent class equivalent function and store return value
	r = VDIFDataStream::calculateControlParams(scan, offsetsec, offsetns);

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
void VDIFMark5DataStream::initialiseFile(int configindex, int fileindex)
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
	int muxFlags;
	XLR_RETURN_CODE xlrRC;

	format = config->getDataFormat(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);	/* Bits per sample.  If complex, bits per component. */
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	inputframebytes = config->getFrameBytes(configindex, streamnum);
	framespersecond = config->getFramesPerSecond(configindex, streamnum)/config->getDNumMuxThreads(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);

	nGap = framespersecond/4;	// 1/4 second gap of data yields a mux break
	startOutputFrameNumber = -1;
	minleftoverdata = 4*inputframebytes;	// waste up to 4 input frames at end of read

	nthreads = config->getDNumMuxThreads(configindex, streamnum);
	threads = config->getDMuxThreadMap(configindex, streamnum);

	muxFlags = VDIF_MUX_FLAG_RESPECTGRANULARITY | VDIF_MUX_FLAG_PROPAGATEVALIDITY;
	if(sampling == Configuration::COMPLEX)
	{
		muxFlags |= VDIF_MUX_FLAG_COMPLEX;
	}
	rv = configurevdifmux(&vm, inputframebytes, framespersecond, nbits, nthreads, threads, nSort, nGap, muxFlags);
	if(rv < 0)
	{
		cfatal << startl << "configurevmux failed with return code " << rv << endl;
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

	if(nrecordedbands > nthreads)
	{
		int nBandPerThread = nrecordedbands/nthreads;

		cinfo << startl << "Note: " << nBandPerThread << " recoded channels (bands) reside on each thread.  Support for this is new.  Congratulations for being bold and trying this out!  Warranty void in the 193 UN recognized nations." << endl;
		
		if(nBandPerThread * nthreads != nrecordedbands)
		{
			cerror << startl << "Error: " << nrecordedbands << " recorded channels (bands) were recorded but they are divided unequally across " << nthreads << " threads.  This is not allowed.  Things will probably get very bad soon..." << endl;
		}
		setvdifmuxinputchannels(&vm, nBandPerThread);
	}
	else if(nrecordedbands < nthreads)
	{
		/* must be a fanout mode (DBBC3 probably) */
		int nThreadPerBand = nthreads/nrecordedbands;

		cinfo << startl << "Note: " << nThreadPerBand << " threads are used to store each channel (band; e.g., this is a VDIF fanout mode).  Support for this is new.  Congratulations for experimenting with plausible code.  Warranty void on weekdays and select weekends." << endl;

		if(nThreadPerBand * nrecordedbands != nthreads)
		{
			cerror << startl << "Error: " << nthreads << " threads were recorded but they are divided unequally across " << nrecordedbands << " record channels (bands).  This is not allowed.  Things are about to go from bad to worse.  Hold onto your HAT..." << endl;
		}
		setvdifmuxfanoutfactor(&vm, nThreadPerBand);
	}

	/* Note: the following fanout concept is an explicit one and is not relevant to VDIF in any way */
	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, vm.outputFrameSize, config->getDDecimationFactor(configindex, streamnum), config->getDAlignmentSeconds(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
        if(fanout != 1)
        {
		cfatal << startl << "Classic fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
#if HAVE_MARK5IPC
                unlockMark5();
#endif
                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	cinfo << startl << "VDIFMark5DataStream::initialiseFile format=" << formatname << endl;

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
		cinfo << startl << "Advancing to next record scan from " << (scanNum+1) << endl;
		++scanNum;
		scanPointer = dirlist.getMark5Scan(scanNum);
		if(scanPointer == 0 || scanPointer->getMjdStart() > jobEndMJD)
		{
			cwarn << startl << "No more data for this job on this module" << endl;
			scanPointer = 0;
			dataremaining = false;
			keepreading = false;
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
				fbytes = scanPointer->getFrameBytes()*scanPointer->getTracks();
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

		if(scanPointer->getFormat() != MK5_FORMAT_VDIF)
		{
			cerror << startl << "Error! A VDIF scan was expected (based on the .input file), but the scan for the matching time is not VDIF formatted!  The actual format number (as found in the .dir file) is: " << scanPointer->getFormat() << endl;

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
	else
	{
		cinfo << startl << "Not updating all configs" << endl;
	}

	// pointer to first byte after end of current scan
	readend = scanPointer->getStart() + scanPointer->getLength();

	lockstart = lockend = lastslot = -1;

	if(readpointer >= 0)
	{
		cinfo << startl << "Scan " << (scanNum+1) <<" initialised" << endl;
	}
	else
	{
		cinfo << startl << "The read thread should die shortly" << endl;
	}

	startReaderThread();
}

void VDIFMark5DataStream::openfile(int configindex, int fileindex)
{
	/* fileindex should never increase for native mark5, but
	 * check just in case. 
	 */
	if(fileindex >= confignumfiles[configindex])
	{
		dataremaining = false;
		keepreading = false;
		cinfo << startl << "VDIFMark5DataStream is exiting because fileindex is " << fileindex << ", while confignumconfigfiles is " << confignumfiles[configindex] << endl;

		return;
	}

	dataremaining = true;
	initialiseFile(configindex, fileindex);
}

void VDIFMark5DataStream::loopfileread()
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
				csevere << startl << "Error (" << perr << ") in readthread unlock of buffer section!" << (lastvalidsegment-1+numdatasegments) % numdatasegments << endl;
			}

			//do the read
			diskToMemory(lastvalidsegment);
			numread++;
		}
cverbose << startl << "Out of inner read loop: keepreading=" << keepreading << " dataremaining=" << dataremaining << endl;
		if(keepreading)
		{
			openfile(bufferinfo[0].configindex, 0);
		}
		if(!keepreading)
		{
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = config->getExecuteSeconds();
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
			cverbose << startl << "readscan == getNumScans -> keepreading = false" << endl;
		}
	}
	if(lockstart >= 0)
	{
		pthread_mutex_unlock(readthreadmutex + (lockstart % (readbufferslots - 1)));
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

	cverbose << startl << "Readthread is exiting; dataremaining=" << dataremaining << ", keepreading=" << keepreading << endl;
}

void VDIFMark5DataStream::servoMark5()
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


int VDIFMark5DataStream::resetDriveStats()
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

int VDIFMark5DataStream::reportDriveStats()
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

void VDIFMark5DataStream::openStreamstor()
{
	XLR_RETURN_CODE xlrRC;

	xlrRC = openMark5(&xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		cfatal << startl << "openMark5 did not return XLR_SUCCESS.  Must abort." << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
}

void VDIFMark5DataStream::closeStreamstor()
{
	sendMark5Status(MARK5_STATE_CLOSE, 0, 0.0, 0.0);
	WATCHDOG( XLRClose(xlrDevice) );
}

void VDIFMark5DataStream::resetStreamstor()
{
	sendMark5Status(MARK5_STATE_RESETTING, 0, 0.0, 0.0);
	WATCHDOG( XLRReset(xlrDevice) );
}

int VDIFMark5DataStream::sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate)
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
