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
#include <sys/socket.h>
#include <mpi.h>
#include <unistd.h>
#include <vdifio.h>
#include "config.h"
#include "vdifsharedmemory.h"
#include "alert.h"


VDIFSharedMemoryDataStream::VDIFSharedMemoryDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) :
	VDIFDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	int perr;

	/* each data buffer segment contains an integer number of frames, 
	 * because thats the way config determines max bytes
	 */

	jobEndMJD = conf->getStartMJD() + (conf->getStartSeconds() + conf->getExecuteSeconds() + 1)/86400.0;

	estimatedbytes -= readbuffersize;	// uncount the buffer size calculated by VDIFDataStream()

	readbufferslots = 8;
	readbufferslotsize = (bufferfactor/numsegments)*conf->getMaxDataBytes(streamnum)*21LL/10LL;
	readbufferslotsize -= (readbufferslotsize % config->getFrameBytes(0, streamnum)); // make it a multiple of frame size
	readbuffersize = readbufferslots * readbufferslotsize;
	// Note: the read buffer is allocated in vdiffile.cpp by VDIFDataStream::initialse()
	// the above values override defaults for file-based VDIF

	estimatedbytes += readbuffersize;	// add back the buffer size calculated here.

	cinfo << startl << "VDIFSharedMemoryDataStream::VDIFSharedMemoryDataStream: Set readbuffersize to " << readbuffersize << endl;
	cinfo << startl << "mdb = " << conf->getMaxDataBytes(streamnum) << "  rbslots=" << readbufferslots << "  readbufferslotsize=" << readbufferslotsize << endl;

	// set up shared memory reader thread
	shmthreadstop = false;
	shmbuffer = 0;
	shmbufferslot = 0;
	shmbufferframe = 0;
	lockstart = lockend = lastslot = -2;
	endindex = 0;

	perr = pthread_barrier_init(&shmthreadbarrier, 0, 2);
	shmthreadmutex = new pthread_mutex_t[readbufferslots];
	for(int m = 0; m < readbufferslots; ++m)
	{
		if(perr == 0)
		{
			perr = pthread_mutex_init(shmthreadmutex + m, 0);
		}
	}

	if(perr == 0)
	{
		pthread_attr_t attr;
		pthread_attr_init(&attr);
		pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
		perr = pthread_create(&shmthread, &attr, VDIFSharedMemoryDataStream::launchshmthreadfunction, this);
		pthread_attr_destroy(&attr);
	}

	if(perr)
	{
		cfatal << startl << "Cannot create the shared memory reader thread!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
}

VDIFSharedMemoryDataStream::~VDIFSharedMemoryDataStream()
{
	shmthreadstop = true;

	/* barriers come in pairs to allow the read thread to always get first lock */
	pthread_barrier_wait(&shmthreadbarrier);
	pthread_barrier_wait(&shmthreadbarrier);

	pthread_join(shmthread, 0);

	for(int m = 0; m < readbufferslots; ++m)
	{
		pthread_mutex_destroy(shmthreadmutex + m);
	}
	delete [] shmthreadmutex;
	pthread_barrier_destroy(&shmthreadbarrier);

	if(shmbuffer)
	{
		detachSharedMemoryBuffer(shmbuffer);
	}
}

int VDIFSharedMemoryDataStream::setsharedmemorybuffersecond(int second)
{
	while(1)
	{
		int nearest = 86400;
		int index;
		
		for(index = 0; index < shmbuffer->nSecond; ++index)
		{
			if(shmbuffer->second[index] == second)
			{
				shmbufferslot = index;
				shmbufferframe = shmbuffer->startFrame[index];
				shmbuffersecond = second;

				cinfo << startl << "shared memory for second " << second << " found in slot " << index << endl;

				return 0;
			}
			else
			{
				int near = second - shmbuffer->second[index];

				if(near < 0)
				{
					near += 86400;
				}
				if(near < nearest && near > 0)
				{
					nearest = near;
				}
			}
		}

		// FIXME: handle case where we start just a bit late...  Need heuristics

		if(nearest <= 30)
		{
			cwarn << startl << "shared memory for second " << second << " is " << nearest << " seconds away..." << index << endl;
		}
		else
		{
			cerror << startl << "shared memory for second " << second << " is " << nearest << " seconds away and I won't wait that long." << index << endl;

			return -1;
		}

		usleep(500000);
	}
}

int VDIFSharedMemoryDataStream::sharedmemorybuffercopy(char *base, size_t offset, int copysize, unsigned int *bytescopied)
{
	int nextSlot;
	int origsize = copysize;
	bool end = false;
	int64_t framesize = shmbuffer->packetSize*shmbuffer->nThread;
	int64_t fps = shmbuffer->packetsPerSecond/shmbuffer->nThread;
	int64_t size;
	char *src;
	char *dest;

	dest = base + offset;

	nextSlot = (shmbufferslot + 1) % shmbuffer->nSecond;

	while(copysize > shmbuffer->packetSize && !end)
	{
		int s;
		int targetFrame;	// wait until this second slot's endFrame gets this high

		if(shmbufferframe < shmbuffer->startFrame[shmbufferslot])
		{
			if(copysize != origsize && shmbufferframe + 3 < shmbuffer->startFrame[shmbufferslot])
			{
				// a significant interruption in data, so the data segments must be split

				break;
			}
			shmbufferframe = shmbuffer->startFrame[shmbufferslot];
		}

		targetFrame = shmbufferframe + copysize/framesize - 1;
		if(targetFrame >= shmbuffer->packetsPerSecond/shmbuffer->nThread)
		{
			targetFrame = shmbuffer->packetsPerSecond/shmbuffer->nThread - 1;
		}

		// wait for current slot to be done (e.g. next slot to have newer data); microsleep 10ms up to 200 times
		for(s = 0; s < 200; ++s)
		{
			if(shmbuffer->second[nextSlot] > shmbuffersecond)
			{
				break;
			}
			usleep(10000);
		}
		if(shmbuffer->endFrame[shmbufferslot] < targetFrame)
		{
			targetFrame = shmbuffer->endFrame[shmbufferslot];

			cinfo << startl << "Changed targetFrame to " << targetFrame << endl;

			end = true;
		}
		size = (targetFrame + 1 - shmbufferframe)*framesize;
		src = shmbuffer->data + (fps*shmbufferslot + shmbufferframe)*framesize;
		memcpy(dest, src, size);
		cverbose << startl << "MEMCPY: dOffset = " << offset << " sOffset = " << ((fps*shmbufferslot + shmbufferframe)*framesize) << "  size = " << size << endl;
		usleep(10000);

		if(shmbuffer->second[nextSlot] > shmbuffersecond && shmbuffer->endFrame[shmbufferslot] == targetFrame)
		{
			cverbose << startl << "Advancing SHM slot from " << shmbufferslot << " to " << nextSlot << endl;
			shmbufferslot = nextSlot;
			nextSlot = (shmbufferslot + 1) % shmbuffer->nSecond;
			shmbuffersecond = shmbuffer->second[shmbufferslot];
			shmbufferframe = shmbuffer->startFrame[shmbufferslot];
			if(shmbuffer->endFrame[shmbufferslot] + 2 < fps)
			{
				// a gap is identified, cause data segment to be split

				end = true;
			}
		}
		else if(targetFrame < shmbuffer->endFrame[shmbufferslot])
		{
			shmbufferframe = targetFrame + 1;

			end = true;	// how could it not be: only way to stop short of second boundary legitimately is if request was short of that.
		}
		else
		{
			shmbufferslot = nextSlot;
			shmbufferframe = 0;
			dataremaining = false;
			keepreading = false;

			end = true;
		}

		dest += size;
		copysize -= size;
	}

	*bytescopied = origsize - copysize;

	return 1;
}

// this function implements the shared memory copier.  It is continuously either filling data into a ring buffer or waiting for a mutex to clear.
void VDIFSharedMemoryDataStream::shmthreadfunction()
{
	int lockmod = readbufferslots-1;	// used to team up slots 0 and readbufferslots-1

	for(;;)
	{
		// No locks shall be set at this point

		/* First barrier is before the locking of slot number 1 */
		pthread_barrier_wait(&shmthreadbarrier);

		readbufferwriteslot = 1;	// always 
		pthread_mutex_lock(shmthreadmutex + (readbufferwriteslot % lockmod));
		if(shmthreadstop)
		{
			cverbose << startl << "shmthreadfunction: shmthreadstop -> this thread will end." << endl;
		}
		/* Second barrier is after the locking of slot number 1 */
		pthread_barrier_wait(&shmthreadbarrier);

		while(!shmthreadstop)
		{
			unsigned int bytes;
			bool endofscan = false;
			int status;

			// This is where the actual read from the shared memory buffer happens
			status = sharedmemorybuffercopy((char *)(readbuffer), readbufferwriteslot*readbufferslotsize, readbufferslotsize, &bytes);

			if(bytes == 0)
			{
				status = -1;
		// Not sure what to do here
			}
			else if(bytes < readbufferslotsize)
			{
				// a partial read
				endofscan = true;
				lastslot = readbufferwriteslot;
				endindex = lastslot*readbufferslotsize + bytes; // No data in this slot from here to end
				cverbose << startl << "Short read: only " << bytes << " bytes where " << readbufferslotsize << " bytes were requested" << endl;
			}

			// FIXME: do the right thing in various error conditions.  code block below is if data is good
			if(status == 1)
			{
				int curslot = readbufferwriteslot;

				++readbufferwriteslot;
				if(readbufferwriteslot >= readbufferslots)
				{
					// Note: we always save slot 0 for wrap-around
					readbufferwriteslot = 1;
				}
				pthread_mutex_lock(shmthreadmutex + (readbufferwriteslot % lockmod));
				pthread_mutex_unlock(shmthreadmutex + (curslot % lockmod));

				if(!dataremaining)
				{
					endofscan = true;
				}
			}
			else
			{
				static int nBadReadStatus = 0;
				++nBadReadStatus;
				if((nBadReadStatus & (nBadReadStatus - 1)) == 0)
				{
					cwarn << startl << "read returned status=" << status << " N=" << nBadReadStatus << endl;
				}
				if(nBadReadStatus > 1)
				{
					keepreading = false;
				}
			}
			if(endofscan || !keepreading)
			{
				break;
			}
		}
		pthread_mutex_unlock(shmthreadmutex + (readbufferwriteslot % lockmod));
		if(shmthreadstop)
		{
			break;
		}

		// No locks shall be set at this point
	} 
}

void *VDIFSharedMemoryDataStream::launchshmthreadfunction(void *self)
{
	VDIFSharedMemoryDataStream *me = (VDIFSharedMemoryDataStream *)self;

	me->shmthreadfunction();

	return 0;
}

void VDIFSharedMemoryDataStream::initialiseFile(int configindex, int fileindex)
{
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	int rv;
	int doUpdate = 0;
	int muxFlags;
	int shmKey;

	format = config->getDataFormat(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	inputframebytes = config->getFrameBytes(configindex, streamnum);
	framespersecond = config->getFramesPerSecond(configindex, streamnum)/config->getDNumMuxThreads(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);

	nGap = framespersecond/4;	// 1/4 second gap of data yields a mux break
	if(nGap > 1024)
	{
		nGap = 1024;
	}
	startOutputFrameNumber = -1;

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
	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, vm.outputFrameSize, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
        if(fanout != 1)
        {
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;

                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	cinfo << startl << "VDIFSharedMemoryDataStream::initialiseFile format=" << formatname << endl;

	shmKey = atoi(datafilenames[configindex][fileindex].c_str());

	shmbuffer = attachSharedMemoryBuffer(shmKey);

	if(shmbuffer == 0)
	{
		cfatal << startl << "Cannot attach to shared memory buffer with key " << shmKey << endl;
		
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	else
	{
		cinfo << startl << "Attached to shared memory buffer with key " << shmKey << endl;
	}

	rv = setsharedmemorybuffersecond(corrstartseconds);
	if(rv < 0)
	{
		cfatal << startl << "Quitting because shared memory is not in range" << endl;

                MPI_Abort(MPI_COMM_WORLD, 1);
	}


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

	lockstart = lockend = lastslot = -1;

	// cause network reading thread to go ahead and start filling buffers
	// these barriers come in pairs...
	pthread_barrier_wait(&shmthreadbarrier);
	pthread_barrier_wait(&shmthreadbarrier);
}

int VDIFSharedMemoryDataStream::dataRead(int buffersegment)
{
	// Note: here readbytes is actually the length of the buffer segment, i.e., the amount of data wanted to be "read" by calling processes. 
	// In this threaded approach the actual size of network reads (as implemented in the ring buffer writing thread) is generally larger.
	// This can account for some amount of missing data (perhaps lost in the ether or maybe just never produced)

	unsigned char *destination = reinterpret_cast<unsigned char *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);
	int n1, n2;	/* slot number range of data to be processed.  Either n1==n2 or n1+1==n2 */
	unsigned int muxend, bytesvisible;
	int lockmod = readbufferslots - 1;
	int muxReturn;
	int muxBits;

	if(samplingtype == Configuration::COMPLEX)
	{
		// muxing complex data is exactly the same as muxing real data, except the number of bits per sample needs to be doubled so we keep real and imaginary parts together
		muxBits = 2*nbits;
	}
	else
	{
		muxBits = nbits;
	}

	if(lockstart < -1)
	{
		csevere << startl << "dataRead lockstart=" << lockstart << " muxindex=" << muxindex << " readbufferslotsize=" << readbufferslotsize << " endindex=" << endindex << " lastslot=" << lastslot << endl;
	}

	if(lockstart == -1)
	{
		// first decoding of scan
		muxindex = readbufferslotsize;	// start at beginning of slot 1 (second slot)
		lockstart = lockend = 1;
		pthread_mutex_lock(shmthreadmutex + (lockstart % lockmod));
	}

	n1 = muxindex / readbufferslotsize;
	if(lastslot >= 0 && muxindex + readbytes > endindex && muxindex < endindex)
	{
		// here fewer than readbytes remain so make sure n2 gets set properly
		n2 = (endindex - 1) / readbufferslotsize;
	}
	else
	{
		n2 = (muxindex + readbytes - 1) / readbufferslotsize;
	}

	// note: it should be impossible for n2 >= readbufferslots because a previous memmove and slot shuffling should have prevented this.
	if(n2 >= readbufferslots)
	{
		csevere << startl << "dataRead n2=" << n2 << " >= readbufferslots=" << readbufferslots << " muxindex=" << muxindex << " readbufferslotsize=" << readbufferslotsize << " n1=" << n1 << " n2=" << n2 << " endindex=" << endindex << " lastslot=" << lastslot << endl;
	}

	if(n2 > n1 && lockend != n2)
	{
		lockend = n2;
		pthread_mutex_lock(shmthreadmutex + (lockend % lockmod));
	}
	
	if(lastslot == n2)
	{
		muxend = endindex;
	}
	else
	{
		muxend = (n2+1)*readbufferslotsize;
	}

	if(muxend <= muxindex)
	{
		csevere << startl << "Weird: muxend=" << muxend << " <= muxindex=" << muxindex << ": this should never be!  readbufferslots=" << readbufferslots << " readbufferslotsize=" << readbufferslotsize << " n1=" << n1 << " n2=" << n2 << " endindex=" << endindex << " lastslot=" << lastslot << endl;

		bufferinfo[buffersegment].validbytes = 0;
		bufferinfo[buffersegment].readto = true;

		// Note that this exit strategy likely will hang mpifxcorr

		dataremaining = false;

		return 0;
	}

	bytesvisible = muxend - muxindex;

//cverbose << "About to mux " << readbytes << " from slot(s) " << n1 << "-" << n2 << endl;

	// multiplex and corner turn the data
	muxReturn = vdifmux(destination, readbytes, readbuffer+muxindex, bytesvisible, &vm, startOutputFrameNumber, &vstats);

	if(muxReturn <= 0)
	{
		dataremaining = false;
		bufferinfo[buffersegment].validbytes = 0;
		readbufferleftover = 0;

		if(muxReturn < 0)
		{
			cerror << startl << "vdifmux() failed with return code " << muxReturn << ", likely input buffer is too small!" << endl;
		}
		else
		{
			cinfo << startl << "vdifmux returned no data.  Assuming end of file." << endl;
		}

		return 0;
	}

	bufferinfo[buffersegment].validbytes = vstats.destUsed;
	bufferinfo[buffersegment].readto = true;
	consumedbytes += vstats.srcUsed;
	if(bufferinfo[buffersegment].validbytes > 0)
	{
		// In the case of VDIF, we can get the time from the data, so use that just in case there was a jump
		bufferinfo[buffersegment].scanns = ((vstats.startFrameNumber % framespersecond) * 1000000000LL) / framespersecond;
		// FIXME: warning! here we are assuming no leap seconds since the epoch of the VDIF stream. FIXME
		// FIXME: below assumes each scan is < 86400 seconds long
		bufferinfo[buffersegment].scan = readscan;
		bufferinfo[buffersegment].scanseconds = (vstats.startFrameNumber / framespersecond)%86400 + intclockseconds - corrstartseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);

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

		// look at difference in data frames consumed and produced and proceed accordingly
		int deltaDataFrames = vstats.srcUsed/(nthreads*inputframebytes) - vstats.destUsed/(nthreads*(inputframebytes-VDIF_HEADER_BYTES) + VDIF_HEADER_BYTES);
		if(deltaDataFrames == 0)
		{
			// We should be able to preset startOutputFrameNumber.  Warning: early use of this was frought with peril but things seem OK now.
			startOutputFrameNumber = vstats.startFrameNumber + vstats.nOutputFrame;
		}
		else
		{
			if(deltaDataFrames < -10)
			{
				static int nGapWarn = 0;
				int nSkip;

				++nGapWarn;
				if( (nGapWarn & (nGapWarn - 1)) == 0 || nGapWarn <= 10)
				{
					cwarn << startl << "Data gap of " << (vstats.destUsed-vstats.srcUsed) << " bytes out of " << vstats.destUsed << " bytes found. startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << " deltaDataFrames=" << deltaDataFrames << " N=" << nGapWarn << endl;
				}

				nSkip = bytesvisible/2;
				nSkip -= (nSkip % inputframebytes);
				muxindex += nSkip;

				if(nGapWarn > 6)
				{
					dataremaining = false;
				}
			}
			else if(deltaDataFrames > 10)
			{
				static int nExcessWarn = 0;

				++nExcessWarn;
				if( (nExcessWarn & (nExcessWarn - 1)) == 0)
				{
					cwarn << startl << "Data excess of " << (vstats.srcUsed-vstats.destUsed) << " bytes out of " << vstats.destUsed << " bytes found. startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << " deltaDataFrames=" << deltaDataFrames << " N=" << nExcessWarn << endl;
					cwarn << startl << "readbufferslotsize=" << readbufferslotsize << " n1=" << n1 << " n2=" << n2 << endl;
				}
			}
			startOutputFrameNumber = -1;
		}
	}
	else
	{
		startOutputFrameNumber = -1;
	}

	muxindex += vstats.srcUsed;

	if(lastslot == n2 && (muxindex+minleftoverdata > endindex || bytesvisible < readbytes / 4) )
	{
		// end of useful data for this scan
		dataremaining = false;
		pthread_mutex_unlock(shmthreadmutex + (lockstart % lockmod));
		if(lockstart != lockend)
		{
			pthread_mutex_unlock(shmthreadmutex + (lockend % lockmod));
		}
		lockstart = lockend = -2;
	}
	else if(muxindex == readbufferslotsize*readbufferslots) // special case where the buffer was used up exactly
	{
		cinfo << startl << "In special case where full buffer was used up exactly" << endl;

		// start again at the beginning of slot 1
		muxindex = readbufferslotsize;

		// need to acquire lock for first slot
		pthread_mutex_lock(shmthreadmutex + 1);

		// unlock existing locks
		while(lockstart < readbufferslots)
		{
			pthread_mutex_unlock(shmthreadmutex + (lockstart % lockmod));
			++lockstart;
		}

		lockstart = 1;
		lockend = 1;
	}
	else
	{
		int n3;
		// note:  in all cases n2 = n1 or n1+1, n3 = n1 or n1+1 and n3 = n2 or n2+1
		// i.e., n3 >= n2 >= n1 and n3-n1 <= 1

		n3 = muxindex / readbufferslotsize;
		while(lockstart < n3)
		{
			pthread_mutex_unlock(shmthreadmutex + (lockstart % lockmod));
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

			int newstart = muxindex % readbufferslotsize;
			memmove(readbuffer + newstart, readbuffer + muxindex, readbuffersize-muxindex);
		cverbose << startl << "Did buffer memmove " << (readbuffersize-muxindex) << " bytes " << endl;
			muxindex = newstart;

			lockend = 0;
		}
	}

	return 0;
}

void VDIFSharedMemoryDataStream::loopfileread()
{
	int perr;
	int numread = 0;

	//lock the outstanding send lock
	perr = pthread_mutex_lock(&outstandingsendlock);
	if(perr != 0)
	{
		csevere << startl << "Error in initial readthread lock of outstandingsendlock!" << endl;
	}

	initialiseFile(bufferinfo[0].configindex, 0);

	dataremaining = true;

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
	else
	{
		cwarn << startl << "Couldn't find any valid data; will be shutting down gracefully!" << endl;
	}
	readthreadstarted = true;
	perr = pthread_cond_signal(&initcond);
	if(perr != 0)
	{
		csevere << startl << "Datastream readthread error trying to signal main thread to wake up!" << endl;
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
		if(!keepreading)
		{
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = config->getExecuteSeconds();
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
			cverbose << startl << "readscan == getNumScans -> keepreading = false" << endl;
		}
	}
	if(lockstart >= 0)
	{
		pthread_mutex_unlock(shmthreadmutex + (lockstart % (readbufferslots - 1)));
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

