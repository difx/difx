/***************************************************************************
 *   Copyright (C) 2007-2014 by Walter Brisken and Adam Deller             *
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
#include "vdifnetwork.h"
#include "alert.h"


VDIFNetworkDataStream::VDIFNetworkDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) :
	VDIFDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	int perr;

	/* each data buffer segment contains an integer number of frames, 
	 * because thats the way config determines max bytes
	 */

	jobEndMJD = conf->getStartMJD() + (conf->getStartSeconds() + conf->getExecuteSeconds() + 1)/86400.0;

	readbufferslots = 8;
	readbufferslotsize = (bufferfactor/numsegments)*conf->getMaxDataBytes(streamnum)*21LL/10LL;
	readbufferslotsize -= (readbufferslotsize % config->getFrameBytes(0, streamnum)); // make it a multiple of frame size
	readbuffersize = readbufferslots * readbufferslotsize;
	// Note: the read buffer is allocated in vdiffile.cpp by VDIFDataStream::initialse()
	// the above values override defaults for file-based VDIF

	cinfo << startl << "VDIFNetworkDataStream::VDIFNetworkDataStream: Set readbuffersize to " << readbuffersize << endl;
	cinfo << startl << "mdb = " << conf->getMaxDataBytes(streamnum) << "  rbslots=" << readbufferslots << "  readbufferslotsize=" << readbufferslotsize << endl;

	// set up network reader thread
	networkthreadstop = false;
	lockstart = lockend = lastslot = -2;
	endindex = 0;

	perr = pthread_barrier_init(&networkthreadbarrier, 0, 2);
	networkthreadmutex = new pthread_mutex_t[readbufferslots];
	for(int m = 0; m < readbufferslots; ++m)
	{
		if(perr == 0)
		{
			perr = pthread_mutex_init(networkthreadmutex + m, 0);
		}
	}

	if(perr == 0)
	{
		pthread_attr_t attr;
		pthread_attr_init(&attr);
		pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
		perr = pthread_create(&networkthread, &attr, VDIFNetworkDataStream::launchnetworkthreadfunction, this);
		pthread_attr_destroy(&attr);
	}

	if(perr)
	{
		cfatal << startl << "Cannot create the network reader thread!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
}

VDIFNetworkDataStream::~VDIFNetworkDataStream()
{
	networkthreadstop = true;

	/* barriers come in pairs to allow the read thread to always get first lock */
	pthread_barrier_wait(&networkthreadbarrier);
	pthread_barrier_wait(&networkthreadbarrier);

	pthread_join(networkthread, 0);

	for(int m = 0; m < readbufferslots; ++m)
	{
		pthread_mutex_destroy(networkthreadmutex + m);
	}
	delete [] networkthreadmutex;
	pthread_barrier_destroy(&networkthreadbarrier);
}

int VDIFNetworkDataStream::readrawnetworkVDIF(int sock, char* ptr, int bytestoread, unsigned int* nread, int packetsize, int stripbytes)
{
	const int MaxPacketSize = 20000;
	int length;
	int goodbytes = packetsize - stripbytes;
	char *ptr0 = ptr;
	char *end = ptr + bytestoread - goodbytes;
	char workbuffer[MaxPacketSize];
	const vdif_header *vh;

	vh = reinterpret_cast<const vdif_header *>(workbuffer + stripbytes);

	if(packetsize > MaxPacketSize)
	{
		cfatal << startl << "Error: readrawnetworkVDIF wants to read packets of size " << packetsize << " where MaxPacketSize=" << MaxPacketSize << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	*nread = 0;

	while(ptr <= end)
	{
		length = recvfrom(sock, workbuffer, MaxPacketSize, 0, 0, 0);
		if(length <= 0)
		{
			// timeout on read?
			break;
		}
		else if(length == packetsize)
		{
			memcpy(ptr, workbuffer + stripbytes, goodbytes);
			ptr += goodbytes;

			if( (vh->frame == 0) && (vh->threadid == 0) && (vh->seconds % 10 == 0) )
			{
				/* first frame of the 10 second interval.  Compare with local clock time for kicks */
				struct timespec ck;
				double deltat;		// [sec]

				clock_gettime(CLOCK_REALTIME, &ck);
				deltat = (ck.tv_sec % 10) - (vh->seconds % 10);
				deltat += ck.tv_nsec*1.0e-9;
				if(deltat < -3.0)
				{
					deltat += 10.0;
				}
				if(deltat > 5)
				{
					deltat -= 10.0;
				}
				int p = cinfo.precision();
				cinfo.precision(6);
				cinfo << startl << "VDIF clock is " << deltat << " seconds behind system clock for antenna " << stationname << endl;
				cinfo.precision(p);
			}
		}
	}

	if(nread)
	{
		*nread = ptr - ptr0;
	}

	return 1;
}

// this function implements the network reader.  It is continuously either filling data into a ring buffer or waiting for a mutex to clear.
void VDIFNetworkDataStream::networkthreadfunction()
{
	int lockmod = readbufferslots-1;	// used to team up slots 0 and readbufferslots-1
	int packetsize;				// for raw packets; reject all packets not this size
	int stripbytes;				// for raw packets; strip this many bytes from beginning of RX packets

	stripbytes = tcpwindowsizebytes/1024;
	packetsize = config->getFrameBytes(0, streamnum) + stripbytes;

	cinfo << startl << "stripbytes=" << stripbytes << " packetsize=" << packetsize << endl;

	for(;;)
	{
		// No locks shall be set at this point

		/* First barrier is before the locking of slot number 1 */
		pthread_barrier_wait(&networkthreadbarrier);

		readbufferwriteslot = 1;	// always 
		pthread_mutex_lock(networkthreadmutex + (readbufferwriteslot % lockmod));
		if(networkthreadstop)
		{
			cverbose << startl << "networkthreadfunction: networkthreadstop -> this thread will end." << endl;
		}
		/* Second barrier is after the locking of slot number 1 */
		pthread_barrier_wait(&networkthreadbarrier);

		while(!networkthreadstop)
		{
			unsigned int bytes;
			bool endofscan = false;
			int status;

			// This is where the actual read from the Mark5 unit happens
			if(ethernetdevice.empty())
			{
				// TCP or regular UDP
				status = readnetwork(socketnumber, (char *)(readbuffer + readbufferwriteslot*readbufferslotsize), readbufferslotsize, &bytes);
			}
			else
			{
				// Raw socket or trimmed UDP
				status = readrawnetworkVDIF(socketnumber, (char *)(readbuffer + readbufferwriteslot*readbufferslotsize), readbufferslotsize, &bytes, packetsize, stripbytes);
			}

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
				pthread_mutex_lock(networkthreadmutex + (readbufferwriteslot % lockmod));
				pthread_mutex_unlock(networkthreadmutex + (curslot % lockmod));

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
		pthread_mutex_unlock(networkthreadmutex + (readbufferwriteslot % lockmod));
		if(networkthreadstop)
		{
			break;
		}

		// No locks shall be set at this point
	} 
}

void *VDIFNetworkDataStream::launchnetworkthreadfunction(void *self)
{
	VDIFNetworkDataStream *me = (VDIFNetworkDataStream *)self;

	me->networkthreadfunction();

	return 0;
}

void VDIFNetworkDataStream::initialiseFile(int configindex, int fileindex)
{
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;

	double startmjd;
	int doUpdate = 0;

	format = config->getDataFormat(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	inputframebytes = config->getFrameBytes(configindex, streamnum);
	framespersecond = config->getFramesPerSecond(configindex, streamnum)/config->getDNumMuxThreads(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);

	nGap = framespersecond/4;	// 1/4 second gap of data yields a mux break
	startOutputFrameNumber = -1;

        outputframebytes = (inputframebytes-VDIF_HEADER_BYTES)*config->getDNumMuxThreads(configindex, streamnum) + VDIF_HEADER_BYTES;

	nthreads = config->getDNumMuxThreads(configindex, streamnum);
	threads = config->getDMuxThreadMap(configindex, streamnum);

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, outputframebytes, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
        if(fanout != 1)
        {
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;

                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	cinfo << startl << "VDIFNetworkDataStream::initialiseFile format=" << formatname << endl;

	startmjd = corrstartday + corrstartseconds/86400.0;

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
	pthread_barrier_wait(&networkthreadbarrier);
	pthread_barrier_wait(&networkthreadbarrier);
}

int VDIFNetworkDataStream::dataRead(int buffersegment)
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
		pthread_mutex_lock(networkthreadmutex + (lockstart % lockmod));
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
		pthread_mutex_lock(networkthreadmutex + (lockend % lockmod));
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
	muxReturn = vdifmux(destination, readbytes, readbuffer+muxindex, bytesvisible, inputframebytes, framespersecond, muxBits, nthreads, threads, nSort, nGap, startOutputFrameNumber, &vstats);
	if(muxReturn < 0)
	{
		cwarn << startl << "vdifmux returned " << muxReturn << endl;
	}


	{
		static int C = 0;

		if(C % 400 == 0)
		{
			cinfo << startl << "C=" << C << " VDIF multiplexing statistics: nValidFrame=" << vstats.nValidFrame << " nInvalidFrame=" << vstats.nInvalidFrame << " nDiscardedFrame=" << vstats.nDiscardedFrame << " nWrongThread=" << vstats.nWrongThread << " nSkippedByte=" << vstats.nSkippedByte << " nFillByte=" << vstats.nFillByte << " nDuplicateFrame=" << vstats.nDuplicateFrame << " bytesProcessed=" << vstats.bytesProcessed << " nGoodFrame=" << vstats.nGoodFrame << " nCall=" << vstats.nCall << " destUsed=" << vstats.destUsed << " srcUsed=" << vstats.srcUsed << endl;
		}
		++C;
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

				++nGapWarn;
				if( (nGapWarn & (nGapWarn - 1)) == 0)
				{
					cwarn << startl << "Data gap of " << (vstats.destUsed-vstats.srcUsed) << " bytes out of " << vstats.destUsed << " bytes found. startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << " deltaDataFrames=" << deltaDataFrames << " N=" << nGapWarn << endl;
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
		pthread_mutex_unlock(networkthreadmutex + (lockstart % lockmod));
		if(lockstart != lockend)
		{
			pthread_mutex_unlock(networkthreadmutex + (lockend % lockmod));
		}
		lockstart = lockend = -2;
	}
	else if(muxindex == readbufferslotsize*readbufferslots) // special case where the buffer was used up exactly
	{
		cinfo << startl << "In special case where full buffer was used up exactly" << endl;

		// start again at the beginning of slot 1
		muxindex = readbufferslotsize;

		// need to acquire lock for first slot
		pthread_mutex_lock(networkthreadmutex + 1);

		// unlock existing locks
		while(lockstart < readbufferslots)
		{
			pthread_mutex_unlock(networkthreadmutex + (lockstart % lockmod));
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
			pthread_mutex_unlock(networkthreadmutex + (lockstart % lockmod));
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
			muxindex = newstart;

			lockend = 0;
		}
	}

	return 0;
}

void VDIFNetworkDataStream::loopnetworkread()
{
	int perr;
	int numread = 0;

	//lock the outstanding send lock
	perr = pthread_mutex_lock(&outstandingsendlock);
	if(perr != 0)
	{
		csevere << startl << "Error in initial readthread lock of outstandingsendlock!" << endl;
	}

	if(ethernetdevice.empty())
	{
		openstream(portnumber, tcpwindowsizebytes/1024);
	}
	else
	{
		int v;
		
		v = openrawstream(ethernetdevice.c_str());
		if(v < 0)
		{
			cfatal << startl << "Cannot open raw socket.  Perhaps root permission is required." << endl;
			
			MPI_Abort(MPI_COMM_WORLD, 1);
		}
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
		pthread_mutex_unlock(networkthreadmutex + (lockstart % (readbufferslots - 1)));
	}
	if(numread > 0)
	{
		perr = pthread_mutex_unlock(&(bufferlock[lastvalidsegment]));
		if(perr != 0)
		{
			csevere << startl << "Error (" << perr << ") in readthread unlock of buffer section!" << lastvalidsegment << endl;
		}
	}

	closestream();

	//unlock the outstanding send lock
	perr = pthread_mutex_unlock(&outstandingsendlock);
	if(perr != 0)
	{
		csevere << startl << "Error (" << perr << ") in readthread unlock of outstandingsendlock!" << endl;
	}

	cverbose << startl << "Readthread is exiting; dataremaining=" << dataremaining << ", keepreading=" << keepreading << endl;
}

#ifdef __APPLE__
int pthread_barrier_init(pthread_barrier_t *barrier, const pthread_barrierattr_t *attr, unsigned int count)
{
    if(count == 0)
    {
        errno = EINVAL;
        return -1;
    }
    if(pthread_mutex_init(&barrier->mutex, 0) < 0)
    {
        return -1;
    }
    if(pthread_cond_init(&barrier->cond, 0) < 0)
    {
        pthread_mutex_destroy(&barrier->mutex);
        return -1;
    }
    barrier->tripCount = count;
    barrier->count = 0;

    return 0;
}

int pthread_barrier_destroy(pthread_barrier_t *barrier)
{
    pthread_cond_destroy(&barrier->cond);
    pthread_mutex_destroy(&barrier->mutex);
    return 0;
}

int pthread_barrier_wait(pthread_barrier_t *barrier)
{
    pthread_mutex_lock(&barrier->mutex);
    ++(barrier->count);
    if(barrier->count >= barrier->tripCount)
    {
        barrier->count = 0;
        pthread_cond_broadcast(&barrier->cond);
        pthread_mutex_unlock(&barrier->mutex);
        return 1;
    }
    else
    {
        pthread_cond_wait(&barrier->cond, &(barrier->mutex));
        pthread_mutex_unlock(&barrier->mutex);
        return 0;
    }
}

#endif
