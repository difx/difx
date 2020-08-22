/***************************************************************************
 *   Copyright (C) 2006-2020 by Adam Deller and Walter Brisken             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mk5.cpp $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <cmath>
#include <cstring>
#include <iomanip>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <mpi.h>
#include "config.h"
#include "alert.h"
#include "vdiffile.h"
#include "mode.h"

// Uncomment below if the read buffer locks are to be debugged
// #define DEBUGLOCKS

/* TODO: 
   - make use of activesec and activescan
 */


/// VDIFDataStream -------------------------------------------------------

VDIFDataStream::VDIFDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
 : DataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	int perr;

	cinfo << startl << "Starting VDIF datastream." << endl;

	// switched power output assigned a name based on the datastream number (MPIID-1)
	int spf_Hz = conf->getDSwitchedPowerFrequency(id-1);
	if(spf_Hz > 0)
	{
		switchedpower = new SwitchedPower(conf, id);
		switchedpower->frequency = spf_Hz;
	}
	else
	{
		switchedpower = 0;
	}

	// Set some VDIF muxer parameters
	nSort = 32;	// allow data to be this many frames out of order without any loss at read boundaries
	nGap = 1000;	// a gap of this many frames will trigger an interruption of muxing
	startOutputFrameNumber = -1;
	memset(&vm, 0, sizeof(vm));

	nGapWarn = 0;
	nExcessWarn = 0;


	// Make read buffer a bit bigger than a data segment size so extra bytes can be filtered out 
	// The excess should be limited to avoid large memory moves of extra data
	// But the amount of excess should be large enough to encompass all reasonable amounts of interloper data
	// Here we give 20% overhead plus 8 MB, just to be on the safe side...

	readbufferslots = 8;

	readbufferslotsize = (bufferfactor/numsegments)*conf->getMaxDataBytes(streamnum)*21LL/10LL;
	readbufferslotsize -= (readbufferslotsize % conf->getFrameBytes(0, streamnum));	// always read in chunks of frame size
	readbuffersize = readbufferslots * readbufferslotsize;
	readbufferleftover = 0;
	readbuffer = new unsigned char[readbuffersize];

	estimatedbytes += readbuffersize;

	// Don't bother to do another read iteration just to salvage this many bytes at the end of a file/scan
	minleftoverdata = 20000;

	resetvdifmuxstatistics(&vstats);
	nthreads = 0; // no threads identified yet
	threads = 0;  // null pointer indicating not yet initialized
	invalidtime = 0;
	vdifmjd = 0.0;

	samplingtype = Configuration::REAL;
	filecheck = Configuration::getFileCheckLevel();
	if(filecheck == Configuration::FILECHECKUNKNOWN)
	{
		cwarn << startl << "env var DIFX_FILE_CHECK_LEVEL was set to " << getenv("DIFX_FILE_CHECK_LEVEL") << " which is not a legal value.  Assuming NONE." << endl;
		filecheck = Configuration::FILECHECKNONE;
	}

	jobEndMJD = conf->getStartMJD() + (conf->getStartSeconds() + conf->getExecuteSeconds() + 1)/86400.0;

	// Initialize some read thread related variables
	lockstart = lockend = lastslot = -2;
	lockmod = readbufferslots - 1;
	endindex = 0;

	readthreadmutex = new pthread_mutex_t[readbufferslots];
	slotMutexOwner = new int[readbufferslots];
	perr = 0;
	for(int m = 0; m < readbufferslots; ++m)
	{
		if(perr == 0)
		{
			perr = pthread_mutex_init(readthreadmutex + m, 0);
		}
		slotMutexOwner[m] = 0;
	}
	if(perr != 0)
	{
		cfatal << startl << "Cannot create the reader thread mutexes!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
}

VDIFDataStream::~VDIFDataStream()
{
	keepreading = false;	// probably this never needs to be made explicit

	for(int m = 0; m < readbufferslots; ++m)
	{
		pthread_mutex_destroy(readthreadmutex + m);
	}
	delete [] readthreadmutex;
	delete [] slotMutexOwner;

	cinfo << startl << "VDIF multiplexing statistics: nValidFrame=" << vstats.nValidFrame << " nInvalidFrame=" << vstats.nInvalidFrame << " nDiscardedFrame=" << vstats.nDiscardedFrame << " nWrongThread=" << vstats.nWrongThread << " nSkippedByte=" << vstats.nSkippedByte << " nFillByte=" << vstats.nFillByte << " nDuplicateFrame=" << vstats.nDuplicateFrame << " bytesProcessed=" << vstats.bytesProcessed << " nGoodFrame=" << vstats.nGoodFrame << " nCall=" << vstats.nCall << endl;
	if(vstats.nWrongThread > 0)
	{
		cerror << startl << "One or more wrong-threads were identified.  This may indicate a correlator configuration error." << endl;
	}
	if(vstats.nDuplicateFrame > 0)
	{
		cerror << startl << "One or more duplicate data frames (same time and thread) were found.  This may indicate serious problems with the digital back end configuration." << endl;
	}
	if(vstats.nFillByte > 3*vstats.bytesProcessed/4)
	{
		cerror << startl << "More than three quarters of the data from this station was unrecoverable (fill pattern)." << endl;
	}
	if(vstats.nFillByte > vstats.bytesProcessed/2)
	{
		cwarn << startl << "More than half of the data from this station was unrecoverable (fill pattern)." << endl;
	}
	if(vstats.nSkippedByte > vstats.bytesProcessed/20)
	{
		cwarn << startl << "More than 5 percent of data from this antenna were unwanted packets.  This could indicate a problem in the routing of data from the digital back end to the recorder." << endl;
	}

	//printvdifmuxstatistics(&vstats);
	if(switchedpower)
	{
		delete switchedpower;
	}
	if(readbuffer)
	{
		delete [] readbuffer;
	}
}

void VDIFDataStream::lockSlot(int slot, int processNum)
{
	int s = slot % lockmod;

#ifdef DEBUGLOCKS
	cinfo << startl << "lockSlot(" << slot << ", " << processNum << ") : lock status: ";
	for(int i = 0; i < readbufferslots; ++i)
	{
		cinfo << slotMutexOwner[i];
	}
	cinfo << endl;
#endif
	if(slot < 0 || slot >= readbufferslots)
	{
		csevere << startl << "lockSlot(" << slot << ", " << processNum << ") : slot out of bounds; lockstart=" << lockstart << " lockend=" << lockend << endl;
	}
	if(slotMutexOwner[s] == processNum)
	{
		csevere << startl << "lockSlot(" << slot << ", " << processNum << ") : slot already owned by " << slotMutexOwner[s] << "; lockstart=" << lockstart << " lockend=" << lockend << endl;
	}
#ifdef DEBUGLOCKS
	else if(slotMutexOwner[s] != 0)
	{
		cinfo << startl << "lockSlot(" << slot << ", " << processNum << ") : will wait for " << slotMutexOwner[s] << " to unlock" << endl;
	}
#endif

	pthread_mutex_lock(readthreadmutex + s);

	slotMutexOwner[s] = processNum;

}

void VDIFDataStream::unlockSlot(int slot, int processNum)
{
	int s = slot % lockmod;
#ifdef DEBUGLOCKS
	int nLeft = 0;

	cinfo << startl << "unlockSlot(" << slot << ", " << processNum << ") : lock status: ";
	for(int i = 0; i < readbufferslots; ++i)
	{
		cinfo << slotMutexOwner[i];
		if(slotMutexOwner[i] == processNum && i != s)
		{
			++nLeft;
		}
	}
	cinfo << endl;
	if(nLeft == 0)
	{
		csevere << startl << "unlockSlot(" << slot << ", " << processNum << ") : process unlocking last lock; lockstart=" << lockstart << " lockend=" << lockend << endl;
	}
#endif
	if(slot < 0 || slot >= readbufferslots)
	{
		csevere << startl << "unlockSlot(" << slot << ", " << processNum << ") : slot out of bounds; lockstart=" << lockstart << " lockend=" << lockend << endl;
	}
	if(slotMutexOwner[s] == 0)
	{
		csevere << startl << "unlockSlot(" << slot << ", " << processNum << ") : slot not locked; lockstart=" << lockstart << " lockend=" << lockend << endl;
	}
	else if(slotMutexOwner[s] != processNum)
	{
		csevere << startl << "unlockSlot(" << slot << ", " << processNum << ") : slot owned by " << slotMutexOwner[s] << "; lockstart=" << lockstart << " lockend=" << lockend << endl;
	}

	slotMutexOwner[s] = 0;

	pthread_mutex_unlock(readthreadmutex + s);
}

void VDIFDataStream::unlockAllSlots(int processNum)
{
#ifdef DEBUGLOCKS
	int n = 0;
#endif

	for(int i = 0; i < readbufferslots; ++i)
	{
		if(slotMutexOwner[i] == processNum)
		{
			slotMutexOwner[i] = 0;
			pthread_mutex_unlock(readthreadmutex + i);
#ifdef DEBUGLOCKS
			++n;
#endif
		}
	}
#ifdef DEBUGLOCKS
	cinfo << startl << "unlockAllSlots(" << processNum << ") : " << n << " slots unlocked" << endl;
#endif
}

// this function needs to be rewritten for subclasses.
void VDIFDataStream::startReaderThread()
{
	int perr;
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	/* get some things set up */
	readbufferwriteslot = 1;
	lockSlot(readbufferwriteslot, 2);

	perr = pthread_create(&readthread, &attr, VDIFDataStream::launchreadthreadfunction, this);
	pthread_attr_destroy(&attr);

	if(perr)
	{
		cfatal << startl << "Cannot create the file reader thread!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	else
	{
		cinfo << startl << "VDIFDataStream::startReaderThread() : starting VDIFDataStream::launchreadthreadfunction ." << endl;
	}
}

// This function is launched once per scan to read data and fill the read buffer.  Reading always begins into slot 1.
void VDIFDataStream::readthreadfunction()
{
	bool endofscan = false;

	// Lock for readbufferweriteslot=1 shall be set at this point by startReaderThread()

	while(keepreading && !endofscan)
	{
		int bytes, curslot;

		if(input.eof())
		{
			bytes = 0;
		}
		else
		{
			input.read(reinterpret_cast<char *>(readbuffer) + readbufferwriteslot*readbufferslotsize, readbufferslotsize);
			bytes = input.gcount();
		}

		if(bytes < readbufferslotsize)
		{
			lastslot = readbufferwriteslot;
			endindex = lastslot*readbufferslotsize + bytes; // No data in this slot from here to end
			cverbose << startl << "At end of scan: shortening read to only " << bytes << " bytes " << "(was " << readbufferslotsize << ")" << endl;
			endofscan = true;
		}
		
		curslot = readbufferwriteslot;

		++readbufferwriteslot;
		if(readbufferwriteslot >= readbufferslots)
		{
			// Note: we always save slot 0 for wrap-around
			readbufferwriteslot = 1;
		}
		lockSlot(readbufferwriteslot, 2);
		unlockSlot(curslot, 2);
	}
	unlockAllSlots(2);
	
	// No locks shall be set at this point
}

// this function needs to be rewritten for subclasses.
void *VDIFDataStream::launchreadthreadfunction(void *self)
{
	VDIFDataStream *me = (VDIFDataStream *)self;
cinfo << startl << "VDIFDataStream::launchreadthreadfunction" << endl;

	me->readthreadfunction();

	return 0;
}

int VDIFDataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
	int bufferindex, framesin, vlbaoffset, looksegment, payloadbytes, framespersecond, framebytes;
	float datarate;

	bufferindex = DataStream::calculateControlParams(scan, offsetsec, offsetns);

	if(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] == Mode::INVALID_SUBINT)
	{
		return 0;
	}

	looksegment = atsegment;
	if(bufferinfo[atsegment].configindex < 0) //will get garbage using this to set framebytes etc
	{
		//look at the following segment - normally has sensible info
		looksegment = (atsegment+1)%numdatasegments;
		if(bufferinfo[atsegment].nsinc != bufferinfo[looksegment].nsinc)
		{
			cwarn << startl << "Incorrectly set config index at scan boundary! Flagging this subint" << endl;
			bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
	
			return bufferindex;
		}
	}
	if(bufferinfo[looksegment].configindex < 0)
	{
		//Sometimes the next segment is still showing invalid due to the geometric delay.
		//try the following segment - if thats no good, get out
		//this is not entirely safe since the read thread may not have set the configindex yet, but at worst
		//one subint will be affected
		looksegment = (looksegment+1)%numdatasegments;
		if(bufferinfo[looksegment].configindex < 0)
		{
			cwarn << startl << "Cannot find a valid configindex to set Mk5-related info.  Flagging this subint" << endl;
			bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;

			return bufferindex;
		}
		if(bufferinfo[atsegment].nsinc != bufferinfo[looksegment].nsinc)
		{
			cwarn << startl << "Incorrectly set config index at scan boundary! Flagging this subint" << endl;
			bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;

			return bufferindex;
		}
	}

	//if we got here, we found a configindex we are happy with.  Find out the mk5 details
	payloadbytes = config->getFramePayloadBytes(bufferinfo[looksegment].configindex, streamnum);
	framebytes = config->getFrameBytes(bufferinfo[looksegment].configindex, streamnum);
	framespersecond = config->getFramesPerSecond(bufferinfo[looksegment].configindex, streamnum);
	payloadbytes *= config->getDNumMuxThreads(bufferinfo[looksegment].configindex, streamnum);
	framebytes = (framebytes-VDIF_HEADER_BYTES)*config->getDNumMuxThreads(bufferinfo[looksegment].configindex, streamnum) + VDIF_HEADER_BYTES;
	framespersecond /= config->getDNumMuxThreads(bufferinfo[looksegment].configindex, streamnum);

	samplingtype = config->getDSampling(bufferinfo[looksegment].configindex, streamnum);

	//set the fraction of data to use to determine system temperature based on data rate
	//the values set here work well for the today's computers and clusters...
	datarate = static_cast<float>(framebytes)*static_cast<float>(framespersecond)*8.0/1.0e6;  // in Mbps
	if(datarate < 512)
	{
		switchedpowerincrement = 1;
	}
	else
	{
		switchedpowerincrement = static_cast<int>(datarate/512 + 0.1);
	}

	//do the necessary correction to start from a frame boundary; work out the offset from the start of this segment
	vlbaoffset = bufferindex - atsegment*readbytes;

	if(vlbaoffset < 0)
	{
		cfatal << startl << "VDIFDataStream::calculateControlParams: vlbaoffset<0: vlbaoffset=" << vlbaoffset << " bufferindex=" << bufferindex << " atsegment=" << atsegment << " readbytes=" << readbytes << ", framebytes=" << framebytes << ", payloadbytes=" << payloadbytes << endl;
		bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
		// WFB20120123 MPI_Abort(MPI_COMM_WORLD, 1);
		
		return 0;
	}

	// bufferindex was previously computed assuming no framing overhead
	framesin = vlbaoffset/payloadbytes;

	// here we enforce frame granularity.  We simply back up to the previous frame that is a multiple of the frame granularity.
	if(framesin % vm.frameGranularity != 0)
	{
		framesin -= (framesin % vm.frameGranularity);
	}

	// Note here a time is needed, so we only count payloadbytes
	long long segoffns = bufferinfo[atsegment].scanns + (long long)((1000000000.0*framesin)/framespersecond);
	bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = bufferinfo[atsegment].scanseconds + ((int)(segoffns/1000000000));
	bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][2] = ((int)(segoffns%1000000000));

	//go back to nearest frame -- here the total number of bytes matters
	bufferindex = atsegment*readbytes + framesin*framebytes;

	//if we are right at the end of the last segment, and there is a jump after this segment, bail out
	if(bufferindex == bufferbytes)
	{
		if(bufferinfo[atsegment].scan != bufferinfo[(atsegment+1)%numdatasegments].scan ||
		   ((bufferinfo[(atsegment+1)%numdatasegments].scanseconds - bufferinfo[atsegment].scanseconds)*1000000000 +
		   bufferinfo[(atsegment+1)%numdatasegments].scanns - bufferinfo[atsegment].scanns - bufferinfo[atsegment].nsinc != 0))
		{
			bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
			
			return 0; //note exit here!!
		}
		else
		{
			cwarn << startl << "Developer error mk5: bufferindex == bufferbytes in a 'normal' situation" << endl;
		}
	}

	if(bufferindex > bufferbytes) /* WFB: this was >= */
	{
		cfatal << startl << "VDIFDataStream::calculateControlParams: bufferindex>=bufferbytes: bufferindex=" << bufferindex << " >= bufferbytes=" << bufferbytes << " atsegment = " << atsegment << endl;
		bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
		MPI_Abort(MPI_COMM_WORLD, 1);

		return 0;
	}

	return bufferindex;
}

void VDIFDataStream::updateConfig(int segmentindex)
{
	//run the default update config, then add additional information specific to Mk5
	DataStream::updateConfig(segmentindex);
	if(bufferinfo[segmentindex].configindex < 0) //If the config < 0 we can skip this scan
	{
		return;
	}

	int oframebytes = (config->getFrameBytes(bufferinfo[segmentindex].configindex, streamnum) - VDIF_HEADER_BYTES)*config->getDNumMuxThreads(bufferinfo[segmentindex].configindex, streamnum) + VDIF_HEADER_BYTES;
	int oframespersecond = config->getFramesPerSecond(bufferinfo[segmentindex].configindex, streamnum) / config->getDNumMuxThreads(bufferinfo[segmentindex].configindex, streamnum);

	//correct the nsinc - should be number of output frames*frame time
	bufferinfo[segmentindex].nsinc = int(((bufferbytes/numdatasegments)/oframebytes)*(1000000000.0/double(oframespersecond)) + 0.5);

	//take care of the case where an integral number of frames is not an integral number of blockspersend - ensure sendbytes is long enough
	//note below, the math should produce a pure integer, but add 0.5 to make sure that the fuzziness of floats doesn't cause an off-by-one error
	bufferinfo[segmentindex].sendbytes = int(((((double)bufferinfo[segmentindex].sendbytes)* ((double)config->getSubintNS(bufferinfo[segmentindex].configindex)))/(config->getSubintNS(bufferinfo[segmentindex].configindex) + config->getGuardNS(bufferinfo[segmentindex].configindex)) + 0.5));
}

void VDIFDataStream::initialiseFile(int configindex, int fileindex)
{
	const int MaxSummaryLength = 256;
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	int muxFlags;
	int rv;

	long long dataoffset = 0;
	struct vdif_file_summary fileSummary;
	char fileSummaryString[MaxSummaryLength];
	int jumpseconds, currentdsseconds;

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

	if(nrecordedbands > nthreads)
	{
		int nBandPerThread = nrecordedbands/nthreads;

		cinfo << startl << "Note: " << nBandPerThread << " recoded channels (bands) reside on each thread.  Support for this is new.  Congratulations for being bold and trying this out!  Warranty void in the 193 UN recognized nations." << endl;
		
		if(nBandPerThread * nthreads != nrecordedbands)
		{
			cerror << startl << "Error: " << nrecordedbands << " recorded channels (bands) were recorded but they are divided unequally across " << nthreads << " threads.  This is not allowed.  Things will probably get very bad soon..." << endl;
		}
		setvdifmuxinputchannels(&vm, nBandPerThread);
		vdiffilesummarysetsamplerate(&fileSummary, static_cast<int64_t>(bw*2000000LL*nBandPerThread));
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
		vdiffilesummarysetsamplerate(&fileSummary, static_cast<int64_t>(bw*2000000LL/nThreadPerBand));
	}

	/* Note: the following fanout concept is an explicit one and is not relevant to VDIF in any way */

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, vm.outputFrameSize, config->getDDecimationFactor(configindex, streamnum), config->getDAlignmentSeconds(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
	if(fanout != 1)
	{
		cfatal << startl << "Classic fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	cinfo << startl << "VDIFDataStream::initialiseFile post-vmux format=" << formatname << endl;

	// Here we need to open the file, read the start time, jump if necessary, and if past end of file, dataremaining = false.  Then set readseconds...

	if(filecheck == Configuration::FILECHECKSEEK)
	{
		// First we get a description of the contents of the purported VDIF file and exit if it looks like not VDIF at all
		rv = summarizevdiffile(&fileSummary, datafilenames[configindex][fileindex].c_str(), inputframebytes);
		if(rv < 0)
		{
			cwarn << startl << "VDIFDataStream::initialiseFile: summary of file " << datafilenames[configindex][fileindex] << " resulted in error code " << rv << ".  This does not look like valid VDIF data." << endl;
			dataremaining = false;

			return;
		}

		// Put file information into log stream
		vdiffilesummarysetsamplerate(&fileSummary, static_cast<int64_t>(bw*2000000LL*nrecordedbands/nthreads));
		snprintvdiffilesummary(fileSummaryString, MaxSummaryLength, &fileSummary);
		cinfo << startl << fileSummaryString << endl;

		// If verbose...
		printvdiffilesummary(&fileSummary);

		// Here set readseconds to time since beginning of job
		readseconds = 86400*(vdiffilesummarygetstartmjd(&fileSummary)-corrstartday) + vdiffilesummarygetstartsecond(&fileSummary)-corrstartseconds + intclockseconds;
		readnanoseconds = vdiffilesummarygetstartns(&fileSummary);
		currentdsseconds = activesec + model->getScanStartSec(activescan, config->getStartMJD(), config->getStartSeconds());

		if(currentdsseconds > readseconds+1)
		{
			jumpseconds = currentdsseconds - readseconds;
			if(activens < readnanoseconds)
			{
				jumpseconds--;
			}

			// set byte offset to the requested time

			int n, d;	// numerator and demoninator of frame/payload size ratio
			n = fileSummary.frameSize;
			d = fileSummary.frameSize - 32;

			dataoffset = static_cast<long long>(jumpseconds*vdiffilesummarygetbytespersecond(&fileSummary)/d*n + 0.5);

			readseconds += jumpseconds;
		}

		// Now set readseconds to time since beginning of scan
		readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
		
		// Advance into file if requested
		if(fileSummary.firstFrameOffset + dataoffset > 0)
		{
			cverbose << startl << "About to seek to byte " << fileSummary.firstFrameOffset << " plus jump " << dataoffset << " to get to the first wanted frame" << endl;

			input.seekg(fileSummary.firstFrameOffset + dataoffset, ios_base::beg);
			if(input.peek() == EOF)
			{
				cinfo << startl << "File " << datafilenames[configindex][fileindex] << " ended before the currently desired time" << endl;
				dataremaining = false;
				input.clear();
			}
		}
	}
	else
	{
		cverbose << startl << "Not doing peek/seek on file due to setting of DIFX_FILE_CHECK_LEVEL env var." << endl;
	}

	lockstart = lockend = lastslot = -1;

	// cause reading thread to go ahead and start filling buffers
	startReaderThread();
}

int VDIFDataStream::testForSync(int configindex, int buffersegment)
{
	// not needed.  vdifmux always leaves perfectly synchonized data behind
	return 0;
}

// This function does the actual file IO, readbuffer management, and VDIF multiplexing.  The result after each
// call is, hopefully, readbytes of multiplexed data being put into buffer segment with potentially some 
// read data left over in the read buffer ready for next time
int VDIFDataStream::dataRead(int buffersegment)
{
	// Note: here readbytes is actually the length of the buffer segment, i.e., the amount of data wanted to be "read" by calling processes. 
	// In this threaded approach the actual size of reads off files, Mark5/Mark6 modules (as implemented in readthreadfunction() ) is generally larger.

	unsigned char *destination = reinterpret_cast<unsigned char *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);
	int n1, n2;	/* slot number range of data to be processed.  Either n1==n2 or n1+1==n2 */
	unsigned int muxend;
	int bytesvisible;
	int muxReturn;

	if(lockstart < -1)
	{
		csevere << startl << "dataRead lockstart=" << lockstart << " muxindex=" << muxindex << " readbufferslotsize=" << readbufferslotsize << " endindex=" << endindex << " lastslot=" << lastslot << endl;
	}

	if(lockstart == -1)
	{
		// first decoding of scan
		muxindex = readbufferslotsize;	// start at beginning of slot 1 (second slot)
		lockstart = lockend = 1;
		lockSlot(lockstart);
	}

	n1 = muxindex / readbufferslotsize;
	if(lastslot >= 0 && muxindex + readbytes > endindex && muxindex < endindex)
	{
		// here fewer than readbytes remain so make sure n2 gets set properly
		n2 = (endindex - 1) / readbufferslotsize;
	}
	else
	{
		n2 = (muxindex + readbytes*2 - 1) / readbufferslotsize;
	}

	// note: it should be impossible for n2 >= readbufferslots because a previous memmove and slot shuffling should have prevented this.
	if(n2 >= readbufferslots)
	{
		csevere << startl << "dataRead n2=" << n2 << " >= readbufferslots=" << readbufferslots << " muxindex=" << muxindex << " readbufferslotsize=" << readbufferslotsize << " n1=" << n1 << " n2=" << n2 << " endindex=" << endindex << " lastslot=" << lastslot << endl;
	}

	while(lockend < n2)
	{
		++lockend;
		lockSlot(lockend);
	}
	
	// muxend contains the last valid buffer read index (minus 1)
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

	// multiplex and corner turn the data
	muxReturn = vdifmux(destination, readbytes, readbuffer+muxindex, bytesvisible, &vm, startOutputFrameNumber, &vstats);

	if(muxReturn <= 0)
	{
		dataremaining = false;
		bufferinfo[buffersegment].validbytes = 0;
		readbufferleftover = 0;

		if(muxReturn < 0)
		{
			unlockSlot(lockend);
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
		vdifmjd = getVDIFFrameDMJD((const vdif_header *)(readbuffer+muxindex), vm.inputFramesPerSecond);

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
		if(abs(deltaDataFrames) < vm.nSort)
		{
			// We should be able to preset startOutputFrameNumber.  Warning: early use of this was frought with peril but things seem OK now.
			startOutputFrameNumber = vstats.startFrameNumber + vstats.nOutputFrame;
		}
		else
		{
			if(deltaDataFrames < -(vm.nSort+10))
			{
				++nGapWarn;
				if( (nGapWarn & (nGapWarn - 1)) == 0)
				{
					cwarn << startl << "Data gap of " << (vstats.destUsed-vstats.srcUsed) << " bytes out of " << vstats.destUsed << " bytes found. startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << " N=" << nGapWarn << " deltaDataFrames=" << deltaDataFrames << endl;
				}
			}
			else if(deltaDataFrames > (vm.nSort+10))
			{
				++nExcessWarn;
				if( (nExcessWarn & (nExcessWarn - 1)) == 0)
				{
					cwarn << startl << "Data excess of " << (vstats.srcUsed-vstats.destUsed) << " bytes out of " << vstats.destUsed << " bytes found. startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << " N=" << nExcessWarn << endl;
				}
			}
			startOutputFrameNumber = -1;
		}
	}
	else
	{
		cwarn << startl << "validbytes == 0" << endl;
		startOutputFrameNumber = -1;
	}

	muxindex += vstats.srcUsed;

	if(lastslot == n2 && (muxindex+minleftoverdata > endindex || bytesvisible < readbytes / 4) )
	{
		// end of useful data for this scan
		cinfo << startl << "End of data for scan; bytesProcessed=" << vstats.bytesProcessed << " nGoodFrame=" << vstats.nGoodFrame << " nCall=" << vstats.nCall << endl;
		dataremaining = false;
		unlockAllSlots();
		lockstart = lockend = -2;
	}
	else if(muxindex == readbufferslotsize*readbufferslots) // special case where the buffer was used up exactly
	{
		cinfo << startl << "In special case where full buffer was used up exactly" << endl;

		// start again at the beginning of slot 1
		muxindex = readbufferslotsize;

		// need to acquire lock for first slot
		lockSlot(1);

		// unlock existing locks
		while(lockstart < readbufferslots)
		{
			unlockSlot(lockstart);
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
		while(lockstart < n3 && lockstart < lockend)
		{
			unlockSlot(lockstart);
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

void VDIFDataStream::diskToMemory(int buffersegment)
{
	u32 *buf;

	buf = reinterpret_cast<u32 *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);

	//do the buffer housekeeping
	waitForBuffer(buffersegment);

	// This function call abstracts away all the details.  The result is multiplexed data populating the 
	// desired buffer segment.
	dataRead(buffersegment);

	// Update estimated read timing variables
	readnanoseconds += (bufferinfo[buffersegment].nsinc % 1000000000);
	readseconds += (bufferinfo[buffersegment].nsinc / 1000000000);
	readseconds += readnanoseconds/1000000000;
	readnanoseconds %= 1000000000;

	if(vstats.destUsed == 0)
	{
		++invalidtime;
	}
	else
	{
		invalidtime = 0;
	}

	// did we just come to the end of job execution time?
	if(readseconds + model->getScanStartSec(readscan, corrstartday, corrstartseconds) >= config->getExecuteSeconds())
	{
		keepreading = false;
		dataremaining = false;
		cinfo << startl << "diskToMemory: end of executeseconds reached.  stopping." << endl;

		if(lockstart >= 0)
		{
			unlockAllSlots();
			pthread_join(readthread, 0);
			lockstart = lockend = -2;
		}
	}

	// did we just cross into next scan?
	if(readseconds >= model->getScanDuration(readscan) && keepreading)
	{
		cinfo << startl << "diskToMemory: end of schedule scan " << readscan << " of " << model->getNumScans() << " detected" << endl;

		// find next valid schedule scan
		do
		{
			++readscan;
		} while(readscan < model->getNumScans() && config->getScanConfigIndex(readscan));

		if(readscan < model->getNumScans())
		{
			//if we need to, change the config
			if(config->getScanConfigIndex(readscan) != bufferinfo[(lastvalidsegment + 1)%numdatasegments].configindex)
			{
				updateConfig((lastvalidsegment + 1)%numdatasegments);
			}
			cinfo << startl << "diskToMemory: starting schedule scan " << readscan << endl;
		}
		else
		{
			// here we just crossed over the end of the job
			cverbose << startl << "readscan==getNumScans -> keepreading=false" << endl;
			
			keepreading = false;
			
			if(lockstart >= 0)
			{
				unlockAllSlots();
				pthread_join(readthread, 0);
				lockstart = lockend = -2;
			}

			bufferinfo[(lastvalidsegment+1)%numdatasegments].scan = model->getNumScans()-1;
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = model->getScanDuration(model->getNumScans()-1);
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
			cinfo << startl << "diskToMemory: no more scans" << endl;
		}
	}

	if(switchedpower && bufferinfo[buffersegment].validbytes > 0)
	{
		static int nt = 0;

		++nt;

                // feed switched power detector
		if(nt % switchedpowerincrement == 0)
		{
			struct mark5_stream *m5stream = new_mark5_stream_absorb(
				new_mark5_stream_memory(buf, bufferinfo[buffersegment].validbytes),
				new_mark5_format_generic_from_string(formatname) );
			if(m5stream)
			{
				mark5_stream_fix_mjd(m5stream, config->getStartMJD());
				switchedpower->feed(m5stream);
				delete_mark5_stream(m5stream);
			}
                }
	}
}
