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
// $Id: vdiffile.cpp 6684 2015-06-02 11:40:23Z HelgeRottmann $
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mk5.cpp $
// $LastChangedRevision: 6684 $
// $Author: HelgeRottmann $
// $LastChangedDate: 2015-06-02 06:40:23 -0500 (Tue, 02 Jun 2015) $
//
//============================================================================
#include <cmath>
#include <cstring>
#include <mpi.h>
#include <iomanip>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include "config.h"
#include "alert.h"
#include <mark6sg/mark6gather.h>
#include <mark6gather_vdif.h>
#include "mode.h"
#include "vdifmark6_datastream.h"



/* TODO: 
   - make use of activesec and activescan
 */


/// VDIFMark6DataStream -------------------------------------------------------

VDIFMark6DataStream::VDIFMark6DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
 : VDIFDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	cinfo << startl << "Starting VDIF Mark6 datastream." << endl;
	mark6gather = 0;
}

VDIFMark6DataStream::~VDIFMark6DataStream()
{
	cinfo << startl << "Ending VDIF Mark6 datastream." << endl;
	closeMark6();
}

void VDIFMark6DataStream::closeMark6()
{
	if(mark6gather != 0)
	{
		sendMark6Activity(MARK6_STATE_CLOSE, bytecount, vdifmjd, mbyterate * 8.0);
		closeMark6Gatherer(mark6gather);
	}
	mark6gather = 0;
}

void VDIFMark6DataStream::openfile(int configindex, int fileindex)
{
	closeMark6();

	cinfo << startl << "Mark6 datastream " << mpiid << " is about to try and open file index " << fileindex << " of configindex " << configindex << endl;
	if(fileindex >= confignumfiles[configindex]) //run out of files - time to stop reading
	{
		dataremaining = false;
		keepreading = false;
		cinfo << startl << "Mark6 datastream " << mpiid << " is exiting because fileindex is " << fileindex << ", while confignumfiles is " << confignumfiles[configindex] << endl;
	
		return;
	}

	dataremaining = true;

	mark6gather = openMark6GathererFromTemplate(datafilenames[configindex][fileindex].c_str());

	cinfo << startl << "mark6gather is " << mark6gather << endl;
	if(mark6gather == 0)
	{
		cerror << startl << "Cannot open vdif mark6 data file " << datafilenames[configindex][fileindex] << endl;
		dataremaining = false;
	
		return;
	}

	if(isMark6GatherComplete(mark6gather) == 0)
	{
		cwarn << startl << "Warning: Mark6 file " << datafilenames[configindex][fileindex] << " seems to have an incomplete set of files.  Your weights may suffer if this is true." << endl;
	}

	if(datafilenames[configindex][fileindex][0] == '/')
	{
		cinfo << startl << "VDIF Mark6 datastream " << mpiid << " has opened file index " << fileindex << ", which was " << datafilenames[configindex][fileindex] << endl;
	}
	else
	{
		cinfo << startl << "VDIF Mark6 datastream " << mpiid << " has opened file index " << fileindex << ", which was " << datafilenames[configindex][fileindex] << " scattered over " << getMark6Root() << "/" << datafilenames[configindex][fileindex] << endl;
	}
	strcpy(mark6activity.scanName, datafilenames[configindex][fileindex].c_str());
	sendMark6Activity(MARK6_STATE_OPEN, 0, 0.0, 0.0);
	bytecount = 0;
	lastbytecount = 0;
	msgsenttime = time(0);

	// change state of module to played
	string cmd = "mk6state played " + datafilenames[configindex][fileindex];
	system(cmd.c_str());

	isnewfile = true;
	//read the header and set the appropriate times etc based on this information
	initialiseFile(configindex, fileindex);
}

void VDIFMark6DataStream::closefile()
{
	if(mark6gather != 0)
	{
		closeMark6();
	}
}

void VDIFMark6DataStream::startReaderThread()
{
	int perr;
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	/* get some things set up */
	readbufferwriteslot = 1;
	lockSlot(readbufferwriteslot, 2);

	perr = pthread_create(&readthread, &attr, VDIFMark6DataStream::launchreadthreadfunction, this);
	pthread_attr_destroy(&attr);

	if(perr)
	{
		cfatal << startl << "Cannot create the Mark6 reader thread!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	else
	{
		cinfo << startl << "VDIFMark6DataStream::startReaderThread() : starting VDIFMark6DataStream::launchreadthreadfunction ." << endl;
	}
}

void VDIFMark6DataStream::readthreadfunction()
{
	bool endofscan = false;

	// Lock for readbufferweriteslot=1 shall be set at this point by startReaderThread()
cinfo << startl << "Starting Mark6 read thread" << endl;

	while(keepreading && !endofscan)
	{
		int bytes, curslot;

		bytes = mark6Gather(mark6gather, reinterpret_cast<char *>(readbuffer) + readbufferwriteslot*readbufferslotsize, readbufferslotsize);

		if(bytes < readbufferslotsize)
		{
			lastslot = readbufferwriteslot;
			endindex = lastslot*readbufferslotsize + bytes;	// No data in this slot from here to end
			cinfo << startl << "At end of scan: shortening Mark6 read to only " << bytes << " bytes " << "(was " << readbufferslotsize << ")" << endl;
			endofscan = true;
		}

		if(bytes > 0)
		{
			time_t now;
			
			bytecount += bytes;
			now = time(0);
			if(msgsenttime < now)
			{
				long long bytediff;

				bytediff = bytecount - lastbytecount;
				mbyterate = (float)bytediff / 1000000.0;
				sendMark6Activity(MARK6_STATE_PLAY, bytecount, vdifmjd, mbyterate * 8.0);
				msgsenttime = now;
				lastbytecount = bytecount;
			}
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
void *VDIFMark6DataStream::launchreadthreadfunction(void *self)
{
	VDIFMark6DataStream *me = (VDIFMark6DataStream *)self;

	me->readthreadfunction();

	return 0;
}

void VDIFMark6DataStream::initialiseFile(int configindex, int fileindex)
{
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	int muxFlags;
	int rv;

	off_t dataoffset = 0;
	struct vdif_file_summary fileSummary;
	int currentdsseconds;

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

	// If verbose...
	//printvdifmux(&vm);

	/* Note: the following fanout concept is an explicit one and is not relevant to VDIF in any way */
	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, vm.outputFrameSize, config->getDDecimationFactor(configindex, streamnum), config->getDAlignmentSeconds(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
	if(fanout != 1)
	{
		cfatal << startl << "Classic fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	cinfo << startl << "VDIFMark6DataStream::initialiseFile format=" << formatname << endl;

	// Here we need to open the file, read the start time, jump if necessary, and if past end of file, dataremaining = false.  Then set readseconds...

	// First we get a description of the contents of the purported VDIF file and exit if it looks like not VDIF at all
	rv = summarizevdifmark6(&fileSummary, datafilenames[configindex][fileindex].c_str(), inputframebytes);
	if(rv < 0)
	{
		cwarn << startl << "VDIFMark6DataStream::initialiseFile: summary of file " << datafilenames[configindex][fileindex] << " resulted in error code " << rv << ".  This does not look like valid VDIF data." << endl;
		dataremaining = false;

		return;
	}

	// If verbose...
	//printvdiffilesummary(&fileSummary);

	// Here set readseconds to time since beginning of job
	readseconds = 86400*(vdiffilesummarygetstartmjd(&fileSummary)-corrstartday) + vdiffilesummarygetstartsecond(&fileSummary)-corrstartseconds + intclockseconds;
	if(fileSummary.framesPerSecond == 0)
	{
		fileSummary.framesPerSecond = framespersecond;
	}

	readnanoseconds = vdiffilesummarygetstartns(&fileSummary);
	currentdsseconds = activesec + model->getScanStartSec(activescan, config->getStartMJD(), config->getStartSeconds());

	if(currentdsseconds > readseconds+1)
	{
		int64_t jumpseconds;
		int64_t n, d;	// numerator and demoninator of frame/payload size ratio

		jumpseconds = currentdsseconds - readseconds;
		if(activens < readnanoseconds)
		{
			jumpseconds--;
		}

		// set byte offset to the requested time

		n = fileSummary.frameSize;
		d = fileSummary.frameSize - 32;

		dataoffset = static_cast<int64_t>(jumpseconds*vdiffilesummarygetbytespersecond(&fileSummary)/d) * n;

		readseconds += jumpseconds;
	}

	// Now set readseconds to time since beginning of scan
	readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
	
#if 0
	// Seeking seems to work on the Mark6 side, but causes very long delay in DiFX getting started.  Not sure why.

	// Advance into file if requested
	if(dataoffset > 0)
	{
		int rv;

		cinfo << startl << "Mark6 about to seek to byte " << dataoffset << " to get to the first wanted frame" << endl;
		rv = seekMark6Gather(mark6gather, dataoffset);
		cinfo << startl << "Mark6 seek finished with return value " << rv << endl;

		if(rv < 0)
		{
			cinfo << startl << "File " << datafilenames[configindex][fileindex] << " ended before the currently desired time" << endl;
			dataremaining = false;
			closefile();
		}
	}
#endif

	lockstart = lockend = lastslot = -1;

	// cause reading thread to go ahead and start filling buffers
	startReaderThread();
}

int VDIFMark6DataStream::sendMark6Activity(enum Mark6State state, long long position, double dataMJD, float rate)
{
	int v = 0;

	mark6activity.state = state;
	//mark6activity.status = 0;
	if(mark6gather)
	{
		strcpy(mark6activity.activeVsn, mark6gather->activeVSN);
	}
	else
	{
		return v;
	}
	mark6activity.position = position;
	mark6activity.rate = rate;
	mark6activity.dataMJD = dataMJD;
	mark6activity.scanNumber = 0; // always

	v = difxMessageSendMark6Activity(&mark6activity);

	return v;
}

