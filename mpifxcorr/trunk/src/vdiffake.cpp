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
#include "vdiffake.h"
#include "alert.h"


VDIFFakeDataStream::VDIFFakeDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		VDIFDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	cinfo << startl << "VDIF fake datastream starting" << endl;
}

VDIFFakeDataStream::~VDIFFakeDataStream()
{
}

/* Here "File" is ignored */
void VDIFFakeDataStream::initialiseFile(int configindex, int fileindex)
{
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	double startmjd;
	int rv;

	format = config->getDataFormat(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	inputframebytes = config->getFrameBytes(configindex, streamnum);
	framespersecond = config->getFramesPerSecond(configindex, streamnum)/config->getDNumMuxThreads(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);

	nthreads = config->getDNumMuxThreads(configindex, streamnum);
	threads = config->getDMuxThreadMap(configindex, streamnum);

	rv = configurevdifmux(&vm, inputframebytes, framespersecond, nbits, nthreads, threads, nSort, nGap, VDIF_MUX_FLAG_RESPECTGRANULARITY);
	if(rv < 0)
	{
		cfatal << startl << "configurevmux failed with return code " << rv << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, vm.outputFrameSize, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
        if(fanout != 1)
        {
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	cinfo << startl << "VDIFFakeDataStream::initialiseFile format=" << formatname << endl;

	startmjd = corrstartday + corrstartseconds/86400.0;

	memset(&header, 0, sizeof(header));
	header.version = 1;

	// set the epoch field based on the MJD
	setVDIFEpochMJD(&header, (int)startmjd);
	
	// set the seconds field
	header.seconds = (int)((startmjd-getVDIFEpochMJD(&header))*86400.0 + 0.5);
	
	setVDIFBitsPerSample(&header, nbits);
	setVDIFFrameBytes(&header, inputframebytes);
	setVDIFNumChannels(&header, 1);
	if(sampling == Configuration::COMPLEX)
	{
		header.iscomplex = 1;
	}

	readseconds = 0;
	readnanoseconds = 0;
}

void VDIFFakeDataStream::openfile(int configindex, int fileindex)
{
	dataremaining = true;
	initialiseFile(configindex, fileindex);
}


// This function does the actual (fake) file IO, readbuffer management, and VDIF multiplexing.  The result after each
// call is, hopefully, readbytes of multiplexed data being put into buffer segment with potentially some 
// read data left over in the read buffer ready for next time
int VDIFFakeDataStream::dataRead(int buffersegment)
{
	int bytes;
	int muxReturn;
	int muxBits;
	unsigned int bytesvisible;

	if(samplingtype == Configuration::COMPLEX)
	{
		// muxing complex data is exactly the same as muxing real data, except the number of bits per sample needs to be doubled so we keep real and imaginary parts together
		muxBits = 2*nbits;
	}
	else
	{
		muxBits = nbits;
	}

	bytes = 0;
	for(unsigned char *dest = readbuffer + readbufferleftover; dest <= readbuffer + readbuffersize - nthreads*inputframebytes; )
	{
		for(int t = 0; t < nthreads; ++t)
		{
			memcpy(dest, &header, sizeof(header));
			header.threadid = threads[t];
			
			dest += inputframebytes;
		}
		nextVDIFHeader(&header, framespersecond);
		bytes += nthreads*inputframebytes;
	}

	bytesvisible = readbufferleftover + bytes;

	// multiplex and corner turn the data
	muxReturn = vdifmux(reinterpret_cast<unsigned char *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]), readbytes, readbuffer, bytesvisible, &vm, startOutputFrameNumber, &vstats);

	if(muxReturn < 0)
	{
		cwarn << startl << "vdifmux returned " << muxReturn << endl;
	}

	consumedbytes += bytes;
	bufferinfo[buffersegment].validbytes = vstats.destUsed;
	bufferinfo[buffersegment].readto = true;
	if(bufferinfo[buffersegment].validbytes > 0)
	{
		// In the case of VDIF, we can get the time from the data, so use that just in case there was a jump
		bufferinfo[buffersegment].scanns = (((vstats.startFrameNumber) % framespersecond) * 1000000000LL) / framespersecond;
		// FIXME: below assumes each scan is < 86400 seconds long
		bufferinfo[buffersegment].scanseconds = ((vstats.startFrameNumber / framespersecond) % 86400) + intclockseconds - corrstartseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
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
					cwarn << startl << "Data gap of " << (vstats.destUsed-vstats.srcUsed) << " bytes out of " << vstats.destUsed << " bytes found. startOutputFrameNumber=" << startOutputFrameNumber << " bytesvisible=" << bytesvisible << " N=" << nGapWarn << endl;
				}
			}
			else if(deltaDataFrames > 10)
			{
				static int nExcessWarn = 0;

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
		startOutputFrameNumber = -1;
	}

	readbufferleftover += (bytes - vstats.srcUsed);

	if(readbufferleftover > 0)
	{
		memmove(readbuffer, readbuffer+vstats.srcUsed, readbufferleftover);
	}
	else if(readbufferleftover < 0)
	{
		cwarn << startl << "readbufferleftover = " << readbufferleftover << "; it should never be negative." << endl;

		readbufferleftover = 0;
	}
	if(readbufferleftover <= minleftoverdata && input.eof())
	{
		readbufferleftover = 0;

		// here we've in one call both read all the remaining data from a file and multiplexed it all without leftovers
		dataremaining = false;
	}

	return bytes;
}

/* this function seems to need to be explicit per class */
void VDIFFakeDataStream::loopfakeread()
{
	int perr;
	int numread = 0;

	//lock the outstanding send lock
	perr = pthread_mutex_lock(&outstandingsendlock);
	if(perr != 0)
	{
		csevere << startl << "Error in initial readthread lock of outstandingsendlock!" << endl;
	}

	//lock the first section to start reading
	dataremaining = false;
	keepreading = true;

	openfile(bufferinfo[0].configindex, filesread[bufferinfo[0].configindex]);

	if(keepreading)
	{
		diskToMemory(numread++);
		diskToMemory(numread++);
		lastvalidsegment = numread;
		perr = pthread_mutex_lock(&(bufferlock[numread]));
		if(perr != 0)
		{
			csevere << startl << "Error in initial readthread lock of first buffer section!" << endl;
		}
	}
	else
	{
		csevere << startl << "Couldn't find any valid data; will be shutting down gracefully!" << endl;
	}
	readthreadstarted = true;
	perr = pthread_cond_signal(&initcond);
	if(perr != 0)
	{
		csevere << startl << "Datastream readthread error trying to signal main thread to wake up!" << endl;
	}
	if(keepreading)
	{
		diskToMemory(numread++);
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

			if(!isnewfile) //can unlock previous section immediately
			{
				//unlock the previous section
				perr = pthread_mutex_unlock(&(bufferlock[(lastvalidsegment-1+numdatasegments)% numdatasegments]));    
				if(perr != 0)
				{
					csevere << startl << "Error (" << perr << ") in readthread unlock of buffer section!" << (lastvalidsegment-1+numdatasegments)%numdatasegments << endl;
				}
			}

			//do the read
			diskToMemory(lastvalidsegment);
			numread++;

			if(isnewfile) //had to wait before unlocking file
			{
				//unlock the previous section
				perr = pthread_mutex_unlock(&(bufferlock[(lastvalidsegment-1+numdatasegments)% numdatasegments]));
				if(perr != 0)
				{
					csevere << startl << "Error (" << perr << ") in readthread unlock of buffer section!" << (lastvalidsegment-1+numdatasegments)%numdatasegments << endl;
				}
			}
			isnewfile = false;
		}
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

	if(lastvalidsegment >= 0)
	{
		cverbose << startl << "Datastream readthread is exiting! Filecount was " << filesread[bufferinfo[lastvalidsegment].configindex] << ", confignumfiles was " << confignumfiles[bufferinfo[lastvalidsegment].configindex] << ", dataremaining was " << dataremaining << ", keepreading was " << keepreading << endl;
	}
	else
	{
		cverbose << startl << "Datastream readthread is exiting, after not finding any data at all!" << endl;
	}
}
