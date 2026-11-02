/***************************************************************************
 *   Copyright (C) 2026 by Walter Brisken                                  *
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

#include <cstring>
#include <cstdlib>
#include <ctype.h>
#include <cmath>
#include <sys/time.h>
#include <mpi.h>
#include <unistd.h>
#include <vdifio.h>
#include "config.h"
#include "vdifonethreadfake.h"
#include "alert.h"


VDIFOneThreadFakeDataStream::VDIFOneThreadFakeDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	cinfo << startl << "VDIF one channel fake datastream starting" << endl;
}

VDIFOneThreadFakeDataStream::~VDIFOneThreadFakeDataStream()
{
}

/* Here "File" is ignored */
void VDIFOneThreadFakeDataStream::initialiseFile(int configindex, int fileindex)
{
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	double startmjd;
	int nbits;
	int nthreads;
	
	format = config->getDataFormat(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	inputframebytes = config->getFrameBytes(configindex, streamnum);
	framespersecond = config->getFramesPerSecond(configindex, streamnum)/config->getDNumMuxThreads(configindex, streamnum);
        bw = config->getDRecordedBandwidth(configindex, streamnum, 0);

	nthreads = config->getDNumMuxThreads(configindex, streamnum);
	if(nthreads != 1)
	{
		cfatal << startl << "VDIFOneThreadFakeDataStream::initialiseFile(): more than one thread found! = " << nthreads << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, inputframebytes, config->getDDecimationFactor(configindex, streamnum), config->getDAlignmentSeconds(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
        if(fanout != 1)
        {
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
                MPI_Abort(MPI_COMM_WORLD, 1);
        }

	cinfo << startl << "VDIFOneThreadFakeDataStream::initialiseFile format=" << formatname << endl;

	startmjd = corrstartday + corrstartseconds/86400.0;

	memset(&header, 0, sizeof(header));
	header.version = 1;
	header.threadid = 0;

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

void VDIFOneThreadFakeDataStream::openfile(int configindex, int fileindex)
{
	dataremaining = true;
	initialiseFile(configindex, fileindex);
}


// This function does the actual (fake) file IO and readbuffer management.
// The result after each call is readbytes of fake single-thread VDIF data being put into buffer segment.
int VDIFOneThreadFakeDataStream::dataRead(int buffersegment)
{
	int bytes;
	unsigned char *buf;

	buf = reinterpret_cast<unsigned char *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);

	bytes = 0;
	for(unsigned char *dest = buf; dest <= buf + readbytes - inputframebytes; )
	{
		memcpy(dest, &header, sizeof(header));
			
		dest += inputframebytes;
		nextVDIFHeader(&header, framespersecond);
		bytes += inputframebytes;
	}

	bufferinfo[buffersegment].validbytes = readbytes;
	bufferinfo[buffersegment].readto = true;

	/* FIXME: set scanns and scanseconds correctly */

	//bufferinfo[buffersegment].scanns = (((vstats.startFrameNumber) % framespersecond) * 1000000000LL) / framespersecond;

	//bufferinfo[buffersegment].scanseconds = ((vstats.startFrameNumber / framespersecond) % 86400) + intclockseconds - corrstartseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
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

	return bytes;
}

/* this function seems to need to be explicit per class */
void VDIFOneThreadFakeDataStream::loopfakeread()
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
