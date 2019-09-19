/***************************************************************************
 *   Copyright (C) 2006-2018 by Adam Deller and Walter Brisken             *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
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
#include <mark5access/mark5bfile.h>
#include <mark5access/mark6gather_mark5b.h>
#include "config.h"
#include "alert.h"
#include "mode.h"
#include "mark5bmark6_datastream.h"



/* TODO: 
   - make use of activesec and activescan
 */


/// Mark5BMark6DataStream -------------------------------------------------------

Mark5BMark6DataStream::Mark5BMark6DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
 : Mark5BDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	cinfo << startl << "Starting Mark5B Mark6 datastream." << endl;
	mark6gather = 0;
	mark6eof = false;
}

Mark5BMark6DataStream::~Mark5BMark6DataStream()
{
	cinfo << startl << "Ending Mark5b Mark6 datastream." << endl;
	closeMark6();
}

void Mark5BMark6DataStream::closeMark6()
{
	if(mark6gather != 0)
	{
		sendMark6Activity(MARK6_STATE_CLOSE, bytecount, fmjd, mbyterate * 8.0);
		closeMark6Gatherer(mark6gather);
	}
	mark6gather = 0;
	mark6eof = false;
}

void Mark5BMark6DataStream::openfile(int configindex, int fileindex)
{
  closeMark6();

  cverbose << startl << "Mark5b Mark6 datastream " << mpiid << " is about to try and open file index " << fileindex << " of configindex " << configindex << endl;
  if(fileindex >= confignumfiles[configindex]) //run out of files - time to stop reading
  {
    dataremaining = false;
    keepreading = false;
    cinfo << startl << "Mark5b Mark6 datastream " << mpiid << " is exiting because fileindex is " << fileindex << ", while confignumfiles is " << confignumfiles[configindex] << endl;
    return;
  }
  
  dataremaining = true;

  mark6gather = openMark6GathererFromTemplate(datafilenames[configindex][fileindex].c_str());

  cverbose << startl << "mark6gather is " << mark6gather << endl;
  if(mark6gather == 0)
  {
    cerror << startl << "Cannot open mark5b mark6 data file " << datafilenames[configindex][fileindex] << endl;
    dataremaining = false;
    return;
  }

  if(isMark6GatherComplete(mark6gather) == 0)
  {
    cwarn << startl << "Warning: Mark6 file " << datafilenames[configindex][fileindex] << " seems to have an incomplete set of files.  Your weights may suffer if this is true." << endl;
  }

  cinfo << startl << "Mark5b Mark6 datastream " << mpiid << " has opened file index " << fileindex << ", which was " << datafilenames[configindex][fileindex] << endl;
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

void Mark5BMark6DataStream::initialiseFile(int configindex, int fileindex)
{
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	int rv;

	long long dataoffset = 0;
	struct mark5b_file_summary fileSummary;
	int jumpseconds, currentdsseconds;

	format = config->getDataFormat(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);	/* Bits per sample.  If complex, bits per component. */
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

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, framebytes, config->getDDecimationFactor(configindex, streamnum), 1, config->getDNumMuxThreads(configindex, streamnum), formatname);
	if(fanout != 1)
	{
		cfatal << startl << "Classic fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	cinfo << startl << "Mark5BMark6DataStream::initialiseFile format=" << formatname << endl;

	// Here we need to open the file, read the start time, jump if necessary, and if past end of file, dataremaining = false.  Then set readseconds...

	// First we get a description of the contents of the purported Mark5B file and exit if it looks like not Mark5B at all
	rv = summarizemark5bmark6(&fileSummary, datafilenames[configindex][fileindex].c_str());
	if(rv < 0)
	{
		cwarn << startl << "Mark5BMark6DataStream::initialiseFile: summary of file " << datafilenames[configindex][fileindex] << " resulted in error code " << rv << ".  This does not look like valid Mark5B data." << endl;
		dataremaining = false;

		return;
	}

	// If verbose...
	//printmark5bfilesummary(&fileSummary);

	// Here set readseconds to time since beginning of job
	readseconds = 86400*(mark5bfilesummarygetstartmjd(&fileSummary)-corrstartday) + mark5bfilesummarygetstartsecond(&fileSummary)-corrstartseconds + intclockseconds;
    if (fileSummary.framesPerSecond == 0)
        {
        fileSummary.framesPerSecond = 25000;
        cwarn << startl << "mk6 Mark5B framesPerSecond is unknown, setting to 25000" << endl;
        }

	readnanoseconds = mark5bfilesummarygetstartns(&fileSummary);
	currentdsseconds = activesec + model->getScanStartSec(activescan, config->getStartMJD(), config->getStartSeconds());

	if(currentdsseconds > readseconds+1)
	{
		jumpseconds = currentdsseconds - readseconds;
		if(activens < readnanoseconds)
		{
			jumpseconds--;
		}

		// set byte offset to the requested time

		dataoffset = static_cast<long long>(jumpseconds*fileSummary.framesPerSecond*10016 + 0.5);

		readseconds += jumpseconds;
	}

	// Now set readseconds to time since beginning of scan
	readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
	
	// Advance into file if requested
#if 0

FIXME: get seeking working

	if(fileSummary.firstFrameOffset + dataoffset > 0)
	{
		off_t seek_pos;
		cverbose << startl << "Mark6 about to seek to byte " << fileSummary.firstFrameOffset << " plus jump " << dataoffset << " to get to the first wanted frame" << endl;

		seek_pos = mark6_sg_lseek(mark6gather, fileSummary.firstFrameOffset + dataoffset, SEEK_SET);

		if(seek_pos != fileSummary.firstFrameOffset + dataoffset)
		{
			cinfo << startl << "File " << datafilenames[configindex][fileindex] << " ended before the currently desired time" << endl;
			dataremaining = false;
			closeMark6();
		}
	}
#endif
}


// This function does the actual file IO, readbuffer management, and Mark5B fixing.  The result after each
// call is, hopefully, readbytes of multiplexed data being put into buffer segment with potentially some 
// read data left over in the read buffer ready for next time
int Mark5BMark6DataStream::dataRead(int buffersegment)
{
	unsigned char *destination;
	int bytes, bytestoread;
	long long bytediff;
	unsigned int bytesvisible;
	int rbs;
	int fixReturn;
	time_t now;

	rbs = readbuffersize - (readbuffersize % framebytes);

	destination = reinterpret_cast<unsigned char *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);

	// Bytes to read
	bytes = rbs - readbufferleftover;

	// if the file is exhausted, just fix any leftover data and return
	if(mark6eof)
	{
		// If there is some data left over, just demux that and send it out
		if(readbufferleftover > minleftoverdata)
		{
			fixReturn = mark5bfix(destination, readbytes, readbuffer, readbufferleftover, framespersecond, startOutputFrameNumber, &m5bstats);

			readbufferleftover = 0;
			bufferinfo[buffersegment].validbytes = m5bstats.destUsed;

			startOutputFrameNumber = -1;
		}
		else
		{
			// Really, this should not happen based, but just in case...
			bufferinfo[buffersegment].validbytes = 0;
		}
		dataremaining = false;

		return 0;
	}

	// execute the file read
	bytestoread = bytes;
	bytes = mark6Gather(mark6gather, reinterpret_cast<char *>(readbuffer) + readbufferleftover, bytestoread);
cinfo << startl << "Mark6 Gather: " << bytestoread << " requested, " << bytes << " received." << endl;
	if(bytes < bytestoread)
	{
		// file ran out
		mark6eof = true;
	}

	if(bytestoread == 0)
	{
		cwarn << startl << "Weird: bytes to read is zero!" << endl;
		mark6eof = true;
	}

	bytesvisible = readbufferleftover + bytes;

	// fix data
	fixReturn = mark5bfix(destination, readbytes, readbuffer, bytesvisible,framespersecond, startOutputFrameNumber, &m5bstats);

	if(fixReturn <= 0)
	{
		dataremaining = false;
		bufferinfo[buffersegment].validbytes = 0;
		readbufferleftover = 0;

		if(fixReturn < 0)
		{
			cerror << startl << "mark5bfix() failed with return code " << fixReturn << ", likely input buffer is too small!" << endl;
		}
		else
		{
			cinfo << startl << "mark5bfix() returned no data.  Assuming end of file." << endl;
		}

		return 0;
	}

	consumedbytes += bytes;
	bufferinfo[buffersegment].validbytes = m5bstats.destUsed;
	bufferinfo[buffersegment].readto = true;
	if(bufferinfo[buffersegment].validbytes > 0)
	{
		// In the case of Mark5B, we can get the time from the data, so use that just in case there was a jump
		bufferinfo[buffersegment].scanns = m5bstats.startFrameNanoseconds;
		// FIXME: warning! here we are assuming no leap seconds in the stream. FIXME
		// FIXME: below assumes each scan is < 86400 seconds long
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

		bytecount += bytes;
		now = time(0);
		if(msgsenttime < now)
		{
			bytediff = bytecount - lastbytecount;
			mbyterate = (float)bytediff / 1000000.0;
			fmjd = corrstartday + (corrstartseconds + model->getScanStartSec(readscan, corrstartday, corrstartseconds) + readseconds + static_cast<double>(readnanoseconds)/1000000000.0)/86400.0;
			sendMark6Activity(MARK6_STATE_PLAY, bytecount, fmjd, mbyterate * 8.0);
			msgsenttime = now;
			lastbytecount = bytecount;
		}

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

	readbufferleftover += (bytes - m5bstats.srcUsed);

	if(readbufferleftover > 0)
	{
		memmove(readbuffer, readbuffer+m5bstats.srcUsed, readbufferleftover);
	}
	else if(readbufferleftover < 0)
	{
		cwarn << startl << "readbufferleftover = " << readbufferleftover << "; it should never be negative." << endl;

		readbufferleftover = 0;
	}
	if(readbufferleftover <= minleftoverdata && mark6eof)
	{
		readbufferleftover = 0;

		// here we've in one call both read all the remaining data from a file and multiplexed it all without leftovers
		dataremaining = false;
	}

	return bytes;
}

void Mark5BMark6DataStream::mark6ToMemory(int buffersegment)
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

	if(m5bstats.destUsed == 0)
	{
		++invalidtime;
	}
	else
	{
		invalidtime = 0;
	}

	// did we just cross into next scan?
	if(readseconds >= model->getScanDuration(readscan))
	{
		cinfo << startl << "mark6ToMemory: end of schedule scan " << readscan << " of " << model->getNumScans() << " detected" << endl;

		// find next valid schedule scan
		do
		{
			++readscan;
		} while(readscan < model->getNumScans() && config->getScanConfigIndex(readscan));

		cinfo << startl << "readscan incremented to " << readscan << endl;

		if(readscan < model->getNumScans())
		{
			//if we need to, change the config
			if(config->getScanConfigIndex(readscan) != bufferinfo[(lastvalidsegment + 1)%numdatasegments].configindex)
			{
				updateConfig((lastvalidsegment + 1)%numdatasegments);
			}
		}
		else
		{
			// here we just crossed over the end of the job
			cverbose << startl << "readscan==getNumScans -> keepreading=false" << endl;
			
			keepreading = false;

			bufferinfo[(lastvalidsegment+1)%numdatasegments].scan = model->getNumScans()-1;
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = model->getScanDuration(model->getNumScans()-1);
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
		}
		cinfo << startl << "mark6ToMemory: starting schedule scan " << readscan << endl;
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

void Mark5BMark6DataStream::loopfileread()
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
	while(!dataremaining && keepreading)
	{
cverbose << startl << "Mark6: opening file " << filesread[bufferinfo[0].configindex] << endl;
		openfile(bufferinfo[0].configindex, filesread[bufferinfo[0].configindex]++);
		if(!dataremaining && mark6gather != 0)
		{
			closeMark6();
		}
	}

cverbose << startl << "Mark6: Opened first usable file" << endl;

	if(keepreading)
	{
		mark6ToMemory(numread++);
		mark6ToMemory(numread++);
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
		mark6ToMemory(numread++);
	}

cverbose << startl << "Mark6: Opened first usable file" << endl;

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
			mark6ToMemory(lastvalidsegment);
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
		if(keepreading)
		{
cverbose << startl << "keepreading is true" << endl;
			closeMark6();

			//if we need to, change the config
			int nextconfigindex = config->getScanConfigIndex(readscan);
			while(nextconfigindex < 0 && readscan < model->getNumScans())
			{
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
				{
					updateConfig((lastvalidsegment + 1)%numdatasegments);
				}
				//if the datastreams for two or more configs are common, they'll all have the same 
				//files.  Therefore work with the lowest one
				int lowestconfigindex = bufferinfo[(lastvalidsegment+1)%numdatasegments].configindex;
				for(int i=config->getNumConfigs()-1;i>=0;i--)
				{
					if(config->getDDataFileNames(i, streamnum) == config->getDDataFileNames(lowestconfigindex, streamnum))
					lowestconfigindex = i;
				}
				openfile(lowestconfigindex, filesread[lowestconfigindex]++);
				bool skipsomefiles = (config->getScanConfigIndex(readscan) < 0)?true:false;
				while(skipsomefiles)
				{
					int nextscan = peekfile(lowestconfigindex, filesread[lowestconfigindex]);
					if(nextscan == readscan || (nextscan == readscan+1 && config->getScanConfigIndex(nextscan) < 0))
					{
						openfile(lowestconfigindex, filesread[lowestconfigindex]++);
					}
					else
					{
						skipsomefiles = false;
					}
				}
			}
		}
	}
	if(mark6gather != 0)
	{
		closeMark6();
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

int Mark5BMark6DataStream::sendMark6Activity(enum Mark6State state, long long position, double dataMJD, float rate)
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

