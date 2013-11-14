/***************************************************************************
 *   Copyright (C) 2006-2013 by Adam Deller and Walter Brisken             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mk5.cpp $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <cmath>
#include <cstring>
#include <mpi.h>
#include <iomanip>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "config.h"
#include "alert.h"
#include "mark5bfile.h"
#include "mode.h"

#include <unistd.h>

#ifdef WORDS_BIGENDIAN
#define FILL_PATTERN 0x44332211UL
#else
#define FILL_PATTERN 0x11223344UL
#endif

/* TODO: 
   - make use of activesec and activescan
   - make FAKE mode work
 */


/// Mark5BDataStream -------------------------------------------------------

Mark5BDataStream::Mark5BDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
 : DataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	//each data buffer segment contains an integer number of frames, because thats the way config determines max bytes
	lastconfig = -1;

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

	// Set some Mark5B fixer parameters
	startOutputFrameNumber = -1;

	// Make read buffer a bit bigger than a data segment size so extra bytes can be filtered out 
	// The excess should be limited to avoid large memory moves of extra data
	// But the amount of excess should be large enough to encompass all reasonable amounts of interloper data
	// Here we choose 1/10 extra as a compromise.  Might be worth a revisit in the future.

	readbuffersize = (bufferfactor/numsegments)*conf->getMaxDataBytes(streamnum)*11/10;
	readbuffersize -= (readbuffersize % 8); // make it a multiple of 8 bytes
	readbufferleftover = 0;
	readbuffer = 0;	// to be allocated via initialize();

	// Don't bother to do another read iteration just to salvage this many bytes at the end of a file/scan
	minleftoverdata = 20000;

	resetmark5bfixstatistics(&m5bstats);
	invalidtime = 0;
}

Mark5BDataStream::~Mark5BDataStream()
{
	long long bytesProcessed = (m5bstats.nValidFrame + m5bstats.nLostPacket)*10016 + m5bstats.nFillByte + m5bstats.nSkippedByte;
	cinfo << startl << "Mark5B fixing statistics: nValidFrame=" << m5bstats.nValidFrame << " nInvalidFrame=" << m5bstats.nInvalidFrame << " nSkippedByte=" << m5bstats.nSkippedByte << " nFillByte=" << m5bstats.nFillByte << " nLostPacket=" << m5bstats.nLostPacket << " nCall=" << m5bstats.nCall << endl;

	if(m5bstats.nFillByte > 3*bytesProcessed/4)
	{
		cerror << startl << "More than three quarters of the data from this station was unrecoverable (fill pattern)." << endl;
	}
	if(m5bstats.nFillByte > bytesProcessed/2)
	{
		cwarn << startl << "More than half of the data from this station was unrecoverable (fill pattern)." << endl;
	}
	if(m5bstats.nSkippedByte > bytesProcessed/20)
	{
		cwarn << startl << "More than 5 percent of data from this antenna were unwanted bytes or fill pattern.  This could indicate a problem in the routing of data from the digital back end to the recorder.  More likely it is a result of an unplugged drive in the module." << endl;
	}

	if(switchedpower)
	{
		delete switchedpower;
	}
	if(readbuffer)
	{
		delete [] readbuffer;
	}
}

void Mark5BDataStream::initialise()
{
	readbuffer = new unsigned char[readbuffersize];
	DataStream::initialise();
}

int Mark5BDataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
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
		cfatal << startl << "Mark5BDataStream::calculateControlParams: vlbaoffset<0: vlbaoffset=" << vlbaoffset << " bufferindex=" << bufferindex << " atsegment=" << atsegment << " readbytes=" << readbytes << ", framebytes=" << framebytes << ", payloadbytes=" << payloadbytes << endl;
		bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
		// WFB20120123 MPI_Abort(MPI_COMM_WORLD, 1);
	
		return 0;
	}

	// bufferindex was previously computed assuming no framing overhead
	framesin = vlbaoffset/payloadbytes;


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
			cwarn << startl << "bufferindex == bufferbytes --> Mode::INVALID_SUBINT" << endl;
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
		cfatal << startl << "Mark5BDataStream::calculateControlParams: bufferindex>=bufferbytes: bufferindex=" << bufferindex << " >= bufferbytes=" << bufferbytes << " atsegment = " << atsegment << endl;
		bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
		MPI_Abort(MPI_COMM_WORLD, 1);
	
		return 0;
	}
	
	return bufferindex;
}

void Mark5BDataStream::updateConfig(int segmentindex)
{
	//run the default update config, then add additional information specific to Mk5
	DataStream::updateConfig(segmentindex);
	if(bufferinfo[segmentindex].configindex < 0) //If the config < 0 we can skip this scan
	{
		return;
	}

	int fb = config->getFrameBytes(bufferinfo[segmentindex].configindex, streamnum);
	int fps = config->getFramesPerSecond(bufferinfo[segmentindex].configindex, streamnum);

	//correct the nsinc - should be number of output frames*frame time
	bufferinfo[segmentindex].nsinc = int(((bufferbytes/numdatasegments)/fb)*(1000000000.0/double(fps)) + 0.5);

	//take care of the case where an integral number of frames is not an integral number of blockspersend - ensure sendbytes is long enough
	//note below, the math should produce a pure integer, but add 0.5 to make sure that the fuzziness of floats doesn't cause an off-by-one error
	bufferinfo[segmentindex].sendbytes = int(((((double)bufferinfo[segmentindex].sendbytes)* ((double)config->getSubintNS(bufferinfo[segmentindex].configindex)))/(config->getSubintNS(bufferinfo[segmentindex].configindex) + config->getGuardNS(bufferinfo[segmentindex].configindex)) + 0.5));
}

void Mark5BDataStream::initialiseFile(int configindex, int fileindex)
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
	nbits = config->getDNumBits(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	framebytes = config->getFrameBytes(configindex, streamnum);
	framespersecond = config->getFramesPerSecond(configindex, streamnum);

	bw = config->getDRecordedBandwidth(configindex, streamnum, 0);

	startOutputFrameNumber = -1;

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, framebytes, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
	if(fanout != 1)
	{
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	cinfo << startl << "Mark5BDataStream::initialiseFile format=" << formatname << endl;

	// Here we need to open the file, read the start time, jump if necessary, and if past end of file, dataremaining = false.  Then set readseconds...

	// First we get a description of the contents of the purported Mark5B file and exit if it looks like not Mark5B at all
	rv = summarizemark5bfile(&fileSummary, datafilenames[configindex][fileindex].c_str());
	if(rv < 0)
	{
		cwarn << startl << "Mark5BDataStream::initialiseFile: summary of file " << datafilenames[configindex][fileindex] << " resulted in error code " << rv << ".  This does not look like valid Mark5B data." << endl;
		dataremaining = false;

		return;
	}
	mark5bfilesummarysettotalbandwidth(&fileSummary, static_cast<int>(bw*nrecordedbands*1000000));
	mark5bfilesummaryfixmjd(&fileSummary, config->getStartMJD());

	// If verbose...
	printmark5bfilesummary(&fileSummary);


	// Here set readseconds to time since beginning of job
	readseconds = 86400*(mark5bfilesummarygetstartmjd(&fileSummary)-corrstartday) + mark5bfilesummarygetstartsecond(&fileSummary)-corrstartseconds + intclockseconds;
	readnanoseconds = mark5bfilesummarygetstartns(&fileSummary);
	currentdsseconds = activesec + model->getScanStartSec(activescan, config->getStartMJD(), config->getStartSeconds());

	if(currentdsseconds > readseconds+1)
	{
		jumpseconds = currentdsseconds - readseconds;
		if(activens < readnanoseconds)
		{
			jumpseconds--;
		}

		const int n=10016, d=10000;	// numerator and denominator of framesize/payload ratio

		// set byte offset to the requested time
		dataoffset = static_cast<long long>(jumpseconds*mark5bfilesummarygetbitrate(&fileSummary)/d*n/8 + 0.5);

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

// FIXME: Warning: this needs some work???
void Mark5BDataStream::initialiseFake(int configindex)
{
	int nrecordedbands, fanout;
	Configuration::dataformat format;
	Configuration::datasampling sampling;
	double bw;

	DataStream::initialiseFake(configindex);

	format = config->getDataFormat(configindex, streamnum);
	nbits = config->getDNumBits(configindex, streamnum);
	sampling = config->getDSampling(configindex, streamnum);
	nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
	bw = config->getDRecordedBandwidth(configindex, streamnum, 0);
	framebytes = config->getFrameBytes(configindex, streamnum);
	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, framebytes, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
	if(fanout < 0)
	{
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	cwarn << startl << "Correlating fake data with format " << formatname << endl;
}

int Mark5BDataStream::testForSync(int configindex, int buffersegment)
{
	// not needed.  mark5bfix always leaves perfectly synchonized data behind
	return 0;
}


// This function does the actual file IO, readbuffer management, and Mark5B fixing.  The result after each
// call is, hopefully, readbytes of multiplexed data being put into buffer segment with potentially some 
// read data left over in the read buffer ready for next time
int Mark5BDataStream::dataRead(int buffersegment)
{
	unsigned long *destination;
	int bytes;
	int fixReturn;

	destination = reinterpret_cast<unsigned long *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);

	// Bytes to read
	bytes = readbuffersize - readbufferleftover;

	// if the file is exhausted, just multiplex any leftover data and return
	if(input.eof())
	{
		// If there is some data left over, just demux that and send it out
		if(readbufferleftover > minleftoverdata)
		{
			mark5bfix(reinterpret_cast<unsigned char *>(destination), readbytes, readbuffer, readbufferleftover, startOutputFrameNumber, framespersecond, &m5bstats);
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
	input.clear();

	input.read(reinterpret_cast<char *>(readbuffer + readbufferleftover), bytes);
	bytes = input.gcount();

	// "fix" Mark5B data: remove stray packets/byts and put good frames on a uniform grid
	fixReturn = mark5bfix(reinterpret_cast<unsigned char *>(destination), readbytes, readbuffer, readbufferleftover + bytes, framespersecond,  startOutputFrameNumber, &m5bstats);
	if(fixReturn < 0)
	{
		cerror << startl << "mark5bfix returned " << fixReturn << endl;

		keepreading = false;
		dataremaining = false;
	}
	if(fixReturn == 0)
	{
		cwarn << startl << "mark5bfix returned zero.  Going to next record scan..." << endl;

		dataremaining = false;
	}

	bufferinfo[buffersegment].validbytes = m5bstats.destUsed;
	bufferinfo[buffersegment].readto = true;
	consumedbytes += m5bstats.srcUsed;
	if(bufferinfo[buffersegment].validbytes > 0)
	{
		// In the case of Mark5B, we can get the time from the data, so use that just in case there was a jump
		bufferinfo[buffersegment].scanns = m5bstats.startFrameNanoseconds;
		bufferinfo[buffersegment].scanseconds = m5bstats.startFrameSeconds + intclockseconds - corrstartseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
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

		if(m5bstats.destUsed == m5bstats.destSize)
		{
			// FIXME: the line below should help things, but it causes first output frame to be invalid.  Hmmm....
			startOutputFrameNumber = m5bstats.startFrameNumber + m5bstats.destUsed/10016;
		}
		else
		{
			if(m5bstats.srcUsed < m5bstats.destUsed - 10*10016)
			{
				// Warn if more than 10 frames of data are missing
				cwarn << startl << "Data gap of " << (m5bstats.destUsed-m5bstats.srcUsed) << " bytes out of " << m5bstats.destUsed << " bytes found" << endl;
			}
			else if(m5bstats.srcUsed > m5bstats.destUsed + 10*10016)
			{
				// Warn if more than 10 frames of unexpected data found
				// Note that 5008 bytes of extra data at scan ends is not uncommon, so specifically don't warn for that.
				cwarn << startl << "Data excess of " << (m5bstats.srcUsed-m5bstats.destUsed) << " bytes out of " << m5bstats.destUsed << " bytes found" << endl;
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
		cwarn << startl << "readbufferleftover=" << readbufferleftover << "; it should never be negative." << endl;

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

// Below here, these are thought to be identical for VDIF and Mark5B

void Mark5BDataStream::diskToMemory(int buffersegment)
{
	unsigned long *buf;

	buf = reinterpret_cast<unsigned long *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);

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
		cinfo << startl << "diskToMemory: end of schedule scan " << readscan << " of " << model->getNumScans() << " detected" << endl;
		
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

			cinfo << startl << "diskToMemory: starting schedule scan " << readscan << endl;
		}
		else
		{
			// here we just crossed over the end of the job
			cinfo << startl << "readscan==getNumScans -> keepreading=false" << endl;

			keepreading = false;

			bufferinfo[(lastvalidsegment+1)%numdatasegments].scan = model->getNumScans()-1;
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = model->getScanDuration(model->getNumScans()-1);
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
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

void Mark5BDataStream::loopfileread()
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
cverbose << startl << "opening file " << filesread[bufferinfo[0].configindex] << endl;
		openfile(bufferinfo[0].configindex, filesread[bufferinfo[0].configindex]++);
		if(!dataremaining)
		{
			input.close();
		}
	}

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
		if(keepreading)
		{
cverbose << startl << "keepreading is true" << endl;
			input.close();

			//if the datastreams for two or more configs are common, they'll all have the same 
			//files.  Therefore work with the lowest one
			int lowestconfigindex = bufferinfo[(lastvalidsegment+1)%numdatasegments].configindex;
			for(int i=config->getNumConfigs()-1;i>=0;i--)
			{
				if(config->getDDataFileNames(i, streamnum) == config->getDDataFileNames(lowestconfigindex, streamnum))
				lowestconfigindex = i;
			}
			openfile(lowestconfigindex, filesread[lowestconfigindex]++);
		}
		if(!keepreading)
		{
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = config->getExecuteSeconds();
			bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
			cverbose << startl << "New record scan -> keepreading=false" << endl;
		}
	}
	if(input.is_open())
	{
		input.close();
	}
	if(numread > 0)
	{
		//cdebug << startl << "READTHREAD: loopfileread: Unlock buffer " << lastvalidsegment << endl; 
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
		cverbose << startl << "Datastream readthread is exiting; filecount=" << filesread[bufferinfo[lastvalidsegment].configindex] << ", confignumfiles=" << confignumfiles[bufferinfo[lastvalidsegment].configindex] << ", dataremaining=" << dataremaining << ", keepreading=" << keepreading << endl;
	}
	else
	{
		cverbose << startl << "Datastream readthread is exiting, after not finding any data at all!" << endl;
	}
}
