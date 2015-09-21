/***************************************************************************
 *   Copyright (C) 2006-2015 by Adam Deller and Walter Brisken             *
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
#include "vdiffile.h"
#include "mode.h"

#include <unistd.h>


/* TODO: 
   - make use of activesec and activescan
 */


/// VDIFDataStream -------------------------------------------------------

VDIFDataStream::VDIFDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
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

	// Set some VDIF muxer parameters
	nSort = 32;	// allow data to be this many frames out of order without any loss at read boundaries
	nGap = 1000;	// a gap of this many frames will trigger an interruption of muxing
	startOutputFrameNumber = -1;
	memset(&vm, 0, sizeof(vm));

	// Make read buffer a bit bigger than a data segment size so extra bytes can be filtered out 
	// The excess should be limited to avoid large memory moves of extra data
	// But the amount of excess should be large enough to encompass all reasonable amounts of interloper data
	// Here we choose 1/10 extra as a compromise.  Might be worth a revisit in the future.

	readbuffersize = (bufferfactor/numsegments)*conf->getMaxDataBytes(streamnum)*11LL/10LL;
	readbuffersize -= (readbuffersize % 8); // make it a multiple of 8 bytes
	readbufferleftover = 0;
	readbuffer = 0;	// to be allocated via initialize();

	// Don't bother to do another read iteration just to salvage this many bytes at the end of a file/scan
	minleftoverdata = 20000;

	resetvdifmuxstatistics(&vstats);
	nthreads = 0; // no threads identified yet
	threads = 0;  // null pointer indicating not yet initialized
	invalidtime = 0;

	samplingtype = Configuration::REAL;
}

VDIFDataStream::~VDIFDataStream()
{
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

void VDIFDataStream::initialise()
{
	readbuffer = new unsigned char[readbuffersize];
	DataStream::initialise();
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
	int nrecordedbands, fanout;
	Configuration::datasampling sampling;
	Configuration::dataformat format;
	double bw;
	int rv;

	long long dataoffset = 0;
	struct vdif_file_summary fileSummary;
	int jumpseconds, currentdsseconds;

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

	rv = configurevdifmux(&vm, inputframebytes, framespersecond, nbits, nthreads, threads, nSort, nGap, VDIF_MUX_FLAG_RESPECTGRANULARITY);
	if(rv < 0)
	{
		cfatal << startl << "configurevmux failed with return code " << rv << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	int nChanPerThread = nrecordedbands/nthreads;
	if(nChanPerThread != 1)
	{
		cinfo << startl << "Note: " << nChanPerThread << " channels reside on each thread.  Support for this is new.  Congratulations for being bold and trying this out!  Warranty void in the 193 UN recognized nations." << endl;
	}
	if(nChanPerThread * nthreads != nrecordedbands)
	{
		cerror << startl << "Error: " << nrecordedbands << " bands were recorded but they are divided unequally across " << nthreads << " threads.  This is not allowed.  Things will probably get very bad soon..." << endl;
	}
	setvdifmuxinputchannels(&vm, nChanPerThread);

	fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, vm.outputFrameSize, config->getDDecimationFactor(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
	if(fanout != 1)
	{
		cfatal << startl << "Fanout is " << fanout << ", which is impossible; no choice but to abort!" << endl;
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	cinfo << startl << "VDIFDataStream::initialiseFile format=" << formatname << endl;

	// Here we need to open the file, read the start time, jump if necessary, and if past end of file, dataremaining = false.  Then set readseconds...

	// First we get a description of the contents of the purported VDIF file and exit if it looks like not VDIF at all
	rv = summarizevdiffile(&fileSummary, datafilenames[configindex][fileindex].c_str(), inputframebytes);
	if(rv < 0)
	{
		cwarn << startl << "VDIFDataStream::initialiseFile: summary of file " << datafilenames[configindex][fileindex] << " resulted in error code " << rv << ".  This does not look like valid VDIF data." << endl;
		dataremaining = false;

		return;
	}

	// If verbose...
	vdiffilesummarysetsamplerate(&fileSummary, static_cast<int64_t>(bw*2000000LL*nChanPerThread));
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
	unsigned char *destination;
	int bytes;
	int muxReturn;
	unsigned int bytesvisible;

	destination = reinterpret_cast<unsigned char *>(&databuffer[buffersegment*(bufferbytes/numdatasegments)]);

	// Bytes to read
	bytes = readbuffersize - readbufferleftover;

	// if the file is exhausted, just multiplex any leftover data and return
	if(input.eof())
	{
		// If there is some data left over, just demux that and send it out
		if(readbufferleftover > minleftoverdata)
		{
			vdifmux(destination, readbytes, readbuffer, readbufferleftover, &vm, startOutputFrameNumber, &vstats);
			readbufferleftover = 0;
			bufferinfo[buffersegment].validbytes = vstats.destUsed;

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

	input.read(reinterpret_cast<char *>(readbuffer) + readbufferleftover, bytes);
	bytes = input.gcount();

	bytesvisible = readbufferleftover + bytes;

	// multiplex and corner turn the data
	muxReturn = vdifmux(destination, readbytes, readbuffer, bytesvisible, &vm, startOutputFrameNumber, &vstats);

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
		// FIXME: warning! here we are assuming no leap seconds since the epoch of the VDIF stream. FIXME
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
		cinfo << startl << "diskToMemory: starting schedule scan " << readscan << endl;
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

void VDIFDataStream::loopfileread()
{
	int perr;
	int numread = 0;

cverbose << startl << "Starting loopfileread()" << endl;

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

cverbose << startl << "Opened first usable file" << endl;

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

cverbose << startl << "Opened first usable file" << endl;

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
		if(keepreading)
		{
cverbose << startl << "keepreading is true" << endl;
			input.close();

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
	if(input.is_open())
	{
		input.close();
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
