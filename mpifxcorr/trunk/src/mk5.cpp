/***************************************************************************
 *   Copyright (C) 2006-2016 by Adam Deller and Walter Brisken             *
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
// $HeadURL$
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
#include "mk5.h"
#include "mode.h"
#include "vdifio.h"

#define MAXPACKETSIZE 100000

#ifdef WORDS_BIGENDIAN
#define FILL_PATTERN 0x44332211UL
#else
#define FILL_PATTERN 0x11223344UL
#endif


/// Mk5DataStream -------------------------------------------------------

Mk5DataStream::Mk5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
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
  framegranularity = 1;
}

Mk5DataStream::~Mk5DataStream()
{
  if(syncteststream != 0)
    delete_mark5_stream(syncteststream);
}

void Mk5DataStream::initialise()
{
  DataStream::initialise();

  int framebytes = config->getFrameBytes(0, streamnum);
  int nframes = readbytes / framebytes;

  estimatedbytes += bufferbytes/numdatasegments; // this accounts for the potential allocation to tempbuf in the event of a data jump

  syncteststream = 0;
  udp_offset = 0;
  if (!readfromfile && !tcp) {
    if (sizeof(vtp_psn64_t)!=8) {
      cfatal << startl << "DataStream assumes long long is 8 bytes, it is " << sizeof(vtp_psn64_t) << " bytes - aborting!!!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
    udpsize = abs(tcpwindowsizebytes/1024)-20-2*4-sizeof(vtp_psn64_t); // RAW socket! IP header is 20 bytes, UDP is 4x2 bytes, 64-bit PSN (VTP protocol) is 8 bytes
    if (udpsize<=0) {
      cfatal << startl << "Datastream " << mpiid << ":" << " implied UDP packet size is negative - aborting!!!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
    udpsize &= ~ 0x7;  // Ensures udpsize multiple of 64 bits
    packet_segmentstart = 0;
    packet_head = 0;
    udp_buf = new char[MAXPACKETSIZE];
    packets_arrived.resize(readbytes/udpsize+2);

    if (sizeof(int)!=4) {
      cfatal << startl << "DataStream assumes int is 4 bytes, it is " << sizeof(int) << " bytes - aborting!!!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }

    invalid_buf = new char[udpsize];
    unsigned int *tmp = (unsigned int*)invalid_buf;
    for (int i=0; i<udpsize/4; i++) {
      tmp[i] = FILL_PATTERN ;
    }

    // UDP statistics
    packet_drop = 0;
    packet_oo = 0;
    packet_duplicate = 0;
    npacket = 0;
    packet_sum = 0;
    lasttime = 0.0;

    udpstats_update = 0.0;
    char *udp_update_str= getenv("DIFX_UDP_UPDATE");
    if (udp_update_str!=0) {
      udpstats_update = atof(udp_update_str);
      if (udpstats_update==0.0) udpstats_update = 2.0;
    } else {
      udpstats_update = 2.0;
    }
  }

  if(config->isDMuxed(0, streamnum)) {
    if(config->getDataFormat(0, streamnum) == Configuration::INTERLACEDVDIF) {
      nframes = config->getDNumMuxThreads(0, streamnum) * readbytes / ((framebytes-VDIF_HEADER_BYTES)*config->getDNumMuxThreads(0, streamnum) + VDIF_HEADER_BYTES);
      datamuxer = new VDIFMuxer(config, streamnum, mpiid, config->getDNumMuxThreads(0, streamnum), framebytes, nframes, config->getFramesPerSecond(0, streamnum)/config->getDNumMuxThreads(0, streamnum), config->getDNumBits(0, streamnum), config->getDMuxThreadMap(0, streamnum));
      estimatedbytes += datamuxer->getEstimatedBytes();
    }
    else
      cfatal << startl << "Requested a muxed datastream but format has no muxed equivalent!" << endl;
  }
}

int Mk5DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
  int bufferindex, framesin, vlbaoffset, looksegment, payloadbytes, framebytes;
  float datarate;
  double framespersecond;

  bufferindex = DataStream::calculateControlParams(scan, offsetsec, offsetns);

  if(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] == Mode::INVALID_SUBINT)
    return 0;

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
  if(config->isDMuxed(bufferinfo[looksegment].configindex, streamnum)) {
    payloadbytes *= config->getDNumMuxThreads(bufferinfo[looksegment].configindex, streamnum);
    framebytes = (framebytes-VDIF_HEADER_BYTES)*config->getDNumMuxThreads(bufferinfo[looksegment].configindex, streamnum) + VDIF_HEADER_BYTES;
    framespersecond /= config->getDNumMuxThreads(bufferinfo[looksegment].configindex, streamnum);
  }

  //set the fraction of data to use to determine system temperature based on data rate
  //the values set here work well for the today's computers and clusters...
  datarate = static_cast<float>(framebytes)*framespersecond*8.0/1.0e6;  // in Mbps
  if(datarate < 512)
  {
    switchedpowerincrement = 1;
  }
  else
  {
    switchedpowerincrement = static_cast<int>(datarate/512 + 0.1);
  }

  //do the necessary correction to start from a frame boundary - work out the offset from the start of this segment
  vlbaoffset = bufferindex - atsegment*readbytes;

  if(vlbaoffset < 0)
  {
    cfatal << startl << "Mk5DataStream::calculateControlParams: vlbaoffset<0: vlbaoffset=" << vlbaoffset << " bufferindex=" << bufferindex << " atsegment=" << atsegment << " readbytes=" << readbytes << ", framebytes=" << framebytes << ", payloadbytes=" << payloadbytes << endl;
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    // WFB20120123 MPI_Abort(MPI_COMM_WORLD, 1);
    return 0;
  }

  // bufferindex was previously computed assuming no framing overhead
  framesin = vlbaoffset/payloadbytes;

  // here we enforce frame granularity.  We simply back up to the previous frame that is a multiple of the frame granularity.
  if(framesin % framegranularity != 0)
  {
    framesin -= (framesin % framegranularity);
    if(framesin < 0)
    {
      framesin += framegranularity;
    }
  }

  // Note here a time is needed, so we only count payloadbytes
  long long segoffns = bufferinfo[atsegment].scanns + (long long)((1000000000.0*framesin)/framespersecond + 0.5);
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
      return 0; //note exit here!!!!
    }
    else
    {
      cwarn << startl << "Developer error mk5: bufferindex == bufferbytes in a 'normal' situation" << endl;
    }
  }

  if(bufferindex > bufferbytes) /* WFB: this was >= */
  {
    cfatal << startl << "Mk5DataStream::calculateControlParams: bufferindex>=bufferbytes: bufferindex=" << bufferindex << " >= bufferbytes=" << bufferbytes << " atsegment = " << atsegment << endl;
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    MPI_Abort(MPI_COMM_WORLD, 1);
    return 0;
  }
  return bufferindex;
}

void Mk5DataStream::updateConfig(int segmentindex)
{
  //run the default update config, then add additional information specific to Mk5
  DataStream::updateConfig(segmentindex);
  if(bufferinfo[segmentindex].configindex < 0) //If the config < 0 we can skip this scan
    return;

  int framebytes = config->getFrameBytes(bufferinfo[segmentindex].configindex, streamnum);
  double framespersecond = config->getFramesPerSecond(bufferinfo[segmentindex].configindex, streamnum);
  if(config->isDMuxed(bufferinfo[segmentindex].configindex, streamnum)) {
    framebytes = (framebytes - VDIF_HEADER_BYTES)*config->getDNumMuxThreads(bufferinfo[segmentindex].configindex, streamnum) + VDIF_HEADER_BYTES;
    framespersecond /= config->getDNumMuxThreads(bufferinfo[segmentindex].configindex, streamnum);
  }

  //correct the nsinc - should be number of frames*frame time
  bufferinfo[segmentindex].nsinc = int(((bufferbytes/numdatasegments)/framebytes)*(1000000000.0/double(framespersecond)) + 0.5);

  //take care of the case where an integral number of frames is not an integral number of blockspersend - ensure sendbytes is long enough
  //note below, the math should produce a pure integer, but add 0.5 to make sure that the fuzziness of floats doesn't cause an off-by-one error
  bufferinfo[segmentindex].sendbytes = config->getDataBytes(bufferinfo[segmentindex].configindex,streamnum);
}

void Mk5DataStream::deriveFormatName(int configindex)
{
  int nbits, nrecordedbands, fanout, framebytes;
  Configuration::dataformat format;
  Configuration::datasampling sampling;
  double bw;

  format = config->getDataFormat(configindex, streamnum);
  nbits = config->getDNumBits(configindex, streamnum);
  sampling = config->getDSampling(configindex, streamnum);
  nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
  bw = config->getDRecordedBandwidth(configindex, streamnum, 0);
  framebytes = config->getFrameBytes(configindex, streamnum);
  if(config->isDMuxed(configindex, streamnum)) {
    framebytes = (framebytes-VDIF_HEADER_BYTES)*config->getDNumMuxThreads(configindex, streamnum) + VDIF_HEADER_BYTES;
  }
  fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, framebytes, config->getDDecimationFactor(configindex, streamnum), config->getDAlignmentSeconds(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
  if (fanout < 0) {
    cfatal << startl << "Fanout is " << fanout << ", which is impossible - no choice but to abort!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
}

void Mk5DataStream::initialiseFile(int configindex, int fileindex)
{
  int offset, framebytes;
  int nbits, nrecordedbands, fanout, jumpseconds, currentdsseconds;
  Configuration::datasampling sampling;
  Configuration::dataformat format;
  double bw, bytespersecond;
  long long dataoffset = 0;

  // FIXME: at some point start using deriveFormatName ???

  format = config->getDataFormat(configindex, streamnum);
  sampling = config->getDSampling(configindex, streamnum);
  nbits = config->getDNumBits(configindex, streamnum);
  nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
  bw = config->getDRecordedBandwidth(configindex, streamnum, 0);
  framebytes = config->getFrameBytes(configindex, streamnum);
  if(config->isDMuxed(configindex, streamnum)) {
    nrecordedbands = 1;
  }
  fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, framebytes, config->getDDecimationFactor(configindex, streamnum), config->getDAlignmentSeconds(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
  if (fanout < 0) {
    cfatal << startl << "Fanout is " << fanout << ", which is impossible - no choice but to abort!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  if(syncteststream != 0)
    delete_mark5_stream(syncteststream);
  syncteststream = new_mark5_stream(
    new_mark5_stream_file(datafilenames[configindex][fileindex].c_str(), 0),
    new_mark5_format_generic_from_string(formatname) );
  if(syncteststream == 0)
  {
    cwarn << startl << " could not open file " << datafilenames[configindex][fileindex] << endl;
    dataremaining = false;
    return;
  }
  if(syncteststream->nchan != nrecordedbands)
  {
    cerror << startl << "Number of recorded bands for datastream " << streamnum << " (" << nrecordedbands << ") does not match with MkV file " << datafilenames[configindex][fileindex] << " (" << syncteststream->nchan << "), will be ignored!" << endl;
  }

  // resolve any day ambiguities
  mark5_stream_fix_mjd(syncteststream, corrstartday);

  mark5_stream_print(syncteststream);

  offset = syncteststream->frameoffset;
  framegranularity = syncteststream->framegranularity;


  cverbose << startl << "Format name = " << formatname << " and framegranularity = " << framegranularity << endl;


  bytespersecond = syncteststream->framebytes/syncteststream->framens * 1e9;

  readseconds = 86400*(syncteststream->mjd-corrstartday) + syncteststream->sec-corrstartseconds + intclockseconds;
  readnanoseconds = int(syncteststream->ns);
  currentdsseconds = activesec + model->getScanStartSec(activescan, config->getStartMJD(), config->getStartSeconds());

  //cout << "Mk5DataStream::initialiseFile" << endl;
  //cout << "  framens= " << syncteststream->framens << endl;
  //cout << "   offset= " << syncteststream->frameoffset << endl;
  //cout << "   mjd= " << syncteststream->mjd << endl;
  //cout << "   sec= " << syncteststream->sec << endl;
  //cout << "    ns= " << syncteststream->ns << endl;
  //cout << "    ns= " << syncteststream->ns << endl;
  //cout << endl;

  if (currentdsseconds  > readseconds+1)
  {
    jumpseconds = currentdsseconds - readseconds;
    if (activens < readnanoseconds)
    {
      jumpseconds--;
    }

    // set byte offset to the requested time
    dataoffset = (long long)(jumpseconds * bytespersecond + 0.5);
    readseconds += jumpseconds;
  }

  while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
    readscan++;
  while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
    readscan--;
  readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
  cverbose << startl << "The frame start is day=" << syncteststream->mjd << ", seconds=" << syncteststream->sec << ", ns=" << syncteststream->ns << ", readscan=" << readscan << ", readseconds=" << readseconds << ", readns=" << readnanoseconds << endl;

  //close the stream used to get the offset, regenerate format name for appropriate in-memory (muxed if muxing)
  delete_mark5_stream(syncteststream);
  syncteststream = 0;
  if(config->isDMuxed(configindex, streamnum)) {
    framebytes = (framebytes-VDIF_HEADER_BYTES)*config->getDNumMuxThreads(configindex, streamnum) + VDIF_HEADER_BYTES;
    nrecordedbands = config->getDNumRecordedBands(configindex, streamnum);
  }
  fanout = config->genMk5FormatName(format, nrecordedbands, bw, nbits, sampling, framebytes, config->getDDecimationFactor(configindex, streamnum), config->getDAlignmentSeconds(configindex, streamnum), config->getDNumMuxThreads(configindex, streamnum), formatname);
  cverbose << startl << "About to seek to byte " << offset << " plus " << dataoffset << " to get to the first frame" << endl;

  input.seekg(offset + dataoffset, ios_base::beg);
  if (input.peek() == EOF) {
    cinfo << startl << "File " << datafilenames[configindex][fileindex] << " ended before the currently desired time" << endl;
    dataremaining = false;
    input.clear();
  }
}


void Mk5DataStream::initialiseFake(int configindex)
{
  DataStream::initialiseFake(configindex);

  deriveFormatName(configindex);

  cwarn << startl << "Correlating fake data with format " << formatname << endl;
}

int Mk5DataStream::testForSync(int configindex, int buffersegment)
{
  int corrday, corrsec;
  int mjd, sec;
  double ns, deltatime;
  struct mark5_stream *mark5stream;
  int offset;
  char * ptr;

  offset = 0;
  corrday = config->getStartMJD();
  corrsec = config->getStartSeconds();
  if(syncteststream != 0)
    delete_mark5_stream(syncteststream);
  syncteststream = new_mark5_stream(new_mark5_stream_memory(&(databuffer[buffersegment*(bufferbytes/numdatasegments)]), bufferbytes/numdatasegments), new_mark5_format_generic_from_string(formatname) );
  if(syncteststream == 0)
  {
    cerror << startl << "Could not create a mark5stream to test for sync!" << endl;
    return 0; //note exit here
  }
  // resolve any day ambiguities
  mark5_stream_fix_mjd(syncteststream, corrday);
  mark5_stream_get_frame_time(syncteststream, &mjd, &sec, &ns);

  deltatime = 86400*(corrday - mjd) + (model->getScanStartSec(bufferinfo[buffersegment].scan, corrday, corrsec) + bufferinfo[buffersegment].scanseconds + corrsec - intclockseconds - sec) + double(bufferinfo[buffersegment].scanns-ns)/1e9;

  if(fabs(deltatime) > 1e-10) //oh oh, a problem
  {
    if(readfromfile || readscan != 0 || readseconds != 0 || readnanoseconds != 0) //its not an error for the *first* network read
    {
      cerror << startl << "Lost Sync on segment " << buffersegment << "! Will attempt to resync. Deltatime was " << deltatime <<  "s" << endl;
      cdebug << startl << "Corrday was " << corrday << ", corrsec was " << corrsec << ". MJD was " << mjd << ", sec was " << sec << "> Readseconds was " << bufferinfo[buffersegment].scanseconds << ". readns was " << bufferinfo[buffersegment].scanns << ", ns was " << (int)ns << ", intclockseconds was " << intclockseconds << endl;
    }
    mark5stream = new_mark5_stream(
    new_mark5_stream_memory(&databuffer[buffersegment*(bufferbytes/numdatasegments)], bufferinfo[buffersegment].validbytes), new_mark5_format_generic_from_string(formatname) );

    if (mark5stream==0)
    {
      //Don't change time (will be deadreckoned from that last segment) but set validbytes to zero (crap data)
      cwarn << startl << "Could not identify Mark5 segment time (" << formatname << ") - this segment will be trash!" << endl;
      bufferinfo[buffersegment].validbytes = 0;
    }
    else
    {
      if(mark5stream->nchan != config->getDNumRecordedBands(configindex, streamnum))
      {
        cerror << startl << "Number of recorded bands for datastream " << streamnum << " (" << config->getDNumRecordedBands(configindex, streamnum) << ") does not match with Mark5 data " << " (" << mark5stream->nchan << "), will be ignored!!!" << endl;
      }

      // resolve any day ambiguities
      mark5_stream_fix_mjd(mark5stream, corrstartday);

      if (configindex != lastconfig) {
        cinfo << startl << "Config has changed!" << endl;
        mark5_stream_print(mark5stream);
        lastconfig = configindex;
        if(switchedpower) {
          switchedpower->flush();
        }
      }

      offset = mark5stream->frameoffset;

      // If offset is non-zero we need to shuffle the data in memory to align on a frame boundary
      if (offset>0) {
        ptr = (char*)&databuffer[buffersegment*(bufferbytes/numdatasegments)];

        if (offset>bufferinfo[buffersegment].validbytes) {
          cerror << startl << "Mark5 offset (" << offset << ") > valid bytes in current segment (" << bufferinfo[buffersegment].validbytes << "!!! Will trash this segment." << endl;
          bufferinfo[buffersegment].validbytes = 0;
        } else {
          cinfo << startl << "************: Shifting " << offset << " bytes in memory to regain sync" << endl;
          memmove(ptr, ptr+offset, bufferinfo[buffersegment].validbytes-offset);
        }
      }

      if (offset <= bufferinfo[buffersegment].validbytes)
      {
        //No need to update validbytes - caller will attempt to fill the missing data
        //just fill in the new times
        readseconds = 86400*(mark5stream->mjd-corrstartday) + mark5stream->sec-corrstartseconds + intclockseconds;
        readnanoseconds = mark5stream->ns;
        while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
          readscan++;
        while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
          readscan--;
        readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
        //also in the buffer
        bufferinfo[buffersegment].scanns = readnanoseconds;
        bufferinfo[buffersegment].scanseconds = readseconds;
        bufferinfo[buffersegment].scan = readscan;

        if(readfromfile || readscan != 0 || readseconds != 0 || readnanoseconds != 0) //its not an error for the *first* network read
        {
          cinfo << startl << "After regaining sync, the frame start day is " << mark5stream->mjd << ", the frame start seconds is " << mark5stream->sec << ", the frame start ns is " << mark5stream->ns << ", readscan is " << readscan << ", readseconds is " << readseconds << ", readnanoseconds is " << readnanoseconds << endl;
        }
      }
      delete_mark5_stream(mark5stream);
    }
  }
  else
  {
    static int nt = 0;

    // Switched power detection
    if(switchedpower && (nt % switchedpowerincrement == 0))
    {
      switchedpower->feed(syncteststream);
    }
  }

  delete_mark5_stream(syncteststream);
  syncteststream = 0;

  return offset;
}

int Mk5DataStream::checkData(int buffersegment)
{
  int mjd, sec, corrday, status;
  double ns, nsperbyte; // Nanosec since 00:00 UTC 1 Jan 2000 - check enough precision
  uint64_t ns2000, endns2000;
  BUFOFFSET_T goodbytes;

  corrday = config->getStartMJD();
  //corrsec = config->getStartSeconds();
  if(syncteststream != 0)
    delete_mark5_stream(syncteststream);

  syncteststream = new_mark5_stream(new_mark5_stream_memory(&(databuffer[buffersegment*(bufferbytes/numdatasegments)]),     bufferinfo[buffersegment].validbytes), new_mark5_format_generic_from_string(formatname) );
  if(syncteststream == 0)
  {
    cerror << startl << "Could not create a mark5stream to test for sync!" << endl;
    return -1; //note exit here
  }

  // resolve any day ambiguities
  mark5_stream_fix_mjd(syncteststream, corrday);

  // Get time of first frame
  mark5_stream_get_frame_time(syncteststream, &mjd, &sec, &ns);

  nsperbyte = syncteststream->framens/(double)syncteststream->framebytes;
  endns2000 = static_cast<uint64_t>((((mjd-51544)*24*60*60+sec)*1e9 + ns)+nsperbyte*bufferinfo[buffersegment].validbytes);

  // Loops over frames looking times past the end of the expected segment end
  while (mark5_stream_next_frame(syncteststream)==0) {

    mark5_stream_get_frame_time(syncteststream, &mjd, &sec, &ns);
    ns2000 = static_cast<uint64_t>(((mjd-51544)*24*60*60+sec)*1e9 + ns);

    if (ns2000 > endns2000) {
      // Copy the rest of the bytes to a temporary buffer for next read and
      // Mark buffer as smaller than expected
      goodbytes = syncteststream->frame - &databuffer[buffersegment*(bufferbytes/numdatasegments)];
      if (tempbuf==0) {
        tempbuf = vectorAlloc_u8(bufferbytes/numdatasegments);
        if (tempbuf==NULL) {
          cerror << startl << "Datastream " << mpiid << " could not allocate temporary buffer. Skipping bytes after data jump" << endl;
        } else {
          tempbytes = bufferinfo[buffersegment].validbytes - goodbytes;
          status = vectorCopy_u8(&databuffer[buffersegment*(bufferbytes/numdatasegments)+goodbytes], tempbuf, tempbytes);
          if(status != vecNoErr) {
            cerror << startl << "Error copying in the DataStream data buffer!!!" << endl;
            tempbytes = 0;
          }
        }
        bufferinfo[buffersegment].validbytes = goodbytes;
      }
    }

    // CJP Musing
    // Could also detect missed frames here easily but then need to deal with
    // by inserting missing frames which will be messy. Probably best approach
    // would be if missing frame is detected, copy rest of buffer to temp
    // storage then fill in blanks and copy good data as needed. Left over bytes
    // Could then be dealt with the same as the current situation
  }

  delete_mark5_stream(syncteststream);
  syncteststream = 0;

  return 0;
}

static double tim(void) {
  struct timeval tv;
  double t;

  gettimeofday(&tv, NULL);
  t = (double)tv.tv_sec + (double)tv.tv_usec/1000000.0;

  return t;
}


void Mk5DataStream::fakeToMemory(int buffersegment)
{
  int nbytes;
  char * readto;
  mark5_stream *ms;

  DataStream::fakeToMemory(buffersegment);

  //get the right place to read to
  if(datamuxer) {
    readto = (char*)datamuxer->getCurrentDemuxBuffer();
    nbytes = datamuxer->getSegmentBytes();
  }
  else {
    readto =  (char*)&databuffer[buffersegment*(bufferbytes/numdatasegments)];
    nbytes = readbytes;
  }

  // Here we insert, as necessary, data framing to keep mk5mode happy
  ms = new_mark5_stream(new_mark5_stream_unpacker(1), new_mark5_format_generic_from_string(formatname) );
  if(ms == 0)
  {
    cerror << startl << "Could not create a mark5stream for frame generation! format=" << formatname << endl;
  }
  else
  {
    if(ms->genheaders != 0)
    {
      ms->genheaders(ms, nbytes/ms->framebytes, (unsigned char *)readto);
    }
    delete_mark5_stream(ms);
  }
}

void Mk5DataStream::networkToMemory(int buffersegment, uint64_t & framebytesremaining)
{
  if (!tcp && udp_offset>readbytes) { // What does this do to networkToMemory - does packet head need updating
    int skip = udp_offset-(udp_offset%readbytes);
    cinfo << startl << "DataStream " << mpiid << ": Skipping over " << skip/1024/1024 << " Mbytes" << endl;
    if (skip > readbytes)
      cinfo << startl << "     " << skip/readbytes << " segments" << endl;
    packet_segmentstart += (skip-(udp_offset%udpsize))/udpsize;
    udp_offset %= readbytes;
    packet_drop += skip/udpsize;
  }
  if(bufferinfo[buffersegment].configindex != lastconfig)
  {
    deriveFormatName(bufferinfo[buffersegment].configindex);
  }

  DataStream::networkToMemory(buffersegment, framebytesremaining);
  framebytesremaining = 2*readbytes;

  // Print UDP packet statistics

  if (!tcp && lasttime!=0.0) {
    // Print statistics.
    double delta, thistime;
    float dropped, oo, rate;

    thistime = tim();
    delta = thistime-lasttime;
    if (delta>1) {

      lasttime = thistime;

      if (npacket==0) {
        dropped = 0;
        oo = 0;
      } else {
        dropped = (float)packet_drop*100.0/(float)(npacket+packet_drop);
        oo = (float)packet_oo*100.0/(float)npacket;
      }
      rate = packet_sum/delta*8/1e6;

      cinfo << fixed << setprecision(2);
      cinfo << startl << "Packets=" << npacket << "  Dropped=" << dropped
            << "  Out-of-order=" << oo << "  Rate=" << rate << " Mbps" << endl;
      // Should reset precision

      packet_drop = 0;
      packet_oo = 0;
      packet_duplicate = 0;
      npacket = 0;
      packet_sum = 0;
    }
  } else {
    lasttime = tim();
  }
}


/****
 ASSUMPTIONS

 udp_offset is left containing number of bytes still to be consumed
 from last UDP packet. If one of more packet from next frame are
 missing then udp_offset points to END of missing data, with udpsize
 bytes are available.  udp_buf actually contains the left over bytes
 which must be dealt with before any more data is read.

*****/


int Mk5DataStream::readnetwork(int sock, char* ptr, int bytestoread, unsigned int* nread)
{
  ssize_t nr;

  if (tcp) {
    DataStream::readnetwork(sock, ptr, bytestoread, nread);
  } else { // UDP
    bool done, first;
    size_t segmentsize;
    ssize_t packet_index, next_udpoffset=0;
    vtp_psn64_t sequence, packet_segmentend, next_segmentstart=0;
    struct msghdr msg;
    struct iovec iov[2];
//    unsigned int headerpackets;

    memset(&msg, 0, sizeof(msg));
    msg.msg_iov        = &iov[0];
    msg.msg_iovlen     = 2;
    iov[0].iov_base = &sequence;
    iov[0].iov_len = sizeof(sequence);
    iov[1].iov_base = udp_buf;
    iov[1].iov_len = MAXPACKETSIZE;

//    headerpackets = (10016*2-1)/udpsize+2;

    packet_segmentend = packet_segmentstart+(bytestoread-(udp_offset%udpsize)-1)/udpsize;
    if (udp_offset>0 && udp_offset<bytestoread) packet_segmentend++;
    // Check above line is correct if udp_offset > udpsize
    segmentsize = packet_segmentend-packet_segmentstart+1;

    //cinfo << startl << "****** udpsize " << udpsize << "  bytestoread " << bytestoread << endl;
    //cinfo << startl << "****** udp_offset = " << udp_offset << endl;
    //cinfo << startl << "****** readnetwork will read packets " << packet_segmentstart << " to " << packet_segmentend << "(" << segmentsize << ")" << endl;

    if (segmentsize>packets_arrived.size()) {
      cfatal << startl << "Mk5DataStream::readnetwork bytestoread too large (" << bytestoread << "/" << segmentsize << ")" << endl;
      cinfo << startl << "packet_segmentstart = " << packet_segmentstart << endl;
      cinfo << startl << "packet_segmentend = " << packet_segmentend << endl;
      cinfo << startl << "segmentsize = " << segmentsize << endl;
      cinfo << startl << "udp_offset = " << udp_offset << endl;
      cinfo << startl << "udpsize = " << udpsize << endl;
      cinfo << startl << "packets_arrived.size() = " << packets_arrived.size() << endl;

      MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (unsigned int i=0; i<segmentsize; i++) {
      packets_arrived[i] = false;
    }
    *nread = 0;
    done = 0;

    // First copy left over bytes from last packet
    if (udp_offset>0) {
      packet_index = (udp_offset-1)/udpsize;
      udp_offset = (udp_offset-1)%udpsize+1;
      if (packet_index<segmentsize) // Check not out of range
        packets_arrived[packet_index] = true;

      if (bytestoread<udp_offset+packet_index*udpsize) {
        if (packet_index>=segmentsize) {
          if (udp_offset==udpsize)
            next_segmentstart = packet_segmentend+1;
          else
            next_segmentstart = packet_segmentend;

          next_udpoffset = udp_offset + packet_index*udpsize-bytestoread;
          packet_drop+=segmentsize;

        } else if (packet_index==0) {
          memcpy(ptr, udp_buf, bytestoread);
          packet_sum += bytestoread;
          npacket++;
          udp_offset -= bytestoread;
          if (udp_offset>0) {
            memmove(udp_buf, udp_buf+bytestoread, udp_offset);
            next_segmentstart = packet_segmentend;
          } else {
            next_segmentstart = packet_segmentend+1;
          }
          next_udpoffset = udp_offset;

        } else {
          int bytes = (bytestoread-udp_offset)%udpsize;
          memcpy(ptr+udp_offset+(packet_index-1)*udpsize, udp_buf, bytes);
          packet_sum += bytes;
          npacket++;
          memmove(udp_buf,udp_buf+bytes, udpsize-bytes);
          udp_offset = udpsize-bytes;
          next_segmentstart = packet_head;  // CHECK THIS IS CORRECT
        }
        done = 1;
#warning "WFB: The next statement is fishy to me.  Shouldn't this be the actual number of bytes memcpyed"
        *nread = bytestoread;

      } else {
        if (packet_index==0) { // Partial packet
          memcpy(ptr, udp_buf, udp_offset);
          packet_sum += udp_offset;
          npacket++;
        } else {
          memcpy(ptr+udp_offset+(packet_index-1)*udpsize, udp_buf, udpsize);
          npacket++;
          packet_sum += udpsize;
        }
      }
    } else {
      udp_offset = udpsize;
    }

    first = 1;
    while (!done) {
      int expectedsize = udpsize+sizeof(vtp_psn64_t);
      nr = recvmsg(sock,&msg,MSG_WAITALL);
      if (nr==-1) { // Error reading socket
        if (errno == EINTR) continue;
        return(nr);
      } else if (nr==0) {  // Socket closed remotely
        cinfo << startl << "Datastream " << mpiid << ": Remote socket closed" << endl;
        return(nr);
      } else if (nr!=expectedsize) {
        cfatal << startl << "DataStream " << mpiid << ": Expected " << expectedsize << " bytes, got " << nr << "bytes. Quitting" << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
      } else {

        if (packet_head==0 && sequence!=0) { // First packet!
          packet_head = sequence;
          cinfo << startl <<  "DataStream " << mpiid << ": First packet has sequence " << sequence << endl;

          packet_segmentstart = sequence;
          packet_segmentend = packet_segmentstart+(bytestoread-1)/udpsize;
        }

        // Check this is a sensible packet
        packet_index = sequence-packet_segmentstart;
        if (packet_index<0) {
          // If this was much smaller we really should assume the sequence has got screwed up and
          // Resync
          packet_oo++;  // This could be duplicate but we cannot tell
          // Probably should decrease packet dropped count, maybe (it was not counted after all)
        } else if (sequence==packet_segmentend) {
          //cinfo << startl << "**Segmentend " << packet_index << " (" << packet_segmentend << ")" << endl;
          int bytes;
          if (udp_offset==udpsize && segmentsize==1)
            bytes = bytestoread;
          else
            bytes = (bytestoread-udp_offset-1)%udpsize+1;
          // Consistence check
          if (bytes<0) {
            cfatal << startl << "Datastream: Error read too many UDP packets!!" << endl;
            cfatal << startl << "   " << bytestoread << " " << udp_offset << " " << bytes << endl;
            MPI_Abort(MPI_COMM_WORLD, 1);
          } else if (bytes > udpsize) {
            cfatal << startl << "Datastream: Error have not read enough UDP packets!!" << endl;
            cfatal << startl << "   " << bytestoread << " " << udp_offset << " " << bytes << endl;
            MPI_Abort(MPI_COMM_WORLD, 1);
          }
          memcpy(ptr+udp_offset+(packet_index-1)*udpsize,udp_buf,bytes);
          packet_sum += bytes;
          memmove(udp_buf,udp_buf+bytes, udpsize-bytes);
          next_udpoffset = udpsize-bytes;
          if (next_udpoffset==0) {
            next_segmentstart = sequence+1;
            npacket++;
          } else
            next_segmentstart = sequence;
          packet_head = sequence;
#warning "WFB: This next statement is fishy to me: what if some leftover bytes were copied?  That count gets lost."
          *nread = bytestoread;
          packets_arrived[packet_index] = true;
          done = 1;
        } else if (packet_index>=segmentsize) {
          //cinfo << startl << "*********Past Segmentend " << sequence << endl;

          if (udp_offset==udpsize)
            next_segmentstart = packet_segmentend+1;
          else
            next_segmentstart = packet_segmentend;

          next_udpoffset = udpsize-(bytestoread-udp_offset)%udpsize+udpsize*(sequence-packet_segmentend);
          //cinfo << startl << "**********udp_offset= " << next_udpoffset << endl;
          packet_head = sequence;
          *nread = bytestoread;
          done = 1;
        } else if (packets_arrived[packet_index]) {
          //cinfo << startl << "**** Duplicate " << packet_index << endl;
          packet_duplicate++;
        } else {
          // This must be good data finally!
          if (first) {
            first = false;
            //cinfo << startl << "**** First packet " << packet_index << endl;
          }
          packets_arrived[packet_index] = true;
          memcpy(ptr+udp_offset+(packet_index-1)*udpsize,udp_buf,udpsize);
          packet_sum += udpsize;
          *nread += udpsize;

          // Do packet statistics
          if (sequence<packet_head) { // Got a packet earlier than the head
            packet_oo++;
          } else {
            packet_head = sequence;
          }
          npacket++;
        }
      }
    }

    // Replace missing packets with fill data
//    bool headermissed = false;
    for (unsigned int i=0; i<segmentsize; i++) {
      if (!packets_arrived[i]) {
        //cinfo << startl << "***** Dropped " << packet_segmentstart+i << "(" << i << ")" << endl;
        packet_drop++; // Will generally count dropped first packet twice
        if (i==0) {
          memcpy(ptr, invalid_buf, udp_offset); // CHECK INITIAL FULL OR EMPTY BUFFER
        } else if (i==packet_segmentend-packet_segmentstart) {
          if (static_cast<int>(udp_offset+(i-1)*udpsize+(bytestoread-udp_offset)%udpsize) > bytestoread)
            cwarn << startl << "Internal Error, trying to copy pass buffer size" << endl;
          else
            memcpy(ptr+udp_offset+(i-1)*udpsize,invalid_buf,(bytestoread-udp_offset)%udpsize);
        } else {
           memcpy(ptr+udp_offset+(i-1)*udpsize,invalid_buf,udpsize);
        }
//	if (i<headerpackets) headermissed = true;
      }
    }
    /*
    if (headermissed) {
       int i;
       for (i=1; i<segmentsize; i++) {
         if (packets_arrived[i]) break;
       }
       cverbose << startl << "Dropped  packets " << packet_segmentstart << " -- "  << packet_segmentstart+i-1 << endl;
    }
    */
    packet_segmentstart = next_segmentstart;
    udp_offset = next_udpoffset;
  }

  return(1);
}

uint64_t Mk5DataStream::openframe()
{
  // The number of segments per "frame" is arbitrary. Just set it to ~ 5sec
  int nsegment;
  nsegment = (int)(2.0e9/bufferinfo[0].nsinc+0.1);

  isnewfile = true;
  return readbytes*nsegment;
}
// vim: shiftwidth=2:softtabstop=2:expandtab
