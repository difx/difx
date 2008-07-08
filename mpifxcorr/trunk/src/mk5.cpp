/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
#include "mk5.h"

Mk5Mode::Mk5Mode(Configuration * conf, int confindex, int dsindex, int fanout, int nchan, int bpersend, int gblocks, int nfreqs, double bw, double * freqclkoffsets, int ninputbands, int noutputbands, int nbits, bool fbank, bool pbin, bool pscrunch, bool postffringe, bool quaddelayinterp, bool cacorrs, int fbytes)
 : Mode(conf, confindex, dsindex, nchan, bpersend, gblocks, nfreqs, bw, freqclkoffsets, ninputbands, noutputbands, nbits, PAYLOADSIZE*fanout+nchan*2, fbank, pbin, pscrunch, postffringe, quaddelayinterp, cacorrs, bw*2)
{
  //work out the number of bytes and samples in a frame
  framesamples = PAYLOADSIZE*fanout;
  framebytes = fbytes;

  //create the VLBA_format struct used for unpacking - this checks that the format has been set correctly
  vf = new_VLBA_format(numbits, numinputbands, fanout, conf->getMkVFormat(confindex, dsindex));
}


Mk5Mode::~Mk5Mode()
{
  delete_VLBA_format(vf);
}

void Mk5Mode::unpack(int sampleoffset)
{
  int status, framesin;

  //work out where to start from
  framesin = (sampleoffset/framesamples);
  unpackstartsamples = framesin*framesamples;

  //unpack one frame plus one FFT size worth of samples
  status = VLBA_unpack(vf, (char*)(&(data[framesin*framebytes])), unpackedarrays, unpacksamples);
  if(status != 0)
    cerr << "Error trying to unpack MkV format data at sampleoffset " << sampleoffset << " from buffer seconds " << bufferseconds << " plus " << buffermicroseconds << " microseconds!!!" << endl;
}


Mk5DataStream::Mk5DataStream(Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
 : DataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
  //each data buffer segment contains an integer number of frames, because thats the way config determines max bytes
}

Mk5DataStream::~Mk5DataStream()
{}

int Mk5DataStream::calculateControlParams(int offsetsec, int offsetsamples)
{
  int bufferindex, framesin, vlbaoffset, payloadbytesperframe;
   
  bufferindex = DataStream::calculateControlParams(offsetsec, offsetsamples);
  
  //do the necessary correction to start from a frame boundary - work out the offset from the start of this segment
  vlbaoffset = bufferindex - atsegment*readbytes;
  payloadbytesperframe = (PAYLOADSIZE*fanout*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
  framesin = vlbaoffset/payloadbytesperframe;

  bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][0] = bufferinfo[atsegment].seconds + (double(bufferinfo[atsegment].nanoseconds) + double(((framesin*(framebytes-headerbytes))*bufferinfo[atsegment].bytespersampledenom)/bufferinfo[atsegment].bytespersamplenum)* bufferinfo[atsegment].sampletimens)/1000000000.0;
  
  //go back to nearest frame
  return atsegment*readbytes + framesin*framebytes;
}

void Mk5DataStream::updateConfig(int segmentindex)
{
  //run the default update config, then add additional information specific to Mk5
  DataStream::updateConfig(segmentindex);
  if(bufferinfo[segmentindex].configindex < 0) //If the config < 0 we can skip this scan
    return;

  framebytes = config->getFrameBytes(bufferinfo[segmentindex].configindex, streamnum);
  fanout = config->getFanout(bufferinfo[segmentindex].configindex, streamnum);
  numbits = config->getDNumBits(bufferinfo[segmentindex].configindex, streamnum);

  //correct the nsinc - should be number of frames*frame time
  bufferinfo[segmentindex].nsinc = int(((bufferbytes/numdatasegments)/framebytes)*bufferinfo[segmentindex].sampletimens*PAYLOADSIZE*fanout);

  //take care of the case where an integral number of frames is not an integral number of blockspersend - ensure sendbytes is long enough
  bufferinfo[segmentindex].sendbytes = int(((((double)bufferinfo[segmentindex].sendbytes)* ((double)config->getBlocksPerSend(bufferinfo[segmentindex].configindex)))/(config->getBlocksPerSend(bufferinfo[segmentindex].configindex) + config->getGuardBlocks(bufferinfo[segmentindex].configindex)) + 0.99));

  //if its inserted headers (VLBA), work out the headerbytes
  if(isVLBA) {
    headerbytes = (framebytes/FRAMESIZE)*(FRAMESIZE-PAYLOADSIZE);
  }
  else {
    headerbytes = 0;
  }
}

void Mk5DataStream::initialiseFile(int configindex, int fileindex)
{
  bool beforelastok, lastok;
  int frameday, skipbytes, mboffset, lastoffset, offsetbefore, deltaday;
  double frameseconds;
  char skipbuffer[MAX_MKV_SKIP];

  //check every MB into the file for 10 MB until we get two in a row that agree - this handles the case of possible sync errors at the start of a Mk5 file
  lastok = false;
  mboffset = 0;
  while(mboffset < 10 && !(lastok && beforelastok))
  {
    cout << "About to open file " << fileindex << " at mboffset " << mboffset << endl;
    beforelastok = lastok;
    offsetbefore = lastoffset;

    //open the file with vlbastream
    vs = VLBA_stream_open(datafilenames[configindex][fileindex].c_str(), numbits, fanout, mboffset*1048576/*offset*/);
    if(vs != 0 && (vs->format == FORMAT_VLBA || vs->format == FORMAT_MARK4)) //sync was found successfully
    {
      lastok = true;
      if(beforelastok && (((mboffset*1048576 + vs->fileoffset - lastoffset) % vs->gulpsize) != 0))
        //the two offsets do not agree - must still be syncing up
        beforelastok = false;

      lastoffset = mboffset*1048576 + vs->fileoffset;
    }
    else
      lastok = false;
    mboffset++;
    if(vs != 0)
      VLBA_stream_close(vs);
  }

  //check to see if we found good data
  if(!beforelastok || !lastok) //obviously didn't
  {
    cerr << "File " << datafilenames[configindex][fileindex] << " could not find consistent sync in first 10 MB - aborting!!!" << endl;
    dataremaining = false;
  }

  //check for consistency - open file back up at right spot
  vs = VLBA_stream_open(datafilenames[configindex][fileindex].c_str(), numbits, fanout, (mboffset-2)*1048576);
  if(vs->nchan != config->getDNumInputBands(configindex, streamnum))
  {
    cerr << "Error - number of input bands for datastream " << streamnum << " (" << config->getDNumInputBands(configindex, streamnum) << ") does not match with MkV file " << datafilenames[configindex][fileindex] << " (" << vs->nchan << "), will be ignored!!!" << endl;
  }

  //set date
  skipbytes = offsetbefore;
  frameday = int(vs->mjd);
  if(vs->format == FORMAT_VLBA)
  {
    deltaday = ((corrstartday-frameday + 500)/1000)*1000;
    frameday += deltaday;
    isVLBA = true;
  }
  else
    isVLBA = false;
  frameseconds = vs->sec;

  readseconds = 86400*(frameday-corrstartday) + int(frameseconds-corrstartseconds) + intclockseconds;
  readnanoseconds = int(1000000000.0*(frameseconds-int(frameseconds)) + 0.5);
  if(frameseconds < corrstartseconds && readnanoseconds != 0) //handle rounding error for negative numbers
    readseconds--;
  cout << "The frame start day is " << frameday << ", the frame start seconds is " << frameseconds << ", corrstartseconds is " << corrstartseconds << " while readseconds is " << readseconds << ", readnanoseconds is " << readnanoseconds << ", intclockseconds is " << intclockseconds << endl;
  cout << "int(frameseconds-corrstartseconds) is " << int(frameseconds-corrstartseconds) << endl;

  //close vlbastream
  VLBA_stream_close(vs);

  cout << "About to read " << skipbytes << " bytes to get to the first frame" << endl;

  //skip the bytes before the first sync word
  for(int i=0;i<skipbytes/MAX_MKV_SKIP;i++)
    input.read(skipbuffer, MAX_MKV_SKIP);
  input.read(skipbuffer, skipbytes - (skipbytes/MAX_MKV_SKIP)*MAX_MKV_SKIP);

  //update all the configs - to ensure that the nsincs and headerbytes are correct
  for(int i=0;i<numdatasegments;i++)
    updateConfig(i);
}
