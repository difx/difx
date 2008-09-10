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
#ifndef MK5_H
#define MK5_H

#include "mode.h"
#include "datastream.h"
#include "mark5access.h"

/** 
 @class Mk5Mode 
 @brief A mode for Mk4 formatted or VLBA formatted Mk5 data

 A mode for MkIV, VLBA, or Mark5B formatted Mk5 data.  All types of band setup should be supported.
 @author Adam Deller
 */
class Mk5Mode : public Mode
{
public:
 /**
  * Constructor: calls Mode constructor then creates a mark5_format struct, using Walter Brisken's mark5access library
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param confindex The index of the configuration this Mode is for
  * @param dsindex The index of the datastream this Mode is for
  * @param nchan The number of channels per subband
  * @param bpersend The number of FFT blocks to be processed in a message
  * @param gblocks The number of additional guard blocks at the end of a message
  * @param nfreqs The number of frequencies for this Mode
  * @param bw The bandwidth of each of these IFs
  * @param freqclkoffsets The time offsets in microseconds to be applied post-F for each of the frequencies
  * @param ninputbands The total number of subbands recorded
  * @param noutputbands The total number of subbands after prefiltering - not currently used (must be = numinputbands)
  * @param nbits The number of bits per sample
  * @param fbank Whether to use a polyphase filterbank to channelise (instead of FFT)
  * @param postffringe Whether fringe rotation takes place after channelisation
  * @param quaddelayinterp Whether delay interpolataion from FFT start to FFT end is quadratic (if false, linear is used)
  * @param cacorrs Whether cross-polarisation autocorrelations are to be calculated
  * @param fsamples The number of samples in a frame per channel
  */
  Mk5Mode(Configuration * conf, int confindex, int dsindex, int nchan, int bpersend, int gblocks, int nfreqs, double bw, double * freqclkoffsets, int ninputbands, int noutputbands, int nbits, bool fbank, bool postffringe, bool quaddelayinterp, bool cacorrs, int framebytes, int framesamples, Configuration::dataformat format);
  virtual ~Mk5Mode();

protected:
 /** 
  * Uses mark5access library to unpack multiplexed, quantised data into the separate float arrays
  * @return The fraction of samples returned
  * @param sampleoffset The offset in number of time samples into the data array
  */
  virtual float unpack(int sampleoffset);

  int framesamples, framebytes, samplestounpack, fanout;
  struct mark5_stream *mark5stream;
};

/**
@class Mk5DataStream 
@brief Datastream which can handle Mk5 formatted data

This class manages a stream of data from a disk or memory, coarsely aligning it with the geocentre and sending segments of 
data to Core nodes for processing as directed by the FxManager.  Mk5Datastream overrides the LBA-style defaults and
implements appropriate functionality for Mk5 formatted data

@author Adam Deller
*/
class Mk5DataStream : public DataStream
{
public:
 /**
  * Constructor: Just passes the parameters on to the Datastream constructor
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param snum This Datastream's index (numbered from 0)
  * @param id This Datastream's MPI id
  * @param ncores The number of Cores in this correlation
  * @param cids Array containing the MPI ids of each Core
  * @param bufferfactor The size of the buffer, in terms of number of "max send sizes" - the biggest "blocks per send*numchannels" from the possible configurations
  * @param numsegments The number of separate segments this buffer will be divided into
  */
  Mk5DataStream(Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
  virtual ~Mk5DataStream();

  virtual void initialise();

protected:
  #include <vector>
 /** 
  * Calculates the correct offset from the start of the databuffer for a given time in the correlation, and calculates
  * the geometric delays at the start and end of each FFT block as control information to pass to the Cores.  Allows for
  * the fact that Mk5 data must be sent as an integer number of frames, starting at a frame boundary
  * @param offsetsec The offset in seconds from the start of the correlation
  * @param offsetns The offset in ns from the given second
  * @return The offset in bytes from the start of the databuffer that this block should start from - must be between 0 and bufferlength
  */
  virtual int calculateControlParams(int offsetsec, int offsetns);

 /** 
  * Updates all the parameters (numchannels, sendbytes etc) for the specified segment of the databuffer.  Allows for
  * the fact that Mk5 data must be sent as an integer number of frames, starting at a frame boundary
  * @param segmentindex The index of the segment to be updated
  */
  virtual void updateConfig(int segmentindex);

 /** 
  * Reads in the header information from a Mk5 formatted file and sets the current segment time information accordingly
  * @param configindex The config index at the current time
  * @param fileindex The number of the file to be opened
  */
  virtual void initialiseFile(int configindex, int fileindex);

  virtual void networkToMemory(int buffersegment, int & framebytesremaining);
  
  void initialiseNetwork(int configindex, int buffersegment);

  virtual int readnetwork(int sock, char* ptr, int bytestoread, int* nread);

  virtual int openframe();

  int framebytes, payloadbytes, framespersecond;

  int lastconfig;

  // UDP values and statistics
  int npacketperframe, udpsize;
  unsigned long packet_drop, packet_oo, packet_duplicate, npacket, udp_offset;
  unsigned long long packet_sum, packet_head, packet_segmentstart;
  double lasttime;
  char *udp_buf, *invalid_buf;
  vector<bool> packets_arrived;

};

int genFormatName(Configuration::dataformat format, int nchan, double bw, int nbits, int framebytes, int decimationfactor, char *formatname);

#endif
