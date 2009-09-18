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
#include <mpi.h>
#include "mk5mode.h"
#include "mk5.h"
#include "alert.h"

Mk5Mode::Mk5Mode(Configuration * conf, int confindex, int dsindex, int nchan, int bpersend, int gblocks, int nfreqs, double bw, double * freqclkoffsets, int ninputbands, int noutputbands, int nbits, bool fbank, bool postffringe, bool quaddelayinterp, bool cacorrs, int framebytes, int framesamples, Configuration::dataformat format)
  : Mode(conf, confindex, dsindex, nchan, bpersend, gblocks, nfreqs, bw, freqclkoffsets, ninputbands, noutputbands, nbits, nchan*2+4, fbank, postffringe, quaddelayinterp, cacorrs, bw*2)
{
  char formatname[64];

  fanout = config->genMk5FormatName(format, ninputbands, bw, nbits, framebytes, conf->getDecimationFactor(confindex), formatname);
  if(fanout < 0)
    initok = false;
  else
  {
    // since we allocated the max amount of space needed above, we need to change
    // this to the number actually needed.
    unpacksamples = nchan*2;
  
    samplestounpack = nchan*2;
    if(fanout > 1)
      samplestounpack += fanout;
  
    //create the mark5_stream used for unpacking
    mark5stream = new_mark5_stream(
        new_mark5_stream_unpacker(0),
    new_mark5_format_generic_from_string(formatname) );
  
    if(mark5stream == 0)
    {
      cfatal << startl << "Mk5Mode::Mk5Mode : mark5stream is null " << endl;
      initok = false;
    }
    else
    {
      sprintf(mark5stream->streamname, "DS%d", dsindex);
      if(framesamples != mark5stream->framesamples)
      {
        cfatal << startl << "Mk5Mode::Mk5Mode : framesamples inconsistent (" << framesamples << "/" << mark5stream->framesamples << ")" << endl;
        initok = false;
      }
      else
      {
        this->framesamples = mark5stream->framesamples;
      }
    }
  }
}

Mk5Mode::~Mk5Mode()
{
  delete_mark5_stream(mark5stream);
}

float Mk5Mode::unpack(int sampleoffset)
{
  int framesin, goodsamples;
  int mungedoffset;

  //work out where to start from
  framesin = (sampleoffset/framesamples);
  unpackstartsamples = sampleoffset - (sampleoffset % mark5stream->samplegranularity);

  //unpack one frame plus one FFT size worth of samples
  goodsamples = mark5_unpack_with_offset(mark5stream, data, unpackstartsamples, unpackedarrays, samplestounpack);
  if(mark5stream->samplegranularity > 1)
  {
    mungedoffset = sampleoffset % mark5stream->samplegranularity;
    for(int i = 0; i < mungedoffset; i++)
      if(unpackedarrays[0][i] != 0.0)
        goodsamples--;
    for(int i = unpacksamples + mungedoffset; i < samplestounpack; i++)
      if(unpackedarrays[0][i] != 0.0)
        goodsamples--;
  }
    
  if(goodsamples < 0)
  {
    cerror << startl << "Error trying to unpack Mark5 format data at sampleoffset " << sampleoffset << " from buffer seconds " << bufferseconds << " plus " << buffermicroseconds << " microseconds!!!" << endl;
    goodsamples = 0;
  }

  return goodsamples/(float)unpacksamples;
}
