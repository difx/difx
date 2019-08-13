/***************************************************************************
 *   Copyright (C) 2006-2016 by Adam Deller                                *
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
#include <mpi.h>
#include "mk5mode.h"
#include "mk5.h"
#include "alert.h"

Mk5Mode::Mk5Mode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling, Configuration::complextype tcomplex, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, int framebytes, int framesamples, Configuration::dataformat format)
  : Mode(conf, confindex, dsindex, recordedbandchan, chanstoavg, bpersend, gsamples, nrecordedfreqs, recordedbw, recordedfreqclkoffs, recordedfreqclkoffsdelta, recordedfreqphaseoffs, recordedfreqlooffs, nrecordedbands, nzoombands, nbits, sampling, tcomplex, recordedbandchan*2+4, fbank, linear2circular, fringerotorder, arraystridelen, cacorrs, recordedbw*2)
{
  char formatname[64];

  fanout = config->genMk5FormatName(format, nrecordedbands, recordedbw, nbits, sampling, framebytes, conf->getDDecimationFactor(confindex, dsindex), conf->getDNumMuxThreads(confindex, dsindex), formatname);
  invalid = 0;
  if(fanout < 0)
    initok = false;
  else
  {
    // since we allocated the max amount of space needed above, we need to change
    // this to the number actually needed.
    this->framesamples = framesamples;
    if (usecomplex) {
      unpacksamples = recordedbandchan;
      samplestounpack = recordedbandchan;
    } else {
      unpacksamples = recordedbandchan*2;
      samplestounpack = recordedbandchan*2;
    }
    //create the mark5_stream used for unpacking
    mark5stream = new_mark5_stream( new_mark5_stream_unpacker(0), new_mark5_format_generic_from_string(formatname) );
    if(mark5stream == 0)
    {
      cfatal << startl << "Mk5Mode::Mk5Mode : mark5stream is null" << endl;
      initok = false;
    }
    else
    {
      if(conf->isNetwork(dsindex))
        mark5stream->blanker = blanker_none;
      if(mark5stream->samplegranularity > 1)
        samplestounpack += mark5stream->samplegranularity;
      string orig_streamname(mark5stream->streamname);
      sprintf(mark5stream->streamname, "DS%d <%s>", dsindex, orig_streamname.c_str());
      if(framesamples != mark5stream->framesamples)
      {
        cfatal << startl << "Mk5Mode::Mk5Mode : framesamples inconsistent (told " << framesamples << "/ stream says " << mark5stream->framesamples << ") - for stream index " << dsindex << endl;
        initok = false;
      }
      else
      {
        this->framesamples = mark5stream->framesamples;
      }
      if(format == Configuration::INTERLACEDVDIF)
      {
        invalid = new int[nrecordedbands];
        perbandweights = new f32*[config->getNumBufferedFFTs(configindex)];
        for(int i=0;i<config->getNumBufferedFFTs(configindex);++i)
        {
          perbandweights[i] = new f32[nrecordedbands];
          for(int b = 0; b < nrecordedbands; ++b)
          {
            perbandweights[i][b] = 0.0;
          }
        }
      }
    }
  }
}

Mk5Mode::~Mk5Mode()
{
  delete_mark5_stream(mark5stream);
  if(invalid)
  {
    delete [] invalid;
  }
}

float Mk5Mode::unpack(int sampleoffset, int subloopindex)
{
  float goodsamples;
  int mungedoffset = 0;

  //work out where to start from
  unpackstartsamples = sampleoffset - (sampleoffset % mark5stream->samplegranularity);

  //unpack one frame plus one FFT size worth of samples
  if(usecomplex) 
  {
    goodsamples = mark5_unpack_complex_with_offset(mark5stream, data, unpackstartsamples, (mark5_float_complex**)unpackedcomplexarrays, samplestounpack);
  }
  else
  {
    goodsamples = mark5_unpack_with_offset(mark5stream, data, unpackstartsamples, unpackedarrays, samplestounpack);
  }
  if(mark5stream->samplegranularity > 1)
    { // CHRIS not sure what this is mean to do
      // WALTER: unpacking of some mark5 modes (those with granularity > 1) must be unpacked not as individual samples but in groups of sample granularity
    int erasedsamples = 0;

    mungedoffset = sampleoffset % mark5stream->samplegranularity;
    for(int i = 0; i < mungedoffset; i++) {
      for(int b = 0; b < mark5stream->nchan; ++b) {
        if(unpackedarrays[b][i] != 0.0) {
          unpackedarrays[b][i] = 0.0;
          erasedsamples++;
        }
      }
    }
    for(int i = unpacksamples + mungedoffset; i < samplestounpack; i++) {
      for(int b = 0; b < mark5stream->nchan; ++b) {
        if(unpackedarrays[b][i] != 0.0) {
          unpackedarrays[b][i] = 0.0;
          erasedsamples++;
        }
      }
    }
    goodsamples -= erasedsamples/(float)(mark5stream->nchan);
  }
  if(perbandweights)
  {
    int totalinvalid;

    if(usecomplex)
    {
      blank_vdif_EDV4_complex(data, unpackstartsamples, (mark5_float_complex**)unpackedcomplexarrays, samplestounpack, invalid);
    }
    else
    {
      blank_vdif_EDV4(data, unpackstartsamples, unpackedarrays, samplestounpack, invalid);
    }

    totalinvalid = 0;
    for(int b = 0; b < mark5stream->nchan; ++b)
    {
      perbandweights[subloopindex][b] = (goodsamples - invalid[b])/(float)unpacksamples;
      totalinvalid += invalid[b];
    }

    goodsamples -= (float)totalinvalid/(float)(mark5stream->nchan);
  }

  if(goodsamples < 0)
  {
    cerror << startl << "Error trying to unpack Mark5 format data at sampleoffset " << sampleoffset << " from data seconds " << datasec << " plus " << datans << " ns!!!" << endl;
    goodsamples = 0;
    for(int b = 0; b < mark5stream->nchan; ++b)
      invalid[b] = 0;
  }

  return goodsamples/(float)unpacksamples;
}
// vim: shiftwidth=2:softtabstop=2:expandtab
