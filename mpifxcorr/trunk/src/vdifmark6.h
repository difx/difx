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
// $Id: vdiffile.h 6214 2014-08-20 22:15:54Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mk5.h $
// $LastChangedRevision: 6214 $
// $Author: WalterBrisken $
// $LastChangedDate: 2014-08-20 17:15:54 -0500 (Wed, 20 Aug 2014) $
//
//============================================================================
#ifndef VDIFMARK6FILE_H
#define VDIFMARK6FILE_H

#include <vdifio.h>
#include "vdiffile.h"

/**
@class VDIFMark6DataStream 
@brief Datastream which can handle VDIF formatted data coming straight off a Mark6 module

This class manages a stream of data from a Mark6 module, coarsely aligning it with the geocentre and sending segments of data to Core nodes for processing as directed by the FxManager.  

@author Adam Deller, Walter Brisken
*/
class VDIFMark6DataStream : public VDIFDataStream
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
  VDIFMark6DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
  virtual ~VDIFMark6DataStream();
  virtual void openfile(int configindex, int fileindex);

protected:
 /** 
  * Reads in the header information from a Mk5 formatted file and sets the current segment time information accordingly
  * @param configindex The config index at the current time
  * @param fileindex The number of the file to be opened
  */
  virtual void initialiseFile(int configindex, int fileindex);

  virtual int dataRead(int buffersegment);

  virtual void mark6ToMemory(int buffersegment);

  virtual void loopfileread();

private:
  void closeMark6();

  int mark6fd;  /* file descriptor for use within mark6sg library */
  bool mark6eof;  /* if true, current file has been exhausted */
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
