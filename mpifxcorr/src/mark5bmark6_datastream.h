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
#ifndef MARK5BMARK6FILE_H
#define MARK5BMARK6FILE_H

#include <mark6sg/mark6gather.h>
#include <difxmessage.h>
#include "mark5bfile.h"

/**
@class Mark5BMark6DataStream 
@brief Datastream which can handle Mark5B formatted data coming straight off a Mark6 module

This class manages a stream of data from a Mark6 module, coarsely aligning it with the geocentre and sending segments of data to Core nodes for processing as directed by the FxManager.  

@author Adam Deller, Walter Brisken
*/
class Mark5BMark6DataStream : public Mark5BDataStream
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
  Mark5BMark6DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
  virtual ~Mark5BMark6DataStream();
  virtual void openfile(int configindex, int fileindex);
  int sendMark6Activity(enum Mark6State mark6state, long long position, double dataMJD, float rate);

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

  DifxMessageMark6Activity mark6activity;

  Mark6Gatherer *mark6gather;  /* structure for Mark6 file gathering */
  bool mark6eof;  /* if true, current file has been exhausted */
  long long bytecount;
  long long lastbytecount;
  time_t msgsenttime;
  float mbyterate;
  double fmjd;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
