/***************************************************************************
 *   Copyright (C) 2007-2016 by Walter Brisken and Adam Deller             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/nativemk5.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef VDIFSHAREDMEMORY_H
#define VDIFSHAREDMEMORY_H

#include <pthread.h>
#include <time.h>
#include "mode.h"
#include "datastream.h"
#include "config.h"

#include "vdiffile.h"
#include "sharedmemorybuffer.h"
#include <difxmessage.h>

#ifdef __APPLE__
#include "pthreadbarrier.h"
#endif

class VDIFSharedMemoryDataStream : public VDIFDataStream
{
public:
	VDIFSharedMemoryDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
	virtual ~VDIFSharedMemoryDataStream();
	virtual void initialiseFile(int configindex, int fileindex);
protected:
	virtual int dataRead(int buffersegment);
	static void *launchshmthreadfunction(void *self);
	void shmthreadfunction();
	virtual void loopnetworkread();
	int setsharedmemorybuffersecond(int second);
	void sharedmemorybuffercopy(char * dest, int copysize, unsigned int *bytescopied);

private:
	int readbufferslots;
	unsigned int readbufferslotsize;
	pthread_t shmthread;
	pthread_mutex_t *shmthreadmutex;
	pthread_barrier_t shmthreadbarrier;
	bool shmthreadstop;
	int lockstart, lockend, lastslot;
	unsigned int endindex, muxindex;
	int readbufferwriteslot;
	double jobEndMJD;

	// shared memory parameters
	SharedMemoryBuffer *shmbuffer;
	int shmbufferslot;	// current "second" slot of SharedMemoryBuffer
	int shmbufferframe;	// current frame number within "second" slot
	int shmbuffersecond;	// current actual second being read
};

#endif
