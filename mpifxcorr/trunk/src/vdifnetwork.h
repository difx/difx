/***************************************************************************
 *   Copyright (C) 2007-2014 by Walter Brisken and Adam Deller             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/nativemk5.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef VDIFNETWORK_H
#define VDIFNETWORK_H

#include <pthread.h>
#include <time.h>
#include "mode.h"
#include "datastream.h"
#include "config.h"

#include "vdiffile.h"
#include <difxmessage.h>

#ifdef __APPLE__
#include "pthreadbarrier.h"
#endif

class VDIFNetworkDataStream : public VDIFDataStream
{
public:
	VDIFNetworkDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
	virtual ~VDIFNetworkDataStream();
	virtual void initialiseFile(int configindex, int fileindex);
protected:
	virtual int dataRead(int buffersegment);
	static void *launchnetworkthreadfunction(void *self);
	void networkthreadfunction();
	virtual void loopnetworkread();

private:
	int readbufferslots;
	unsigned int readbufferslotsize;
	pthread_t networkthread;
	pthread_mutex_t *networkthreadmutex;
	pthread_barrier_t networkthreadbarrier;
	bool networkthreadstop;
	int lockstart, lockend, lastslot;
	unsigned int endindex, muxindex;
	int readbufferwriteslot;
	double jobEndMJD;

	// network parameters
	int sock;
	int skipbytes;		// number of bytes to trim off beginning of packets
};

#endif
