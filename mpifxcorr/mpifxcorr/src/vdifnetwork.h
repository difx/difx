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
	int readrawnetworkVDIF(int sock, char* ptr, int bytestoread, unsigned int* nread, int packetsize, int stripbytes);
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
