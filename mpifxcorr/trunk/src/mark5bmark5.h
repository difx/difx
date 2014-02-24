/***************************************************************************
 *   Copyright (C) 2007-2013 by Walter Brisken and Adam Deller             *
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

#ifndef Mark5BMARK5_H
#define Mark5BMARK5_H

#include <pthread.h>
#include <time.h>
#include "mode.h"
#include "datastream.h"
#include "config.h"

#ifdef HAVE_XLRAPI_H
#include "mark5dir.h"
#endif

#include "mark5bfile.h"
#include <difxmessage.h>

class Mark5BMark5DataStream : public Mark5BDataStream
{
public:
	Mark5BMark5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
	virtual ~Mark5BMark5DataStream();
	virtual void initialiseFile(int configindex, int fileindex);
	virtual void openfile(int configindex, int fileindex);
	virtual int calculateControlParams(int scan, int offsetsec, int offsetns);
	int sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate);
	int resetDriveStats();
	int reportDriveStats();

protected:
#ifdef HAVE_XLRAPI_H
	void setDiscModuleState(SSHANDLE xlrDevice, const char *newState);
	virtual int dataRead(int buffersegment);
	static void *launchmark5threadfunction(void *self);
	void mark5threadfunction();
	void servoMark5();
	virtual void loopfileread();
#endif

private:
#ifdef HAVE_XLRAPI_H
	Mark5Module module;
	int scanNum;
	const Mark5Scan *scanPointer;
	long long readpointer, readend;
	SSHANDLE xlrDevice;
	unsigned int readbufferslots, readbufferslotsize;
	pthread_t mark5thread;
	pthread_mutex_t *mark5threadmutex;
#ifdef __APPLE__
#include "pthreadbarrier.h"
#endif
	pthread_barrier_t mark5threadbarrier;
	bool mark5xlrfail;
	bool mark5threadstop;
	int lockstart, lockend, lastslot;
	unsigned int endindex, fixindex;
	unsigned int readbufferwriteslot;
	double jobEndMJD;
#endif

	DifxMessageMk5Status mk5status;

	int filltime;
	long long invalidstart;
	int newscan;
	double lastrate;
	int nrate;
	int nError;
	int nDMAError;
	bool noMoreData;
	bool noDataOnModule;
	int readDelayMicroseconds;
	int nReads;

	void openStreamstor();
	void closeStreamstor();
	void resetStreamstor();
};

#endif
