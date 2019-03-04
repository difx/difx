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

#ifndef Mark5BMARK5_H
#define Mark5BMARK5_H

#include <pthread.h>
#include <time.h>
#include "mode.h"
#include "datastream.h"
#include "config.h"

#ifdef HAVE_XLRAPI_H
#include <xlrapi.h>
#include <dirlist/dirlist.h>
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
	DirList dirlist;
	int scanNum;
	const DirListDatumMark5 *scanPointer;
	long long readpointer, readend;
	SSHANDLE xlrDevice;
	int readbufferslots;
	unsigned int readbufferslotsize;
	pthread_t mark5thread;
	pthread_mutex_t *mark5threadmutex;
#ifdef __APPLE__
#include "pthreadbarrier.h"
#endif
	pthread_barrier_t mark5threadbarrier;
	bool mark5xlrfail;
	volatile bool mark5threadstop;
	int lockstart, lockend, lastslot;
	unsigned int endindex, fixindex;
	int readbufferwriteslot;
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
	unsigned int readDelayMicroseconds;
	int nReads;

	void openStreamstor();
	void closeStreamstor();
	void resetStreamstor();
};

#endif
