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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef NATIVEMK5_H
#define NATIVEMK5_H

#include <pthread.h>
#include <time.h>
#include <mark5access.h>
#include "mode.h"
#include "datastream.h"
#include "config.h"

#ifdef HAVE_XLRAPI_H
#include <xlrapi.h>
#include <dirlist/dirlist.h>
#endif

#include "mk5.h"
#include <difxmessage.h>

class NativeMk5DataStream : public Mk5DataStream
{
public:
	NativeMk5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
	virtual ~NativeMk5DataStream();
	virtual void initialiseFile(int configindex, int fileindex);
	virtual void openfile(int configindex, int fileindex);
	virtual void loopfileread();
	virtual int calculateControlParams(int scan, int offsetsec, int offsetns);
	virtual int readonedemux(bool resetreference, int buffersegment);
	int moduleRead(u32 * destination, int nbytes, long long start, int buffersegment);
	int sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate);
	int resetDriveStats();
	int reportDriveStats();

protected:
	void moduleToMemory(int buffersegment);
#ifdef HAVE_XLRAPI_H
	void setDiscModuleState(SSHANDLE xlrDevice, const char *newState);
#endif

private:
#ifdef HAVE_XLRAPI_H
        DirList dirlist;
        int scanNum;
	const DirListDatumMark5 *scanPointer;
	long long readpointer;
	SSHANDLE xlrDevice;
#endif

	DifxMessageMk5Status mk5status;

	int invalidtime;
	int filltime;
	long long invalidstart;
	u32 lastval;
	struct mark5_stream *mark5stream;
	int newscan;
	double lastrate;
	int nrate;
	int nError;
        int nDMAError;
	bool noMoreData;
        bool noDataOnModule;
	int nfill, ninvalid, ngood;
        int readDelayMicroseconds;
	int nReads;

        void openStreamstor();
        void closeStreamstor();
        void resetStreamstor();
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
