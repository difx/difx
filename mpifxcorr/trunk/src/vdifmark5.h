/***************************************************************************
 *   Copyright (C) 2007-2020 by Walter Brisken and Adam Deller             *
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

#ifndef VDIFMARK5_H
#define VDIFMARK5_H

#include <time.h>
#include "mode.h"
#include "datastream.h"
#include "config.h"

#ifdef HAVE_XLRAPI_H
#include <xlrapi.h>
#include <dirlist/dirlist.h>
#endif

#include "vdiffile.h"
#include <difxmessage.h>

class VDIFMark5DataStream : public VDIFDataStream
{
public:
	VDIFMark5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
	virtual ~VDIFMark5DataStream();
	virtual void initialiseFile(int configindex, int fileindex);
	virtual void launchreadthreadfunction(void *self);
	virtual void openfile(int configindex, int fileindex);
	virtual int calculateControlParams(int scan, int offsetsec, int offsetns);
	int sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate);
	int resetDriveStats();
	int reportDriveStats();

protected:
#ifdef HAVE_XLRAPI_H
	void setDiscModuleState(SSHANDLE xlrDevice, const char *newState);
	void readthreadfunction();
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
