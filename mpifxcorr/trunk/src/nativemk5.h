/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken and Adam Deller                  *
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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef NATIVEMK5_H
#define NATIVEMK5_H

#include "mode.h"
#include "datastream.h"
#include "mark5access.h"

#ifdef HAVE_XLRAPI_H
#include "mark5dir.h"
#endif

#include "mk5.h"
#include <difxmessage.h>

class NativeMk5DataStream : public Mk5DataStream
{
public:
	NativeMk5DataStream(Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
	virtual ~NativeMk5DataStream();
	virtual void initialiseFile(int configindex, int fileindex);
	virtual void openfile(int configindex, int fileindex);
	virtual void loopfileread();
	virtual int calculateControlParams(int scan, int offsetsec, int offsetns);

protected:
	void moduleToMemory(int buffersegment);
	int sendMark5Status(enum Mk5State state, int scanNumber, long long position, double dataMJD, float rate);

private:
#ifdef HAVE_XLRAPI_H
	struct Mark5Module module;
	struct Mark5Scan *scan;
	long long readpointer;
	SSHANDLE xlrDevice;
#endif

	DifxMessageMk5Status mk5status;

	int executeseconds;
	int invalidtime;
	int filltime;
	long long invalidstart;
	unsigned long lastval;
	struct mark5_stream *mark5stream;
	int newscan;
	double lastrate;
	int nrate;
	int nError;
	bool nomoredata;
	int nfill, ninvalid, ngood;
};

#endif
