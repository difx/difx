/***************************************************************************
 *   Copyright (C) 2016-2017 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __DIRLIST_DATUM_MARK5_H__
#define __DIRLIST_DATUM_MARK5_H__

#include <set>
#include <ostream>
#include "dirlist_datum.h"

class DirListDatumMark5 : public DirListDatum
{
public:
	virtual ~DirListDatumMark5() { }
	bool setFromOldString(const char *str);
	long long getStart() const { return start; }
	virtual long long getLength() const { return length; }
	int getIntSec() const { return intSec; }
	int getFrameNumInSecond() const { return frameNumInSecond; }
	int getFramesPerSecond() const { return framesPerSecond; }
	int getFrameBytes() const { return frameBytes; }
	int getFrameOffset() const { return frameOffset; }
	virtual int getTracks() const { return tracks; }
	int getFormat() const { return format; }
	virtual void print(std::ostream &os, bool doEOL = true) const;
	virtual long long getStartPointer() const { return start + frameOffset; }
	virtual bool setFromTokens(const std::vector<std::string> &tokens);

private:
	long long start;	/* start pointer on disk */
	long long length;	/* number of bytes */
	int intSec;		/* integer second portion of timestamp of first frame */
	int frameNumInSecond;	/* frame number since last 1 second tick */
	int framesPerSecond;	/* number of frames per second (always integer) */
	int frameBytes;		/* length of entire frame in bytes */
	int frameOffset;	/* bytes to start of first frame */
	int tracks;
	int format;
	std::set<int> startThreads;	// set of threads present at beginning of the stream
	std::set<int> endThreads;	// set of threads present at end of the stream
};

std::ostream& operator << (std::ostream &os, const DirListDatumMark5 &x);

#endif
