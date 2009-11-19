/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __PY_STREAM_H__
#define __PY_STREAM_H__

#include <fstream>
#include <string>
#include "vextables.h"

class pystream : public ofstream
{
private:
	string ant;
	string sw[4];	// 4x4 switch state
	string obsCode;
	double lastValid;
	int lastSourceId;
	int lastModeId;
	int lastChannelSet;
	vector<map<string,unsigned int> > ifIndex;	// for each scan, map from IF name to number

	void calcIfIndex(const VexData *V);
public:
	void open(const string& antennaName, const VexData *V);
	void close();
	int writeHeader(const VexData *V);
	int writeRecorderInit(const VexData *V);
	int writeDbeInit(const VexData *V);
	int writeLoifTable(const VexData *V);
	int writeSourceTable(const VexData *V);
	int writeScans(const VexData *V);
};

#endif
