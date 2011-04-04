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
public:
	enum scripttype {VLBA, EVLA, GBT};
	void open(const string& antennaName, const VexData *V, scripttype stype);
	void close();
	void addPhasingSource(const string srcname);
	int writeHeader(const VexData *V);
	int writeRecorderInit(const VexData *V);
	int writeDbeInit(const VexData *V);
	int writeLoifTable(const VexData *V);
	int writeSourceTable(const VexData *V);
	int writeScans(const VexData *V);
	int writeScansGBT(const VexData *V);
        void setDBEPersonality(string filename);

private:
	scripttype currenttype;
	string evlavcidir;
	string ant;
	string sw[4];	// 4x4 switch state
	string obsCode;
	string fileName;
	string dbefileName;
	double lastValid;
	int lastSourceId;
	int lastModeId;
	int lastChannelSet;
	int evlaintsec, evlasbchan, evlasbbits;
	double evlavciversion;
	double mjd0;
	vector<map<string,unsigned int> > ifIndex;	// for each scan, map from IF name to number
	vector<string> phasingsources;

	void calcIfIndex(const VexData *V);
	void writeVCI(const VexData *V, int modeindex, string filename);
};

#endif
