/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken, Adam Deller                *
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
 * $Id: optsources.h 4205 2012-01-05 01:03:11Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/optresources.h $
 * $LastChangedRevision: 4205 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2012-01-04 18:03:11 -0700 (Wed, 04 Jan 2012) $
 *
 *==========================================================================*/

#ifndef __RESOURCES_STREAM_H__
#define __RESOURCES_STREAM_H__

#include <fstream>
#include <string>
#include <vector>
#include <map>
#include "vextables.h"

class optresources : public std::ofstream
{
public:
	enum PersonalityType {RDBE_UNKNOWN, RDBE_NONE, RDBE_PFB, RDBE_DDC};
	enum RecorderType {RECORDER_NONE, RECORDER_MARK5C};

	optresources(): personalityType(RDBE_UNKNOWN), recorderType(RECORDER_MARK5C), lastValid(0.0), lastSourceId(-1), lastModeId(-1), lastChannelSet(-1), evlaIntSec(0), evlasbChan(0), evlasbBits(0), evlaVCIVersion(0.0), mjd0(0.0), isMark5A(false) {};
	void open(const std::string& antennaName, const VexData *V);
	void close();
	void addPhasingSource(const std::string &sourceName);
	void figurePersonality(const VexData *V);
	int maxIFs(const VexData *V) const;
	int writeHeader(const VexData *V);
	int writeComment(const std::string &commentString);
	int writeRecorderInit(const VexData *V);
	int writeDbeInit(const VexData *V);
	void writeImplicitConversionComment(const std::vector<unsigned int> &implicitConversions);
	int writeChannelSet(const VexSetup *setup, int modeNum);
	int writeChannelSet5A(const VexSetup *setup, int modeNum);
	int writeLoifTable(const VexData *V);
	int writeSourceTable(const VexData *V);
	int writeScans(const VexData *V);
	int writeScansGBT(const VexData *V);
        void setDBEPersonality(const std::string &filename);
	void setDBEPersonalityType(PersonalityType pt) { personalityType = pt; }
	void setRecorderType(RecorderType rt) { recorderType = rt; }
	void setMark5A(bool x) { isMark5A = x; }

private:
	PersonalityType personalityType;
	RecorderType recorderType;
	std::string evlaVCIDir;
	std::string ant;
	std::string sw[4];	// 4x4 switch state
	std::string obsCode;
	std::string fileName;
	std::string dbeFilename;
	double lastValid;
	int lastSourceId;
	int lastModeId;
	int lastChannelSet;
	int evlaIntSec, evlasbChan, evlasbBits;
	double evlaVCIVersion;
	double mjd0;
	bool isMark5A;
	std::vector<std::map<std::string,unsigned int> > ifIndex;	// for each scan, map from IF name to number
	std::vector<std::string> phasingSources;

	void calcIfIndex(const VexData *V);
	void writeVCI(const VexData *V, int modeIndex, const std::string &filename);
	std::string VLArcvr(std::string);
};

#endif
