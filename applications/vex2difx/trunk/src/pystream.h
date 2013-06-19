/***************************************************************************
 *   Copyright (C) 2009-2013 by Walter Brisken, Adam Deller                *
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
#include <vector>
#include <map>
#include <string>
#include <cstring>
#include "vextables.h"
#include "vlbadefaults.h"

class pystream : public std::ofstream
{
public:
	enum ScriptType {SCRIPT_VLBA, SCRIPT_EVLA, SCRIPT_GBT};
	enum PersonalityType {RDBE_UNKNOWN, RDBE_NONE, RDBE_PFB, RDBE_DDC};
	enum RecorderType {RECORDER_NONE, RECORDER_MARK5C};
	enum DataFormat {FORMAT_NONE, FORMAT_UNKNOWN, FORMAT_MIXED, FORMAT_VLBA, FORMAT_MARK5B, FORMAT_VDIF};

	pystream(): scriptType(SCRIPT_VLBA), personalityType(RDBE_UNKNOWN), recorderType(RECORDER_MARK5C), dataFormat(FORMAT_NONE), lastValid(0.0), lastSourceId(-1), lastModeId(-1), lastChannelSet(-1), evlaIntSec(0), evlasbChan(0), evlasbBits(0), evlaVCIVersion(0.0), mjd0(0.0), isMark5A(false) {};
	void open(const std::string& antennaName, const VexData *V, ScriptType sType);
	void close();
	void addPhasingSource(const std::string &sourceName);
	void figurePersonality(const VexData *V);
	int maxIFs(const VexData *V) const;
	int writeHeader(const VexData *V);
	int writeComment(const std::string &commentString);
	int writeRecorderInit(const VexData *V);
	int writeDbeInit(const VexData *V);
	void writeXCubeInit();
	void writeImplicitConversionComment(const std::vector<unsigned int> &implicitConversions);
	int writeChannelSet(const VexSetup *setup, int modeNum);
	int writeDDCChannelSet(const VexSetup *setup, int modeNum);
	int writeChannelSet5A(const VexSetup *setup, int modeNum);
	int writeLoifTable(const VexData *V);
	int writeDDCLoifTable(const VexData *V);
	int writeSourceTable(const VexData *V);
	int writeDDCSourceTable(const VexData *V);
	int writeScans(const VexData *V);
	int writeDDCScans(const VexData *V);
	int writeScansGBT(const VexData *V);
	void setDBEPersonality(const std::string &filename);
	void setDBEPersonalityType(PersonalityType pt) { personalityType = pt; }
	PersonalityType getDBEPersonalityType() { return personalityType; }
	void setRecorderType(RecorderType rt) { recorderType = rt; }
	void setDataFormat(DataFormat df) { dataFormat = df; }
	DataFormat getDataFormat() { return dataFormat; }
	void setMark5A(bool x) { isMark5A = x; }

private:
	ScriptType scriptType;
	PersonalityType personalityType;
	RecorderType recorderType;
	DataFormat dataFormat;
	std::string evlaVCIDir;
	std::string ant;
	std::string sw[4];	// 4x4 switch state
	int swInUse;
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
	std::vector<bool> loifSetupFirstUse;

	void calcIfIndex(const VexData *V);
	void writeVCI(const VexData *V, int modeIndex, const std::string &filename);

	// determine how many DBEs we will need
	int numDBE;
	int numDBEChan;
    bool need2DBEs;
    char dbeAppend[2]; // {'a'|'b'} '\0'
	// which modes need 2DBEs by mode
    std::vector<bool> need2DBEbyMode;
	// how many channels per IF by mode
	std::vector<std::vector<int> > chanPerIF;
	// how many IF in use by mode
	std::vector<int> IFinUse;
	// IF numbers ordered by how many channels are assigned to each; highest at index 0; by mode
	std::vector<std::vector<int> > orderedIFNums;
	// which IF is assigned to which DBE/IF by mode
    std::vector<std::vector<std::vector<int> > > IF2DBEassign;
	// how many IF are in use by DBE and by mode
    std::vector<std::vector<int> > IFinUseByDBE;
	// if IF has more than MAX_IF_PER_DBE channels assigned this tells us how to split them across DBEs
	std::vector<std::vector<int> > chanByDBE;
	// with which channel distribution are we going?
	std::vector<int> chanDist;
	// channels assigned to DBEs
	std::vector<std::vector<int> > chanAssign;
};

#endif
