/***************************************************************************
 *   Copyright (C) 2015-2022 by Walter Brisken & Adam Deller               *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __VEX_ANTENNA_H__
#define __VEX_ANTENNA_H__

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <difxio.h>
#include "interval.h"
#include "vex_clock.h"
#include "vex_basebanddata.h"
#include "vex_networkdata.h"
#include "vex_extension.h"

bool isVLBA(const std::string &antName);

class VexAntenna
{
public:
	// Note: keep the enum and nasmythName in vex_antenna.cpp in sync
	enum NasmythType
	{
		NasmythNone = 0, 
		NasmythRight, 
		NasmythLeft,

		NasmythError		// list terminator / error
	};
	static const char nasmythName[][8];

	VexAntenna() : x(0.0), y(0.0), z(0.0), dx(0.0), dy(0.0), dz(0.0), posEpoch(0.0), axisOffset(0.0), tcalFrequency(0), polConvert(false) {}

	double getVexClocks(double mjd, double *coeffs) const;		// This version of the function is deprecated
	double getVexClocks(double mjd, double *coeffs, int *clockorder, int maxorder) const;
	bool hasClockModel() const { return (!clocks.empty()); }
	bool hasData(const Interval &timerange) const;
	void removeBasebandData(int streamId);
	bool hasVSNs() const { return !vsns.empty(); }
	bool isVLBA() const { return ::isVLBA(defName); }
	void setAntennaPolConvert(bool doConvert) { polConvert = doConvert; }
	NasmythType getNasmyth(const std::string &bandLink) const;

	std::string name;		// Deprecated
	std::string defName;		// if name != defName, things may be very confused...
	std::string difxName;		// Name to be used in difx
	std::string twoCharSiteCode;	// Not likely used...
	std::string oneCharSiteCode;	// Used by mark4 data processing path; set to NULL if not provided

	double x, y, z;		// (m) antenna position in ITRF
	double dx, dy, dz;	// (m/sec) antenna velocity
	double posEpoch;	// mjd
	std::string axisType;
	double axisOffset;	// (m)
	std::vector<VexClock> clocks;
	int tcalFrequency;	// Hz
	bool polConvert;	// If true, swap polarization basis RL->XY or XY->RL

	// actual baseband data is associated with the antenna 
	std::vector<VexBasebandData> vsns;	// indexed by vsn number
	std::vector<VexBasebandData> files;	// indexed by file number
	std::vector<VexNetworkData> ports;	// indexed by stream number
	std::vector<VexExtension> extensions;	// extensions linked from $STATION block

	std::map<std::string, NasmythType> nasmyth;	// maps band link to nasmyth type; default to NasmythNone if not found

	// Some antenna/site things that are not based on vex parameters (but could be carried as an extension)

};

VexAntenna::NasmythType stringToNasmyth(const std::string &platform);

bool usesCanonicalVDIF(const std::string &antName);

std::ostream& operator << (std::ostream &os, const VexAntenna &x);

#endif
