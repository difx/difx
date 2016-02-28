/***************************************************************************
 *   Copyright (C) 2015-2016 by Walter Brisken & Adam Deller               *
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

#ifndef __VEX_MODE_H__
#define __VEX_MODE_H__

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <difxio.h>
#include "vex_subband.h"
#include "vex_setup.h"

class VexMode
{
public:
	VexMode() {}

	int addSubband(double freq, double bandwidth, char sideband, char pol);
	int getPols(char *pols) const;
	int getBits() const;
	int getMinBits() const;
	int getMaxBits() const;
	int getMinSubbands() const;
	int nRecordChan(const std::string &antName) const;
	int nStream() const;
	const VexSetup* getSetup(const std::string &antName) const;
	double getLowestSampleRate() const;	/* samples per second */
	double getHighestSampleRate() const;	/* samples per second */
	double getAverageSampleRate() const;	/* samples per second */
	void swapPolarization(const std::string &antName);
	void setSampling(const std::string &antName, unsigned int streamId, enum SamplingType dataSampling);
	void setPhaseCalInterval(const std::string &antName, int phaseCalIntervalMHz);
	void selectTones(const std::string &antName, enum ToneSelection selection, double guardBandMHz);
	void generateRecordChans();

	std::string defName;

	std::vector<VexSubband> subbands;
	std::vector<VexSubband> zoombands;
	std::map<std::string,VexSetup> setups;	// indexed by antenna name
};

std::ostream& operator << (std::ostream &os, const VexMode &x);

#endif
