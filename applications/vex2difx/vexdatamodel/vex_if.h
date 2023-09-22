/***************************************************************************
 *   Copyright (C) 2015-2021 by Walter Brisken & Adam Deller               *
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

#ifndef __VEX_IF_H__
#define __VEX_IF_H__

#include <iostream>
#include <string>
#include <vector>

class VexIF
{
public:
	enum SwitchedPowerAmplitude
	{
		SP_unset,			// Vex file did not indicate a setting for switched power
		SP_off,
		SP_low,
		SP_high,
		SP_error
	};

	static const char SwitchedPowerAmplitudeName[][8];

	VexIF() : ifSSLO(0.0), ifSideBand(' '), pol(' '), phaseCalIntervalMHz(0.0), phaseCalBaseMHz(0.0), ifSampleRate(0.0), spAmp(SP_unset), spFreq(0.0) {}
	std::string bandName() const;
	std::string VLBABandName() const;
	double getLowerEdgeFreq() const;

	std::string ifLink;			// Use to connect this IF to other bits
	std::string ifName;			// Name of the IF; this should only be used casually, not to navigate the IF structure
	double ifSSLO;				// [Hz] SSLO of the IF
	char ifSideBand;			// U or L or D (D sideband not supported yet)
	char pol;				// R or L
	float phaseCalIntervalMHz;		// MHz, typically 1 or 5 (or 0 if none)
	float phaseCalBaseMHz;			// MHz, typically 0 (VEX Rev 1.5b1, 2002)
	double ifSampleRate;			// [samples/sec] non-zero only for digital IFs
	std::string rxName;			// Name of the receiver the IF is connected to
	std::vector<double> upstreamSSLO;	// Signed sum of upstream (downconverter) LO settings in signal path order
	std::vector<char> upstreamSideBand;	// U or L or D (D sideband not supported yet)
	enum SwitchedPowerAmplitude spAmp;
	double spFreq;				// [Hz]

	// special values needed for VLBA, extracted from comment line
	std::string comment;
};

enum VexIF::SwitchedPowerAmplitude stringToSwitchedPowerAmplitude(const char *s);

std::ostream& operator << (std::ostream &os, const VexIF &x);

#endif
