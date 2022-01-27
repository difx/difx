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

#ifndef __VEX_CHANNEL_H__
#define __VEX_CHANNEL_H__

#include <iostream>
#include <string>
#include <vector>
#include <difxio.h>

class VexChannel		// Antenna-specific baseband channel details
{
public:
	VexChannel() : recordChan(-1), subbandId(-1), bbcFreq(0.0), bbcBandwidth(0.0), bbcSideBand(' '), threadId(0) {}
	void selectTones(float toneIntervalMHz, float toneBaseMHz, enum ToneSelection selection, double guardBandMHz);
	char bandCode() const;
	double centerFreq() const;		// returns Hz
	friend bool operator ==(const VexChannel &c1, const VexChannel &c2);
	friend bool operator <(const VexChannel &c1, const VexChannel &c2);
	friend bool sameTuning(const VexChannel &c1, const VexChannel &c2);

	int recordChan;				// channel number on recorded media or threadnum on stream	(< 0 indicates non-recording)
	int subbandId;				// 0-based index; -1 means unset
	double bbcFreq;				// sky frequency tuning of the BBC (Hz)
	double bbcBandwidth;			// bandwidth (Hz)
	char bbcSideBand;			// sideband of the BBC
	std::string bandLink;			// column 1 of the $FREQ chan_def line; may be unset as this parameter is optional
	std::string chanName;			// column 8 of the $FREQ chan_def line, but fill in with column 5 if not present
	std::string ifLink;			// name of the IF this channel came from
	std::string chanLink;			// column 5 of the $FREQ chan_def line
	std::string bbcName;			// name given in VEX of this channel in the BBC table
	std::string phaseCalName;		// name of phase cal setup for this channel
	std::vector<unsigned int> tones;	// pulse cal tones to extract, directly from PHASE_CAL_DETECT
	int threadId;				// thread Id for this channel (assigned based on channel names)
};

std::ostream& operator << (std::ostream &os, const VexChannel &x);

#endif
