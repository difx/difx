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

#ifndef __VEX_SETUP_H__
#define __VEX_SETUP_H__

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include "vex_if.h"
#include "vex_channel.h"
#include "vex_stream.h"
#include "vex_extension.h"

class VexSetup	// Container for all antenna-specific settings
{
public:
	enum SetupType		// Determines how parsing of channel information is performed
	{
		SetupIncomplete,
		SetupTracks,
		SetupS2,
		SetupBitstreams,
		SetupDatastreams,
		SetupMergedDatastreams
	};

	static const char setupTypeName[][20];

	VexSetup() : type(SetupIncomplete), streams(1) {};
	float phaseCalIntervalMHz() const;
	float phaseCalBaseMHz() const;
	void sortChannels();				// sorts by channel name
	bool hasUniqueRecordChans() const;		// true if each channel's recordChan parameter is unique
	void assignRecordChans();
	double firstTuningForIF(const std::string &ifLink) const;	// returns Hz
	double averageTuningForIF(const std::string &ifLink) const;	// returns Hz
	double dataRateMbps() const;
	void setPhaseCalInterval(float phaseCalIntervalMHz);
	void setPhaseCalBase(float phaseCalBaseMHz);
	void selectTones(enum ToneSelection selection, double guardBandMHz);
	bool usesFormat(enum VexStream::DataFormat format) const;
	size_t nStream() const { return streams.size(); }
	size_t nRecordChan() const;		/* number of channels presumed to have been recorded, per vex file */
	size_t nPresentChan() const;		/* number of channels thought to be present in the actual data (c.f. threadsAbsent) */
	unsigned int getBits() const;
	unsigned int getMinBits() const;
	unsigned int getMaxBits() const;
	double getLowestSampleRate() const;	/* samples per second */
	double getHighestSampleRate() const;	/* samples per second */
	double getAverageSampleRate() const;	/* samples per second */
	bool hasDuplicateSubbands() const;
	int getPolarizations() const;
	int getConvertedPolarizations() const;
	VexStream *getVexStreamByLink(const std::string streamLink);
	VexChannel *getVexChannelByLink(const std::string chanLink);
	VexIF *getVexIFByLink(const std::string &ifLink);
	const VexIF *getVexIFByLink(const std::string &ifLink) const;

	SetupType type;
	std::map<std::string,VexIF> ifs;		// Indexed by name in the vex file, such as IF_A	FIXME: change to vector<> rather than map<> ?
	std::vector<VexChannel> channels;
	std::vector<VexStream> streams;			// or "datastreams".  
	std::vector<VexExtension> extensions;		// extensions referenced from a $MODE block
};

std::ostream& operator << (std::ostream &os, const VexSetup &x);

#endif
