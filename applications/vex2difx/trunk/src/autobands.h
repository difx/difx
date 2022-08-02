/***************************************************************************
 *   Copyright (C) 2009-2017 by Walter Brisken & Adam Deller               *
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
 * $HeadURL: $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __AUTOBANDS_H__
#define __AUTOBANDS_H__

#include <string>
#include <vector>
#include <deque>
#include <iostream>
#include <limits>

class ZoomFreq;
class freq;

#include "zoomfreq.h"	// class ZoomFreq from vex2difx
#include "freq.h"		// class freq from vex2difx

class AutoBands
{
public:
	AutoBands() : outputbandwidth(-1), verbosity(0), permitgaps(false) { }
	AutoBands(double outputbandwidth_Hz, int verbosity=0, bool permitgaps=false);
	~AutoBands();

public:

	/// Helper class that describes a spectral region provided by one particular antenna,
	/// this can be a recorded band or a zoom band or just a virtual band.
	class Band {
	public:
		double flow;		///< Start frequency of the spectral region in Hz
		double fhigh;		///< Stop frequency of the spectral in Hz
		int antenna;		///< Arbitrary but unique antenna identifier

		Band(double flow_, double fhigh_, int antenna_)
			: flow(flow_),fhigh(fhigh_),antenna(antenna_) { }

		double bandwidth() const;
		bool operator==(const freq& rhs) const;
		bool operator==(const Band& rhs) const;
		friend std::ostream& operator << (std::ostream &os, const AutoBands::Band &x);
	};

	/// Helper class that describes one continuous spectral region in sky frequency,
	/// and holds the count of antenna-bands that can supply this spectral region
	class Span {
	public:
		double flow;
		double fhigh;
		int antennacount;	///< Number of antennas that can provide this spectral region
		int bandcount;		///< Number of bands that can provide this spectral region; can exceed 'antennacount'!
		bool continued;		///< Flag, true if there exists (elsewhere) another Span such that this->fhigh == other->flow

		Span(double flow_, double fhigh_, int antcount_, int bandcount_)
			: flow(flow_),fhigh(fhigh_),antennacount(antcount_),bandcount(bandcount_),continued(false) { }
		static bool compare_bandwidths(const AutoBands::Span& lhs, const AutoBands::Span& rhs);
		double bandwidth() const;
		friend std::ostream& operator << (std::ostream &os, const AutoBands::Span &x);
	};

	/// Class Outputband describes a spectral window intended for final visibility data output.
	/// The class also holds a collection of Bands (zooms, recorded) that without overlap
	/// nor gaps assemble together to fully cover the spectral window.
	class Outputband {
	public:
		double fbandstart;	///< Start frequency of spectral window in Hz
		double bandwidth;	///< Bandwidth of spectral window in Hz
		std::vector<Band> constituents;	///< Collection of bands that produce/cover the total 'bandwidth' - if the Outputband is complete

		/// C'stor, describe a spectral window and start from an initially empty list of constituents
		Outputband(double fbandstart_Hz, double bandwidth_Hz) : fbandstart(fbandstart_Hz),bandwidth(bandwidth_Hz)
		{
			this->constituents.clear();
		}

		/// Copy C'stor required for storage std::vector<AutoBands::Outputband>
		Outputband(const Outputband& o)
		{
			this->fbandstart = o.fbandstart;
			this->bandwidth = o.bandwidth;
			this->constituents = o.constituents;
		}

		// Default C'stor
		Outputband()
		{
			this->fbandstart = 0;
			this->bandwidth = 0;
			this->constituents.clear();
		}

		/// Grow the list of consituent bands by one
		void extend(double fstart, double bw);

		/// Grow the list of consituent bands by one
		void extend(const Band& b) { this->extend(b.flow, b.bandwidth()); }

		/// Return the total bandwidth of the outputband constituents
		double constituentsBandwidth() const;

		/// Check whether constituent band(s) fully cover the preset 'bandwidth' of the outputband
		bool isComplete() const;

		bool operator==(const freq& rhs) const;
		bool operator==(const Outputband& rhs) const;
		Band& operator[] (int n) { return constituents[n]; }
		friend std::ostream& operator << (std::ostream &os, const AutoBands::Outputband &x);
	};

public:

	/// Return automatically determined best-fit common bandwidth, or 0 on auto-detect failure
	double autoBandwidth();

	/// Set target bandwidth to be used when generating output bands, c.f. generateOutputbands()
	void setBandwidth(double outputbandwidth_Hz);

	/// Get pre-set target bandwidth
	double getBandwidth() const { return outputbandwidth; }

	/// Reinitialize
	void clear();

	/// Add information about the recorded bands of an antenna
	void addRecbands(const std::vector<double>& fstart, const std::vector<double>& fstop, int antId = -1);

	/// Add information about the recorded bands of an antenna
	void addRecbands(const std::vector<freq>& freqs, int antId = -1);

	/// Return the greatest-common-divisor of a list of frequencies
	double getGranularity(const std::vector<double>& freqs) const;

	/// Automatically build a set of outputbands and their constituent bands,
	/// based upon previously registered (cf. addRecbands()) recorded bands.
	/// If explicit outputband positions were registered (cf. addUserOutputbands())
	/// these will attempt to be generated. Otherwise, bands are auto-generated.
	int generateOutputbands(int Nant_min=0, double fstart_Hz=0.0);

	/// Add user-specified outputbands with a single consituent equal to a vex2difx ZoomFreq
	void addUserOutputbands(const std::vector<ZoomFreq>& zf);

	/// Add user-specified outputband with a single consituent equal to a vex2difx ZoomFreq
	void addUserOutputband(const ZoomFreq& zf);

	/// Locate 'inputfreq' among consituents of iternal outputbands, then locate that outputband in 'allfreqs'.
	int lookupDestinationFreq(const freq& inputfreq, const std::vector<freq>& allfreqs) const;

	/// Create a list of intra-outputband channels that contain no imaginary component (edge channels of consitituents)
	int listEdgeChannels(const Outputband& band, std::deque<int>& channels, double fftSpecRes_Hz, double finalSpecRes_Hz, double maxFraction = 0) const;

	int listEdgeChannels(const int bandIdx, std::deque<int>& channels, double fftSpecRes_Hz, double finalSpecRes_Hz, double maxFraction = 0) const
	{ return listEdgeChannels(this->outputbands[bandIdx], channels, fftSpecRes_Hz, finalSpecRes_Hz, maxFraction); }

	void setVerbosity(int verbosity);

	//variables
	std::vector<Band> bands;
	std::vector<Span> spans;
	std::vector<Outputband> outputbands; // auto/final
	std::vector<Outputband> userOutputbands; // user-registered explicit bands to consider
	double minrecfreq, maxrecfreq;
	unsigned int Nant;
	double outputbandwidth;
	bool permitgaps;

private:
	int verbosity;

private:

	/// Check if freq range falls in its entirety inside any of the recorded bands, at *all* antennas
	bool covered(double f0, double f1) const;

	/// Determine frequency spans where at least Nant_min antennas overlap. Populates ::spans via ::bands.
	void analyze(int Nant_min=0);

	/// Based on user-registered outputbands, try to fill them with parts of available recorded bands.
	int generateOutputbandsExplicit(int Nant_min=0);

	/// Automatically deduce "good" outputbands to fill the available spectrum
	int generateOutputbandsAutomatic(int Nant_min=0, double fstart_Hz=0.0);

	/// Simplify an outputband definition by merging its list of consituent bands where possible,
	/// trimming away any overlapped portions of consituent bands.
	void simplify(AutoBands::Outputband& mergeable, int Nant_min=0) const;

	//TODO: double adjustStartFreq(double f0, double f1, double finitial, double df);

	void barchart(std::ostream& os,
		const std::vector<double>& start,
		const std::vector<double>& stop,
		const std::vector<int>& idtag,
		const int startmarker='a',
		double xmin=std::numeric_limits<double>::quiet_NaN(),
		double xmax=std::numeric_limits<double>::quiet_NaN()) const;

private:
	friend std::ostream& operator << (std::ostream &os, const AutoBands &x);

};

std::ostream& operator << (std::ostream& os, const AutoBands& x);
std::ostream& operator << (std::ostream& os, const AutoBands::Band& x);
std::ostream& operator << (std::ostream& os, const AutoBands::Span& x);
std::ostream& operator << (std::ostream& os, const AutoBands::Outputband& x);

#endif
