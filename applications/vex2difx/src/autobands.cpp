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
 * $Id: autobands.cpp 10661 2022-10-04 12:42:35Z JanWagner $
 * $HeadURL: $
 * $LastChangedRevision: 10661 $
 * $Author: JanWagner $
 * $LastChangedDate: 2022-10-04 20:42:35 +0800 (二, 2022-10-04) $
 *
 *==========================================================================*/

#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstdio>

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <iterator>
#include <vector>
#include <set>

#include "autobands.h"
#include "freq.h"       // class freq
#include "zoomfreq.h"	// class ZoomFreq

template class std::vector<AutoBands::Outputband>;

////////////////////////////////////////////////////////////////////////////////////////////////////////////

double AutoBands::Band::bandwidth() const
{
	return fhigh - flow;
}

double AutoBands::Span::bandwidth() const
{
	return fhigh - flow;
}

bool AutoBands::Band::operator==(const freq& rhs) const
{
	return (rhs.fq == flow) && (rhs.bw == (fhigh-flow)) && (rhs.sideBand == 'U');
}

bool AutoBands::Band::operator==(const Band& rhs) const
{
	return (rhs.flow == flow) && (rhs.fhigh == fhigh); // purposefully ignoring ::antenna here
}

bool AutoBands::Outputband::operator==(const freq& rhs) const
{
	return (rhs.fq == fbandstart) && (rhs.bw == bandwidth) && (rhs.sideBand == 'U');
}

bool AutoBands::Outputband::operator==(const Outputband& rhs) const
{
	return (rhs.fbandstart == fbandstart) && (rhs.bandwidth == bandwidth)
		&& (rhs.constituents == constituents);
}

void AutoBands::Outputband::extend(double fstart, double bw)
{
	constituents.push_back(AutoBands::Band(fstart, fstart+bw, 0));
}

double AutoBands::Outputband::constituentsBandwidth() const
{
	double constituentbwsum = 0;
	for(std::vector<AutoBands::Band>::const_iterator b = constituents.begin(); b != constituents.end(); ++b)
	{
		constituentbwsum += b->bandwidth();
	}
	return constituentbwsum;
}

bool AutoBands::Outputband::isComplete() const
{
	double constituentbwsum = 0;
	double constituentstart = std::numeric_limits<double>::infinity();
	for(std::vector<AutoBands::Band>::const_iterator b = constituents.begin(); b != constituents.end(); ++b)
	{
		constituentbwsum += b->bandwidth();
		constituentstart = std::min(constituentstart, b->flow);
	}
	//return (constituentbwsum == bandwidth) && (constituentstart == fbandstart);
	return (fabs(constituentbwsum - bandwidth) < 1) && (fabs(constituentstart - fbandstart) < 1);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

AutoBands::AutoBands(double outputbandwidth_Hz, int verbosity, bool permitgaps)
{
	this->outputbandwidth = outputbandwidth_Hz;
	this->verbosity = verbosity;
	this->permitgaps = permitgaps;
	clear();
}

AutoBands::~AutoBands()
{
}

void AutoBands::setVerbosity(int verbosity)
{
	this->verbosity = verbosity;
}

void AutoBands::clear()
{
	this->Nant = 0;
	spans.clear();
	outputbands.clear();
}

/**
 * Set the bandwidth (Hz) of the desired outputbands.
 */
void AutoBands::setBandwidth(double outputbandwidth_Hz)
{
	outputbandwidth = outputbandwidth_Hz;
	if(outputbandwidth < 1e6)
	{
		outputbandwidth  *= 1e6;
	}
}

/**
 * Add a recorded band if it has not been added for the same antenna Id before
 */
void AutoBands::addRecbandUnique(const AutoBands::Band& recband)
{
	bool exists = false;

	// Detect if duplicate exists
	// NB: cannot use 'if(std::find(bands.begin(), bands.end(), AutoBands::Band(lo, hi, antId)) == bands.end())'
	// here since on purpose our operator== compares just start/stop freqs and ignores antenna Id
	for(std::vector<AutoBands::Band>::const_iterator b = bands.begin(); b != bands.end(); ++b)
	{
		if((*b == recband) && (b->antenna == recband.antenna))
		{
			exists = true;
			break;
		}
	}

	if(!exists)
	{
		bands.push_back(recband);
	}
}

/**
 * Add information about the recorded bands of an antenna.
 */
void AutoBands::addRecbands(const std::vector<double>& fstart, const std::vector<double>& fstop, int antId)
{
	assert(fstart.size() == fstop.size());
	if(antId == -1)
	{
		antId = Nant;
	}
	for(unsigned i = 0; i < fstart.size(); i++)
	{
		double lo = fstart[i], hi = fstop[i];
		if(lo > hi)
		{
			std::swap(lo, hi);
		}
		addRecbandUnique(AutoBands::Band(lo, hi, antId));
	}
	Nant++;
}

/**
 * Add information about the recorded bands of an antenna.
 */
void AutoBands::addRecbands(const std::vector<freq>& freqs, int antId)
{
	if(antId == -1)
	{
		antId = Nant;
	}
	for(unsigned i = 0; i < freqs.size(); i++)
	{
		double lo = freqs[i].fq;
		double hi = freqs[i].fq + ((freqs[i].sideBand == 'U') ? freqs[i].bw : -freqs[i].bw);
		if(lo > hi)
		{
			std::swap(lo, hi);
		}
		addRecbandUnique(AutoBands::Band(lo, hi, antId));
	}
	Nant++;
}

/**
 * Return the greatest-common-divisor of a list of frequencies
 */
double AutoBands::getGranularity(const std::vector<double>& args) const
{
	if(args.size() < 1)
	{
		return 1.0;
	}

	double curr_gcd = args[0];
	for(unsigned i = 1; i < args.size(); i++)
	{
		double arg = args[i];
		if(arg < curr_gcd)
		{
			std::swap(arg, curr_gcd);
		}
		while (1)
		{
			if(std::fabs(curr_gcd) < 0.001)
			{
				curr_gcd = arg;
				break;
			}
			double rem = arg - std::floor(arg / curr_gcd) * curr_gcd;
			arg = curr_gcd;
			curr_gcd = rem;
		}

	}

	// TODO: perhaps there is a tidier way for GCD in C++ than the above

	return curr_gcd;
}

/**
 * Return automatically determined best-fit common bandwidth, or 0 on failure
 */
double AutoBands::autoBandwidth()
{
	if(spans.empty())
	{
		this->analyze();
	}

	if(spans.empty())
	{
		if(verbosity > 1)
		{
			std::cout << "AutoBands::autoBandwidth(): Warning: Not enough data to determine the common bandwidth.\n";
		}
		return 0.0;
	}

	const AutoBands::Span& m = (*std::min_element(spans.begin(), spans.end(), AutoBands::Span::compare_bandwidths));
	const AutoBands::Span& M = (*std::max_element(spans.begin(), spans.end(), AutoBands::Span::compare_bandwidths));
	if(m.bandwidth() == M.bandwidth())
	{
		return m.bandwidth();
	}
	return 0.0;
}

/**
 * Check if freq range falls in its entirety inside any of the recorded bands, at *all* antennas.
 *
 * NB: Ideally we'd check only the set of definitely present antennas (v2d antennas=... + VEX scan antenna
 * list + filelist) since e.g. the most heavily channelized antenna might actually be absent and
 * thus better optimization of auto-zoombands (i.e., a reduction to fewer zooms) might be possible.
 * However, not all the required info is available from vex2difx at band construction/mapping stage to do that,
 * hence we have to include all antennas in this check even if some antennas might be 'droppped' later on.
 */
bool AutoBands::covered(double f0, double f1) const
{
	std::set<int> coverers;
	for(std::vector<AutoBands::Band>::const_iterator b = bands.begin(); b != bands.end(); ++b)
	{
		if(((*b).flow <= f0) && (f1 <= (*b).fhigh))
		{
			coverers.insert((*b).antenna);
		}
	}
	return (coverers.size() >= this->Nant);
}

/**
 * Simplify an outputband definition by merging its list of consituent bands where possible,
 * trimming away any overlapped portions of consituent bands.
 */
void AutoBands::simplify(AutoBands::Outputband& mergeable, int Nant_min) const
{
	assert(!mergeable.constituents.empty());

	if(Nant_min <= 0)
	{
		Nant_min = this->Nant;
	}

	if(verbosity > 0)
	{
		std::cout << "Simplifying outputband at " << mergeable.fbandstart*1e-6 << " bw " << mergeable.bandwidth*1e-6 << " MHz\n";
	}

	// Start from blank outputband
	Outputband merged(mergeable.fbandstart, mergeable.bandwidth);
	double f0 = mergeable.fbandstart;
	double f1 = f0 + mergeable.bandwidth;

	// Reduce bands of 'mergeable' into hopefully fewer bands in new 'merged'
	for(std::vector<AutoBands::Band>::const_iterator bnext = (mergeable.constituents.begin()) + 1; bnext != mergeable.constituents.end(); ++bnext)
	{
		f1 = bnext->fhigh;
		if(covered(f0, f1))
		{
			if(verbosity > 2)
			{
				std::cout << "    found cover for " << f0*1e-6 << "--" << f1*1e-6 << ", continuing and trying to merge-in the next band constituent\n";
			}
		}
		else
		{
			if(verbosity > 2)
			{
				std::cout << "    no simple cover for " << f0*1e-6 << "--" << f1*1e-6 << ", keeping " << f0*1e-6 << "--" << bnext->flow*1e-6 << "\n";
			}
			merged.extend(f0, bnext->flow - f0);
			f0 = bnext->flow;
			if((bnext + 1) != mergeable.constituents.end())
			{
				// set f1 ahead of exiting loop next
				f1 = (*(bnext + 1)).fhigh;
			}
		}
	}

	// Handle leftover
	if((f1 > f0) && covered(f0, f1))
	{
		merged.extend(f0, f1-f0);
	}

	// Retain the original if simplification incomplete
	if(mergeable.isComplete() && !merged.isComplete())
	{
		merged = mergeable;
	}

	// Print the details if something was merged
	if(verbosity > 1)
	{
		if(merged.constituents.size() < mergeable.constituents.size())
		{
			std::cout << "    merged " << mergeable.constituents.size()
				<< " sub-spans into " << merged.constituents.size() << "\n";
		}
		else
		{
			std::cout << "    not reducible, input had " << mergeable.constituents.size()
				<< " sub-spans, output has " << merged.constituents.size() << "\n";
		}
	}

	// Store the result
	mergeable = merged;

}

/**
 * Determine frequency spans where at least Nant antennas overlap
 * Spans are the frequency axis split at the band edges of recorded bands of every antenna.
 * Spans without freq gaps to the next span are marked as continued.
 */
void AutoBands::analyze(int Nant_min)
{
	// Init
	if(Nant_min <= 0)
	{
		Nant_min = this->Nant;
	}
	spans.clear();

	if(verbosity > 0)
	{
		std::cout << "Analyzing all registered recorded bands to detect band edges\n";
	}

	// Sorted set of all recorded band edges
	std::set<double> bandedges;
	for(unsigned i = 0; i < bands.size(); i++)
	{
		bandedges.insert(bands[i].flow);
		bandedges.insert(bands[i].fhigh);
	}

	// Convert band edge set into a list, to be able to index it
	std::vector<double> fedges;
	std::copy(bandedges.begin(), bandedges.end(), std::back_inserter(fedges));

	// Split the freq axis into spans (smallest band slices between any two rec band edges)
	for(unsigned n = 1; n < fedges.size(); n++)
	{
		double span_lo = fedges[n-1], span_hi = fedges[n];

		// Count how many antennas have this span
		int antcount = 0, bandcount = 0;
		std::set<int> antennasInSpan;
		for(unsigned k = 0; k < bands.size(); k++)
		{
			if(span_lo >= bands[k].flow && span_hi <= bands[k].fhigh)
			{
				antennasInSpan.insert(bands[k].antenna);
				bandcount++;
			}
		}
		antcount = antennasInSpan.size();

		// Keep the span if enough antennas provide data for it
		if(antcount >= Nant_min && bandcount >= Nant_min)
		{
			spans.push_back(AutoBands::Span(span_lo, span_hi, antcount, bandcount));
			if(verbosity > 2)
			{
				std::cout << "    retain  " << span_lo*1e-6 << "--" << span_hi*1e-6 << " MHz bw " << (span_hi-span_lo)*1e-6 << " with " << bandcount << " rec bands, " << antcount << " antennas\n";
			}
		}
		else if(verbosity > 2)
		{
			std::cout << "    discard " << span_lo*1e-6 << "--" << span_hi*1e-6 << " MHz bw " << (span_hi-span_lo)*1e-6 << " with " << bandcount << " rec bands, "
				<< antcount << " antennas < " << Nant_min << " antennas\n";
		}
	}

	// Mark directly adjecent spans as 'continued'; last entry defaults to .continued=False
	for(unsigned n = 1; n < spans.size(); n++)
	{
		spans[n-1].continued = (spans[n-1].fhigh == spans[n].flow);
	}
}

/**
 * Produce a set of output bands and a list of their input bands.
 *
 * The default is to automatically detect suitable positions for outputbands.
 * However, when addUserOutputband() was used, the stored explicitly specified outputbands will be filled.
 *
 * Utilizes internally stored previously added information of antenna recorded frequencies.
 * Output bands have the requested bandwidth. They can be direct matches to recorded bands, band slices (zoom) of recorded bands,
 * or pieces of several band slices (zoom sets) taken from neighbouring recorded bands.
 */
int AutoBands::generateOutputbands(int Nant_min, double fstart_Hz)
{
	// Clear old results
	outputbands.clear();

	// Make sure that spans have been detected
	if(spans.empty())
	{
		analyze(Nant_min);
	}
	if(spans.empty())
	{
		return -1;
	}

	// Generate bands in full-auto mode? Or use only explicitly specified/added bands?
	if(userOutputbands.size() <= 0)
	{
		return generateOutputbandsAutomatic(Nant_min, fstart_Hz);
	}
	else
	{
		return generateOutputbandsExplicit(Nant_min);
	}
}

/**
 * Produce a set of output bands and a list of their input bands.
 * Utilizes internally stored previously added information of antenna recorded frequencies.
 * Output bands have the requested bandwidth. They can be direct matches to recorded bands, band slices (zoom) of recorded bands,
 * or pieces of several band slices (zoom sets) taken from neighbouring recorded bands.
 */
int AutoBands::generateOutputbandsAutomatic(int Nant_min, double fstart_Hz)
{
	// Parameters
	assert (outputbandwidth > 0);
	const unsigned Nspans = spans.size();
	const double minspanfreq = spans[0].flow;
	unsigned span = 0;
	double foutstart;
	if(Nant_min <= 0)
	{
		Nant_min = this->Nant;
	}
	if(fstart_Hz > 0)
	{
		foutstart = std::max(fstart_Hz, minspanfreq);
	}
	else
	{
		foutstart = minspanfreq;
	}

	if(verbosity > 0)
	{
		std::cout << "Building automatic bands starting from " << fstart_Hz*1e-6 << " MHz with target bw " << outputbandwidth*1e-6 << " MHz\n";
	}


	// Assemble output bands using recorded bands in full or in pieces
	while (span < Nspans)
	{
		double f0 = spans[span].flow;
		double f1 = spans[span].fhigh;

		// Catch when large gaps between spans, e.g., geodetic/DDC mode
		if(foutstart < f0)
		{
			foutstart = f0;
		}
		else if(f1 < foutstart)
		{
			span++;
			continue;
		}

		// Shift start to fall on a 'more integer' MHz if possible
		// TODO

		// Generate band : insert as many bands into current span as possible
		while ((foutstart + outputbandwidth) <= f1)
		{
			if(verbosity > 1)
			{
				std::cout << "    case 1 adding " << std::setw(10) << outputbandwidth*1e-6 << " MHz bw from span " << std::setw(2) << span
					<< " @ " << std::setw(10) << (foutstart-f0)*1e-6 << " MHz "
					<< "to out#" << (int)outputbands.size() << " " << foutstart*1e-6 << " MHz\n";
			}

			AutoBands::Outputband ob(foutstart, outputbandwidth);
			ob.extend(foutstart, outputbandwidth);

			outputbands.push_back(ob);
			foutstart += outputbandwidth;

			if(verbosity > 1)
			{
				std::cout << "Building further automatic bands continuing from " << foutstart*1e-6 << " MHz\n";
			}
		}

		// Generate band : insert one band by combining smaller pieces of spans and patch over to next span(s) if needed
		double span_bw_remain = f1 - foutstart;
		if((span_bw_remain > 0) && spans[span].continued)
		{

			// If overlap in rec bands at least at one antenna, may
			// try a slight shift to begin at a more 'integer' MHz
			// TODO

			// Now piece together 'self.outputbw' amout of band from consecutive spans
			AutoBands::Outputband ob(foutstart, outputbandwidth);
			double bw_needed = outputbandwidth;
			double slicestartfreq = foutstart;
			while ((span < Nspans) && (bw_needed > 0))
			{
				double bw_utilized;
				if((slicestartfreq - f0) < 0)
				{
					break;
				}
				bw_utilized = std::min(bw_needed, span_bw_remain);
				bw_needed -= bw_utilized;
				if(verbosity > 1)
				{
					std::cout << "    case 2 adding " << std::setw(10) << bw_utilized*1e-6 << " MHz bw from span " << std::setw(2) << span
						<< " @ " << std::setw(10) << (slicestartfreq-f0)*1e-6 << " MHz "
						<< "to out#" << (int)outputbands.size() << " " << foutstart*1e-6 << " MHz, remain " << bw_needed*1e-6 << " MHz\n";
				}

				// Add bandwidth to outputband
				ob.extend(slicestartfreq, bw_utilized);
				slicestartfreq += bw_utilized;
				span_bw_remain = f1 - slicestartfreq;

				// If out of remaining bw in this span, overflow into the next span if there is no gap
				if(span_bw_remain <= 0)
				{
					if(!spans[span].continued)
					{
						break;
					}
					span++;
					if(span < Nspans)
					{
						f0 = spans[span].flow;
						f1 = spans[span].fhigh;
						span_bw_remain = f1 - slicestartfreq;
						assert(span_bw_remain >= 0);
					}
				}

			}

			// Store the details of the completed outputband
			if(bw_needed <= 0)
			{
				simplify(ob, Nant_min);
				assert(ob.isComplete());
				outputbands.push_back(ob);
				foutstart += outputbandwidth;
				if(verbosity > 1)
				{
					std::cout << "Building further automatic bands continuing from " << foutstart*1e-6 << " MHz\n";
				}
			}
			else
			{
				if(verbosity > 1)
				{
					std::cout << "    dropping incomplete out fq " << foutstart*1e-6 << " MHz, was short by " << bw_needed*1e-6 << " MHz\n";
				}
				foutstart += bw_needed;
				// span++; // perhaps?
			}


		}
		else
		{
			// No band remains in current span
			span++;
		}

	}

	return outputbands.size();
}

int AutoBands::generateOutputbandsExplicit(int Nant_min)
{
	assert (outputbandwidth > 0);
	if(Nant_min <= 0)
	{
		Nant_min = this->Nant;
	}

	// Assemble user bands using recorded bands in full or in pieces
	for(std::vector<AutoBands::Outputband>::const_iterator userband = userOutputbands.begin(); userband != userOutputbands.end(); ++userband)
	{
		AutoBands::Outputband newband(userband->fbandstart, userband->bandwidth);

		double bw_needed = userband->bandwidth;
		double foutstart = userband->fbandstart;

		if(verbosity > 0)
		{
			std::cout << "Building user band at " << userband->fbandstart*1e-6 << " bw " << userband->bandwidth*1e-6 << " MHz\n";
		}

		while(!newband.isComplete())
		{
			int startspan = -1;
			for(size_t span=0; span<spans.size(); span++)
			{
				if(spans[span].flow <= foutstart && foutstart < spans[span].fhigh)
				{
					startspan = span;
					// break;
				}
			}
			if(startspan < 0)
			{
				std::cout << "Warning: user band at " << userband->fbandstart*1e-6 << " bw " << userband->bandwidth*1e-6 << " MHz could not be extended past " << foutstart*1e-6 << " due to lack of covering VEX frequencies\n";
				break;
			}

			//double f0 = spans[startspan].flow;
			double f1 = spans[startspan].fhigh;
			double span_bw_remain = f1 - foutstart;
			double bw_utilized = std::min(span_bw_remain, bw_needed);

			newband.extend(foutstart, bw_utilized);
			if(verbosity > 0)
			{
				std::cout << "    adding " << std::setw(10) << bw_utilized*1e-6 << " MHz starting at " << foutstart*1e-6 << " MHz, "
					<< "filled to " << std::setw(10) << newband.constituentsBandwidth()*1e-6 << " MHz\n";
			}


			foutstart += bw_utilized;
			bw_needed -= bw_utilized;
		}

		if(newband.isComplete())
		{
			simplify(newband, Nant_min);
			outputbands.push_back(newband);
		}

	}

	return outputbands.size();
}

/**
 * Add user-specified bands that should be considered as output
 * bands. This is needed for lookupDestinationFreq() lookup
 * of any explicit bands that were not auto-generated by AutoBands.
 */
void AutoBands::addUserOutputbands(const std::vector<ZoomFreq>& bands)
{
	for(unsigned int n = 0; n < bands.size(); n++)
	{
		addUserOutputband(bands[n]);
	}
}

/**
 * Add a user-specified band that should be considered as output
 * bands. This is needed for lookupDestinationFreq() lookup
 * of any explicit bands that were not auto-generated by AutoBands.
 */
void AutoBands::addUserOutputband(const ZoomFreq& band)
{
	// New outputband with a single constituent band
	Outputband userband(band.frequency, band.bandwidth);
	userband.extend(band.frequency, band.bandwidth);

	// Add if not pre-existing
	if (std::find(outputbands.begin(), outputbands.end(), userband) == outputbands.end())
	{
		if (verbosity > 1)
		{
			std::cout << "AutoBands::addUserOutputband() : adding " << userband << std::endl;
		}
		userOutputbands.push_back(userband);
	}
}

/**
 * Look through the internal list of output bands and search for the first
 * output band that contains 'inputfreq' as one of its constituents.
 *
 * Once the output band of 'inputfreq' has been determined, looks through
 * the list of frequencies 'allfreqs' and locates a match for that output
 * band. Returns the index of that match is returned.
 *
 * If any of the two search stages fails to locate a frequency, the search
 * is repeated with a sideband flipped band having the same sky coverage.
 */
int AutoBands::lookupDestinationFreq(const freq& inputfreq, const std::vector<freq>& allfreqs) const
{
	int outputband_index = -1;

	// Find 'inputfreq' in constituents
	for(unsigned n = 0; n < outputbands.size() && outputband_index < 0; n++)
	{
		const AutoBands::Outputband& ob = outputbands[n];
		for(unsigned m = 0; m < ob.constituents.size(); m++)
		{
			// std::cout << "checking: " << inputfreq << " ?= " << ob.constituents[m] << std::endl;
			if (ob.constituents[m] == inputfreq)
			{
				outputband_index = n;
				break;
			}
		}
	}

	// Or, find band-flipped 'inputfreq' in constituents
	if (outputband_index < 0)
	{
		freq flipped = inputfreq;
		flipped.flip();
		for(unsigned n = 0; n < outputbands.size() && outputband_index < 0; n++)
		{
			const AutoBands::Outputband& ob = outputbands[n];
			for(unsigned m = 0; m < ob.constituents.size(); m++)
			{
				// std::cout << "checking: inv input of " << flipped << " ?= " << ob.constituents[m] << std::endl;
				if (ob.constituents[m] == flipped)
				{
					outputband_index = n;
					break;
				}
			}
		}
	}

	if (outputband_index < 0)
	{
		return -1;
	}

	const AutoBands::Outputband& destination = outputbands[outputband_index];
	for(unsigned n = 0; n < allfreqs.size(); n++)
	{
		if (destination == allfreqs[n])
		{
//std::cout << " match : " << inputfreq << " to band " << destination << " with Fq index " << n << "\n";
			return n;
		}
	}

	for(unsigned n = 0; n < allfreqs.size(); n++)
	{
		freq flipped = allfreqs[n];
		flipped.flip();
		if (destination == flipped)
		{
			return n;
		}
	}

	return -1;
}

/**
 * Create a list of intra-outputband channels that contain no imaginary component (edge channels of consitituents).
 *
 * In case of spectral averaging (DFT bins -> final channels) the contribution of DFT bins from one of the
 * constituent bands to the final channel may be negligible. In this case flagging is decided per maximum allowed
 * contribution of the shorter constituent band to the total output channel.
 * Example: bands a+b pre-avg input:[a18 a19 b0 b1 b2 b3 b4 b5]/avg=8 --> out:[ch0] has 'a' contribute 25% and 'b' 75%.
 * Flagging of output ch0 can be omitted with maxFraction=0.25.
 *
 * \param channels Storage to fill out with channel numbers
 * \param fftSpecRes_Hz Resolution of spectral channels across outputband during DiFX processing
 * \param finalSpecRes_Hz Resolution of spectral channels across outputband after final spectral averaging in DiFX if any
 * \param maxFraction Max fraction of allowed non-flaggable contribution of two constituents to one output channel; range [0; 1.0], default 0
 * \return Number of edge channels detected, or -1 on error
 */
int AutoBands::listEdgeChannels(const AutoBands::Outputband& outputband, std::deque<int>& channels, double fftSpecRes_Hz, double finalSpecRes_Hz, double maxFraction) const
{
	const unsigned M = outputband.constituents.size();;
	const int avg = finalSpecRes_Hz / fftSpecRes_Hz;

	channels.clear();

	if (verbosity > 2)
	{
		std::cout << "outputband with " << (int)std::floor(outputband.bandwidth/fftSpecRes_Hz) << "/" << avg << " FFT bins:\n";
	}

	if (std::fabs(std::fmod(finalSpecRes_Hz, fftSpecRes_Hz)) > 1e-6)
	{
		return -1;
	}

	for(unsigned m = 0; m < M; m++)
	{
		const AutoBands::Band& sub = outputband.constituents[m] ;
		const double foffset = sub.flow - outputband.fbandstart;
		if (foffset < 0)
		{
			std::cout << "AutoBands error: unexpected offset of " << foffset << " Hz for " << outputband << " constituent band " << sub << "\n";
			return -1;
		}

		// Flag the first bin (DC) of all constituent bands except for the first band; DC bin is okay in first band
		// Note: no need to flag the last N/2+1 bin (Nyquist) of consituent bands, since all Nyquist except the final end up automatically at DC-bin location of next band
		if (m == 0)
		{
			if (verbosity > 2)
			{
				std::cout << "   constituent " << (m+1) << "/" << outputband.constituents.size() << " " << sub << " at : DC bin 0 of output band, no need to flag\n";
			}
		}
		else
		{
			const int choffset_int = std::floor(foffset / fftSpecRes_Hz);
			const int choffset_ext_subbin = choffset_int % avg;
			const int choffset_ext = choffset_int / avg;

			bool doFlag = false;
			if (avg > 1 && maxFraction > 0)
			{
				const double fraction = choffset_ext_subbin / (double)avg;
				doFlag = (fraction > maxFraction && fraction < (1 - maxFraction));
			}
			else
			{
				doFlag = true;
			}

			if (doFlag)
			{
				channels.push_back(choffset_ext);
			}

			if (verbosity > 2)
			{
				std::cout << "   constituent " << (m+1) << "/" << outputband.constituents.size() << " " << sub << " at : DC bin "
					<< choffset_int << " falls into bin " << (choffset_ext_subbin+1) << " of " << avg << "-bin average, final output channel " << choffset_ext
					<< " : flag=" << (doFlag ? "yes" : "no") << "\n";
			}
		}
	}

	if (verbosity > 2)
	{
		std::cout << "   flagged channels: ";
		std::copy(channels.begin(), channels.end(), std::ostream_iterator<int>(std::cout, " "));
		std::cout << "\n";
	}

	return channels.size();
}


void AutoBands::barchart(
	std::ostream& os,
	const std::vector<double>& start, const std::vector<double>& stop,
	const std::vector<int>& idtag, const int startmarker,
	double xmin, double xmax) const
{
	const int screenwidth = 110;

	if(std::isnan(xmin))
	{
		xmin = *std::min_element(start.begin(), start.end());
	}
	if(std::isnan(xmax))
	{
		xmax = *std::max_element(stop.begin(), stop.end());
	}

	const double fstep = (xmax - xmin) / screenwidth;
	for(unsigned k = 0; k < start.size(); k++)
	{
		double bw = stop[k] - start[k];
		int L0 = std::ceil((start[k] - xmin)/fstep);
		int L1 = std::ceil(bw/fstep);
		int L2 = screenwidth - L1 - L0;
		if(L2 < 0)
		{
			L1 += L2;
			L2 = 0;
		}
		int id = startmarker + idtag[k]%26;
		os << std::string(L0, '.') << std::string(L1, id) << std::string(L2, '.') << "\n";
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

/// Function for std::min_element and others for sorting Span's
bool AutoBands::Span::compare_bandwidths(const AutoBands::Span& lhs, const AutoBands::Span& rhs)
{
	return (lhs.bandwidth() < rhs.bandwidth());
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator << (std::ostream& os, const AutoBands& x)
{
	// Show recorded bands as a bar chart
	os << "Recorded bands:\n";
	std::vector<double> xmin, xmax;
	std::vector<int> id;
	for(unsigned k = 0; k < x.bands.size(); k++)
	{
		xmin.push_back(x.bands[k].flow);
		xmax.push_back(x.bands[k].fhigh);
		id.push_back(x.bands[k].antenna);
	}
	x.barchart(os, xmin, xmax, id);

	// Show auto-detected usable spans as a bar chart
	os << "Detected spans with array-wide common cover:\n";
	xmin.clear();
	xmax.clear();
	id.clear();
	for(unsigned k = 0; k < x.spans.size(); k++)
	{
		xmin.push_back(x.spans[k].flow);
		xmax.push_back(x.spans[k].fhigh);
		id.push_back(k);
	}
	x.barchart(os, xmin, xmax, id, 'A');

	// Show auto-detected usable spans numerically / listed
	std::copy(x.spans.begin(), x.spans.end(), std::ostream_iterator<AutoBands::Span>(os, "\n"));

	// Show output bands and how they are assembled
	os << "Generated " << x.outputbands.size() << " output bands\n";
	std::copy(x.outputbands.begin(), x.outputbands.end(), std::ostream_iterator<AutoBands::Outputband>(os, ""));

	return os;
}

std::ostream& operator << (std::ostream& os, const AutoBands::Band& x)
{
	os << std::fixed
		<< "start at " << std::setw(15) << std::setprecision(8) << (x.flow*1e-6) << " MHz with bw "
		<< std::setw(11) << std::setprecision(7) << (x.bandwidth()*1e-6) << " MHz";
	return os;
}

std::ostream& operator << (std::ostream& os, const AutoBands::Span& x)
{
	os << "Span " << std::fixed
		<< std::setw(15) << std::setprecision(8) << (x.flow*1e-6)  << " -- "
		<< std::setw(15) << std::setprecision(8) << (x.fhigh*1e-6) << " MHz, "
		<< std::setw(11) << std::setprecision(7) << (x.bandwidth()*1e-6) << " MHz bw, "
		<< x.bandcount << " rec.bands, " << (x.continued ? "contig" : "gap");
	return os;
}

std::ostream& operator << (std::ostream& os, const AutoBands::Outputband& x)
{
	os << "Output band at " << std::fixed
		<< std::setw(15) << std::setprecision(8) << (x.fbandstart*1e-6)
		<< " with bw "
	 	<< (x.bandwidth*1e-6)
		<< "\n";
	for(unsigned n = 0; n < x.constituents.size(); n++)
	{
		os << "   input " << std::setw(2) << n << " " << x.constituents[n] << "\n";
	}
	os << "   total bandwidth " << x.constituentsBandwidth()*1e-6 << " of " << x.bandwidth*1e-6 << " MHz \n";
	return os;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
