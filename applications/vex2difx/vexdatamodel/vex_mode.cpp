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
 * $Id: vex_mode.cpp 10363 2022-01-27 22:57:59Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision: 10363 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2022-01-28 06:57:59 +0800 (五, 2022-01-28) $
 *
 *==========================================================================*/

#include <cstdlib>
#include "vex_mode.h"
#include "vex_utility.h"

int VexMode::addSubband(double freq, double bandwidth, char sideband, char pol)
{
	VexSubband S(freq, bandwidth, sideband, pol);

	for(std::vector<VexSubband>::const_iterator it = subbands.begin(); it != subbands.end(); ++it)
	{
		if(S == *it)
		{
			return it - subbands.begin();
		}
	}

	subbands.push_back(S);

	return subbands.size() - 1;
}

int VexMode::getPols(char *pols) const
{
	int n=0;
	bool L=false, R=false, X=false, Y=false, H=false, V=false;
	std::vector<VexSubband>::const_iterator it;

	for(it = subbands.begin(); it != subbands.end(); ++it)
	{
		if(it->pol == 'R')
		{
			R = true;
		}
		else if(it->pol == 'L')
		{
			L = true;
		}
		else if(it->pol == 'X')
		{
			X = true;
		}
		else if(it->pol == 'Y')
		{
			Y = true;
		}
		else if(it->pol == 'H')
		{
			H = true;
		}
		else if(it->pol == 'V')
		{
			V = true;
		}
		else
		{
			std::cerr << "Error: VexMode::getPols: subband with illegal polarization (" << it->pol << ") encountered." << std::endl;
			
			exit(EXIT_FAILURE);
		}
	}

	if(R) 
	{
		*pols = 'R';
		++pols;
		++n;
	}
	if(L)
	{
		*pols = 'L';
		++pols;
		++n;
	}
	if(n)
	{
		return n;
	}
	if(X) 
	{
		*pols = 'X';
		++pols;
		++n;
	}
	if(Y)
	{
		*pols = 'Y';
		++pols;
		++n;
	}
	if(H)
	{
		*pols = 'H';
		++pols;
		++n;
	}
	if(V)
	{
		*pols = 'V';
		++pols;
		++n;
	}

	return n;
}

int VexMode::getBits() const
{
	static int firstTime = 1;
	unsigned int nBit = setups.begin()->second.getBits();
	std::map<std::string,VexSetup>::const_iterator it;

	for(it = setups.begin(); it != setups.end(); ++it)
	{
		unsigned int nb = it->second.getBits();

		if(nb != nBit)
		{
			if(nBit != 0 && nb != 0 && firstTime)
			{
				std::cerr << "Note: getBits: Mode=" << defName << " differing number of bits: " << nBit << "," << nb << std::endl;
				std::cerr << "  Will proceed, but note that some metadata may be incorrect." << std::endl;

				firstTime = 0;
			}

			if(nb > nBit)
			{
				nBit = nb;
			}
		}

	}

	return nBit;
}

int VexMode::getMinBits() const
{
	unsigned int minBit = 0;
	std::map<std::string,VexSetup>::const_iterator it;

	for(it = setups.begin(); it != setups.end(); ++it)
	{
		unsigned int mb = it->second.getMinBits();

		if(mb > 0 && (mb < minBit || minBit == 0))
		{
			minBit = mb;
		}

	}

	return minBit;
}

int VexMode::getMaxBits() const
{
	unsigned int maxBit = 0;
	std::map<std::string,VexSetup>::const_iterator it;

	for(it = setups.begin(); it != setups.end(); ++it)
	{
		unsigned int mb = it->second.getMaxBits();

		if(mb > 0 && (mb > maxBit || maxBit == 0))
		{
			maxBit = mb;
		}

	}

	return maxBit;
}

// returns zero if number of bits differs across channels or antennas
int VexMode::zBits() const
{
	unsigned int maxBit = 0;
	unsigned int minBit = 0;
	std::map<std::string,VexSetup>::const_iterator it;

	for(it = setups.begin(); it != setups.end(); ++it)
	{
		unsigned int mb;
		
		mb = it->second.getMaxBits();
		if(mb > 0 && (mb > maxBit || maxBit == 0))
		{
			maxBit = mb;
		}

		mb = it->second.getMinBits();
		if(mb > 0 && (mb < minBit || minBit == 0))
		{
			minBit = mb;
		}
	}

	if(maxBit == minBit)
	{
		return maxBit;
	}
	else
	{
		return 0;
	}
}

// returns zero if not consistent
int VexMode::zRecordChan() const
{
	unsigned int nRecChan = 0;

	for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++ it)
	{
		if(nRecChan == 0)
		{
			nRecChan = it->second.nRecordChan();
		}
		else if(nRecChan != it->second.nRecordChan())
		{
			return 0;
		}
	}

	return nRecChan;
}

// [Hz] channel bandwidth, or zero if not consistent
double VexMode::zBandwidth() const
{
	double sampleRate = 0.0;

	for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++ it)
	{
		double s1, s2;

		s1 = it->second.getLowestSampleRate();
		s2 = it->second.getHighestSampleRate();

		if(s1 != s2)
		{
			return 0.0;
		}
		if(sampleRate == 0.0)
		{
			sampleRate = s1;
		}
		else if(sampleRate != s1)
		{
			return 0.0;
		}
	}

	return sampleRate/2.0;	
}


int VexMode::getMinSubbands() const
{
	int minSubbands = 0;

	for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
	{
		int s;

		s = it->second.channels.size();
		if(s > 0 && (s < minSubbands || minSubbands == 0))
		{
			minSubbands = s;
		}
	}

	return minSubbands;
}

// return maximum number of datastreams that could be active for this mode
int VexMode::nStream() const
{
	int ns = 0;	
	
	for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
	{
		ns += it->second.streams.size();
	}

	return ns;
}

const VexSetup* VexMode::getSetup(const std::string &antName) const
{
	std::map<std::string,VexSetup>::const_iterator it;

	it = setups.find(antName);
	if(it == setups.end())
	{
		return 0;
	}

	return &it->second;
}

/* Returns lowest sample rate in samples/second */
double VexMode::getLowestSampleRate() const
{
	if(setups.empty())
	{
		return 0.0;
	}
	else
	{
		double sr = 1.0e30;	// A very large number
		
		for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
		{
			double lsr = it->second.getLowestSampleRate();

			if(lsr < sr && lsr > 0.0)
			{
				sr = lsr;
			}
		}

		if(sr > 1.0e29)
		{
			sr = 0.0;
		}

		return sr;
	}
}

/* Returns highest sample rate in samples/second */
double VexMode::getHighestSampleRate() const
{
	if(setups.empty())
	{
		return 0.0;
	}
	else
	{
		double sr = 0.0;
		
		for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
		{
			double hsr = it->second.getHighestSampleRate();

			if(hsr > sr)
			{
				sr = hsr;
			}
		}

		return sr;
	}
}

/* Returns average sample rate in samples/second */
double VexMode::getAverageSampleRate() const
{
	if(setups.empty())
	{
		return 0.0;
	}
	else
	{
		double sr = 0.0;
		
		for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
		{
			sr += it->second.getAverageSampleRate();
		}

		sr /= setups.size();

		return sr;
	}
}

void VexMode::swapPolarization(const std::string &antName)
{
	std::map<std::string,VexSetup>::iterator it = setups.find(antName);
	if(it != setups.end())
	{
		if(it->first == antName)
		{
			// change IF pols
			for(std::map<std::string,VexIF>::iterator vit = it->second.ifs.begin(); vit != it->second.ifs.end(); ++vit)
			{
				vit->second.pol = swapPolarizationCode(vit->second.pol);
			}

			// reassign subband index for each channel
			for(std::vector<VexChannel>::iterator cit = it->second.channels.begin(); cit != it->second.channels.end(); ++cit)
			{
				char origPol = subbands[cit->subbandId].pol;
				cit->subbandId = addSubband(cit->bbcFreq, cit->bbcBandwidth, cit->bbcSideBand, swapPolarizationCode(origPol));
			}
		}
	}
}

void VexMode::setSampling(const std::string &antName, unsigned int streamId, enum SamplingType dataSampling)
{
	std::map<std::string,VexSetup>::iterator it = setups.find(antName);
	if(it != setups.end())
	{
		if(it->first == antName)
		{
			if(streamId >= it->second.nStream())
			{
				std::cerr << "Developer Error: VexMode::setSampling: streamId = " << streamId << " which is too big.  Number of streams in mode " << defName << " antenna " << antName << " is " << it->second.nStream() << std::endl;

				exit(EXIT_FAILURE);
			}
			it->second.streams[streamId].dataSampling = dataSampling;
		}
	}
}

void VexMode::setPhaseCalInterval(const std::string &antName, int phaseCalIntervalMHz)
{
	std::map<std::string,VexSetup>::iterator it = setups.find(antName);
	if(it != setups.end())
	{
		if(it->first == antName)
		{
			it->second.setPhaseCalInterval(phaseCalIntervalMHz);
		}
	}
}

void VexMode::selectTones(const std::string &antName, enum ToneSelection selection, double guardBandMHz)
{
	for(std::map<std::string,VexSetup>::iterator it = setups.begin(); it != setups.end(); ++it)
	{
		if(it->first == antName)
		{
			it->second.selectTones(selection, guardBandMHz);
		}
	}
}

void VexMode::generateRecordChans()
{
	for(std::map<std::string,VexSetup>::iterator it = setups.begin(); it != setups.end(); ++it)
	{
		if(!it->second.hasUniqueRecordChans())
		{
			std::cout << "Note: Mode " << defName << " antenna " << it->first << " did not have a channel ordering defined.  Sorting by channel names to establish the order." << std::endl;
			it->second.assignRecordChans();
		}
	}
}

int VexMode::nRecordChan(const std::string &antName) const
{
	std::map<std::string,VexSetup>::const_iterator it = setups.find(antName);
	if(it != setups.end())
	{
		return it->second.nRecordChan();
	}
	else
	{
		return 0;
	}
}

bool VexMode::hasDuplicateBands() const
{
	if(hasDuplicates(subbands) | hasDuplicates(zoombands))
	{
		return true;
	}

	for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
	{
		if(it->second.hasDuplicateSubbands())
		{
			return true;
		}
	}

	return false;
}

int VexMode::getPolarizations() const
{
	int rv = 0;

	for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
	{
		rv |= it->second.getPolarizations();
	}

	return rv;
}

int VexMode::getConvertedPolarizations(const std::list<std::string> &antsToConvert) const
{
	int rv = 0;

	for(std::map<std::string,VexSetup>::const_iterator it = setups.begin(); it != setups.end(); ++it)
	{
		int pols;
		
		pols = it->second.getPolarizations();

		if(find(antsToConvert.begin(), antsToConvert.end(), it->first) != antsToConvert.end())
		{
			int convertedPols;

			convertedPols = 0;
			if(pols & DIFXIO_POL_RL)
			{
				convertedPols |= DIFXIO_POL_XY;
			}
			if(pols & DIFXIO_POL_XY)
			{
				convertedPols |= DIFXIO_POL_RL;
			}
			if(pols & DIFXIO_POL_ERROR)
			{
				convertedPols |= DIFXIO_POL_ERROR;
			}
			pols = convertedPols;
		}

		rv |= pols;
	}

	return rv;
}

std::ostream& operator << (std::ostream &os, const VexMode &x)
{
	unsigned int nSubband = x.subbands.size();

	os << "Mode " << x.defName << std::endl;
	for(unsigned int i = 0; i < nSubband; ++i)
	{
		os << "  Subband[" << i << "]=" << x.subbands[i] << std::endl;
	}
	for(std::map<std::string,VexSetup>::const_iterator it = x.setups.begin(); it != x.setups.end(); ++it)
	{
		os << "  Setup[" << it->first << "]" << std::endl;
		os << it->second;
	}
	
	return os;
}
