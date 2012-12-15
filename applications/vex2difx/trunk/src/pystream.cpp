/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken / Adam Deller               *
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

#include <cstdlib>
#include <cmath>
#include <sstream>
#include <time.h>
#include <string.h>
#include "evladefaults.h"
#include "pystream.h"

void pystream::open(const string& antennaName, const VexData *V, ScriptType sType)
{
	string extension;

	evlaIntSec     = DEFAULT_EVLA_INT_SEC;
	evlasbBits     = DEFAULT_EVLA_SB_BITS;
	evlasbChan     = DEFAULT_EVLA_SB_CHAN;
	evlaVCIDir     = DEFAULT_EVLA_VCI_DIR;
	evlaVCIVersion = DEFAULT_EVLA_VCI_VER;
	scriptType = sType;
	ant = antennaName;
	lastValid = 0.0;
	lastSourceId = -1;
	lastModeId = -1;
	lastChannelSet = -1;
	obsCode = V->getExper()->name;
	calcIfIndex(V);
	if(obsCode == "")
	{
		obsCode = "Unknown";
	}
	for(int i = 0; i < 4; ++i)
	{
		sw[i] = "";
	}
	mjd0 = V->obsStart();
	
	extension = ".py";

	fileName = string(obsCode) + string(".") + antennaName + extension;
	ofstream::open(fileName.c_str());
}

void pystream::addPhasingSource(const string &sourceName)
{
	phasingSources.push_back(sourceName);
}

int switchPosition(const char *val)
{
	char c = val[0];

	// make upper case
	if(c >= 'a' && c <= 'z')
	{
		c -= ('a'-'A');
	}

	if(c == 'A')
	{
		return 1;
	}
	else if(c == 'B')
	{
			return 2;
	}
	else if(c == 'C')
	{
			return 3;
	}
	else if(c == 'D')
	{
		return 4;
	}
	else if(c == 'T')
	{
		return 5;
	}
	else if(c == 'G')
	{
		return 6;
	}
	else
	{
		return -1;
	}
}

void pystream::calcIfIndex(const VexData *V)
{
	map<string,VexIF>::const_iterator it;
	unsigned int nMode = V->nMode();

	ifIndex.clear();
	ifIndex.resize(nMode);

	for(unsigned int m = 0; m < nMode; ++m)
	{
		const VexMode *mode = V->getMode(m);
		const VexSetup *setup = mode->getSetup(ant);
		unsigned int nif=0;

		if(!setup)
		{
			continue;
		}

		for(it = setup->ifs.begin(); it != setup->ifs.end(); ++it)
		{
			ifIndex[m][it->second.name] = nif;
			++nif;
		}
	}
}

int pystream::maxIFs(const VexData *V) const
{
	unsigned int nMode = V->nMode();
	unsigned int maxIFs = 0;

	for(unsigned int modeNum = 0; modeNum < nMode; ++modeNum)
	{
		const VexMode *mode = V->getMode(modeNum);
		const VexSetup *setup = mode->getSetup(ant);

		if(!setup)
		{
			continue;
		}

		if(setup->ifs.size() > maxIFs)
		{
			maxIFs = setup->ifs.size();
		}
	}

	return maxIFs;
}

void pystream::close()
{
	int p = precision();
	precision(14);

	if(lastValid != 0.0)
	{
		if(scriptType == SCRIPT_VLBA || scriptType == SCRIPT_GBT)
		{
			double deltat = floor((lastValid-mjd0 + 1.0/86400.0)*86400.0 + 0.5);
			*this << "array.wait(mjdStart + " << deltat << "*second)" << endl;
		}
		else
		{
			*this << "subarray.disconnectPhasing(autophase0, 'A')" << endl;
			*this << "subarray.disconnectPhasing(autophase1, 'B')" << endl;
			*this << "subarray.disconnectPhasing(autophase2, 'C')" << endl;
			*this << "subarray.disconnectPhasing(autophase3, 'D')" << endl;
			*this << endl;

			*this << "print 'setting up termination source.'" << endl;
			*this << "intentionEnd=Intention()" << endl;
			*this << "intentionEnd.addIntent('suppressData=\"true\"')" << endl;
			*this << "sourceEnd=Source(6.28318531*array.lst(array.time()+5.0/86400.), 0)" << endl;
			*this << "sourceEnd.setName('FINISH')" << endl;
			*this << "sourceEnd.setIntention(intentionEnd)" << endl;
			*this << "subarray.setSource(sourceEnd)" << endl;
			*this << "subarray.execute(array.time()+5.0/86400.)" << endl;
		}
	}

	precision(p);

	ofstream::close();
}

int pystream::writeHeader(const VexData *V)
{
	double day = floor(mjd0);
	double sec = floor((mjd0-day)*86400.0 + 0.5);
        lastValid  = mjd0-(5.0/86400.0);
        string tab = "";

    if(scriptType == SCRIPT_GBT)
	{
        // os.getenv() gets exception in executor
        // (for running same script by the executor and by astrid)
		*this << "import os" << endl << endl;
		*this << "isAstrid = 0" << endl;
                *this << "if 1:" << endl;
                *this << "    try:" << endl;
		*this << "        if os.getenv('ASTRIDVLBA') == '1':" << endl;
                *this << "            isAstrid = 1" << endl;
                *this << "    except:" << endl;
                *this << "        pass" << endl << endl;
                *this << "if not isAstrid:" << endl;
        tab = "    ";
        }

	*this << tab << "from edu.nrao.evla.observe import Mark5C" << endl;
	switch(scriptType)
	{
	case SCRIPT_VLBA:
	case SCRIPT_GBT:
		*this << tab << "from edu.nrao.evla.observe import MatrixSwitch" << endl;
		*this << tab << "from edu.nrao.evla.observe import RDBE" << endl;
		*this << tab << "from edu.nrao.evla.observe import VLBALoIfSetup" << endl;
		*this << tab << "from edu.nrao.evla.observe import Parameters" << endl;
		*this << tab << "from edu.nrao.evla.observe import bbc" << endl;
		break;
	case SCRIPT_EVLA:
		*this << "includePath = \"/home/mchost/evla/include/\"" << endl;
		*this << "execfile(includePath+\"printers.py\")" << endl;
		*this << "execfile(includePath+\"tmjd.py\")" << endl;
		break;
	}
	*this << endl;
	*this << "second = 1.0/86400.0" << endl;
	*this << endl;
	*this << "deltat2 = 1" << endl;
	*this << endl;
	*this << "obsCode = '" << obsCode << "'" << endl;
	if(scriptType == SCRIPT_VLBA || scriptType == SCRIPT_GBT)
	{
		*this << "stnCode = '" << ant << "'" << endl;
	}
	else if(scriptType == SCRIPT_EVLA)
	{
		*this << "programName = 'vex2script'\n" << endl;
	}
	*this << "mjdStart = " << day << " + " << sec << "*second" << endl;
	*this << endl;

	return 0;
}

int pystream::writeComment(const string &commentString)
{
	*this << "# " << commentString << endl;
	*this << endl;

	return 0;
}

int pystream::writeRecorderInit(const VexData *V)
{
#warning "FIXME For now, use of recorder is based purely on Mark5A or not"
	if(!isMark5A)
	{
		*this << "recorder0 = Mark5C('-1')" << endl;

#warning "FIXME For now, set up single recorder in Mark5B mode"
		// Need to check requested format/mode first
		*this << "recorder0.setMode('Mark5B')" << endl;
		*this << "recorder0.setPSNMode(0)" << endl;
		*this << "recorder0.setPacket(0, 0, 36, 5008)" << endl;

		*this << "subarray.setRecorder(recorder0)" << endl;
		*this << endl;
	}

	return 1;
}

static int bwValidDDC(int bwHz)
{
	for(int bw = 128000000; bw > 10000; bw /= 2)
	{
		if(bw == bwHz)
		{
			return true;
		}
	}

	return false;
}

static int tuneValidPFB(int tuneHz)
{
	int n;

	n = (1040000000 - tuneHz) / 32000000;

	return (tuneHz == 1040000000 - 32000000*n);
}

void pystream::figurePersonality(const VexData *V)
{
	if(personalityType != RDBE_UNKNOWN)
	{
		cerr << "Developer error: pystream::figurePersonality called with personalityType != RDBE_UNKNOWN (" << RDBE_UNKNOWN << ").  It was " << personalityType << "." << endl;

		exit(EXIT_FAILURE);
	}

	for(unsigned int m = 0; m < V->nMode(); ++m)
	{
		const VexMode *mode = V->getMode(m);
		const VexSetup *setup = mode->getSetup(ant);
		int nChan = setup->channels.size();
		int nRecChan = setup->nRecordChan;
		bool pfbOK = true;
		bool ddcOK = true;

		// don't let trivialities determine mode
		if(setup->formatName == "" || setup->formatName == "NONE" || nChan == 0)
		{
			continue;
		}

		if(nRecChan != 1 && nRecChan != 2 && nRecChan != 4 && nRecChan != 8)	// Currently this is true, may change with VDIF
		{
			ddcOK = false;
		}
		if(nRecChan != 16)
		{
			pfbOK = false;
		}

		for(unsigned int i = 0; i < setup->channels.size(); ++i)
		{
			double bw = setup->channels[i].bbcBandwidth;
			int bwHz = static_cast<int>(bw + 0.5);
			char sb = setup->channels[i].bbcSideBand;
			unsigned int nBit = setup->nBit;
			const VexIF *vif = setup->getIF(setup->channels[i].ifName);
			if(!vif)
			{
				cerr << "Developer error: pystream::figurePersonality: setup->getIF(" << setup->channels[i].ifName << ") returned NULL" << endl;

				exit(EXIT_FAILURE);
			}
			double freq = setup->channels[i].bbcFreq;
			double tune = freq - vif->ifSSLO;
			int tuneHz;

			if(tune < 0.0)
			{
				tune = -tune;
				sb = (sb == 'U') ? 'L' : 'U';
			}
			tuneHz = static_cast<int>(tune + 0.5);
			
			if(nBit != 2 && setup->channels[i].recordChan >= 0)
			{
				cerr << "Error: " << nBit << " bits Quantization requested for mode " << mode->defName << ".  Only 2 bits are allowed now." << endl;

				exit(EXIT_FAILURE);
			}
			if(sb != 'L')
			{
				pfbOK = false;
			}
			if(bwHz != 32000000)
			{
				pfbOK = false;
			}
			if(!bwValidDDC(bwHz))
			{
				ddcOK = false;
			}
			if(!tuneValidPFB(tuneHz))
			{
				pfbOK = false;
			}
		}

		if(!pfbOK && !ddcOK)
		{
			cerr << "Error: mode " << mode->defName << " is not suitable for either PFB or DDC on antenna " << ant << endl;

			exit(EXIT_FAILURE);
		}
		else if(!ddcOK)
		{
			if(personalityType == RDBE_DDC)
			{
				cerr << "Error: conflicting modes.  PFB needed for mode " << mode->defName << " whereas at least one prior mode required DDC." << endl;

				exit(EXIT_FAILURE);
			}
			personalityType = RDBE_PFB;
		}
		else if(!pfbOK)
		{
			if(personalityType == RDBE_PFB)
			{
				cerr << "Error: conflicting modes.  DDC needed for mode " << mode->defName << " whereas at least one prior mode required PFB." << endl;

				exit(EXIT_FAILURE);
			}
			personalityType = RDBE_DDC;
		}
	}

	if(personalityType == RDBE_UNKNOWN)
	{
		personalityType = RDBE_PFB;	// most sensible default for now
	}
}

int pystream::writeDbeInit(const VexData *V)
{
	if(scriptType == SCRIPT_VLBA || scriptType == SCRIPT_GBT)
	{
#warning "FIXME For now, set up single RDBE"
		if(personalityType == RDBE_UNKNOWN)
		{
			figurePersonality(V);
		}
		if(personalityType == RDBE_PFB)
		{
			if(dbeFilename[0])
			{
				if(strcasestr(dbeFilename.c_str(), "DDC") != 0)
				{
					cerr << "Warning: Personality " << dbeFilename << " looks to be for DDC, but this project is using PFB." << endl;
				}
				*this << "dbe0 = RDBE(0, 'pfb', " << dbeFilename << ")" << endl;
			}
			else
			{
				*this << "dbe0 = RDBE(0, 'pfb')" << endl;
			}
		}
		else if(personalityType == RDBE_DDC)
		{
			if(dbeFilename[0])
			{
				if(strcasestr(dbeFilename.c_str(), "PFB") != 0)
				{
					cerr << "Warning: Personality " << dbeFilename << " looks to be for PFB, but this project is using DDC." << endl;
				}
				*this << "dbe0 = RDBE(0, 'ddc', " << dbeFilename << ")" << endl;
			}
			else
			{
				*this << "dbe0 = RDBE(0, 'ddc')" << endl;
			}
		}
		if(personalityType == RDBE_PFB || personalityType == RDBE_DDC)
		{
			*this << "dbe0.setALC(1)" << endl;
			*this << "dbe0.setFormat('Mark5B')" << endl;
			*this << "dbe0.setPSNMode(0)" << endl;
			*this << "dbe0.setPacket(0, 0, 40, 5008)" << endl;
			if(maxIFs(V) <= 2)
			{
				*this << "subarray.setDBE(dbe0)" << endl;
			}
			else
			{
				*this << "# subarray.setDBE(dbe0)  # Not set because max number of IFs is " << maxIFs(V) << endl;
			}
			*this << endl;
		}
	}
	else if(scriptType == SCRIPT_EVLA)
	{
		//do nothing - the correlator setup gets done at the same time as the LO/IF
	}

	return 1;
}

void pystream::writeImplicitConversionComment(const vector<unsigned int> &implicitConversions)
{
	if(!implicitConversions.empty())
	{
		*this << "# implicit conversion performed on basebands:";
		for(vector<unsigned int>::const_iterator uit = implicitConversions.begin(); uit != implicitConversions.end(); ++uit)
		{
			*this << " " << *uit;
		}
		*this << endl;
	}
}

int pystream::writeChannelSet(const VexSetup *setup, int modeNum)
{
	*this << "channelSet" << modeNum << " = [ \\" << endl;

	vector<unsigned int> implicitConversions;
	for(unsigned int i = 0; i < setup->channels.size(); ++i)
	{
		unsigned int inputNum = ifIndex[modeNum][setup->channels[i].ifName];
		double bw = setup->channels[i].bbcBandwidth;
		char sb = setup->channels[i].bbcSideBand;
		unsigned int nBit = setup->nBit;
		unsigned int threadId = 0;
		const VexIF *vif = setup->getIF(setup->channels[i].ifName);
		if(!vif)
		{
			cerr << "Developer error: setup->getIF(" << setup->channels[i].ifName << ") returned NULL" << endl;

			exit(EXIT_FAILURE);
		}
		double freq = setup->channels[i].bbcFreq;
		double tune = freq - vif->ifSSLO;

		if(tune < 0.0)
		{
			tune = -tune;
			sb = (sb == 'U') ? 'L' : 'U';
		}

		if(freq > 550e6 && freq < 650e6 && tune > 1000e6)
		{
			tune -= 500e6;
			implicitConversions.push_back(i);
		}

		*this << "  bbc(" << inputNum << ", " << (tune*1.0e-6) << ", " << (bw*1.0e-6) << ", '" << sb << "', " << nBit << ", " << threadId << ")";
		if(i < setup->channels.size()-1)
		{
			*this << ",";
		}
		*this << " \\" << endl;
	}
	*this << "  ]" << endl;

	writeImplicitConversionComment(implicitConversions);

	return 0;
}

static int roundChanNum(double fchan, char sb)
{
	int chan = static_cast<int>(fchan);
	if(sb == 'U')
	{
		if(fchan - chan < 1.0e-9)	// is less than one nano-channel offset?
		{
			--chan;			// then since upper side-band cross the boundary
		}
	}
	else
	{
		if(chan+1 - fchan < 1.0e-9)	// is less than one nano-channel offset?
		{
			++chan;			// then since lower side-band cross the boundary
		}
	}

	return chan;
}

int pystream::writeChannelSet5A(const VexSetup *setup, int modeNum)
{
	const int MaxChannels = 16;
	const int bwMHz = 32;			// MHz;
	const double bwDBE = bwMHz*1.0e6;	// Hz
	bool channelMask[2][MaxChannels];	// first index: IF, second index: freq chan
	int channelCount[2];			// index: IF
						// 0 = 1040-1008 MHz, 1 = 1008-976 MHz, ..., 15 = 560-528 MHz
	vector<unsigned int> implicitConversions;

	for(int i = 0; i < 2; ++i)
	{
		for(int c = 0; c < MaxChannels; ++c)
		{
			channelMask[i][c] = false;
		}
	}

	// First go through to find required DBE channels
	for(unsigned int i = 0; i < setup->channels.size(); ++i)
	{
		unsigned int inputNum = ifIndex[modeNum][setup->channels[i].ifName];
		double bw = setup->channels[i].bbcBandwidth;
		char sb = setup->channels[i].bbcSideBand;
		int chan;
		double fchan, freq, tune;
		const VexIF *vif = setup->getIF(setup->channels[i].ifName);
		if(!vif)
		{
			cerr << "Developer error: pystream.writeChannelSet5A: setup->getIF(" << setup->channels[i].ifName << ") returned NULL" << endl;

			exit(EXIT_FAILURE);
		}
		freq = setup->channels[i].bbcFreq;
		tune = freq - vif->ifSSLO;

		if(inputNum > 1)
		{
			continue;
		}

		if(tune < 0.0)
		{
			tune = -tune;
			sb = (sb == 'U') ? 'L' : 'U';
		}

		if(freq > 550e6 && freq < 650e6 && tune > 1000e6)
		{
			tune -= 500e6;
			implicitConversions.push_back(i);
		}

		// In this case, at most 2 channels are needed to span the original channel
		// DC end of band

		// Handle DC side here
		fchan = (1.040e9-tune)/bwDBE;
		chan = roundChanNum(fchan, sb);

		if(chan >= 0 && chan < MaxChannels)
		{
			channelMask[inputNum][chan] = true;
		}

		// Handle Nyquist side here
		if(sb == 'U')
		{
			fchan += bw/bwDBE;
		}
		else
		{
			fchan -= bw/bwDBE;
		}
		fchan = (1.040e9-tune)/bwDBE;
		chan = roundChanNum(fchan, sb);

		if(chan >= 0 && chan < MaxChannels)
		{
			channelMask[inputNum][chan] = true;
		}
	}

	for(int i = 0; i < 2; ++i)
	{
		channelCount[i] = 0;
		for(int c = 0; c < MaxChannels; ++c)
		{
			if(channelMask[i][c])
			{
				++channelCount[i];
			}
		}
	}

#warning "FIXME: Really should detect here 90/50cm and be smarter about channel selection"
	if(channelCount[0] == 0 && channelCount[1] == 0)
	{
		// Use every other channel

		for(int i = 0; i < 2; ++i)
		{
			for(int c = 1; c < MaxChannels; c += 2)
			{
				channelMask[i][c] = true;
			}
		}
		channelCount[0] = MaxChannels/2;
		channelCount[1] = MaxChannels/2;
	}
	else if(channelCount[1] == 0)
	{
		// Use all of input zero
		for(int c = 0; c < MaxChannels; ++c)
		{
			channelMask[0][c] = true;
		}
		channelCount[0] = MaxChannels;
	}
	else if(channelCount[0] == 0)
	{
		// Use all of input one
		for(int c = 0; c < MaxChannels; ++c)
		{
			channelMask[1][c] = true;
		}
		channelCount[1] = MaxChannels;
	}
	else
	{
		// The general case.

		// First: match corresponding channels across-wise
		for(int c = 0; c < MaxChannels; ++c)
		{
			if(channelMask[0][c] && !channelMask[1][c])
			{
				channelMask[1][c] = true;
				++channelCount[1];
			}
			else if(channelMask[1][c] && !channelMask[0][c])
			{
				channelMask[0][c] = true;
				++channelCount[0];
			}
			if(channelCount[0] + channelCount[1] >= MaxChannels)
			{
				break;
			}
		}
		if(channelCount[0] + channelCount[1] < MaxChannels)
		{
			// Then: work outward from middle filling in additional channels
			for(int x = 0; x < MaxChannels/2; ++x)
			{
				int c;
				
				if(channelCount[0] >= MaxChannels/2)
				{
					break;
				}
				c = MaxChannels/2-1 + x;
				if(!channelMask[0][c])
				{
					channelMask[0][c] = channelMask[1][c] = true;
					++channelCount[0];
					++channelCount[1];
				}

				if(channelCount[0] >= MaxChannels/2)
				{
					break;
				}
				c = MaxChannels/2-1 - x;
				if(!channelMask[0][c])
				{
					channelMask[0][c] = channelMask[1][c] = true;
					++channelCount[0];
					++channelCount[1];
				}
			}
		}
	}

	if(channelCount[0] + channelCount[1] != MaxChannels)
	{
		cerr << "Developer error: pystream.writeChannelSet5A: Total number of channels is " << (channelCount[0] + channelCount[1]) << " but should be " << MaxChannels << endl;

		exit(EXIT_FAILURE);
	}

	// Finally, write out all selected channels
	*this << "channelSet" << modeNum << " = [";
	bool first = true;
	for(int i = 0; i < 2; ++i)
	{
		for(int c = 0; c < MaxChannels; ++c)
		{
			if(channelMask[i][c])
			{
				int freqMHz = 1040 - 32*c;

				if(first)
				{
					first = false;
				}
				else
				{
					*this << ",";
				}
				*this << " \\" << endl;

				*this << "  bbc(" << i << ", " << freqMHz << ", " << bwMHz << ", 'L', 2, 0)";
			}
		}
	}
	*this << " \\" << endl;
	*this << "  ]" << endl;

	writeImplicitConversionComment(implicitConversions);

	return 0;
}

int pystream::writeLoifTable(const VexData *V)
{
	map<string,VexIF>::const_iterator it;
	int p;
	stringstream ss;
	unsigned int nMode = V->nMode();
	bool isRDV = false;

	loifSetupFirstUse.resize(nMode);

	p = precision();
	precision(15);

	if(V->getExper()->name.find("RDV") != string::npos)
	{
		isRDV = true;
	}

	for(unsigned int modeNum = 0; modeNum < nMode; ++modeNum)
	{
		const VexMode *mode = V->getMode(modeNum);
		const VexSetup *setup = mode->getSetup(ant);

		// initialize this here, will use this during writeScans
		loifSetupFirstUse[modeNum] = true;

		if(!setup)
		{
			continue;
		}

		if(scriptType == SCRIPT_VLBA || scriptType == SCRIPT_GBT)
		{
			if(setup->ifs.size() > 2)
			{
				cout << "Warning: mode " << mode->defName << " wants " << setup->ifs.size() << " IFs, and we can currently only use 2" << endl;
				cout << "Might be ok for S/X setup or wideband Cband receiver" << endl;
			}

			*this << "loif" << modeNum << " = VLBALoIfSetup()" << endl;
			for(it = setup->ifs.begin(); it != setup->ifs.end(); ++it)
			{
				const int MaxCommentLength = 256;
				const VexIF &i = it->second;
				char comment[MaxCommentLength] = {0};
				double firstTune = fabs(setup->firstTuningForIF(i.name) - i.ifSSLO);

				//*this << "# first tuning = " << (firstTune * 1.0e-6) << " MHz" << endl;
 				*this << "loif" << modeNum << ".setIf('" << i.name << "', '" << i.VLBABandName() << "', '" << i.pol << "', " << (i.ifSSLO / 1.0e6) << ", '" << i.ifSideBand << "'";


				strncpy(comment, i.comment.c_str(), MaxCommentLength-1);
				if(comment[0] == 0 && isRDV)
				{
					if(i.ifSSLO == 2.9e9)
					{
						strcpy(comment, "* 13cm 0 NA");
					}
					else if(i.ifSSLO == 7.9e9)
					{
						strcpy(comment, "* 4cm 0 NA");
					}
					else
					{
						cout << "Error: RDV experiment using unexpected LO frequency " << (i.ifSSLO / 1.0e6) << " MHz.  Please file a bug report." << endl;
					}
				}
				if(comment[0] != '\0')
				{
					int len = strlen(comment);
					int off = 1;
					int field_count = 0;
					// parse BACKWARDS from end of string for three space-separated tokens
					// comment format: * [other comments] [{receiver} {FirstLO} {BROAD|NARROW|NA}]
					// trailing spaces are permitted
					while(field_count <= 2 && off < len)
					{
						// remove trailing WS
						while(comment[len - off] == ' ' || comment[len - off] == '\t')
						{
							// printf("len: %i -- off: %i -- str: <%s>\n", len, off, (&comment[len - off]));
							++off;
						}
						// terminate string and advance offset past WS
						comment[len - (off - 1)] = '\0';
						++off;
						// printf("parsing field %i\n", field_count);
						while(comment[len - off] != ' ' && comment[len - off] != '\t' && off < len)
						{
							// printf( "char >%c<\n", startOfComment[len-off] );
							// printf("len: %i -- off: %i -- str: <%s>\n", len, off, (&comment[len - off]));
							++off;
						}
						if(field_count == 0)
						{
							//check format of comment
							if(strcmp("BROAD", &(comment[len - off + 1])) != 0 && strcmp("NARROW", &(comment[len - off + 1])) != 0 && strcmp("NA", &(comment[len - off + 1])) != 0)
							{
								// comment doesn't fit our "special format", don't process
								field_count = 3;
								continue;
							}
						}
						// assign value to proper field
						switch(field_count)
						{
							// filter
							case 0: 
								*this << ", '" << &(comment[len - off + 1]) << "'";
								++field_count;
								break;
							// firstLO
							case 1:
								*this << ", " << atoi(&comment[len - off + 1]);
								++field_count;
								break;
							// receiver
							case 2:
								*this << ", '" << &(comment[len - off + 1]) << "'";
								++field_count;
								break;
						}
						// terminate partial string
						comment[len - off] = '\0';
						++off;
						// printf("remaining comment: >%s<\n", comment);
					}
				} 
				else 
				{
					// no comment to process
					cerr << "Error: vex file contains if_def without needed receiver information" << endl;
					cerr << "Receiver and filter information is required in if_def line comments" << endl;

					exit(EXIT_FAILURE);
				}

				// tuning of first channel attached to this IF; not needed for GBT
				if(scriptType != SCRIPT_GBT) {
					*this << ", " << (firstTune * 1.0e-6);
				}

				// close statement
				*this << ")" << endl;
			}
			*this << "loif" << modeNum << ".setPhaseCal(" << (setup->phaseCalIntervalMHz()) << ")" << endl;

			// auto gain/attenuation control
			*this << "loif" << modeNum << ".setDBEParams(0, -1, -1, 10, 0)" << endl;
			if(setup->ifs.size() == 2)
				*this << "loif" << modeNum << ".setDBEParams(1, -1, -1, 10, 0)" << endl;

			*this << "loif" << modeNum << ".setDBERemember(0, 1)" << endl;
			if(setup->ifs.size() == 2)
				*this << "loif" << modeNum << ".setDBERemember(1, 1)" << endl;

			if(isMark5A)
			{
				writeChannelSet5A(setup, modeNum); 
			}
			else
			{
				writeChannelSet(setup, modeNum); 
			}

			*this << endl;
		}
		else if(scriptType == SCRIPT_EVLA)
		{
			double freq1, freq2;
			freq1 = -1;
			freq2 = -1;

			//work out how many different LOs there are - complain if more than two (frequencies, not freq/pols)
			if(setup->ifs.size() > 4)
                        {
				cerr << "Error: mode " << mode->defName << " wants " << setup->ifs.size() << " IFs, and we can currently only use 4" << endl;

				exit(EXIT_FAILURE);
			}
			//better be two dual pol, otherwise abort
			for(it = setup->ifs.begin(); it != setup->ifs.end(); ++it)
			{
				const VexIF &i = it->second;
				if(freq1 < 0)
				{
					freq1 = i.getLowerEdgeFreq();
				}
				else
				{
					if(i.getLowerEdgeFreq() == freq1)
					{
						continue;
					}
					else
					{
						if(freq2 < 0)
						{
							freq2 = i.getLowerEdgeFreq();
						}
						else
						{
							if(i.getLowerEdgeFreq() != freq2)
							{
								cerr << "Error: More than 2 IF frequencies" << endl;

								exit(EXIT_FAILURE);
							}
						}
					}
				}
			}

			//can always get away with the 2x1 GHz 8bit samplers
			const VexIF & i1 = setup->ifs.begin()->second;
			const VexIF *i2 = &i1;
			if(setup->ifs.size() == 2)
			{
				i2 = &(setup->ifs.begin()++)->second;
			}
			if(freq2 < 0)
			{
				freq2 = i2->getLowerEdgeFreq();
			}
			*this << "loif" << modeNum << " = LoIfSetup('" << i1.VLBABandName() << "', " << freq1/1.0e6 << ", 0.0, " << freq2/1.0e6 << ", 0.0)" << endl;
			//write an appropriate VCI document, and set it up to be used
			ss << modeNum;
			string vcifilename = evlaVCIDir + "/" + obsCode + ss.str() + ".vci";
			writeVCI(V, modeNum, vcifilename);
			*this << "setup" << modeNum << " = '" << vcifilename << "'" << endl;
			*this << endl;
		}
	}
	if(scriptType == SCRIPT_EVLA)
	{
		*this << "autophase0 = subarray.registerPhasing('A')" << endl;
		*this << "autophase1 = subarray.registerPhasing('B')" << endl;
		*this << "autophase2 = subarray.registerPhasing('C')" << endl;
		*this << "autophase3 = subarray.registerPhasing('D')" << endl;
		*this << "subarray.usePhasing(autophase0, 'A')" << endl;
		*this << "subarray.usePhasing(autophase1, 'B')" << endl;
		*this << "subarray.usePhasing(autophase2, 'C')" << endl;
		*this << "subarray.usePhasing(autophase3, 'D')" << endl;
	}

	precision(p);

	return nMode;
}

int pystream::writeSourceTable(const VexData *V)
{
	int nSource;
	int p;
	string intentstring;

	nSource = V->nSource();

	p = precision();
	precision(15);

	for(int s = 0; s < nSource; ++s)
	{
		const VexSource *S = V->getSource(s);
		*this << "source" << s << " = Source(" << S->ra << ", " << S->dec << ")" << endl;
		*this << "source" << s << ".setName('" << S->defName << "')" << endl;
		if(scriptType == SCRIPT_EVLA)
		{
			*this << "intent" << s << " = Intention()" << endl;
			//No point putting in calibrator code until its populated in the vex file
			//*this << "intent" << s << ".addIntent('CalibratorCode=\"" << S->calCode << "\"')" << endl;
			intentstring = "UNSPECIFIED";
			for(unsigned int i = 0; i < phasingSources.size(); ++i)
			{
				if(phasingSources.at(i) == S->defName)
				{
					intentstring = "CALIBRATE_AUTOPHASE";
				}
			}
			*this << "intent" << s << ".addIntent('ScanIntent=\"" << intentstring << "\"')" << endl;
		}

		*this << endl;
	}

	precision(p);

	return nSource;
}

int pystream::writeScansGBT(const VexData *V)
{
	int nScan;

	nScan = V->nScan();
	for(int s = 0; s < nScan; ++s)
	{
	}

	return nScan;
}

int pystream::writeScans(const VexData *V)
{
	int p;
	int n = 0;
	int nScan;
	const char *switchOutput[] = {"1A", "1B", "2A", "2B"};
	double recordSeconds = 0.0;
    string tab;
	double endLastScan = 0.0;

	nScan = V->nScan();

	p = precision();
	precision(14);

	for(int s = -1; s < nScan; ++s)
	{
		const VexScan *scan = (s==-1) ? V->getScan(0) : V->getScan(s);
		if(s ==  -1)
		{
			*this << "# Setup Scan " << endl;
		}
                else
		{
			*this << "# Scan " << s << " = " << scan->defName << endl;
		}
		if(scan->stations.count(ant) == 0)
		{
			*this << "# Antenna " << ant << " not in scan " << scan->defName << endl;
		}
		else
		{
            bool record = scan->recordEnable.find(ant)->second;
            if (ant == "GB")
            {
                if (!record)
                    *this << "# pointing scan for the GBT" << endl;
            }
			const VexInterval *arange = &scan->stations.find(ant)->second;

			int modeId = V->getModeIdByDefName(scan->modeDefName);
			const VexMode* mode = V->getModeByDefName(scan->modeDefName);
			if(mode == 0)
			{
				cerr << "Error: scan=" << scan->defName << " ant=" << ant << " mode=" << scan->modeDefName << " -> mode=0" << endl;
				continue;
			}
			
			const VexSetup* setup = mode->getSetup(ant);

			if(setup == 0)
			{
				cerr << "Error: scan=" << scan->defName << " ant=" << ant << " mode=" << scan->modeDefName << " -> setup=0" << endl;
				continue;
			}

			if(modeId != lastModeId)
			{

				*this << "# changing to mode " << mode->defName << endl;
				if(scriptType == SCRIPT_VLBA || scriptType == SCRIPT_GBT)
				{
					*this << "subarray.setVLBALoIfSetup(loif" << modeId << ")" << endl;

					map<string,unsigned int>::const_iterator ifit;
					for(ifit = ifIndex[modeId].begin(); ifit != ifIndex[modeId].end(); ++ifit)
					{
						if(ifit->first != sw[ifit->second])
						{
							sw[ifit->second] = ifit->first;
                            *this << "subarray.set4x4Switch('" << switchOutput[ifit->second] << "', "
                                         << switchPosition(ifit->first.c_str()) << ")" << endl;
						}
					}
					*this << "subarray.setChannels(dbe0, channelSet" << modeId << ")" << endl;
				}
				else if(scriptType == SCRIPT_EVLA)
				{
					*this << "subarray.setLoIfSetup(loif" << modeId << ")" << endl;
					*this << "subarray.setWidarSetup('setup" << modeId << "')" << endl;
				}

				lastModeId = modeId;
			}

			int sourceId = V->getSourceIdByDefName(scan->sourceDefName);
			if(scriptType == SCRIPT_EVLA)
			{
				*this << "intent" << sourceId << ".addIntent('ScanNumber=" << s+1 << "')" << endl;
			}
			if(sourceId != lastSourceId)
			{
				*this << "subarray.setSource(source" << sourceId << ")" << endl;
				lastSourceId = sourceId;
			}

			// TODO Once we control antenna movements, make sure we do not include antenna movement in
			// the setup scan if end time of first scan has already elapsed, so we do not interfere with
			// movement of antenna for subsequent scans. We still must execute all other setups steps.
			double deltat1 = floor((arange->mjdStart-mjd0)*86400.0 + 0.5);
			double deltat2 = floor((arange->mjdStop-mjd0)*86400.0 + 0.5);
			// execute() at stop time minus 5 seconds
			// arbitrary amount picked to allow commands to get sent to MIBs before they need to get run on MIBs
			double deltat3 = floor((arange->mjdStop-mjd0)*86400.0 + 0.5-5);

			if(s != -1)
			{
				// check for minimum time between scans as required by legacy system
				if( loifSetupFirstUse[modeId] ) {
					if( (arange->mjdStart - endLastScan)*86400.0 < 15.0) {
						// move start time
						deltat1 = floor((endLastScan-mjd0)*86400.0 + 15.0 + 0.5);
						cerr << "Scan " << scan->defName << " does not have minimum gap to previous scan ("
							<< (arange->mjdStart - endLastScan)*86400.0 <<  " vs 15). Start time moved!" << endl;
					}
					loifSetupFirstUse[modeId] = false;
				} else {
					if( (arange->mjdStart - endLastScan)*86400.0 < 5.0) {
						// move start time
						deltat1 = floor((endLastScan-mjd0)*86400.0 + 5.0 + 0.5);
						cerr << "Scan " << scan->defName << " does not have minimum gap to previous scan ("
							<< (arange->mjdStart - endLastScan)*86400.0 <<  " vs 5). Start time moved!" << endl;
					}
				}

				// recognize scans that do not record to Mark5C, but still set switches (need to pass scan start time)
				if(scan->nRecordChan(V, ant) == 0 || recorderType == RECORDER_NONE)
				{
					if( scriptType != SCRIPT_GBT ) {
                        *this << "#print 'Not a recording scan but still set switches for " << scan->defName << ".'" << endl;
                    }
					*this << "subarray.setSwitches(mjdStart + " << deltat1 << "*second, mjdStart+" << deltat2
						<< "*second, obsCode+'_'+stnCode+'_'+'" << scan->defName << "')" << endl;
				}
				else if(setup->formatName == "MARK5B")
				{
					*this << "recorder0.setPacket(0, 0, 36, 5008)" << endl;
					*this << "subarray.setRecord(mjdStart + " << deltat1 << "*second, mjdStart+" << deltat2 << "*second, '" << scan->defName << "', obsCode, stnCode )" << endl;
					recordSeconds += (deltat2-deltat1);
				}
				else
				{
					cerr << "Error: pystream::writeScans: Can't figure out how to record!  formatName=" << setup->formatName
						<< "  nRecChan=" << scan->nRecordChan(V, ant) << "  recType=" << recorderType << endl;

					exit(EXIT_FAILURE);
				}
                                tab = "";
			        if (ant == "GB" && !record)
			        {
				    // code for GBT pointing scan - set source as a 'peak' type
				    *this << "if isAstrid:" << endl;
				    tab = "    ";
				    *this << tab << "source" << sourceId << ".setPeak(True)" << endl;
				}

				// only start scan if we are at least 10sec away from scan end
				// NOTE - if this changes to a value less than 5sec may need to revisit Executor RDBE code
				// in case of scan starting later than start time
				*this << tab << "if array.time() < mjdStart + (" << deltat2 << "-10)*second:" << endl;
				*this << tab << "  subarray.execute(mjdStart + " << deltat3 << "*second)" << endl;
				*this << tab << "else:" << endl;
				*this << tab << "  print 'Skipping scan which ended at time ' + str(mjdStart+" << deltat2 << "*second) + ' since array.time is ' + str(array.time())" << endl;
				if (ant == "GB" && !record)
				{
					// code for GBT pointing scan - reset source as a 'track' type
					*this << tab << "source" << sourceId << ".setPeak(False)" << endl;
				    tab = "";
				}
				lastValid = arange->mjdStop;
				endLastScan = arange->mjdStop;
			}
			else
			{
				*this << "# Setup scan - run right away, but do not start recording" << endl;
				*this << "subarray.execute( array.time() + 2*second )" << endl;
			}
		}
		*this << endl;
	}

	cout << "There are " << static_cast<int>(recordSeconds) << " seconds of recording at " << ant << endl;

	precision(p);

	return n;
}

void pystream::writeVCI(const VexData *V, int modeIndex, const string &filename)
{
	string bbNames[2] = {"A0/C0", "B0/D0"};
	string swbbNames[2] = {"AC8BIT", "BD8BIT"};
	char timeString[64];
	int msgId, subarrayId, numIndFreqs;
	double centreFreq;
	bool found;
	string descString, indent, activateString;
	ofstream output(filename.c_str(), ios::trunc);
	struct tm tim;
	time_t now;
	const VexMode *mode = V->getMode(modeIndex);
	const VexSetup *setup = mode->getSetup(ant);

	if(!output.is_open() || output.fail())
	{
		cout << "Could not open VCI file " << filename << " - aborting!" << endl;

		exit(EXIT_FAILURE);
	}

	subarrayId = 1;
	indent = "";
	descString = "Project " + obsCode;
	activateString = "vlbistart" + obsCode;
	now = time(NULL);
	tim = *(gmtime(&now));
	strftime(timeString,64,"%Y-%m-%dT%H:%M:%S.000-00:00",&tim);
	msgId = (tim.tm_sec + 11)*tim.tm_mday*tim.tm_year;

	output << indent << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" << endl;
	output << indent << "<widar:vciRequest version=\"" << evlaVCIVersion << "\" timeStamp=\"" << timeString << "\" msgId=\"" << msgId++ << "\" desc=\"" << descString << "\" xmlns:widar=\"http://www.nrc.ca/namespaces/widar\">" << endl;
	indent += "    ";
	output << indent << "<widar:subArray timeStamp=\"" << timeString << "\" subarrayId=\"" << subarrayId << "\" activationId=\"" << activateString << "\" action=\"create\">" << endl;
	indent += "    ";
	output << indent << "<widar:stationInputOutput sid=\"all\">" << endl;
	indent += "    ";
        output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"R\" bbid=\"0\"/>" << endl;
	output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"L\" bbid=\"2\"/>" << endl;
	output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"R\" bbid=\"4\"/>" << endl;
	output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"L\" bbid=\"6\"/>" << endl;

	numIndFreqs = 0;
	double *ifFreqs = new double[setup->ifs.size()];
	map<string,VexIF>::const_iterator it = setup->ifs.begin();
	
	for(unsigned int i = 0; i < setup->ifs.size(); ++i, ++it)
	{
		const VexIF & vif = it->second;
		found = false;
		for(int j = 0; j < numIndFreqs; ++j)
		{
			//cout << "Checking " << vif.getLowerEdgeFreq() << " against " << ifFreqs[j] << endl;
			if(vif.getLowerEdgeFreq() == ifFreqs[j])
			{
				cout << "Skipping IF " << i+1 << "/" << setup->ifs.size() << endl;
				found = true; //skip this "IF", its the same frequency as a previous one
				break;
			}
		}
		if(found)
		{
			continue;
		}
		ifFreqs[numIndFreqs++] = vif.getLowerEdgeFreq();
		output << indent << "<widar:baseBand singlePhaseCenter=\"yes\" name=\"" << bbNames[numIndFreqs] << "\" swbbName=\"" << swbbNames[numIndFreqs] << "\" inQuant=\"8\" bw=\"1024000000\" bbB=\"" << 4*numIndFreqs+2 << "\" bbA=\"" << 4*numIndFreqs << "\">" << endl;
		indent += "    ";
		for(unsigned int j = 0; j < setup->channels.size(); ++j)
		{
			if(setup->channels[j].ifName != vif.name) //this channel belongs to the other IF
			{
				continue;
			}
			centreFreq = setup->channels[j].bbcFreq - vif.getLowerEdgeFreq() + setup->channels[j].bbcBandwidth/2.0;
			if(setup->channels[i].bbcSideBand == 'L')
			{
				centreFreq -= setup->channels[i].bbcBandwidth;
			}
			output << indent << "<widar:subBand sbid=\"" << j << "\" rqNumBits=\"" << evlasbBits << "\" centralFreq=\"" << ((long long)centreFreq) << "\" bw=\"" << static_cast<int>(setup->channels[j].bbcBandwidth) << "\">" << endl;
			indent += "    ";
			output << indent << "<widar:polProducts>" << endl;
			indent += "    ";
			output << indent << "<widar:pp spectralChannels=\"" << evlasbChan << "\" id=\"1\" correlation=\"A*A\"/>" << endl;
			output << indent << "<widar:pp spectralChannels=\"" << evlasbChan << "\" id=\"2\" correlation=\"A*B\"/>" << endl;
			output << indent << "<widar:pp spectralChannels=\"" << evlasbChan << "\" id=\"3\" correlation=\"B*A\"/>" << endl;
			output << indent << "<widar:pp spectralChannels=\"" << evlasbChan << "\" id=\"4\" correlation=\"B*B\"/>" << endl;
			output << indent << "<widar:blbProdIntegration recirculation=\"1\" minIntegTime=\"200.0\" ltaIntegFactor=\"2500\" ccIntegFactor=\"2\" cbeIntegFactor=\"" << evlaIntSec << "\"/>" << endl;
			output << indent << "<widar:blbPair quadrant=\"" << numIndFreqs+1 << "\" numBlbPairs=\"1\" firstBlbPair=\"" << j+1 << "\"/>" << endl;
			output << indent << "<widar:stationPacking algorithm=\"maxPack\"/>" << endl;
			output << indent << "<widar:productPacking algorithm=\"maxPack\"/>" << endl;
			indent = indent.substr(0, indent.length()-4);
			output << indent << "</widar:polProducts>" << endl;
			indent = indent.substr(0, indent.length()-4);
			output << indent << "</widar:subBand>" << endl;
		}
		indent = indent.substr(0, indent.length()-4);
		output << indent << "</widar:baseBand>" << endl;
	}
	indent = indent.substr(0, indent.length()-4);
	output << indent << "</widar:stationInputOutput>" << endl;
	indent = indent.substr(0, indent.length()-4);
	output << indent << "</widar:subArray>" << endl;
	output << indent << "<widar:activationTrigger timeStamp=\"" << timeString << "\" msgId=\"" << msgId++ << "\" activationTime=\"" << timeString << "\" activationId=\"" << activateString << "\"/>" << endl;
	indent = indent.substr(0, indent.length()-4);
	output << indent << "</widar:vciRequest>" << endl;
	output.close();
	delete [] ifFreqs;
}

void pystream::setDBEPersonality(const string &filename)
{
	dbeFilename = filename;

	if(strcasestr(dbeFilename.c_str(), "DDC") != 0)
	{
		setDBEPersonalityType(RDBE_DDC);
	}
	else if(strcasestr(dbeFilename.c_str(), "PFB") != 0)
	{
		setDBEPersonalityType(RDBE_PFB);
	}
}
