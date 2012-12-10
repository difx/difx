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
 * $Id: optresources.cpp 4831 2012-09-20 06:03:40Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/optresources.cpp $
 * $LastChangedRevision: 4831 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2012-09-20 00:03:40 -0600 (Thu, 20 Sep 2012) $
 *
 *==========================================================================*/

#include <cstdlib>
#include <cmath>
#include <sstream>
#include <time.h>
#include <string.h>
#include "evladefaults.h"
#include "optresources.h"

void optresources::open(const string& antennaName, const VexData *V)
{
	string extension;

	evlaIntSec     = DEFAULT_EVLA_INT_SEC;
	evlasbBits     = DEFAULT_EVLA_SB_BITS;
	evlasbChan     = DEFAULT_EVLA_SB_CHAN;
	evlaVCIDir     = DEFAULT_EVLA_VCI_DIR;
	evlaVCIVersion = DEFAULT_EVLA_VCI_VER;
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
	
	extension = ".resources";

	fileName = string(obsCode) + string(".") + antennaName + extension;
	ofstream::open(fileName.c_str());
}

void optresources::addPhasingSource(const string &sourceName)
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

void optresources::calcIfIndex(const VexData *V)
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

int optresources::maxIFs(const VexData *V) const
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

void optresources::close()
{
	int p = precision();
	precision(14);

	if(lastValid != 0.0)
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

	precision(p);

	ofstream::close();
}

int optresources::writeHeader(const VexData *V)
{
	*this << "VERSION; 1;" << endl;

	return 0;
}

int optresources::writeComment(const string &commentString)
{
	*this << "# " << commentString << endl;
	*this << endl;

	return 0;
}

int optresources::writeRecorderInit(const VexData *V)
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

void optresources::figurePersonality(const VexData *V)
{
	if(personalityType != RDBE_UNKNOWN)
	{
		cerr << "Developer error: optresources::figurePersonality called with personalityType != RDBE_UNKNOWN (" << RDBE_UNKNOWN << ").  It was " << personalityType << "." << endl;

		exit(EXIT_FAILURE);
	}

	for(int m = 0; m < V->nMode(); ++m)
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
				cerr << "Developer error: optresources::figurePersonality: setup->getIF(" << setup->channels[i].ifName << ") returned NULL" << endl;

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

void optresources::writeImplicitConversionComment(const vector<unsigned int> &implicitConversions)
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

int optresources::writeChannelSet(const VexSetup *setup, int modeNum)
{
	*this << "loif" << modeNum;

	for(unsigned int i = 0; i < setup->channels.size(); ++i)
	{
	for( unsigned j=i; j<setup->channels.size(); j++ ) {
			if( setup->channels[i].ifName == "A" ) {
				if( setup->channels[j].ifName == "C" ) {
					if(setup->channels[i].bbcFreq != setup->channels[j].bbcFreq) {
						cerr << "Invalid frequency assignment between A(" << setup->channels[i].bbcFreq
							<< ") and C(" << setup->channels[j].bbcFreq << ")" << endl;
						exit(EXIT_FAILURE);
					}
				}
			}
			if( setup->channels[i].ifName == "C" ) {
				if( setup->channels[j].ifName == "A" ) {
					if(setup->channels[i].bbcFreq != setup->channels[j].bbcFreq) {
						cerr << "Invalid frequency assignment between C(" << setup->channels[i].bbcFreq
							<< ") and A(" << setup->channels[j].bbcFreq << ")" << endl;
						exit(EXIT_FAILURE);
					}
				}
			}
			if( setup->channels[i].ifName == "B" ) {
				if( setup->channels[j].ifName == "D" ) {
					if(setup->channels[i].bbcFreq != setup->channels[j].bbcFreq) {
						cerr << "Invalid frequency assignment between B(" << setup->channels[i].bbcFreq
							<< ") and D(" << setup->channels[j].bbcFreq << ")" << endl;
						exit(EXIT_FAILURE);
					}
				}
			}
			if( setup->channels[i].ifName == "D" ) {
				if( setup->channels[j].ifName == "B" ) {
					if(setup->channels[i].bbcFreq != setup->channels[j].bbcFreq) {
						cerr << "Invalid frequency assignment between D(" << setup->channels[i].bbcFreq
							<< ") and B(" << setup->channels[j].bbcFreq << ")" << endl;
						exit(EXIT_FAILURE);
					}
				}
			}
		}
		*this << "; ";

		// run through channels we already processed up to i-1 to see if this is the 2nd of a R/L pair
		bool foundPair = false;
		for( unsigned j=0; i!=0 && j<=(i-1) && !foundPair; j++ ) {
//			cerr << "f[" << i << "]: " << setup->channels[i].bbcFreq << ": f[" << j << "]: " << setup->channels[j].bbcFreq << endl;
			if(setup->channels[i].ifName == "A" && setup->channels[j].ifName == "C"
				|| setup->channels[i].ifName == "C" && setup->channels[j].ifName == "A"
				|| setup->channels[i].ifName == "B" && setup->channels[j].ifName == "D"
				|| setup->channels[i].ifName == "D" && setup->channels[j].ifName == "B") {
				foundPair = true;
				cerr << "found pair at i=" << i << endl;
			}
		}
		cerr << "ifName for chan " << i << ": " << setup->channels[i].ifName << endl;
		if( foundPair ) {
			continue;
		}

		unsigned int inputNum = ifIndex[modeNum][setup->channels[i].ifName];
		double bw = setup->channels[i].bbcBandwidth;
		char sb = setup->channels[i].bbcSideBand;
		unsigned int nBit = setup->nBit;
		unsigned int threadId = 0;
		const VexIF *vif = setup->getIF(setup->channels[i].ifName);
		double freq = setup->channels[i].bbcFreq;
		if(!vif)
		{
			cerr << "Developer error: setup->getIF(" << setup->channels[i].ifName << ") returned NULL" << endl;

			exit(EXIT_FAILURE);
		}

		*this << freq + ((sb == 'L') ? -1 : 1)*bw/2 << "MHz";
	}

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

int optresources::writeLoifTable(const VexData *V)
{
	map<string,VexIF>::const_iterator it;
	int p;
	stringstream ss;
	unsigned int nMode = V->nMode();
	string filter, firstLO, receiver;
	string out;

	p = precision();
	precision(15);

	*this << "# <name>; <receiver> ; 1; <arraySumming>; <baseband pair>, <centerSkyFreq>, <subbandCount>, <subbandBandwidth>, <blbpsPerSubband>, <polarizationProducts>;" << endl;
	for(unsigned int modeNum = 0; modeNum < nMode; ++modeNum)
	{
		const VexMode *mode = V->getMode(modeNum);
		const VexSetup *setup = mode->getSetup(ant);
		string lastBaseband;
		bool once = true;

		if(!setup)
		{
			continue;
		}

			// resourceName
			ss << "loif" << modeNum << "; ";
			for(it = setup->ifs.begin(); it != setup->ifs.end(); ++it)
			{
				const int MaxCommentLength = 256;
				const VexIF &i = it->second;
				const VexIF  *vif;
				char comment[MaxCommentLength] = {0};
				double firstTune = fabs(setup->firstTuningForIF(i.name) - i.ifSSLO);

//				*this << "first: " << setup->firstTuningForIF(i.name) << ": SSLO " << i.ifSSLO << endl;
// 				*this << "loif" << modeNum << "; ";
// 				ss << "loif" << modeNum << "; ";
				// ".setIf('" << i.name << "', '" << i.VLBABandName() << "', '" << i.pol << "', " << (i.ifSSLO / 1.0e6) << ", '" << i.ifSideBand << "'";


				strncpy(comment, i.comment.c_str(), MaxCommentLength-1);
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
							if(strcmp("BROAD", &(comment[len - off + 1])) != 0
								&& strcmp("NARROW", &(comment[len - off + 1])) != 0
								&& strcmp("NA", &(comment[len - off + 1])) != 0)
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
								 filter = string(&comment[len - off + 1]);
								++field_count;
								break;
							// firstLO
							case 1:
								firstLO = string(&comment[len - off + 1]);
								++field_count;
								break;
							// receiver
							case 2:
								receiver = string(&comment[len - off + 1]);
								++field_count;
								break;
						}
						// terminate partial string
						comment[len - off] = '\0';
						++off;
						// printf("remaining comment: >%s<\n", comment);
					}
					if( once )  {
						ss << receiver << "; 1; ; ";
						once = false;
					}
					lastBaseband = string(i.name);
					if( lastBaseband == "A" || lastBaseband == "C" ) {
						ss << "A0/C0";
					} else if( lastBaseband == "B" || lastBaseband == "D" ) {
						ss << "B0/D0";
					}
					ss << ", ";
				} 
				else 
				{
					// no comment to process
					cerr << "Error: vex file contains if_def without needed receiver information" << endl;
					cerr << "Receiver and filter information is required in if_def line comments" << endl;

					exit(EXIT_FAILURE);
				}

				for(unsigned int k = 0; k < setup->channels.size(); k++) {
					// look for channel that uses this IF; there should only be one
					if( setup->channels[k].ifName == string(i.name) ) {
						cerr << "ifName for chan " << k << ": " << setup->channels[k].ifName << endl;
						// run through channels we already processed up to i-1 to see if this is the
						// 2nd of a R/L pair in A/C or B/D
						bool foundPair = false;
						for( unsigned j=0; k!=0 && j<=(k-1) && !foundPair; j++ ) {
							cerr << "f[" << k << "]: " << setup->channels[k].bbcFreq << ": f[" << j << "]: " << setup->channels[j].bbcFreq << endl;
							if(setup->channels[k].ifName == "A" && setup->channels[j].ifName == "C"
								|| setup->channels[k].ifName == "C" && setup->channels[j].ifName == "A"
								|| setup->channels[k].ifName == "B" && setup->channels[j].ifName == "D"
								|| setup->channels[k].ifName == "D" && setup->channels[j].ifName == "B") {
								foundPair = true;
								if(setup->channels[k].bbcFreq != setup->channels[j].bbcFreq) {
									cerr << "Invalid frequency assignment between " << setup->channels[k].ifName
										<< "(" << setup->channels[k].bbcFreq << ") and " << setup->channels[j].ifName
										<< "(" << setup->channels[j].bbcFreq << ")" << endl;
									exit(EXIT_FAILURE);
								}
								cerr << "processing k=" << k << ", found 1st half of pair at j=" << j << endl;
							}
						}
// TODO ? need to process both IF of a pair to figure out polarizations, single or dual; handle if we have only a single IF
						if( foundPair ) {
							ss.str(string());
							continue;
						}
						// run through channels we still need to process from k+1 up to setup->channels.size() to figure out polarization
						for( unsigned j=k+1; j< setup->channels.size(); j++ ) {
							cerr << "f[" << k << "]: " << setup->channels[k].bbcFreq << ": f[" << j << "]: " << setup->channels[j].bbcFreq << endl;
							if(setup->channels[k].ifName == "A" && setup->channels[j].ifName == "C"
								|| setup->channels[k].ifName == "C" && setup->channels[j].ifName == "A"
								|| setup->channels[k].ifName == "B" && setup->channels[j].ifName == "D"
								|| setup->channels[k].ifName == "D" && setup->channels[j].ifName == "B") {
								foundPair = true;
								cerr << "processing k=" << k << ", found 2nd half of pair at j=" << j << endl;
								vif = setup->getIF(setup->channels[j].ifName);
								cerr << "pol: k=" << i.pol << " and j=" << vif->pol << endl;
							}
						}
						string pol;
						if( foundPair ) {
							if( i.pol == vif->pol ) // vif will be set if found is true
								pol = i.pol;
							else
								pol = "DUAL";
						}
						double bw = setup->channels[k].bbcBandwidth;
						char sb = setup->channels[k].bbcSideBand;
						double freq = setup->channels[k].bbcFreq;
						*this << ss.str() << (freq + ((sb == 'L') ? -1 : 1)*bw/2)/1000 << "KHz, , , , " << pol << "; ";
						ss.str(string());
					}
				}
			}
			*this << endl;
	}
	precision(p);

	return nMode;
}

int optresources::writeScans(const VexData *V)
{
	int p;
	int n = 0;
	int nScan;
	const char *switchOutput[] = {"1A", "1B", "2A", "2B"};
	double recordSeconds = 0.0;

	nScan = V->nScan();

	p = precision();
	precision(14);

	for(int s = 0; s < nScan; ++s)
	{
		const VexScan *scan = V->getScan(s);
		*this << "# Scan " << s << " = " << scan->defName << endl;

		if(scan->stations.count(ant) == 0)
		{
			*this << "# Antenna " << ant << " not in scan " << scan->defName << endl;
		}
		else
		{
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

				lastModeId = modeId;
			}

			int sourceId = V->getSourceIdByDefName(scan->sourceDefName);
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

			// recognize scans that do not record to Mark5C, but still set switches (need to pass scan start time)
			if(scan->nRecordChan(V, ant) == 0 || recorderType == RECORDER_NONE)
			{
				*this << "print 'Not a recording scan but still set switches for " << scan->defName << ".'" << endl;
				*this << "subarray.setSwitches(mjdStart + " << deltat1 << "*second, mjdStart+" << deltat2 << "*second, obsCode+'_'+stnCode+'_'+'" << scan->defName << "')" << endl;
			}
			else if(setup->formatName == "MARK5B")
			{
				*this << "recorder0.setPacket(0, 0, 36, 5008)" << endl;
				*this << "subarray.setRecord(mjdStart + " << deltat1 << "*second, mjdStart+" << deltat2 << "*second, '" << scan->defName << "', obsCode, stnCode )" << endl;
				recordSeconds += (deltat2-deltat1);
			}
			else
			{
				cerr << "Error: optresources::writeScans: Can't figure out how to record!  formatName=" << setup->formatName << "  nRecChan=" << scan->nRecordChan(V, ant) << "  recType=" << recorderType << endl;

				exit(EXIT_FAILURE);
			}

			// only start scan if we are at least 10sec away from scan end
			// NOTE - if this changes to a value less than 5sec may need to revisit Executor RDBE code
			// in case of scan starting later than start time
			*this << "if array.time() < mjdStart + (" << deltat2 << "-10)*second:" << endl;
			*this << "  subarray.execute(mjdStart + " << deltat3 << "*second)" << endl;
			*this << "else:" << endl;
			*this << "  print 'Skipping scan which ended at time ' + str(mjdStart+" << deltat2 << "*second) + ' since array.time is ' + str(array.time())" << endl;
			lastValid = arange->mjdStop;
		}
		*this << endl;
	}

	cout << "There are " << static_cast<int>(recordSeconds) << " seconds of recording at " << ant << endl;

	precision(p);

	return n;
}

void optresources::setDBEPersonality(const string &filename)
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
