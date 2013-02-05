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

using namespace std;

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

string optresources::VLArcvr(string receiver)
{
	// VLA notation
	if( receiver == "P" || receiver == "p"
		|| receiver == "L" || receiver == "l"
		|| receiver == "S" || receiver == "s"
		|| receiver == "C" || receiver == "c"
		|| receiver == "X" || receiver == "x"
		|| receiver == "Ku" || receiver == "ku"
		|| receiver == "K" || receiver == "k"
		|| receiver == "Q" || receiver == "q" )
		return receiver;
	// VLBA notation
	if( receiver == "90cm" || receiver == "50cm" )
		return "P";
	if( receiver == "20cm" )
		return "L";
	if( receiver == "13cm" )
		return "S";
	if( receiver == "6cm" )
		return "C";
	if( receiver == "4cm" )
		return "X";
	if( receiver == "2cm" )
		return "Ku";
	if( receiver == "1cm" || receiver == "1.3cm" )
		return "K";
	if( receiver == "7mm" )
		return "Q";
	// receiver not recognized
	return "<unknown receiver>";
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

	*this << "# <name>; <receiver> ; 1; <arraySumming>; {<baseband pair>, <centerSkyFreq>, <subbandCount>,"
		<< " <subbandBandwidth>, <blbpsPerSubband>, <polarizationProducts>;}" << endl;
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
			if( setup->ifs.size() > 0 ) {
				ss << "loif" << modeNum << "; ";
			} else {
				cerr << mode->defName << " (loif" << modeNum << ") doesn't have any setup defined - skipping!" << endl;
			}
			for(it = setup->ifs.begin(); it != setup->ifs.end(); ++it)
			{
				const int MaxCommentLength = 256;
				const VexIF &i = it->second;
				const VexIF  *vif = 0;
				char comment[MaxCommentLength] = {0};

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
						//printf("parsing field %i\n", field_count);
						while(comment[len - off] != ' ' && comment[len - off] != '\t' && off < len)
						{
							//printf( "char >%c< >%i<\n", comment[len-off], comment[len-off] );
							//printf("len: %i -- off: %i -- str: <%s>\n", len, off, (&comment[len - off]));
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
					}
					if( receiver.empty() ) {
						cerr << "Error: vex file contains if_def without needed receiver information" << endl;
						exit(EXIT_FAILURE);
					}
					if( once )  {
						ss << VLArcvr(receiver) << "; 1; Y; ";
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
//						cerr << "ifName for chan " << k << ": " << setup->channels[k].ifName << endl;
						// run through channels we already processed up to i-1 to see if this is the
						// 2nd of a R/L pair in A/C or B/D
						bool foundPair = false;
						for( unsigned j=0; k!=0 && j<=(k-1) && !foundPair; j++ ) {
//							cerr << "f[" << k << "]: " << setup->channels[k].bbcFreq << ": f[" << j
//								<< "]: " << setup->channels[j].bbcFreq << endl;
							if((setup->channels[k].ifName == "A" && setup->channels[j].ifName == "C")
								|| (setup->channels[k].ifName == "C" && setup->channels[j].ifName == "A")
								|| (setup->channels[k].ifName == "B" && setup->channels[j].ifName == "D")
								|| (setup->channels[k].ifName == "D" && setup->channels[j].ifName == "B")) {
								foundPair = true;
								if(setup->channels[k].bbcFreq != setup->channels[j].bbcFreq) {
/*									cerr << "Invalid frequency assignment between " << setup->channels[k].ifName
										<< "(" << setup->channels[k].bbcFreq << ") and " << setup->channels[j].ifName
										<< "(" << setup->channels[j].bbcFreq << ")" << endl;
*/									exit(EXIT_FAILURE);
								}
//								cerr << "processing k=" << k << ", found 1st half of pair at j=" << j << endl;
							}
						}
// TODO ? need to process both IF of a pair to figure out polarizations, single or dual; handle if we have only a single IF
						if( foundPair ) {
								ss.str(string());
								continue;
							}
						// run through channels we still need to process from k+1 up to setup->channels.size() to figure out polarization
						for( unsigned j=k+1; j< setup->channels.size(); j++ ) {
//							cerr << "f[" << k << "]: " << setup->channels[k].bbcFreq << ": f[" << j
//									<< "]: " << setup->channels[j].bbcFreq << endl;
							if((setup->channels[k].ifName == "A" && setup->channels[j].ifName == "C")
									|| (setup->channels[k].ifName == "C" && setup->channels[j].ifName == "A")
									|| (setup->channels[k].ifName == "B" && setup->channels[j].ifName == "D")
									|| (setup->channels[k].ifName == "D" && setup->channels[j].ifName == "B")) {
								foundPair = true;
//								cerr << "processing k=" << k << ", found 2nd half of pair at j=" << j << endl;
								vif = setup->getIF(setup->channels[j].ifName);
								if(vif) {
//									cerr << "pol: k=" << i.pol << " and j=" << vif->pol << endl;
								 } else {
									cerr << "developer error: somehow vif=0 after foundPair=true - pol: k=" << i.pol << endl;
									exit(0);
								}
							}
						}
						string pol;
						if( foundPair ) {
							if(!vif) {
								cerr << "developer error: somehow vif=0 and foundPair=true" << endl;
								exit(0);
							}

							if( i.pol == vif->pol ) // vif will be set if found is true
								pol = i.pol;
							else
								pol = "FULL";  // force to use full for now
								//pol = "DUAL";
						}
						double bw = setup->channels[k].bbcBandwidth;
						char sb = setup->channels[k].bbcSideBand;
						double freq = setup->channels[k].bbcFreq;
						*this << ss.str() << (freq + ((sb == 'L') ? -1 : 1)*bw/2)/1000 << "kHz, ," << bw/1000 << "kHz, ," << pol << "; ";
						ss.str(string());
					}
				}
			}
			*this << endl;
	}
	precision(p);

	return nMode;
}
