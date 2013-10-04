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
	*this << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" << endl << endl;

	*this << "<sss:vex2opt version=\"3\" xmlns:sss=\"http://www.nrao.edu/namespaces/sss\">" << endl;

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

void optresources::getIFComment(const VexIF *i) {
	const int MaxCommentLength = 256;
	char comment[MaxCommentLength] = {0};
	string filter, firstLO, receiver;
	bool once = true;

				strncpy(comment, i->comment.c_str(), MaxCommentLength-1);
				if(comment[0] != '\0')
				{
					int len = strlen(comment);
					int off = 1;
					int field_count = 0;
					int MAX_FIELDS = 5;
					// parse BACKWARDS from end of string for three space-separated tokens
					// comment format: * [other comments] [{FirstSynth} {SecondSynth} {receiver} {FirstLO} {BROAD|NARROW|NA}]
					// trailing spaces are permitted
					while(field_count <= (MAX_FIELDS-1) && off < len)
					{
						// remove trailing WS
						while(comment[len - off] == ' ' || comment[len - off] == '\t')
						{
//							printf("len: %i -- off: %i -- str: <%s>\n", len, off, (&comment[len - off]));
							++off;
						}
						// terminate string and advance offset past WS
						comment[len - (off - 1)] = '\0';
						++off;
//						printf("parsing field %i - comment: %s\n", field_count, comment);
						while(comment[len - off] != ' ' && comment[len - off] != '\t' && off < len)
						{
//							printf("char >%c< >%i<\n", comment[len-off], comment[len-off] );
//							printf("len: %i -- off: %i -- str: <%s>\n", len, off, (&comment[len - off]));
							++off;
						}
//						printf("process: %s\n", &comment[len-off+1]);
						if(field_count == 0)
						{
							//check format of comment
							if(strcmp("BROAD", &(comment[len - off + 1])) != 0
								&& strcmp("NARROW", &(comment[len - off + 1])) != 0
								&& strcmp("NA", &(comment[len - off + 1])) != 0)
							{
								// comment doesn't fit our "special format", don't process
								field_count = MAX_FIELDS;
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
							// firstSynth
							case 3:
								++field_count;
								break;
							// secondSynth
							case 4:
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
						*this << "band=\"" << VLArcvr(receiver) << "\" tInt=\"1.0\" arraySumming=\"true\">" << endl;
						once = false;
					}
				} 
				else 
				{
					// no comment to process
					cerr << "Error: vex file contains if_def without needed receiver information" << endl;
					cerr << "Receiver and filter information is required in if_def line comments" << endl;

					exit(EXIT_FAILURE);
				}

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

		if( setup->ifs.size() > 0 ) {
			*this << "  <resource name=\"loif" << modeNum << "\" ";
		}
		const VexIF  *vif0 = setup->getIF(setup->channels[0].ifName);
		if( vif0 == 0 ) {
			cerr << "Unknown IF? channel 0" << endl;
			exit(EXIT_FAILURE);
		}
		getIFComment(vif0);

			// resourceName
			if( setup->ifs.size() > 0 ) {
			} else {
				cerr << mode->defName << " (loif" << modeNum << ") doesn't have any setup defined - skipping!" << endl;
			}
// extraneous braces, remove them
			{
				const VexIF  *vif0 = 0;
				const VexIF  *vif1 = 0;

				*this << "    <eightBit>" << endl;
				bool used[setup->channels.size()];
				for(unsigned int k = 0; k < setup->channels.size(); k++)
					used[k] = false;
				// loop twice to cover A/C and B/D pairs
				for(unsigned int ifCnt = 0; ifCnt < 2; ifCnt++) {
					string if0;
					string if1;
					if( ifCnt == 0 ) {
						*this << "      <a0c0>" << endl; //  centerGHz=\"" << 99999.9999;
//						*this << "\">" << endl;
						if0 = "A";
						if1 = "C";
					} else {
						*this << "      <b0d0>" << endl; //  centerGHz=\"" << 99999.9999;
//						*this << "\">" << endl;
						if0 = "B";
						if1 = "D";
					}
//					cerr << "pair: " << if0 << "/" << if1 << endl;
					// find channel pairs, check polarization
					for(unsigned int k = 0; k < setup->channels.size(); k++) {
						// check if we have already handled this channel, or it it's from the IF pair
						// we're not looking for this time around
						if( used[k]
							|| (setup->channels[k].ifName != if0 && setup->channels[k].ifName != if1) ) {
							continue;
						}
						vif0 = setup->getIF(setup->channels[k].ifName);
						if( vif0 == 0 ) {
							cerr << "Unknown IF? channel " << k << endl;
							exit(EXIT_FAILURE);
						}

//						cerr << "k: " << k << " freq: " << setup->channels[k].bbcFreq << " IF: " << setup->channels[k].ifName << endl;
						used[k] = true;
						bool foundPair = false;	
						string pol;
						for( unsigned j=k+1; j<=setup->channels.size() && !foundPair; j++ ) {
//							cerr << "ifName for chan " << j << ": " << setup->channels[j].ifName << endl;
							if((setup->channels[k].ifName == if0 && setup->channels[j].ifName == if1)
								|| (setup->channels[k].ifName == if1 && setup->channels[j].ifName == if0)) {
								foundPair = true;
								used[j] = true;
//								cerr << "foundPair - f[" << k << "]: " << setup->channels[k].bbcFreq << ": f[" << j
//									<< "]: " << setup->channels[j].bbcFreq << endl;
								if(setup->channels[k].bbcFreq != setup->channels[j].bbcFreq) {
									cerr << "Invalid frequency assignment between " << setup->channels[k].ifName
										<< "(" << setup->channels[k].bbcFreq << ") and " << setup->channels[j].ifName
										<< "(" << setup->channels[j].bbcFreq << ")" << endl;
									exit(EXIT_FAILURE);
								}
								vif1 = setup->getIF(setup->channels[j].ifName);
								if( vif1 == 0 ) {
									cerr << "Unknown IF? channel " << j << endl;
									exit(EXIT_FAILURE);
								}
								if( vif0->pol == vif1->pol ) // vif will be set if found is true
									pol = vif0->pol;
								else
									pol = "FULL";  // full = dual, only correlation cares about difference

								double bw   = setup->channels[k].bbcBandwidth;
		                        char   sb   = setup->channels[k].bbcSideBand;
        		                double freq = setup->channels[k].bbcFreq;
								*this << "        <subband centerGHz=\"" 
										<< ((freq + (((sb == 'L') ? -1 : 1)*(bw/2))) / 1000000000)
										<< "\" bwMHz=\"" << (bw/1000000) << "\""
										<< " products=\"" << pol << "\""
										<< " blbps=\"1\""
										<< " vdifThreadA=\"" << k << "\""
										<< " vdifThreadB=\"" << j << "\""
										<< "/>" << endl;
							}
						}
					}
					if( ifCnt == 0 ) {
						*this << "      </a0c0>" << endl;
					} else {
						*this << "      </b0d0>" << endl;
					}
					
				}
			}
			*this << "    </eightBit>" << endl;
			*this << "  </resource>" << endl;
			*this << "</sss:vex2opt>" << endl;
			*this << endl;
	}
	precision(p);

	return nMode;
}
