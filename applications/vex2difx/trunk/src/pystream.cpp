/***************************************************************************
 *   Copyright (C) 2009-2011 by Walter Brisken / Adam Deller               *
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

void pystream::open(const string& antennaName, const VexData *V, scripttype stype)
{
	string extension;

	evlaintsec     = DEFAULT_EVLA_INT_SEC;
	evlasbbits     = DEFAULT_EVLA_SB_BITS;
	evlasbchan     = DEFAULT_EVLA_SB_CHAN;
	evlavcidir     = DEFAULT_EVLA_VCI_DIR;
	evlavciversion = DEFAULT_EVLA_VCI_VER;
	currenttype = stype;
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
	for(int i = 0; i < 4; i++)
	{
		sw[i] = "";
	}
	mjd0 = V->obsStart();
	
	if(stype == GBT)
	{
		extension = ".turtle";
	}
	else
	{
		extension = ".py";
	}

	fileName = string(obsCode) + string(".") + antennaName + extension;
	ofstream::open(fileName.c_str());
}

void pystream::addPhasingSource(const string srcname)
{
	phasingsources.push_back(srcname);
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

	for(unsigned int m = 0; m < nMode; m++)
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
			nif++;
		}
	}
}

void pystream::close()
{
	int p = precision();
	precision(14);

	if(lastValid != 0.0)
	{
		if(currenttype == VLBA)
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

	*this << "from edu.nrao.evla.observe import Mark5C" << endl;
	if(currenttype == VLBA)
	{
		*this << "from edu.nrao.evla.observe import MatrixSwitch" << endl;
		*this << "from edu.nrao.evla.observe import RDBE" << endl;
		*this << "from edu.nrao.evla.observe import VLBALoIfSetup" << endl;
		*this << "from edu.nrao.evla.observe import Parameters" << endl;
		*this << "from edu.nrao.evla.observe import bbc" << endl;
	}
	else if(currenttype == EVLA)
	{
		*this << "includePath = \"/home/mchost/evla/include/\"" << endl;
		*this << "execfile(includePath+\"printers.py\")" << endl;
		*this << "execfile(includePath+\"tmjd.py\")" << endl;
	}
	else if(currenttype == GBT)
	{
		*this << "execfile(\"/users/gbvlbi/obs/newvlbadefs.py\")" << endl;
		*this << "execfile(\"/users/gbvlbi/obs/" << obsCode << "_setup.py\")" << endl; 
	}
	*this << endl;
	*this << "second = 1.0/86400.0" << endl;
	*this << endl;
	*this << "deltat2 = 1" << endl;
	*this << endl;
	*this << "obsCode = '" << obsCode << "'" << endl;
	if(currenttype == VLBA)
	{
		*this << "stnCode = '" << ant << "'" << endl;
	}
	else if(currenttype == EVLA)
	{
		*this << "programName = 'vex2script'\n" << endl;
	}
	*this << "mjdStart = " << day << " + " << sec << "*second" << endl;
	*this << endl;

	return 0;
}

int pystream::writeRecorderInit(const VexData *V)
{
	*this << "recorder0 = Mark5C('-1')" << endl;

#warning "FIXME For now, set up single recorder in Mark5B mode"
	// Need to check requested format/mode first
	*this << "recorder0.setMode('Mark5B')" << endl;
	*this << "recorder0.setPSNMode(0)" << endl;
	*this << "recorder0.setPacket(0, 0, 40, 5008)" << endl;

	*this << "subarray.setRecorder(recorder0)" << endl;
	*this << endl;

	return 1;
}

int pystream::writeDbeInit(const VexData *V)
{
	if(currenttype == VLBA || currenttype == GBT)
	{
#warning "FIXME For now, set up single RDBE"
                int m = 0;
		const VexMode *mode = V->getMode(m);
		const VexSetup *setup = mode->getSetup(ant);
		if(setup)
		{
                    const VexFormat &F = setup->format;
                    if( F.channels.size() > 0  &&
                        F.channels.size() <= 8 ) {
                        // DDC
                        *this << "dbe0 = RDBE(0, 'ddc'";
                        if( dbefileName[0] == '\0' )
                                *this << ")" << endl;
                        else
                                *this << ", '" << dbefileName << "')" << endl;
                    } else if( F.channels.size() == 16 )
                        // PFB
                        *this << "dbe0 = RDBE(0, 'pfb')" << endl;
                    else {
                        cerr << "Incorrect number of channels: " << F.channels.size() << endl;

//                        exit(EXIT_FAILURE);
                    }
		}
                // use PFB as default for now
                else
                    *this << "dbe0 = RDBE(0, 'pfb')" << endl;
		*this << "dbe0.setALC(1)" << endl;
		*this << "dbe0.setFormat('Mark5B')" << endl;
                *this << "dbe0.setPSNMode(0)" << endl;
		*this << "dbe0.setPacket(0, 0, 40, 5008)" << endl;
		*this << "subarray.setDBE(dbe0)" << endl;
		*this << endl;
	}
	else if(currenttype == EVLA)
	{
		//do nothing - the correlator setup gets done at the same time as the LO/IF
	}

	return 1;
}

int pystream::writeLoifTable(const VexData *V)
{
	map<string,VexIF>::const_iterator it;
	int p;
	stringstream ss;
        unsigned int init_channels = 0;

	unsigned int nMode = V->nMode();

	p = precision();
	precision(15);

	for(unsigned int m = 0; m < nMode; m++)
	{
		const VexMode *mode = V->getMode(m);
		const VexSetup *setup = mode->getSetup(ant);

		if(!setup)
		{
			continue;
		}

		const VexFormat &F = setup->format;

		if(currenttype == VLBA)
		{
			if(F.format != "NONE")
			{
				if(init_channels == 0) {
					init_channels = F.channels.size();
				} else if( init_channels != F.channels.size()) {
					// TODO is this really a problem?
					cerr << "number of channels from " << init_channels << " initially to " << F.channels.size() << " which is currently not supported." << endl; 
				}
			}

			if(setup->ifs.size() > 2)
			{
				cout << "Warning: mode " << mode->defName << " wants " << setup->ifs.size() << " IFs, and we can currently only use 2" << endl;
				cout << "Might be ok for S/X setup or wideband Cband receiver" << endl;
			}

			*this << "loif" << m << " = VLBALoIfSetup()" << endl;
			for(it = setup->ifs.begin(); it != setup->ifs.end(); ++it)
			{
				const VexIF &i = it->second;
                char comment[256] = {0};
                *this << "loif" << m << ".setIf('" << i.name << "', '" << i.VLBABandName() << "', '" << i.pol << "', " << (i.ifSSLO / 1.0e6)
                        << ", '" << i.ifSideBand << "'";

                strncpy(comment, i.comment.c_str(), 255);
                if (comment[0] != '\0') {
                    int len = strlen(comment);
                    int off = 1;
                    int field_count = 0;
                    // parse BACKWARDS from end of string for three space-separated tokens
                    // comment format: * [other comments] [{receiver} {FirstLO} {BROAD|NARROW|NA}]
                    // trailing spaces are permitted
                    while (field_count <= 2
                            && off < len) {
                        // remove trailing WS
//                                    printf("removing WS\n");
                        while (comment[len - off] == ' ' || comment[len - off] == '\t') {
//                            printf("len: %i -- off: %i -- str: <%s>\n", len, off, (&comment[len - off]));
                            off++;
                        }
                        // terminate string and advance offset past WS
                        comment[len - (off - 1)] = '\0';
                        off++;
//                        printf("parsing field %i\n", field_count);
                        while (comment[len - off] != ' '
                                && comment[len - off] != '\t'
                                && off < len) {
//                            printf( "char >%c<\n", startOfComment[len-off] );
//                            printf("len: %i -- off: %i -- str: <%s>\n", len, off, (&comment[len - off]));
                            off++;
                        }
                        if (field_count == 0) {
                            //check format of comment
                            if (strcmp("BROAD", &(comment[len - off + 1])) != 0
                                    && strcmp("NARROW", &(comment[len - off + 1])) != 0
                                    && strcmp("NA", &(comment[len - off + 1])) != 0) {
                                // comment doesn't fit our "special format", don't process
                                field_count = 3;
                                continue;
                            }
                        }
                        // assign value to proper field
                        switch (field_count) {
                                // filter
                            case 0: * this << ", '" << &(comment[len - off + 1]) << "'";
                                field_count++;
                                break;
                                // firstLO
                            case 1: * this << ", " << atoi(&comment[len - off + 1]);
                                field_count++;
                                break;
                                // receiver
                            case 2: * this << ", '" << &(comment[len - off + 1]) << "'";
                                field_count++;
                                break;
                        }
                        // terminate partial string
                        comment[len - off] = '\0';
                        off++;
//                        printf("remaining comment: >%s<\n", comment);
                    }
				} else { // no comment to process
				}

				// close statement
				*this << ")" << endl;
			}
			*this << "loif" << m << ".setPhaseCal(" << (setup->phaseCalIntervalMHz()) << ")" << endl;
			// auto gain/attenuation control
			*this << "loif" << m << ".setDBEParams(0, -1, -1, 10, 0)" << endl;
			*this << "loif" << m << ".setDBEParams(1, -1, -1, 10, 0)" << endl;

			*this << "loif" << m << ".setDBERemember(0, 1)" << endl;
			*this << "loif" << m << ".setDBERemember(1, 1)" << endl;

			*this << "channelSet" << m << " = [ \\" << endl;

			vector<unsigned int> implicitConversions;
			for(unsigned i = 0; i < F.channels.size(); i++)
			{
				unsigned int inputNum = ifIndex[m][F.channels[i].ifname];
				double bw = F.channels[i].bbcBandwidth;
				char sb = F.channels[i].bbcSideBand;
				unsigned int nBit = F.nBit;
				unsigned int threadId = 0;
				const VexIF *vif = setup->getIF(F.channels[i].ifname);
				if(!vif)
				{
					cerr << "Developer error: setup->getIF(" << F.channels[i].ifname << ") returned NULL" << endl;

					exit(EXIT_FAILURE);
				}
				double freq = F.channels[i].bbcFreq;
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
				if(i < F.channels.size()-1)
				{
					*this << ",";
				}
				*this << " \\" << endl;
			}
			*this << "  ]" << endl;
			if(!implicitConversions.empty())
			{
				*this << "# implicit conversion performed on basebands:";
				for(vector<unsigned int>::const_iterator uit = implicitConversions.begin();
					uit != implicitConversions.end(); ++uit)
				{
					*this << " " << *uit;
				}
				*this << endl;
			}

			*this << endl;
		}
		else if(currenttype == EVLA)
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
			*this << "loif" << m << " = LoIfSetup('" << i1.VLBABandName() << "', " << freq1/1.0e6 << ", 0.0, " << freq2/1.0e6 << ", 0.0)" << endl;
			//write an appropriate VCI document, and set it up to be used
			ss << m;
			string vcifilename = evlavcidir + "/" + obsCode + ss.str() + ".vci";
			writeVCI(V, m, vcifilename);
			*this << "setup" << m << " = '" << vcifilename << "'" << endl;
			*this << endl;
		}
	}
	if(currenttype == EVLA)
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

	for(int s = 0; s < nSource; s++)
	{
		const VexSource *S = V->getSource(s);
		*this << "source" << s << " = Source(" << S->ra << ", " << S->dec << ")" << endl;
		*this << "source" << s << ".setName('" << S->defName << "')" << endl;
		if(currenttype == EVLA)
		{
			*this << "intent" << s << " = Intention()" << endl;
			//No point putting in calibrator code until its populated in the vex file
			//*this << "intent" << s << ".addIntent('CalibratorCode=\"" << S->calCode << "\"')" << endl;
			intentstring = "UNSPECIFIED";
			for(unsigned int i=0;i<phasingsources.size();i++)
			{
				if(phasingsources.at(i) == S->defName)
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
	for(int s = 0; s < nScan; s++)
	{
	}

	return nScan;
}

int pystream::writeScans(const VexData *V)
{
	int p;
	int n = 0;
	int nScan;
	map<string,VexIF>::const_iterator it;
	const char *switchOutput[] = {"1A", "1B", "2A", "2B"};

	nScan = V->nScan();

	p = precision();
	precision(14);

	for(int s = -1; s < nScan; s++)
	{
		const VexScan *scan = (s==-1)?V->getScan(0):V->getScan(s);
		if( s ==  -1 )
			*this << "# Setup Scan " << endl;
                else
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

			const VexFormat F = setup->format;
			if(modeId != lastModeId)
			{

				*this << "# changing to mode " << mode->defName << endl;
				if(currenttype == VLBA)
				{
					*this << "subarray.setVLBALoIfSetup(loif" << modeId << ")" << endl;

					map<string,unsigned int>::const_iterator ifit;
					for(ifit = ifIndex[modeId].begin(); ifit != ifIndex[modeId].end(); ++ifit)
					{
						if(ifit->first != sw[ifit->second])
						{
							sw[ifit->second] = ifit->first;
							*this << "subarray.set4x4Switch('" << switchOutput[ifit->second] << "', " << switchPosition(ifit->first.c_str()) << ")" << endl;
						}
					}
					*this << "subarray.setChannels(dbe0, channelSet" << modeId << ")" << endl;
				}
				else if(currenttype == EVLA)
				{
					*this << "subarray.setLoIfSetup(loif" << modeId << ")" << endl;
					*this << "subarray.setWidarSetup('setup" << modeId << "')" << endl;
				}

				lastModeId = modeId;
			}

			int sourceId = V->getSourceIdByDefName(scan->sourceDefName);
			if(currenttype == EVLA)
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
			// execute() at previous stop time minus 5 seconds
			// arbitrary amount picked to allow commands to get sent to MIBs before they need to get run on MIBs
			double deltat3 = floor((lastValid-mjd0)*86400.0 + 0.5-5);
			// just in case our setup scan caused the auto leveling to lock onto a bad value make
			// it forget
			// TODO this - like a lot of things - only works for one RDBE now
			if( s == 0 ) {
				*this << "dbe0.setDBEForget(0)" << endl;
				*this << "dbe0.setDBEForget(1)" << endl;
			}
			if( s != -1 ) {
				// recognize scans that do not record to Mark5C, but still set switches (need to pass scan start time)
                if (F.format == "MARK5B") {
					*this << "recorder0.setPacket(0, 0, 40, 5008)" << endl;
                    * this << "subarray.setRecord(mjdStart + " << deltat1 << "*second, mjdStart+" << deltat2 \
						<< "*second, '" << scan->defName << "', obsCode, stnCode )" << endl;
				}
                else {
                    *this << "print \"Not a recording scan - still set switches for " << scan->defName << ".\"" << endl;
                    *this << "subarray.setSwitches(mjdStart + " << deltat1 << "*second, mjdStart+" << deltat2 \
                        << "*second)" << endl;
				}
                // only start scan if we are at least 10sec away from scan end
                // NOTE - if this changes to a value less than 5sec may need to revisit Executor RDBE code
                // in case of scan starting later than start time
                *this << "if array.time() < mjdStart + (" << deltat2 << "-10)*second:" << endl;
                *this << "  subarray.execute(mjdStart + " << deltat3 << "*second)" << endl;
                *this << "else:" << endl;
                *this << "  print \"Skipping scan which ended at time \" + str(mjdStart+" << deltat2 \
					<< "*second) + \" since array.time is \" + str(array.time())" << endl;
                lastValid = arange->mjdStop;
            } else {
                *this << "# Setup scan - run right away, but do not start recording" << endl;
                *this << "subarray.execute( array.time() )" << endl;
			}
		}
		*this << endl;
	}

	precision(p);

	return n;
}

void pystream::writeVCI(const VexData *V, int modeindex, string filename)
{
	string bbnames[2] = {"A0/C0", "B0/D0"};
	string swbbnames[2] = {"AC8BIT", "BD8BIT"};
	char timestring[64];
	int msgid, subarrayid, numindfreqs;
	double centrefreq;
	bool found;
	string descstring, indent, activatestring;
	ofstream output(filename.c_str(), ios::trunc);
	struct tm tim;
	time_t now;
	const VexMode *mode = V->getMode(modeindex);
	const VexSetup *setup = mode->getSetup(ant);
	const VexFormat &F = setup->format;
	double * iffreqs = new double[setup->ifs.size()];
	map<string,VexIF>::const_iterator it = setup->ifs.begin();

	if(!output.is_open() || output.fail())
	{
		cout << "Could not open VCI file " << filename << " - aborting!" << endl;

		exit(EXIT_FAILURE);
	}

	subarrayid = 1;
	indent = "";
	descstring = "Project " + obsCode;
	activatestring = "vlbistart" + obsCode;
	now = time(NULL);
	tim = *(gmtime(&now));
	strftime(timestring,64,"%Y-%m-%dT%H:%M:%S.000-00:00",&tim);
	msgid = (tim.tm_sec + 11)*tim.tm_mday*tim.tm_year;

	output << indent << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" << endl;
	output << indent << "<widar:vciRequest version=\"" << evlavciversion << "\" timeStamp=\"" << timestring << "\" msgId=\"" << msgid++ << "\" desc=\"" << descstring << "\" xmlns:widar=\"http://www.nrc.ca/namespaces/widar\">" << endl;
	indent += "    ";
	output << indent << "<widar:subArray timeStamp=\"" << timestring << "\" subarrayId=\"" << subarrayid << "\" activationId=\"" << activatestring << "\" action=\"create\">" << endl;
	indent += "    ";
	output << indent << "<widar:stationInputOutput sid=\"all\">" << endl;
	indent += "    ";
        output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"R\" bbid=\"0\"/>" << endl;
	output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"L\" bbid=\"2\"/>" << endl;
	output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"R\" bbid=\"4\"/>" << endl;
	output << indent << "<widar:bbParams sourceType=\"FORM\" sourceId=\"0\" sideband=\"lower\" polarization=\"L\" bbid=\"6\"/>" << endl;
	numindfreqs = 0;
	for(unsigned int i=0;i<setup->ifs.size();i++,it++)
	{
		const VexIF & vif = it->second;
		found = false;
		for(int j=0;j<numindfreqs;j++)
		{
			//cout << "Checking " << vif.getLowerEdgeFreq() << " against " << iffreqs[j] << endl;
			if(vif.getLowerEdgeFreq() == iffreqs[j])
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
		iffreqs[numindfreqs++] = vif.getLowerEdgeFreq();
		output << indent << "<widar:baseBand singlePhaseCenter=\"yes\" name=\"" << bbnames[numindfreqs] << "\" swbbName=\"" << swbbnames[numindfreqs] << "\" inQuant=\"8\" bw=\"1024000000\" bbB=\"" << 4*numindfreqs+2 << "\" bbA=\"" << 4*numindfreqs << "\">" << endl;
		indent += "    ";
		for(unsigned int j=0;j<F.channels.size(); j++)
		{
			if(F.channels[j].ifname != vif.name) //this channel belongs to the other IF
				continue;
			centrefreq = F.channels[j].bbcFreq - vif.getLowerEdgeFreq() + F.channels[j].bbcBandwidth/2.0;
			if(F.channels[i].bbcSideBand == 'L')
			{
				centrefreq -= F.channels[i].bbcBandwidth;
			}
			output << indent << "<widar:subBand sbid=\"" << j << "\" rqNumBits=\"" << evlasbbits << "\" centralFreq=\"" << ((long long)centrefreq) << "\" bw=\"" << static_cast<int>(F.channels[j].bbcBandwidth) << "\">" << endl;
			indent += "    ";
			output << indent << "<widar:polProducts>" << endl;
			indent += "    ";
			output << indent << "<widar:pp spectralChannels=\"" << evlasbchan << "\" id=\"1\" correlation=\"A*A\"/>" << endl;
			output << indent << "<widar:pp spectralChannels=\"" << evlasbchan << "\" id=\"2\" correlation=\"A*B\"/>" << endl;
			output << indent << "<widar:pp spectralChannels=\"" << evlasbchan << "\" id=\"3\" correlation=\"B*A\"/>" << endl;
			output << indent << "<widar:pp spectralChannels=\"" << evlasbchan << "\" id=\"4\" correlation=\"B*B\"/>" << endl;
			output << indent << "<widar:blbProdIntegration recirculation=\"1\" minIntegTime=\"200.0\" ltaIntegFactor=\"2500\" ccIntegFactor=\"2\" cbeIntegFactor=\"" << evlaintsec << "\"/>" << endl;
			output << indent << "<widar:blbPair quadrant=\"" << numindfreqs+1 << "\" numBlbPairs=\"1\" firstBlbPair=\"" << j+1 << "\"/>" << endl;
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
	output << indent << "<widar:activationTrigger timeStamp=\"" << timestring << "\" msgId=\"" << msgid++ << "\" activationTime=\"" << timestring << "\" activationId=\"" << activatestring << "\"/>" << endl;
	indent = indent.substr(0, indent.length()-4);
	output << indent << "</widar:vciRequest>" << endl;
	output.close();
	delete [] iffreqs;
}

void pystream::setDBEPersonality(string filename) {
    dbefileName = filename;
}
