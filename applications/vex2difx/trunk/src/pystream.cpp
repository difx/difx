/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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

		for(it = setup->ifs.begin(); it != setup->ifs.end(); it++)
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

	*this << "from edu.nrao.evla.observe import Mark5C" << endl;
	if(currenttype == VLBA)
	{
		*this << "from edu.nrao.evla.observe import MatrixSwitch" << endl;
		*this << "from edu.nrao.evla.observe import PFBPersonality" << endl;
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
	*this << "recorder0 = Mark5C(-1)" << endl;

	// FIXME For now, set up single recorder in Mark5B mode
	// Need to check requested format/mode first
	*this << "recorder0.setMode('Mark5B')" << endl;
	*this << "recorder0.setPSNMode(1)" << endl;
	*this << "recorder0.setPacket(46, 0, 8, 5008)" << endl;

	*this << "subarray.setRecorder(recorder0)" << endl;
	*this << endl;

	return 1;
}

int pystream::writeDbeInit(const VexData *V)
{
	if(currenttype == VLBA || currenttype == GBT)
	{
		*this << "dbe0 = RDBE(0, 'pfb')" << endl;
		*this << "dbe0.setAGC(1)" << endl;
		*this << "dbe0.setFormat('Mark5B')" << endl;
		*this << "dbe0.setPacket(46, 0, 8, 5008)" << endl;
		*this << "subarray.setDBE(dbe0, None)" << endl;
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
			if(setup->ifs.size() > 2)
			{
				cout << "Warning: mode " << mode->name << " wants " << setup->ifs.size() << " IFs, and we can currently only use 2" << endl;
			}

			*this << "loif" << m << " = VLBALoIfSetup()" << endl;
			for(it = setup->ifs.begin(); it != setup->ifs.end(); it++)
			{
				const VexIF &i = it->second;
				*this << "loif" << m << ".setIf('" << i.name << "', '" << i.VLBABandName() << "', '" << i.pol << "', " << (i.ifSSLO/1.0e6) << ", '" << i.ifSideBand << "')" << endl;
			}
			*this << "loif" << m << ".setPhaseCal(" << (setup->phaseCal()/1.0e6) << ")" << endl;
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
					cerr << "Developer error" << endl;
					exit(0);
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
			if(implicitConversions.size() > 0)
			{
				*this << "# implicit conversion performed on basebands:";
				for(vector<unsigned int>::const_iterator uit = implicitConversions.begin();
					uit != implicitConversions.end(); uit++)
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
				cerr << "Error: mode " << mode->name << " wants " << setup->ifs.size() << " IFs, and we can currently only use 4, I'm aborting." << endl;
				exit(1);
			}
			//better be two dual pol, otherwise abort
			for(it = setup->ifs.begin(); it != setup->ifs.end(); it++)
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
								cerr << "More than 2 IF frequencies - aborting!" << endl;
								exit(1);
							}
						}
					}
				}
			}

			//can always get away with the 2x1 GHz 8bit samplers
			const VexIF & i1 = setup->ifs.begin()->second;
			const VexIF & i2 = i1;
			if(setup->ifs.size() == 2)
			{
				const VexIF & i2 = (setup->ifs.begin()++)->second;
			}
			if(freq2 < 0)
			{
				freq2 = i2.getLowerEdgeFreq();
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
		*this << "source" << s << ".setName('" << S->name << "')" << endl;
		if(currenttype == EVLA)
		{
			*this << "intent" << s << " = Intention()" << endl;
			//No point putting in calibrator code until its populated in the vex file
			//*this << "intent" << s << ".addIntent('CalibratorCode=\"" << S->calCode << "\"')" << endl;
			intentstring = "UNSPECIFIED";
			for(int i=0;i<phasingsources.size();i++)
			{
				if(phasingsources.at(i) == S->name)
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

	for(int s = 0; s < nScan; s++)
	{
		const VexScan *scan = V->getScan(s);
		*this << "# Scan " << s << " = " << scan->name << endl;
		if(scan->stations.count(ant) == 0)
		{
			*this << "# Antenna " << ant << " not in scan " << scan->name << endl;
		}
		else
		{
			const VexInterval *arange = &scan->stations.find(ant)->second;

			int modeId = V->getModeId(scan->modeName);
			if(modeId != lastModeId)
			{
				const VexMode* mode = V->getMode(scan->modeName);

				if(mode == 0)
				{
					cerr << "Error: scan=" << scan->name << " ant=" << ant << " mode=" << scan->modeName << " -> mode=0" << endl;
					continue;
				}

				const VexSetup* setup = mode->getSetup(ant);

				if(setup == 0)
				{
					cerr << "Error: scan=" << scan->name << " ant=" << ant << " mode=" << scan->modeName << " -> setup=0" << endl;
					continue;
				}

				*this << "# changing to mode " << mode->name << endl;
				if(currenttype == VLBA)
				{
					*this << "subarray.setVLBALoIfSetup(loif" << modeId << ")" << endl;

					map<string,unsigned int>::const_iterator ifit;
					for(ifit = ifIndex[modeId].begin(); ifit != ifIndex[modeId].end(); ifit++)
					{
						if(ifit->first != sw[ifit->second])
						{
							sw[ifit->second] = ifit->first;
							*this << "subarray.set4x4Switch('" << switchOutput[ifit->second] << "', " << switchPosition(ifit->first.c_str()) << ")" << endl;
						}
					}


					*this << "dbe0.setChannels(channelSet" << modeId << ")" << endl;
				}
				else if(currenttype == EVLA)
				{
					*this << "subarray.setLoIfSetup(loif" << modeId << ")" << endl;
					*this << "subarray.setWidarSetup('setup" << modeId << "')" << endl;
				}

				lastModeId = modeId;
			}

			int sourceId = V->getSourceId(scan->sourceName);
			if(currenttype == EVLA)
			{
				*this << "intent" << sourceId << ".addIntent('ScanNumber=" << s+1 << "')" << endl;
			}
			if(sourceId != lastSourceId)
			{
				*this << "subarray.setSource(source" << sourceId << ")" << endl;
				lastSourceId = sourceId;
			}

			double deltat1 = floor((arange->mjdStart-mjd0)*86400.0 + 0.5);
			double deltat2 = floor((arange->mjdStop-mjd0)*86400.0 + 0.5);
			double deltat3 = floor((scan->mjdVex-mjd0)*86400.0 + 0.5);
			*this << "subarray.setRecord(mjdStart + " << deltat1 << "*second, mjdStart+" << deltat2 << "*second, '" << scan->name << "', obsCode, stnCode )" << endl;
			*this << "if array.time() < mjdStart + " << deltat2 << "*second:" << endl;
			*this << "  subarray.execute(mjdStart + " << deltat3 << "*second)" << endl;
			*this << "else:" << endl;
			*this << "  print \"Skipping scan which ended at time \" + str(mjdStart+deltat2*second) + \" since array.time is \" + str(array.time())" << endl;

			lastValid = arange->mjdStop;
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
		exit(1);
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
	for(int i=0;i<setup->ifs.size();i++,it++)
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
		for(int j=0;j<F.channels.size(); j++)
		{
			if(F.channels[j].ifname != vif.name) //this channel belongs to the other IF
				continue;
			centrefreq = F.channels[j].bbcFreq - vif.getLowerEdgeFreq() + F.channels[j].bbcBandwidth/2.0;
			if(F.channels[i].bbcSideBand == 'L')
			{
				centrefreq -= F.channels[i].bbcBandwidth;
			}
			output << indent << "<widar:subBand sbid=\"" << j << "\" rqNumBits=\"" << evlasbbits << "\" centralFreq=\"" << ((long long)centrefreq) << "\" bw=\"" << ((int)F.channels[j].bbcBandwidth) << "\">" << endl;
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
