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

#include "pystream.h"

void pystream::open(const string& antennaName, const VexData *V)
{
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
	ofstream::open((string(obsCode) + string(".") + antennaName + ".py").c_str());
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
	int p;

	p = precision();
	precision(14);

	if(lastValid != 0.0)
	{
		*this << "array.wait(" << (lastValid + 1.0/86400.0) << ")" << endl;
	}

	precision(p);

	ofstream::close();
}

int pystream::writeHeader(const VexData *V)
{
	*this << "from edu.nrao.evla.observe import Mark5C" << endl;
	*this << "from edu.nrao.evla.observe import MatrixSwitch" << endl;
	*this << "from edu.nrao.evla.observe import PFBPersonality" << endl;
	*this << "from edu.nrao.evla.observe import RDBE" << endl;
	*this << "from edu.nrao.evla.observe import VLBALoIfSetup" << endl;
	*this << "from edu.nrao.evla.observe import Parameters" << endl;
	*this << endl;
	*this << "obsCode = '" << obsCode << "'" << endl;
	*this << "stnCode = '" << ant << "'" << endl;
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

	*this << "subarray.setRecorder(recorder0, None)" << endl;
	*this << endl;

	return 1;
}

int pystream::writeDbeInit(const VexData *V)
{
	*this << "dbe0 = RDBE(0, 'pfb')" << endl;
	*this << "dbe0.setAGC(1)" << endl;
	*this << "dbe0.setFormat('Mark5B')" << endl;
	*this << "dbe0.setPacket(46, 0, 8, 5008)" << endl;
	*this << "subarray.setDBE(dbe0, None)" << endl;
	*this << endl;

	return 1;
}

int pystream::writeLoifTable(const VexData *V)
{
	map<string,VexIF>::const_iterator it;
	int p;

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
		*this << "]" << endl;
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

	precision(p);

	return nMode;
}

int pystream::writeSourceTable(const VexData *V)
{
	int nSource;
	int p;

	nSource = V->nSource();

	p = precision();
	precision(15);

	for(int s = 0; s < nSource; s++)
	{
		const VexSource *S = V->getSource(s);
		*this << "source" << s << " = Source(" << S->ra << ", " << S->dec << ")" << endl;
		*this << "source" << s << ".setName('" << S->name << "')" << endl;
		*this << endl;
	}

	precision(p);

	return nSource;
}

int pystream::writeScans(const VexData *V)
{
	int p;
	int n = 0;
	int nScan;
	map<string,VexIF>::const_iterator it;

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
				*this << "subarray.setVLBALoIfSetup(loif" << modeId << ")" << endl;

				map<string,unsigned int>::const_iterator ifit;
				for(ifit = ifIndex[modeId].begin(); ifit != ifIndex[modeId].end(); ifit++)
				{
					if(ifit->first != sw[ifit->second])
					{
						sw[ifit->second] = ifit->first;
						*this << "subarray.set4x4Switch(" << ifit->second << ", '" << ifit->first << "')" << endl;
					}
				}


				*this << "dbe0.setChannels(channelSet" << modeId << ")" << endl;

				lastModeId = modeId;
			}

			int sourceId = V->getSourceId(scan->sourceName);
			if(sourceId != lastSourceId)
			{
				*this << "subarray.setSource(source" << sourceId << ")" << endl;
				lastSourceId = sourceId;
			}

			*this << "subarray.setRecord(" << arange->mjdStart << ", " << arange->mjdStop << ", '\%s_\%s_\%s' % (obsCode, stnCode, '" << scan->name << "') )" << endl;
			*this << "if array.time() < " << arange->mjdStop << ":" << endl;
			*this << "  subarray.execute(" << scan->mjdVex << ")" << endl;

			lastValid = arange->mjdStop;
		}

		*this << endl;
	}

	precision(p);

	return n;
}
