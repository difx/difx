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
 * $Id: optscans.cpp 4563 2012-05-18 04:06:06Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/optscans.cpp $
 * $LastChangedRevision: 4563 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2012-05-17 22:06:06 -0600 (Thu, 17 May 2012) $
 *
 *==========================================================================*/

#include <cstdlib>
#include <cmath>
#include <sstream>
#include <time.h>
#include <string.h>
#include "evladefaults.h"
#include "optscans.h"

void optscans::open(const string& antennaName, const VexData *V)
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
	
	extension = ".scans";

	fileName = string(obsCode) + string(".") + antennaName + extension;
	ofstream::open(fileName.c_str());
}

void optscans::addPhasingSource(const string &sourceName)
{
	phasingSources.push_back(sourceName);
}

void optscans::close()
{
	ofstream::close();
}

int optscans::writeHeader(const VexData *V)
{

}

int optscans::writeComment(const string &commentString)
{
	*this << "# " << commentString << endl;
	*this << endl;

	return 0;
}

int optscans::writeScans(const VexData *V)
{
	int p;
	int n = 0;
	int nScan;
	map<string,VexIF>::const_iterator it;
	double recordSeconds = 0.0;
    double day = floor(mjd0);
    double allsec = floor((mjd0-floor(mjd0))*86400.0 + 0.5);
	double lastTime = 0.0;


	nScan = V->nScan();

	p = precision();
	precision(14);

	for(int s = -1; s < nScan; ++s)
	{	
		const VexScan *scan = (s==-1) ? V->getScan(0) : V->getScan(s);
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

			// TODO Once we control antenna movements, make sure we do not include antenna movement in
			// the setup scan if end time of first scan has already elapsed, so we do not interfere with
			// movement of antenna for subsequent scans. We still must execute all other setups steps.
			double deltat1 = floor((arange->mjdStart-mjd0)*86400.0 + 0.5);
			double deltat2 = floor((arange->mjdStop-mjd0)*86400.0 + 0.5);
			if(s != -1)
			{
				// don't respect data_good time, just use scan length
				int    hour = (int)floor( (deltat2-lastTime) / 3600);
				int    min  = (int)floor(((deltat2-lastTime)-(hour*3600)) / 60);
				int    sec  = (int)floor( (deltat2-lastTime)-(hour*3600)-(min*60));
				bool   applyPhase = false;

				// STD; ; J2345+0123; X Continuum; Stop Time (LST); 12:34:56; CW; y; n; CalFlux, CalGain; ;
				*this << "STD; " << optscans::obsCode << " " << scan->defName << "; "
					<< scan->sourceDefName << "; " << "loif" << modeId << "; " 
					<< " UTD; " << hour << "h" << min << "m" << sec << "s"
					<< "; ; N; ";
				if ( scan->intent.find("APPLY_AUTOPHASE") != string::npos )
					 *this << "Y";
				else
					 *this << "N";
				 *this << "; ; Y; ObsTgt";
				if( scan->intent.find("DETERMINE_AUTOPHASE") != string::npos )
					 *this << ",CALIBRATE_PHASE," << scan->intent;
				*this <<"; ;" << endl;
				lastTime = deltat2;
			}
		}
		*this << endl;
	}

	cout << "There are " << static_cast<int>(recordSeconds) << " seconds of recording at " << ant << endl;

	precision(p);

	return n;
}
