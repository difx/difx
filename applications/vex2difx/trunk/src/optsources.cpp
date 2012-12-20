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
 * $Id: optsources.cpp 4563 2012-05-18 04:06:06Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/optsources.cpp $
 * $LastChangedRevision: 4563 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2012-05-17 22:06:06 -0600 (Thu, 17 May 2012) $
 *
 *==========================================================================*/

#include <cstdlib>
#include <cmath>
#include <sstream>
#include <time.h>
#include <math.h>
#include <string.h>
#include "evladefaults.h"
#include "optsources.h"

using namespace std;

void optsources::open(const string& antennaName, const VexData *V)
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
	
	extension = ".sources";

	fileName = string(obsCode) + string(".") + antennaName + extension;
	ofstream::open(fileName.c_str());
}

void optsources::addPhasingSource(const string &sourceName)
{
	phasingSources.push_back(sourceName);
}

void optsources::close()
{
	ofstream::close();
}

int optsources::writeHeader(const VexData *V)
{

}

int optsources::writeComment(const string &commentString)
{
	*this << "# " << commentString << endl;
	*this << endl;

	return 0;
}

int optsources::writeSourceTable(const VexData *V)
{
	int nSource;
	int p;
	string intentstring;

	nSource = V->nSource();

	p = precision();
	precision(15);

	*this << "# <source name>; <group name>; <coord sys>; <epoch>; <long>; <lat>; <refFrame>; <convention>; <velocity>;" << endl;
	for(int s = 0; s < nSource; ++s)
	{
		const VexSource *S = V->getSource(s);
		*this << S->defName << "; " << string(obsCode) << "; equatorial; "
			<< "J2000" << "; ";
		*this << (S->ra*180)/M_PI << "; " << (S->dec*180)/M_PI << "; "
			<< "; " << "; " << "; ";
		*this << endl;
	}

	precision(p);

	return nSource;
}
