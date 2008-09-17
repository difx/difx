/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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
 * $Id: mark5_stream.c 777 2008-09-10 14:48:08Z HelgeRottmann $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision: 777 $
 * $Author: HelgeRottmann $
 * $LastChangedDate: 2008-09-10 08:48:08 -0600 (Wed, 10 Sep 2008) $
 *
 *==========================================================================*/

#ifndef __CORRPARAM_H__
#define __CORRPARAM_H__

#include <vector>
#include <string>

using namespace std;

/* See http://astronomy.ivec.org/dokuwiki/doku.php/difx/configuration */

class PhaseCenter
{
public:
	double ra;
	double dec;
	string name;
	// pulsar index
	// ephemeris
	char calcode;
	int qualifier;
};

class CorrSetup
{
public:
	string setupName;

	double tInt;		/* integration time */
	int nChan;		/* channels per sub-band */
	int doPolar;		/* 0 for no cross pol, otherwise full pol */
	int doAuto;		/* write autocorrelations */
	int blocksPerSend;	/* literal */
	vector<PhaseCenter> centers;
};

class CorrRule
{
public:
	vector<string> scanName;
	vector<string> sourceName;
	vector<char> calCode;
	vector<int> qualifier;

	string setupName;	/* pointer to CorrSetup */
};

class CorrParams
{
public:
	/* global parameters */
	double mjdStart;
	double mjdStop;
	int minSubarraySize;
	vector<string> antennaList;

	/* setups to apply */
	vector<CorrSetup> setups;

	/* rules to determine which setups to apply */
	vector<CorrRule> rules;

	bool useAntenna(const string &antName) const;
};

#endif
