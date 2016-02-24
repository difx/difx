/***************************************************************************
 *   Copyright (C) 2010-2016 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <string>
#include <iostream>
#include <fstream>
#include <mark5access.h>
#include "configuration.h"

class SwitchedPower
{
public:
	SwitchedPower();
	SwitchedPower(const Configuration * conf, int mpiid);
	~SwitchedPower();
	void init();
	int open();
	int realloc(int n);
	int close();
	int flush();			// write to disk the existing data
	int feed(mark5_stream *ms);	// take entire stream and compute power

	int datastreamId;
	std::ofstream output;		// ostream for text file being written

	int interval;			// in seconds, the accumulation period
	int frequency;			// the switched power cycle frequency	(e.g., 80 Hz for VLBA)

	unsigned int *stats;
	int nchan;
	unsigned int *counts;
	double *highOn, *highOff;	// per IF, the number of high states
	double *nOn, *nOff;		// per IF, the total number of states
	int startmjd;			// mjd of accumulation start
	int startsec;			
	double startns;
	// note: the following three numbers may be improper in the sense that endns may be > 1e9 or endsec may be >= 86400
	int endmjd;
	int endsec;
	double endns;
	bool opened;
	bool failed;
	std::string filepath;
	int startMJD;
	int startSeconds;
};
