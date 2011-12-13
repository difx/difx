/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
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
