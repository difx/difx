/*   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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

#include <fstream>
#include <cstdlib>
#include <string>
#include "configuration.h"

//constants

//variables
Configuration * config;
ifstream * input;
ofstream * output;
double * profile;
double * normprofile;
double * scratch;
float * visibilities;
string line, difxfile;
int njobs, nbins, nchannels, baseline, bin, freqindex;
int startmjd, atmjd, startsec, atsec, nextsec, runsec, runcount;
double profiletotal, weight;

//functions
