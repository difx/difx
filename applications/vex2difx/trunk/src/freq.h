/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

/*
This is a helper class used within vex2difx.cpp
*/

#ifndef __FREQ_H__
#define __FREQ_H__

#include <vector>

class freq
{
public:
	freq(double f=0.0, double b=0.0, char s=' ', double isr=0.0, double osr=0.0, int os=0, int d=0, int iz=0, unsigned int t=0) 
		: fq(f), bw(b), inputSpecRes(isr), outputSpecRes(osr), overSamp(os), decimation(d), isZoomFreq(iz), toneSetId(t), sideBand(s) {};
	double fq;		// Hz
	double bw;		// Hz
	double inputSpecRes;	// Hz
	double outputSpecRes;	// Hz
	int overSamp;
	int decimation;
	int isZoomFreq;
	unsigned int toneSetId;
	char sideBand;

	int specAvg() const { return static_cast<int>(outputSpecRes/inputSpecRes + 0.5); }
};

int getFreqId(std::vector<freq>& freqs, double fq, double bw, char sb, double isr, double osr, int os, int d, int iz, unsigned int t);

#endif
