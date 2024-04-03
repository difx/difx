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

/*
This is a helper class used within vex2difx.cpp
*/

#ifndef __FREQ_H__
#define __FREQ_H__

#include <cassert>
#include <vector>
#include <ostream>

class freq
{
public:
	freq(double f=0.0, double b=0.0, char s=' ', double isr=0.0, double osr=0.0, int d=0, int iz=0, unsigned int t=0, const std::string &rx="")
		: fq(f), bw(b), inputSpecRes(isr), outputSpecRes(osr), decimation(d), isZoomFreq(iz), toneSetId(t), sideBand(s), rxName(rx)
	{
		assert(fq >= 0);
		assert(bw >= 0);
		assert(sideBand == 'L' || sideBand == 'U');
		assert(inputSpecRes > 0);
	}

	//variables
	double fq;		// Hz
	double bw;		// Hz
	double inputSpecRes;	// Hz
	double outputSpecRes;	// Hz
	int decimation;
	int isZoomFreq;
	unsigned int toneSetId;
	char sideBand;		// 'U' or 'L'
	std::string rxName;	// name of the receiver; keep unset if disagrees across the participating antennas

	//methods
	int specAvg() const { return static_cast<int>(outputSpecRes/inputSpecRes + 0.5); }
	void flip();
	friend bool operator== (const freq& lhs, const freq& rhs);
	friend std::ostream& operator << (std::ostream& os, const freq& f);
};

inline bool operator== (const freq& lhs, const freq& rhs)
{
	return (lhs.fq  == rhs.fq &&
		lhs.bw  == rhs.bw &&
		lhs.sideBand      == rhs.sideBand &&
		lhs.inputSpecRes  == rhs.inputSpecRes &&
		lhs.outputSpecRes == rhs.outputSpecRes &&
		lhs.decimation    == rhs.decimation &&
		lhs.isZoomFreq    == rhs.isZoomFreq &&
		lhs.toneSetId     == rhs.toneSetId);
}

//freq.cpp
int getFreqId(std::vector<freq>& freqs, const freq& newfq); // note: func may blank out freqs[<match>].rxName
int getFreqId(std::vector<freq>& freqs, double fq, double bw, char sb, double isr, double osr, int d, int iz, unsigned int t, const std::string &rx); // note: func may blank out freqs[<match>].rxName
int addFreqId(std::vector<freq>& freqs, const freq& newfq);
int addFreqId(std::vector<freq>& freqs, double fq, double bw, char sb, double isr, double osr, int d, int iz, unsigned int t, const std::string &rx);
std::ostream& operator << (std::ostream& os, const freq& f);

#endif
