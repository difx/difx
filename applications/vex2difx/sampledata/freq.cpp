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

#include "freq.h"
#include <iomanip>

/**
 * getFreqId(freqs[], freq cstor params)
 * Has side effects: If freq found in freqs but rxName disagrees, blanks out rxName in the matching entry
 * \return Returns index of first freq identical to fqinfo if a match is found in vector freqs[], otherwise returns -1
 */
int getFreqId(std::vector<freq>& freqs, double fq, double bw, char sb, double isr, double osr, int d, int iz, unsigned int t, const std::string &rx)
{
	freq fqinfo(fq, bw, sb, isr, osr, d, iz, t, rx);
	return getFreqId(freqs, fqinfo);
}

/**
 * getFreqId(freqs[], freq object)
 * Has side effects: If freq found in freqs but rxName disagrees, blanks out rxName in the matching entry
 * \return Returns index of first freq identical to fqinfo if a match is found in vector freqs[], otherwise returns -1
 */
int getFreqId(std::vector<freq>& freqs, const freq& fqinfo)
{
	for(std::vector<freq>::iterator it = freqs.begin(); it != freqs.end(); ++it)
	{
		if(fqinfo == *it)
		{
			if(fqinfo.rxName != it->rxName)
			{
				it->rxName.clear();	// if we get disagreement, then blank it
			}

			// use iterator math to get index
			return it - freqs.begin();
		}
	}

	return -1;
}

/**
 * addFreqId(freqs[], freq cstor params)
 * If given freq not yet in vector freqs[], appends it as a new entry.
 * If already found but rx (rxName) disagrees, blanks out rxName of the existing entry before returning its index.
 * \return Returns index of added new frequency entry, or if existing entry if a match was found.
 */
int addFreqId(std::vector<freq>& freqs, double fq, double bw, char sb, double isr, double osr, int d, int iz, unsigned int t, const std::string &rx)
{
	freq newfq(fq, bw, sb, isr, osr, d, iz, t, rx);
	return addFreqId(freqs, newfq);
}

/**
 * addFreqId(freqs[], freq object)
 * If given freq not yet in vector freqs[], appends it as a new entry.
 * If already found but rx (rxName) disagrees, blanks out rxName of the existing entry before returning its index.
 * \return Returns index of added new frequency entry, or if existing entry if a match was found.
 */
int addFreqId(std::vector<freq>& freqs, const freq& newfq)
{
	int id = getFreqId(freqs, newfq);
	if(id >= 0)
	{
		return id;
	}

	// not in list yet, so add
	freqs.push_back(newfq);

	return freqs.size() - 1;
}

void freq::flip()
{
	if (sideBand == 'U')
	{
		sideBand = 'L';
		fq += bw;
	}
	else
	{
		sideBand = 'U';
		fq -= bw;
	}
}

std::ostream& operator << (std::ostream& os, const freq& f)
{
	os << std::setw(15) << std::setprecision(8)
		<< f.fq*1e-6 << " MHz "<< f.bw*1e-6 << " MHz sb:" << f.sideBand
		<< " z:" << f.isZoomFreq << " rx:" << f.rxName;
	return os;
}
