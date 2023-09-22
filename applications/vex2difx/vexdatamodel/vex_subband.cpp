/***************************************************************************
 *   Copyright (C) 2015-2017 by Walter Brisken & Adam Deller               *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include "vex_subband.h"

bool operator == (const VexSubband &s1, const VexSubband &s2)
{
	if(s1.pol       != s2.pol       ||
	   s1.freq      != s2.freq      ||
	   s1.sideBand  != s2.sideBand  ||
	   s1.bandwidth != s2.bandwidth)
	{
		return false;
	}
	else
	{
		return true;
	}
}

bool hasDuplicates(const std::vector<VexSubband> &subbands)
{
	unsigned int n;

	n = subbands.size();

	if(n > 1)
	{
		for(unsigned i = 1; i < n; ++i)
		{
			for(unsigned j = 0; j < i; ++j)
			{
				if(subbands[i] == subbands[j])
				{
					return true;
				}
			}
		}
	}

	return false;
}

std::ostream& operator << (std::ostream &os, const VexSubband &x)
{
	os << "[" << x.freq << " Hz, " << x.bandwidth << " Hz, sb=" << x.sideBand << ", pol=" << x.pol << "]";
	
	return os;
}
