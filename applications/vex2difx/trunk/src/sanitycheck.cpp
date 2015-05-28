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
 * $Id: util.h 4795 2012-09-06 20:21:51Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision: 4795 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2012-09-06 14:21:51 -0600 (Thu, 06 Sep 2012) $
 *
 *==========================================================================*/

#include <vector>
#include <list>
#include <iostream>
#include "sanitycheck.h"

static bool illegalSourceName(const std::string &name)
{
	if(name.size() > VexSource::MAX_SRCNAME_LENGTH)
	{
		return true;
	}
	else if(name.find_first_of("/") != std::string::npos)
	{
		return true;
	}
	else
	{
		return false;
	}
}

int sanityCheckConsistency(const VexData *V, const CorrParams *P)
{
	std::vector<SourceSetup>::const_iterator s;
	std::vector<AntennaSetup>::const_iterator a;
	std::vector<CorrRule>::const_iterator r;
	std::list<std::string>::const_iterator l;

	int nWarn = 0;

	for(s = P->sourceSetups.begin(); s != P->sourceSetups.end(); ++s)
	{
		int n = 0;

		if(V->getSourceBySourceName(s->vexName) == 0)
		{
			std::cerr << "Warning: source " << s->vexName << " referenced in .v2d file but is not in vex file" << std::endl;
			++nWarn;
		}
		if(!s->pointingCentre.ephemFile.empty())
		{
			n += 1;
		}
		if(!s->pointingCentre.ephemObject.empty())
		{
			n += 2;
		}
		if(!s->pointingCentre.naifFile.empty())
		{
			n += 4;
		}
		if(n != 0 && n != 7)
		{
			std::cerr << "Warning: source " << s->vexName << " seems to have an incomplete set of ephemeris parameters.  All or none of ephemObject, ephemFile, naifFile must be given.  Error code = " << n << std::endl;
			++nWarn;
		}
	}

	for(a = P->antennaSetups.begin(); a != P->antennaSetups.end(); ++a)
	{
		if(a->vexName == "DEFAULT")
		{
			std::cout << "FYI: Using a default antenna setup." << std::endl;
		}
	}

	for(r = P->rules.begin(); r != P->rules.end(); ++r)
	{
		for(l = r->scanName.begin(); l != r->scanName.end(); ++l)
		{
			if(V->getScanByDefName(*l) == 0)
			{
				std::cerr << "Warning: scan " << *l << " referenced in RULE " << r->ruleName << " in .v2d file but is not in vex file" << std::endl;
				++nWarn;
			}
		}
		for(l = r->sourceName.begin(); l != r->sourceName.end(); ++l)
		{
			if(V->getSourceBySourceName(*l) == 0)
			{
				std::cerr << "Warning: source " << *l << " referenced in RULE " << r->ruleName << " in .v2d file but is not in vex file" << std::endl;
				++nWarn;
			}
		}
		for(l = r->modeName.begin(); l != r->modeName.end(); ++l)
		{
			if(V->getModeByDefName(*l) == 0)
			{
				std::cerr << "Warning: mode " << *l << " referenced in RULE " << r->ruleName << " in .v2d file but is not in vex file" << std::endl;
				++nWarn;
			}
		}
	}

	// Verify that final source names are legal
	for(unsigned int s = 0; s < V->nSource(); ++s)
	{
		const VexSource *S = V->getSource(s);

		if(S->sourceNames.empty())
		{
			std::cout << "Warning: vex source def block " << S->defName << " has no source names!" << std::endl;
			++nWarn;
			continue;
		}
		else if(S->sourceNames.size() > 1)
		{
			std::cout << "Warning: vex source def block " << S->defName << " has more than 1 source names.  Only the first is being considered!" << std::endl;
			++nWarn;
		}

		const SourceSetup *sourceSetup = P->getSourceSetup(S->sourceNames);
		if(sourceSetup && !sourceSetup->pointingCentre.difxName.empty())
		{
			if(illegalSourceName(sourceSetup->pointingCentre.difxName))
			{
				std::cerr << "Warning: illegal source name (" << sourceSetup->pointingCentre.difxName << ") provided in SOURCE section for source " << S->defName << std::endl;
				++nWarn;
			}
		}
		else
		{
			if(illegalSourceName(S->sourceNames[0]))
			{
				std::cerr << "Warning: illegal source name (" << S->sourceNames[0] << ") in vex file.  Please correct by renaming in the SOURCE section of the .v2d file" << std::endl;
				++nWarn;
			}
		}
#warning "FIXME: check phase center names for legality"
	}

	// warn on two VexSources with the same sourceNames[] entries
	if(V->nSource() > 1)
	{
		for(unsigned int s2 = 1; s2 < V->nSource(); ++s2) 
		{
			const VexSource *S2 = V->getSource(s2);
			unsigned int ns2 = S2->sourceNames.size();
			for(unsigned int s1 = 0; s1 < s2; ++s1)
			{
				const VexSource *S1 = V->getSource(s1);
				unsigned int ns1 = S1->sourceNames.size();
				for(unsigned int n2 = 0; n2 < ns2; ++n2)
				{
					for(unsigned int n1 = 0; n1 < ns1; ++n1)
					{
						if(S1->sourceNames[n1] == S2->sourceNames[n2])
						{
							std::cerr << "Warning: two sources with the same name:" << std::endl;
							std::cerr << "  " << S1->defName << "[" << n1 << "] = " << S1->sourceNames[n1] << std::endl;
							std::cerr << "  " << S2->defName << "[" << n2 << "] = " << S2->sourceNames[n2] << std::endl;
							++nWarn;
						}
					}
				}
			}
		}
	}

	return nWarn;
}

