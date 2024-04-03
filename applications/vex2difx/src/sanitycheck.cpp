/***************************************************************************
 *   Copyright (C) 2015-2017 by Walter Brisken                             *
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

#include <vector>
#include <list>
#include <iostream>
#include <cstdlib>
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
	int polarizations;

	int nWarn = 0;
	int nError = 0;

	polarizations = V->getConvertedPolarizations();
	if((polarizations & DIFXIO_POL_RL) && (polarizations & DIFXIO_POL_XY))
	{
		if(polarizations != V->getPolarizations())
		{
			std::cerr << "Warning: after performing requested polarization conversions, both linear and circular polarizations are due to be correlated.  Use at your own risk!  polarizations=" << polarizations << std::endl;
		}
		else
		{
			std::cerr << "Warning: both linear and circular polarizations are listed in the .vex file.  Mixed polarization mode is still considered experimental within DiFX.  Use at your own risk!" << std::endl;
		}
		++nWarn;
	}
	if(polarizations & DIFXIO_POL_ERROR)
	{
		std::cerr << "Error: the .vex file contains a polarization that is not R, L, X, Y, H, or V.  Cannot proceed." << std::endl;
		++nError;
	}

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
		if(n != 0 && n != 3 && n != 7)
		{
			std::cerr << "Warning: source " << s->vexName << " seems to have an unsupported set of ephemeris parameters.  Both or neither of ephemObject and ephemFile bust be given and naifFile cannot occur without both of them.  Error code = " << n << std::endl;
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

	// Check clock models
	bool highOrderClocks = false;
	for(unsigned int a = 0; a < V->nAntenna(); ++a)
	{
		const VexAntenna *A = V->getAntenna(a);

		if(!P->fakeDatasource)
		{
			if(!A->hasClockModel())
			{
				std::cerr << "Warning: antenna " << A->defName << ": no clock model was provided." << std::endl;
				++nWarn;
			}
		}
		for(std::vector<VexClock>::const_iterator it = A->clocks.begin(); it != A->clocks.end(); ++it)
		{
			if(it->accel != 0.0 || it->jerk != 0.0)
			{
				highOrderClocks = true;
			}
		}
	}
	if(highOrderClocks)
	{
		std::cerr << "Warning: One or more antennas has a clock model with high order (beyond linear) terms.  These terms may be neglected at some point in the processing or reporting (e.g., in mpifxcorr, difx2fits, or difx2mark4)." << std::endl;
		++nWarn;
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
			std::cout << "Note: vex source def block " << S->defName << " has more than 1 source names.  Only the first is being considered!" << std::endl;
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

	for(unsigned int m = 0; m < V->nMode(); ++m)
	{
		const VexMode *M = V->getMode(m);
		if(M->hasDuplicateBands())
		{
			std::cerr << "Warning: mode " << M->defName << " has duplicate bands (either recorded or zoom).  Behavior of the correlator is undefined in this case.  The .vex file might need modification to proceed." << std::endl;

			++nWarn;
		}
		for(std::map<std::string,VexSetup>::const_iterator  s = M->setups.begin(); s != M->setups.end(); ++s)
		{
			for(std::vector<VexStream>::const_iterator t = s->second.streams.begin(); t != s->second.streams.end(); ++t)
			{
				if(t->nBit == 0)
				{
					std::cerr << "Error: number of bits not defined for mode " << M->defName << " antenna " << s->first << std::endl;
					++nError;
				}
				if(t->format == VexStream::FormatVDIF)
				{
					if(t->dataFrameSize() <= 0)
					{
						if(P->useAntenna(s->first))
						{
							std::cerr << "Error: data frame size not set for VDIF mode " << M->defName << " antenna " << s->first << std::endl;
							++nError;
						}
						else
						{
							std::cerr << "Note: data frame size not set for VDIF mode " << M->defName << " for unused antenna " << s->first << std::endl;
						}
					}
					if(!t->singleThread && t->threads.empty())
					{
						if(P->useAntenna(s->first))
						{
							std::cerr << "Error: no threads specified for INTERLACEDVDIF mode " << M->defName << " antenna " << s->first << std::endl;
							++nError;
						}
						else
						{
							std::cerr << "Note: no threads specified for INTERLACEDVDIF mode " << M->defName << " for unused antenna " << s->first << std::endl;
						}
					}
				}
			}
		}
	}

	if(nError > 0)
	{
		exit(EXIT_FAILURE);
	}

	return nWarn;
}

