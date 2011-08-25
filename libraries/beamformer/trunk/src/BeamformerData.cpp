/***************************************************************************
 *   Copyright (C) 2011 by Jan Wagner                                      *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include "BeamformerData.h"

namespace bf {

/**
 * Human-readable data output to stream
 */
std::ostream &operator<<(std::ostream& os, Beams_t const& b)
{
   os << "Beamformer data: Nbeams=" << b.Nbeams << ", Nchan=" << b.Nchan << ", Nant=" << b.Nant << "\n";

   os << "Frequencies: ";
   for (int cc=0; cc<b.Nchan; cc++) {
      double f = b.freqs(cc);
      os << f << " ";
   }

   os << "\nBeams: ";
   for (int bb=0; bb<b.Nbeams; bb++) {
      os << "beam#" << bb << " phi=" << double(b.phi[bb]) << " theta=" << double(b.theta[bb]) << "  ";
   }

   os << "\nSteerings:\n";
   for (int bb=0; bb<b.Nbeams; bb++) {
      for (int cc=0; cc<b.Nchan; cc++) {
         os << " beam#" << bb << " chan#" << cc << ":";
         os << "  " << b.steerings.slice(cc).row(bb);
      }
   }

   return os;
}  

} // namespace bf
