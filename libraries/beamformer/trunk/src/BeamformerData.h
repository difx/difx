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

#ifndef _BEAMFORMER_DATA_H
#define _BEAMFORMER_DATA_H

#include <armadillo>

#include <iostream>

namespace bf {

/** Phased array, steering angles of individual beams */
typedef struct Beams_tt {
   int Nbeams;
   int Nchan;
   int Nant;
   arma::Col<double> phi;    // radian
   arma::Col<double> theta;  // radian
   arma::Col<double> freqs;  // list of channel frequencies in Hertz
   arma::Cube<arma::cx_double> steerings; // array steerings(Nbeams,Nant,Nchan)
   
   void init() {
      phi.zeros(Nbeams);
      theta.zeros(Nbeams);
      freqs.zeros(Nchan);
      steerings.zeros(Nbeams, Nant, Nchan);
   }

   void init(int Nb, int Na, int Nc) {
      Nbeams = Nb;
      Nant = Na;
      Nchan = Nc;
      init();
   }

} Beams_t;

extern std::ostream &operator<<(std::ostream& os, Beams_t const& b);

} // namespace bf

#endif // _BEAMFORMER_DATA_H
