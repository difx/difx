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

   //! Number of beams. Equal to the expected number of items in the phi[], theta[] angles list.
   int Nbeams;

   //! Number of channels in the data, must be at least 1 and each needs an entry in the freqs[] list
   int Nchan;

   //! Number of antennas in the array, including RFI reference antennas
   int Nant;

   //! Beam steering angle phi in radians, azimuth angle of the signal
   arma::Col<double> phi;

   //! Beam steering angle theta in radians, tilt angle of plane wave normal from zenith
   arma::Col<double> theta;

   //! List of channel frequencies in Hertz
   arma::Col<double> freqs;

   //! Matrix with pre-computed steerings (Nbeams x Nant x Nchan) @see BeamformerWeights::generateSteerings()
   arma::Cube<arma::cx_double> steerings;

   /**
    * Set all current data to zero.
    */   
   void init() {
      phi.zeros(Nbeams);
      theta.zeros(Nbeams);
      freqs.zeros(Nchan);
      steerings.zeros(Nbeams, Nant, Nchan);
   }

   /**
    * Change dimensions of the data storage and set all data to zero.
    * @param[in] Nb Number of beams
    * @param[in] Na Number of antennas
    * @param[in] Nc Number of channels
    */
   void init(const int Nb, const int Na, const int Nc) {
      Nbeams = Nb;
      Nant = Na;
      Nchan = Nc;
      init();
   }

} Beams_t;


/**
 * Human-readable data output to stream
 */
extern std::ostream &operator<<(std::ostream& os, Beams_t const& b);

} // namespace bf

#endif // _BEAMFORMER_DATA_H
