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

#ifndef _BEAMFORMERWEIGHTS_H
#define _BEAMFORMERWEIGHTS_H

#include "Covariance.h"
#include "Decomposition.h"
#include "Decompositions.h"
#include "BeamformerData.h"

#include <armadillo>

namespace bf {

class BeamformerWeights {

   friend std::ostream &operator<<(std::ostream&, BeamformerWeights const&);

   private:

      BeamformerWeights(const BeamformerWeights&);
      BeamformerWeights& operator= (const BeamformerWeights&);

   public:

      BeamformerWeights() {
         _beamW.ones(1, 1, 1);
      }

      ~BeamformerWeights() { }

   public:

      /**
       * Calculate steering vectors for the angles of each beam in the beams list, using
       * the specified array element positions. Steering vectors are the phase delay
       * of a plane wave experienced at each individual element of the array.
       *
       * @param[in,out] beams The Beams_t struct whose angles to convert into complex steerings
       * @param[in] ae        Array element positions
       * @return Steerings written back into the beams_t struct
       */
      void generateSteerings(Beams_t& beams, ArrayElements const& ae) const;


      /**
       * Compute MVDR weights using specified steering and covariance matrix.
       * The factor 'b' determines the type of beamforming.
       * With b==0 weights are classical, non-adaptive.
       * With 1=>b>0 weights shift towards MVDR (fully MVDR at b==1).
       * With b>1 weights are MVDR Cox Projection WNGC.
       *
       * @param[in] beams The Beams_t struct with beam steerings to use for MVDR weights
       * @param[in] cov   The covariance data to use
       * @param[in] b     Factor for Cox Projection WNGC
       */
      void generateMVDR(Beams_t const& beams, Covariance const& cov, const double b);

      /**
       * Compute MVDR weights using specified steering and decomposed covariance matrix.
       * Pre-decomposed covariance speeds up computation of the (pseudo)inverse needed by MVDR.
       *
       * The factor 'b' determines the type of beamforming.
       * With b==0 weights are classical, non-adaptive.
       * With 1=>b>0 weights shift towards MVDR (fully MVDR at b==1).
       * With b>1 weights are MVDR Cox Projection WNGC.
       *
       * @param[in] beams The Beams_t struct with beam steerings to use for MVDR weights
       * @param[in] deco  Some decomposition of the covariance data.
       * @param[in] b     Factor for Cox Projection WNGC
       */
      void generateMVDR(Beams_t const& beams, Decomposition const& deco, const double b);


      /**
       * Accessor to computed weights.
       * @return reference to 3D cube of size Nbeams x Nant x Nchan
       */
      const arma::Cube<arma::cx_double>& getWeights() const { return _beamW; }

      /**
       * Accessor to computed weights of one beam.
       * @param[in] beam Index of the beam in 0..Nbeams-1
       * @return reference to 2D matrix of size Nbeams x Nant
       */
      const arma::Mat<arma::cx_double>& getWeights(int beam) const { return _beamW.slice(beam); }
      
   private:

      arma::Cube<arma::cx_double> _beamW; // Nbeams x Nant x Nchan

};

extern std::ostream &operator<<(std::ostream&, BeamformerWeights const&);
      
} // namespace bf

#endif

