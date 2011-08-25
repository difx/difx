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

#include "BeamformerWeights.h"

#include <armadillo>

namespace bf {

/**
 * Calculate steering vectors for the angles of each beam in the beams list, using
 * the specified array element positions and channel frequencies. Steering vectors 
 * are the phase delay of a plane wave experienced at each individual element of the array.
 *
 * @param[inout] beams The Beams_t struct whose angles to convert into complex steerings
 * @param[in] ae Array element positions
 * @return Steerings written back into the beams_t struct
 */
void BeamformerWeights::generateSteerings(Beams_t& beams, ArrayElements const& ae) 
{
   const ElementXYZ_t xyz = ae.getPositionSet();

   // Zero the old steering data
   beams.steerings.set_size(beams.Nbeams, xyz.Nant, beams.Nchan);

   // Compute steerings of each beam
   for (int b=0; b<beams.Nbeams; b++) {

      double phi = beams.phi[b];
      double theta = beams.theta[b];
      double k_src[3] = { std::sin(theta)*std::cos(phi), std::sin(theta)*std::sin(phi), std::cos(theta) };

      for (int a=0; a<xyz.Nant; a++) {

         double k_proj = (k_src[0]*xyz.x[a] + k_src[1]*xyz.y[a] + k_src[2]*xyz.z[a]);

         for (int cc=0; cc<beams.Nchan; cc++) {

            // plane-wave delay as a complex phase
            double phase = ((2*M_PI)/(299792458.0/beams.freqs[cc])) * k_proj;
            std::complex<double> cxphase(std::cos(phase), -std::sin(phase)); // =exp(-i*xyz*k_src)
            beams.steerings(b,a,cc) = cxphase;

        }
      }
   }
}



/**
 * Compute MVDR weights using specified steering and covariance matrix.
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
void BeamformerWeights::generateMVDR(Beams_t const& beams, Covariance const& cov, const double b)
{
   arma::Cube<arma::cx_double> const& Rxx = cov.get();

   if (cov.N_chan() != beams.Nchan) {
      std::string err("BeamformerWeights::generateMVDR(): covariance and beam Nchan do not match");
      std::cout << err << "\n";
      throw err;
   }

   if (cov.N_ant() != beams.Nant) {
      std::string err("BeamformerWeights::generateMVDR(): covariance and beam Nant do not match");
      std::cout << err << "\n";
      throw err;
   }

   _beamW.set_size(beams.Nbeams, beams.Nant, beams.Nchan);

   for (int b=0; b<beams.Nbeams; b++) {

      for (int cc=0; cc<beams.Nchan; cc++) {

         if (b == 0) {

            // Classical non-adaptive beamformer
            arma::Row<arma::cx_double> const& steering = arma::strans( beams.steerings.slice(cc).row(b) );
            _beamW.slice(cc).row(b) = steering;

         } else {

            arma::Mat<arma::cx_double> inverse = arma::inv(Rxx.slice(cc));
            arma::Row<arma::cx_double> const& steering = arma::strans( beams.steerings.slice(cc).row(b) );
            arma::Row<arma::cx_double> weights;

            // MVDR
            //std::complex<double> inv_power = arma::as_scalar(arma::conj(steering) * inverse * steering);
            weights = (inverse*steering) / (arma::conj(steering) * inverse * steering);

            // Cox Projection
            if (b > 1) {
               // modify weights into: weights = weights + b*w2
               // where weights decomposed into w1,w2 
               // the w1=w|parallel|s and w2=w/orthogonal\s projections
            }

            // Store result
            _beamW.slice(cc).row(b) = arma::strans(weights);

         }
      }
   }
}


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
 * @param[in] b     Factor for Cox Projection WNGC.
 */
void generateMVDR(Beams_t const& beams, Decomposition const& deco, const double b)
{
}


} // namespace bf
