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

#include "CovarianceModifier.h"

#include <armadillo>

namespace bf {

/**
 * Attempts to clean RFI from a Covariance data set. 
 *
 * Uses two or more RFI reference antennas and subtracts
 * the weaker RFI seen by the array elements themselves from
 * the covariance matrix.
 * 
 * The reference antennas need to be present in the 
 * covariance matrix. The full cross-correlation
 * data between reference antennas and array elements must
 * be present in the covariance matrix.
 * 
 * For efficiency, cross-correlation matrix layout
 * needs to be such that RFI reference antennas come first.
 * Thus antennas with low indices (0, 1, 2, ...) must be
 * the RFI reference antennas. All remaining antennas must be
 * elements of the array.
 *
 * If interferers are not overlapping in frequency,
 * there is at most one interferer per channel.
 *
 * At cost of higher noise, more than one RFI per channel
 * can be handled, provided that Nrfi <= N_ref_antennas;
 *
 * If Nrfi > N_ref_antennas, no subtraction is possible.
 * 
 * Correction bias: any correlated noise in the reference
 * antenna data adds bias to the correction. This can be
 * avoided by not spatially co-locating the reference antennas.
 *
 * Toxicity: at low INR or no RFI presence, the correction reduces
 * to zero, the used algorithms are safe.
 *
 * Applicability: all RFI including sporadic, dynamic environments
 * with beamformer adaptive nulling, but not for Pulsar observations.
 * 
 * @param[in] Iref List of reference antenna indices between 0..Nant-1
 * @param[in] Nrfi Expected number of strong RFI signals per channel
 * @return Returns 0 on success 
 */
int CovarianceModifier::templateSubtraction(arma::Col<int> const& Iref, const int Nrfi)
{
   return templateSubtraction(Iref, Nrfi, 0, _cov.N_chan() - 1);
}


/**
 * Attempts to clean RFI in a subset of channels of a Covariance data set.
 * See templateSubtraction(arma::Col, in) for details on processing.
 *
 * @param[in] Iref    List of reference antenna indices between 0..Nant-1
 * @param[in] Nrfi    Expected number of strong RFI signals per channel
 * @param[in] startch First channel 0..Nch-1 to process
 * @param[in] endch   Last channel 0..Nch-1 to process (inclusive)
 * @return Returns 0 on success
 */
int CovarianceModifier::templateSubtraction(arma::Col<int> const& Iref, const int Nrfi, const int startch, const int endch)
{
   int Nant = _cov.N_ant();
   int Nref = Iref.n_elem;
   int Nast = Nant - Nref;

   // *** IFF Nrfi<=Nrefant Then ***
   // van der Veen, Boonstra, "Spatial filtering of RF interference in Radio
   // Astronomy using a reference antenna", 2004

   if ((Nrfi > Nref) || (Nrfi < 1) || (Nref < 1) || (Nref >= Nant)) {
      std::cout << "templateSubtraction: Nrfi=" << Nrfi << ", Nref=" << Nref << ", Nant=" << Nant << ": more RFI than references or all antennas are references!\n";
      return -1;
   } 

   if (!(Nrfi==1 && Nref==2)) {

      // std::cout << "CovarianceModifier::templateSubtraction generic\n";

      // Matlab:
      // R00=chRxx(antIdx,antIdx);
      // R11=chRxx(refIdx,refIdx);
      // R01=chRxx(antIdx,refIdx);
      // R10=chRxx(refIdx,antIdx);
      // clean = R00 - (R01*(inv(R11)))*R10;
      //
      // ATLAS/LAPACK:
      // index spans of submatrix views must be
      // continuos, e.g. chRxx([1 3 5], [1 3 5]) won't work,
      // hence we require reference antennas be at the
      // top-left corner of the covariance matricex
      //
      //       | R11 | R10 |
      // Rxx = | ----+---- |
      //       | R01 | R00 |

      arma::Mat<bf::complex> R00;
      arma::Mat<bf::complex> R11;
      arma::Mat<bf::complex> R01;
      arma::Mat<bf::complex> R10;
      R00.zeros(Nast, Nast);
      R11.zeros(Nref, Nref);
      R01.zeros(Nast, Nref);
      R10.zeros(Nref, Nast);

      for (int cc=startch; cc<=endch; cc++) {

         R00 = _cov._Rxx.slice(cc).submat( arma::span(Nref, Nant-1), arma::span(Nref, Nant-1) );
         R11 = _cov._Rxx.slice(cc).submat( arma::span(0,    Nref-1), arma::span(0,    Nref-1) ); 

         std::complex<bf::real> astro_auto_mean = arma::mean(R00.diag());
         std::complex<bf::real> ref_auto_mean = arma::mean(R11.diag());
         double avg_INR = std::abs(ref_auto_mean) / std::abs(astro_auto_mean);
         if (avg_INR < 10) {
            std::cout << "Channel " << cc << " mean auto-corr of reference antennas less than +10dB "
                      << "from array, too low INR! Assuming no RFI!\n";
            continue;
         }

         // Compute clean "R00 - (R01*(inv(R11)))*R10" where unfortunately the 2nd term is not 100% Hermitian
         R01 = _cov._Rxx.slice(cc).submat( arma::span(Nref, Nant-1), arma::span(0,    Nref-1) ); 
         R10 = _cov._Rxx.slice(cc).submat( arma::span(0,    Nref-1), arma::span(Nref, Nant-1) ); 
         _cov._Rxx.slice(cc).submat( arma::span(Nref, Nant-1), arma::span(Nref, Nant-1) )     
            -= (R01 * arma::inv(R11)) * R10;

         // Note: instead of arma::inv(R11) inverse,
         // may also use a pseudo-inverse e.g. SVD-based pseudo-inverse:
         //    cx_mat U; vec s; cx_mat V;
         //    svd(U,s,V, R11);
         //    s_inv = strans ( diag( 1 / diag(s) ) );
         //    R11psinv = V * s_inv * trans(U);
         // resulting in subtracted template being
         // -= (R01 * R11psinv) * R10;

      }

      return 0;
   }


   // *** IFF Nrfi=1 && Nrefant==2 Then ***
   // Briggs-Kesteven, 2000

   if (Nrfi==1 && Nref==2) {

      // std::cout << "CovarianceModifier::templateSubtraction Briggs\n";

      // Matlab:
      // Cn1 = chRxx(refant_idcs(1),:);
      // Cn2 = chRxx(refant_idcs(2),:);
      // C12 = chRxx(refant_idcs(1),refant_idcs(2));
      //
      // // [A1] Direct method -- fails if noise>>RFI in C<ref1,ref2>
      // autogains = (Cn1.*conj(Cn2)) ./ C12;
      //
      // // [A2] Expanded method
      // Plug in an estimate of correlated noise in C<ref1,ref2>
      // powsqr = 1e-8; // 1e-8*mean(diag(chRxx(ant_idcs,ant_idcs)));
      //
      // // [B] Compute gains
      // crossgains = (conj(transpose(Cn1)) * Cn2) .* C12) ./ (powsqr + C12.*conj(C12));
      //
      // // [C] Sanity checks : amplitude closure, real-valued zero-phase autocorrs 
      // // ampl. closure: ForAll {src1,src2 ; src1!=src2} 1 == C<src1,ref1>*C<src2,ref2>/(C<src2,ref1>*C<src1,ref2>)
      // // real-valued autocorr: abs(angle(diag(C-crossgains))) < err
      // // [D] Subtraction
      // chRxx = chRxx - crossgains;

      arma::Mat<bf::complex> R00;
      arma::Mat<bf::complex> R11;
      arma::Row<bf::complex> Cn1;
      arma::Row<bf::complex> Cn2;
      arma::Row<bf::complex> C12;

      R00.zeros(Nast, Nast);
      R11.zeros(Nref, Nref);
      Cn1.zeros(Nant);
      Cn2.zeros(Nant);
      C12.zeros(1);

      for (int cc=startch; cc<=endch; cc++) {

         R00 = _cov._Rxx.slice(cc).submat( arma::span(Nref, Nant-1), arma::span(Nref, Nant-1) );
         R11 = _cov._Rxx.slice(cc).submat( arma::span(0,    Nref-1), arma::span(0,    Nref-1) ); 

         double astro_auto_mean = std::abs( arma::mean(R00.diag()) );
         double ref_auto_mean   = std::abs( arma::mean(R11.diag()) );
         double avg_INR = ref_auto_mean / astro_auto_mean;
         if (avg_INR < 10) {
            std::cout << "Channel " << cc << " mean auto-corr of reference antennas less than +10dB "
                      << "from array, too low INR! Assuming no RFI!\n";
            continue;
         }

         Cn1 = _cov._Rxx.slice(cc).row(0); // Reference antenna 1
         Cn2 = _cov._Rxx.slice(cc).row(1); // Reference antenna 2
         C12 = _cov._Rxx.slice(cc)(0,1);   // Cross ref 1x2
         double powsqr = 1e-8 * (astro_auto_mean/Nant); // Any small enough value

         // Correction by subtracting cross-gains; it unfortunately is not precisely Hermitian

         std::complex<bf::real> scale12 = arma::as_scalar(C12 / (powsqr + C12*arma::conj(C12)));
         arma::Mat<bf::complex> crossgains = scale12 * (arma::trans(Cn1) * Cn2);

         // _cov._Rxx.slice(cc) -= (crossgains);
         _cov._Rxx.slice(cc) -= arma::trans(crossgains);

         // TODO: somewhere along synthetic model 1xSrc+1xRFI construction, C12 ends up differing
         // by error>1e-2 from the Matlab C12, and scale12 ends up with error>1e-15,
         // after which the diagonal elements in the post-subtraction covariance matrix
         // have error>1e-5 on the real and imag parts, though off-diagonal parts are fine(!?)
         // => this leads to a slight difference in the Matlab result vs the C++ result

      }

      return 0;
   }

   std::cout << "CovarianceModifier::templateSubtraction(): no handler for case Nrfi=" << Nrfi << ", Nref=" << Nref << "\n";
   return -1;
}

} // namespace bf

