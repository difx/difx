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

#include "DecompositionModifier.h"
#include "DecompositionAnalyzer.h"

#include <armadillo>

namespace bf {

/**
 * Classic nulling of dominant eigenvalues of the decomposition.
 * Each channel is checked with DecompositionAnalyzer using MDL and 3sigma.
 * The initial interference count estimate is Nrfi=max(MDL,3sig).
 * If Nmax>0 this estimate is clipped to Nrfi=min(Nrfi,Nmax).
 * Finally the Nrfi largest eigenvalues are nulled.
 * Nulling is done using median replacement.
 *
 * When original covariance data contains disconnected antennas,           
 * you must specify the total number of disconnected antennas with Ndiscard.
 * Otherwise the MDL estimate on Nrfi will be wrong.
 *
 * In pulsar and fast transient observations you should take care to
 * null only channels that are not expected to contain the observable.
 * @param[in] Nmax     Upper limit on detected interferers to null or <1 to null all
 * @param[in] autodetect True to estimate Nrfi, false to apply Nrfi=Nmax>0 directly.
 * @param[in] startch  First channel where to start nulling
 * @param[in] endch    Last channel to null (inclusive)
 * @param[in] Ndiscard Number of smallest eigenvalues to ignore in MDL and 3sigma estimation.
 */
void DecompositionModifier::interfererNulling(const int Nmax, const bool autodetect, const int startch, const int endch, const int Ndiscard)
{
   DecompositionAnalyzer da(_dc);

   /* Handle SVD and EVD */
   if (_dc._deco_type == Decomposition::SVD || _dc._deco_type == Decomposition::EVD) {

      for (int ch=startch; (ch<=endch) && (ch<_dc.N_chan); ch++) {

         double noise_pwr = 0;
         bool sorted_decreasing;
         int Nrfi;

         // Get vector with sorted eigenvalues
         arma::Col<bf::real>& ev = _dc._single_out_vector;
         if (_dc.N_chan > 1) {
            ev = _dc._batch_out_vectors.col(ch);
         }

         // Sort order may sometimes change, detect it
         sorted_decreasing = (ev(0) > ev(ev.n_elem-1));

         // Detect the number of interferers
         if (autodetect) {

            // Use combination of MDL and 3-sigma threshold
            int Nrfi_mdl, Nrfi_3sig;
            da.getMDL(ch, _dc.M_smp, Ndiscard, Nrfi_mdl);
            da.get3Sigma(ch, Ndiscard, Nrfi_3sig);

            // Choose largest and clip to user limit
            Nrfi = std::max(Nrfi_3sig, Nrfi_mdl);
            if (Nmax >= 1) {
               Nrfi = std::min(Nmax, Nrfi);
            }

            std::cout << "Ch " << ch << " detected Nrfi: 3sig=" << Nrfi_3sig << ", MDL=" << Nrfi_mdl << ", Nrfi=" << Nrfi << "\n";

         } else {

            Nrfi = std::abs(Nmax);

         }

         // No changes if no RFI
         if (Nrfi < 1) { continue; }

         // Estimate the noise power in channels without interferer
         if (sorted_decreasing) {
            noise_pwr = arma::median(ev.rows(Nrfi, ev.n_elem-1));
         } else {
            noise_pwr = arma::median(ev.rows(0, ev.n_elem-Nrfi-1));
         }

         // Nulling by overwriting with noise power estimate
         // Write directly to output and not via reference variable
         int i_offset = 0;
         if (!sorted_decreasing) {
            i_offset = ev.n_elem - Nrfi - 1;
         }
         if (_dc.N_chan > 1) {
            for (int nn=0; nn<Nrfi; nn++) {
               _dc._batch_out_vectors.col(ch)[i_offset+nn] = noise_pwr; 
            } 
         } else {
            for (int nn=0; nn<Nrfi; nn++) {
               _dc._single_out_vector[i_offset+nn] = noise_pwr; 
            } 
         }

      }

   } else {

      std::cout << "DecompositionModifier::interfererNulling(): decomposition type not SVD nor EIG/EVD and currently unsupported\n";

   }
}


/**
 * http://iopscience.iop.org/1538-3881/140/6/2086/pdf/1538-3881_140_6_2086.pdf
 */
/*
getProjectionMatrix(bool noiseSpace)
{
   C = U*lambda*herm(U) = (Urfi|Unoise) * (lambda_rfi+noiseSigma^2|noiseSigma^2) * trans(herm(Urfi)|herm(Unoise));
   Pnoise = Unoise*herm(Unoise);
   Prfi = Urfi*herm(Urfi);
   if (noiseSpace) {
      Cnew = Pnoise*C*Pnoise;
   } else {
       // interf-free covar obtained by subtracting away the projection onto the interference subspace
       Cnew = C - Prfi*C*Prfi;
   }
}
*/

} // namespace bf

