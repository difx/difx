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

/**
 * Classic nulling of dominant eigenvalues of the decomposition. 
 * Internally, DecompositionAnalyzer is run on each channel to estimate the
 * number of interferers, whose corresponding eigenvalues are then
 * replaced by an estimate of the average noise space power.
 *
 * In pulsar and fast transient observations you should take care to 
 * null only channels that are not expected to contain the observable.
 * @param[in] Nmax     Upper limit on detected interferers to null or <1 to null all
 * @param[in] nodetect If true apply Nmax>0 directly and do not estimate interferer count
 * @param[in] startch  First channel where to start nulling
 * @param[in] endch    Last channel to null (inclusive)
 */
void DecompositionModifier::interfererNulling(const int Nmax, const bool nodetect, const int startch, const int endch)
{
   DecompositionAnalyzer da(_dc);

   /* Handle SVD and EVD */
   if (_dc._deco_type == Decomposition::SVD || _dc._deco_type == Decomposition::EVD) {

      for (int ch=startch; (ch<=endch) && (ch<_dc.N_chan); ch++) {

         double noise_pwr = 0;
         bool sorted_decreasing;
         int Nrfi;

         // Get vector with eigenvalues
         arma::Col<double>& ev = _dc._single_out_vector;
         if (_dc.N_chan > 1) {
            ev = _dc._batch_out_vectors.col(ch);
         }
         sorted_decreasing = (ev(0) > ev(ev.n_elem-1));

         // Detect and limit the number of interferers
         if (nodetect) {
            Nrfi = std::abs(Nmax);
         } else {
            da.getMDL(ch, Nrfi); // MDL or AIC; MDL tends to be "better"
            if (Nmax >= 1) {
               Nrfi = std::min(Nmax, Nrfi);
            }
         }

         if (Nrfi < 1) { continue; }

         // Estimate the noise power in channels without interferer
         if (sorted_decreasing) {
            noise_pwr = arma::mean(ev.rows(Nrfi, ev.n_elem-1));
         } else {
            noise_pwr = arma::mean(ev.rows(0, ev.n_elem-Nrfi-1));
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
      // ...
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
