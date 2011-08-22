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

/**
 * Tries to clean RFI from a Covariance data set. 
 * Uses two or more RFI reference antennas and subtracts
 * the weaker RFI seen by the array elements themselves from
 * the covariance matrix.
 * 
 * The reference antennas need to be present in the 
 * covariance matrix. The full cross-correlation / coherence
 * data between the reference antenna and array elements must
 * also be present in the covariance matrix.
 * 
 * If interferers are not overlapping in frequency,
 * there is at most one interferer per channel.
 * 
 * @param[in] cov  Covariance object to analyse and modify
 * @param[in] Iref List of reference antenna indices between 0..Nant-1
 * @return Returns 0 on success 
 */
int CovarianceModifier::templateSubtraction(Covariance& cov, arma::Col<int> const& Iref)
{
   // *** IFF Nrfi==Nrefant Then ***
   //
   // van der Veen, Boonstra, "Spatial filtering of RF interference in Radio
   // Astronomy using a reference antenna", 2004
   //
   // R00=chRxx(antIdx,antIdx);
   // R11=chRxx(refIdx,refIdx);
   // R01=chRxx(antIdx,refIdx);
   // R10=chRxx(refIdx,antIdx);
   // clean = R00 - (R01*(inv(R11)))*R10;
   //
   // *** IFF Nrfi=1 && Nrefant==2 Then ***
   //
   // Briggs-Kesteven, 2000
   //
   // Cn1 = transpose(chRxx(refant_idcs(1),:));
   // Cn2 = transpose(chRxx(refant_idcs(2),:));
   // C12 = chRxx(refant_idcs(1),refant_idcs(2));
   //
   // // Sanity check for RFI presence
   // astro_ac_mean = mean(diag(chRxx(ant_idcs,ant_idcs)));
   // ref_ac_mean = mean(diag(chRxx(refant_idcs,refant_idcs)));
   // if (ref_ac_mean < 10*astro_ac_mean) {
   //    fprintf(1, 'Reference AC average is below +10dB of array AC average, no RFI.\n');
   //    continue;
   // }
   //     
   // // [A] Direct method
   // // This fails if noise>>RFI in the C<1,2>=C<ref1,ref2>
   // autogains = (Cn1.*conj(Cn2)) ./ C12;
   //
   // // [B] Expanded method
   // // 1) Plug in an estimate of correlated noise in C<ref1,ref2>
   // powsqr = 1e-8;% 1e-8*mean(diag(chRxx(ant_idcs,ant_idcs)));
   //
   // // 2) Compute gains
   // // Mistake in Kesteven paper, they miss nominator conj(C12)
   // // and have: gains = (Cn1.*conj(Cn2).*C12) ./ (powsqr + C12.*conj(C12));
   // crossgains = (Cn1 * conj(transpose(Cn2)) .* conj(C12)) ./ (powsqr + C12.*conj(C12));
   // crossgains = conj(crossgains); // since we chose 1xN row vectors (not Nx1 col vecs)
   //
   // // 3)-4) sanity checks
   //
   // // 5) subtract
   // cleanRxx = chRxx - crossgains;

   return 0;
}
