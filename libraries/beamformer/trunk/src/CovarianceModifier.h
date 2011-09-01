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

#ifndef _COVARIANCEMODIFIER_H
#define _COVARIANCEMODIFIER_H

#include "Covariance.h"

namespace bf {

/** 
 * Set of non-toxic RFI mitigation algorithms that are applied to
 * the raw covariance data directly. Currently this
 * includes two RFI reference antenna methods for
 * RFI templating and subtraction. They are applicable for all RFI 
 * types including sporadic, dynamic environments with beamformer 
 * adaptive nulling, but might work less well in pulsar observations.
 */
class CovarianceModifier {

   private:
      CovarianceModifier();
      CovarianceModifier(CovarianceModifier const&);
      CovarianceModifier& operator= (CovarianceModifier const&);

   public:
      /**
       * C'stor. Ties object to Covariance class to be modified.
       * @param[in] cov Covariance class to modify
       */
      CovarianceModifier(Covariance& cov) : _cov(cov) { }

      /**
       * D'stor
       */
      ~CovarianceModifier() { }

   public:

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
      int templateSubtraction(arma::Col<int> const& Iref, const int Nrfi);

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
      int templateSubtraction(arma::Col<int> const& Iref, const int Nrfi, const int startch, const int endch);

   private:
      Covariance& _cov;
};

} // namespace bf

#endif // _COVARIANCEMODIFIER_H
