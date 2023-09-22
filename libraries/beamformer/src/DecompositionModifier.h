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

#ifndef _DECOMPOSITIONMODIFIER_H
#define _DECOMPOSITIONMODIFIER_H

#include "BeamformerTypeDefs.h"

#include "Decomposition.h"
#include "ArrayElements.h"

namespace bf {

/** 
 * Set of RFI mitigation algorithms that are applied to
 * the SVD/EVD/QR/... decomposed covariance data.
 */
class DecompositionModifier {

   private:
      DecompositionModifier();

   public:
      /**
       * C'stor. Ties object to Decomposition class to be modified.
       * @param[in] dc Decomposition class to modify
       */
      DecompositionModifier(Decomposition& dc) : _dc(dc) { }

      /**
       * D'stor
       */
      ~DecompositionModifier() { }

   public:

      /**
       * Classic nulling of dominant eigenvalues of the decomposition.
       * Each channel is checked with DecompositionAnalyzer using MDL and 3sigma.
       * The initial interference count estimate is Nrfi=max(MDL,3sig).
       * If Nmax>0 this estimate is clipped to Nrfi=min(Nrfi,Nmax).
       * Finally the Nrfi largest eigenvalues are nulled.
       * Nulling is done using median replacement.
       *
       * In pulsar and fast transient observations you should take care to 
       * null only channels that are not expected to contain the observable.
       * @param[in] Nmax       Upper limit on detected interferers to null or <1 to null all
       * @param[in] autodetect True to estimate Nrfi, false to apply Nrfi=Nmax>0 directly.
       * @param[in] startch    First channel where to start nulling
       * @param[in] endch      Last channel to null (inclusive)
       */
       void interfererNulling(const int Nmax, const bool autodetect, const int startch, const int endch) {
          interfererNulling(Nmax, autodetect, startch, endch, /*Ndiscard:*/ 0);
       }

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
       * @param[in] Nmax       Upper limit on detected interferers to null or <1 to null all
       * @param[in] autodetect True to estimate Nrfi, false to apply Nrfi=Nmax>0 directly.
       * @param[in] startch    First channel where to start nulling
       * @param[in] endch      Last channel to null (inclusive)
       * @param[in] Ndiscard   Number of smallest eigenvalues to ignore in MDL and 3sigma estimation.
       */
       void interfererNulling(const int Nmax, const bool autodetect, const int startch, const int endch, const int Ndiscard);

   private:
      Decomposition& _dc;

};

} // namespace bf

#endif // _DECOMPOSITIONMODIFIER_H

