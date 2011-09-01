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
       * Internally, DecompositionAnalyzer is run on each channel to estimate the
       * number of interferers, whose corresponding eigenvalues are then
       * replaced by an estimate of the average noise space power.
       *
       * In pulsar and fast transient observations you should take care to 
       * null only channels that are not expected to contain the observable.
       * @param[in] Nmax     Upper limit on detected interferers to null or <1 to null all
       * @param[in] nodetect If true, do not estimate interferer count from data, instead null Nmax>0 values directly
       * @param[in] startch  First channel where to start nulling
       * @param[in] endch    Last channel to null (inclusive)
       */
       void interfererNulling(const int Nmax, const bool nodetect, const int startch, const int endch);

   private:
      Decomposition& _dc;

};

} // namespace bf

#endif // _DECOMPOSITIONMODIFIER_H

