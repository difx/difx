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

/** 
 * Set of RFI mitigation algorithms that are applied to
 * the raw covariance data directly.
 */
class CovarianceModifier {

   private:
      CovarianceModifier();

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

       void templateSubtraction() { }

   private:
      Covariance& _cov;
};

#endif // _COVARIANCEMODIFIER_H
