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

#ifndef _DECOMPOSITIONANALYZER_H
#define _DECOMPOSITIONANALYZER_H

#include "Decompositions.h"

namespace bf {

/**
 * Helper class to pull information out of a covariance matrix decomposition.
 */
class DecompositionAnalyzer {

   private:
      DecompositionAnalyzer();

   public:

      /**
       * C'stor
       * @param[in] deco Reference to decomposition results to analyze
       */
      DecompositionAnalyzer(Decomposition const& deco);

      /**
       * C'stor
       * @param[in] deco Reference to decomposition results to analyze
       */
      DecompositionAnalyzer(SVDecomposition const& deco);

      /**
       * C'stor
       * @param[in] deco Reference to decomposition results to analyze
       */
      DecompositionAnalyzer(EVDecomposition const& deco);

      /** D'stor */
      ~DecompositionAnalyzer() { }

   public:

      /** 
       * An MDL detector that makes a guess on the number of eigenvalues
       * that are above an unknown noise power threshold. This can work 
       * reasonably but requires more than N/2 of eigenvalues are indeed 
       * from noise space.
       * @param[in]    channel   Which channel of multi-channel data to analyse
       * @param[in]    M_smp     Number of samples (x(t)'*x(t) matrices) that were averaged before decomposition
       * @param[in,out] rank     Storage for final determined rank (1..N), 0 means not found
       * @return Returns the minimum detected MDL value.
       */
      double getMDL(int, const int, int&) const;

      /** 
       * An AIC detector that makes a guess on the number of eigenvalues
       * that are above an unknown noise power threshold. This can work 
       * reasonably but requires more than N/2 of eigenvalues are indeed 
       * from noise space.
       * @param[in]    channel   Which channel of multi-channel data to analyse
       * @param[in]    M_smp     Number of samples (x(t)'*x(t) matrices) that were averaged before decomposition
       * @param[in,out] rank     Storage for final determined rank (1..N), 0 means not found
       * @return Returns the minimum detected MDL value.
       */
      double getAIC(int, const int, int&) const;

      /** Wraps getMDL() call with M_smp stored in decomposition object */
      double getMDL(int c, int& ref) const { return getMDL(c, _deco.M_smp, ref); }

      /** Wraps getAIC() call with M_smp stored in decomposition object */
      double getAIC(int c, int& ref) const { return getAIC(c, _deco.M_smp, ref); }

   public:

      /** Unit test */
      bool utest();

   private:

      /**
       * Compute AIC and MDL information criterion for subset of eigenvalues.              
       * @param[in]    k     Criterion parameter, must be 0 <= k < (N=len(eigs))
       * @param[in]    M_smp Number of samples (x(t)'*x(t) matrices) that were averaged before decomposition
       * @param[in]    eigs  Vector containing eigenvalues, pre-sorted descendingly, eigs(0)>=eigs(1)>=...>=eigs(N-1)
       * @param[in,out] AIC  Result of computing AIC(k)
       * @param[in,out] MDL  Result of computing MDL(k)
       * @return Criterion values by reference
       */
      void compute_IC_k(const unsigned int k, const int M_smp, arma::Col<double> const& eigs, double& AIC, double& MDL) const;

   private:

      Decomposition const& _deco;
};

} // namespace bf

#endif // _DECOMPOSITIONANALYZER_H
