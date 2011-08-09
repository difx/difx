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

#include "DecompositionAnalyzer.h"

#include <armadillo>

#include <limits>
#include <cmath>

/**
 * @file DecompositionAnalyzer.cpp
 * Class for analyzing array covariance matrix decompositions.
 *
 * The rank of the signal (RFI) subspace i.e. the unknown number of 
 * interferers is estimated using some information criterion.
 *
 * [1] M. Wax, T. Kailath, "Detection of Signals by Information Theoretic Criteria", 
 * IEEE, Vol. ASSP-33, No. 2, April 1985 
 */

/**
 * C'stor
 * @param[in] deco Reference to decomposition results to analyze
 */
DecompositionAnalyzer::DecompositionAnalyzer(SVDecomposition const& deco) : _deco(deco)
{
   _deco_type = 0;
}


/**
 * C'stor
 * @param[in] deco Reference to decomposition results to analyze
 */
DecompositionAnalyzer::DecompositionAnalyzer(EVDecomposition const& deco) : _deco(deco)
{
   _deco_type = 1;
}


/**
 * An MDL detector that makes a guess on the number of eigenvalues
 * that are above an unknown noise power threshold. This can work 
 * reasonably but requires more than N/2 of eigenvalues are indeed 
 * from noise space.
 * @param[in]    channel   Which channel of multi-channel data to analyse
 * @param[inout] rank      Storage for final determined rank (1..N), 0 means not found
 * @return Returns the minimum detected MDL value.
 */
double DecompositionAnalyzer::getMDL(int channel, int& rank) const
{
   double min_IC = 1.0/0.0; //infinity();
   arma::Col<double> eigs_unsorted;
   arma::Col<double> eigs;

   rank = 0;
   channel %= (_deco.N_chan + 1);

   /* Handle SVD and EVD */
   if (_deco_type == 0 || _deco_type == 1) {

      // Get lambdas and make sure lamda(1)>lambda(2)>..>lambda(N)
      if (_deco.N_chan <= 1) {
         eigs_unsorted = _deco._single_out_vector;
      } else {
         eigs_unsorted = _deco._batch_out_vectors.col(channel);
      }
      if (eigs_unsorted(0) <= eigs_unsorted(eigs_unsorted.n_elem-1)) {
        eigs = arma::sort((arma::Col<double> const)eigs_unsorted, /*0=asc,1=desc*/1);
      } else {
        eigs = eigs_unsorted;
      }

      // Compute MDL for varying number of retained smallest elements
      for (unsigned int ii=0; ii<eigs.n_elem; ii++) {

         double AIC, MDL;
         this->compute_IC_k(ii, eigs, AIC, MDL);

         if (std::isnan(MDL) || std::isinf(MDL)) {
            std::cout << "Warning: MDL(k=" << ii << "/" << (eigs.n_elem-1) << ") is NaN or INF\n";
            continue;
         }
         if (MDL < min_IC) {
            min_IC = MDL;
            rank = ii + 1;
         }
      }
   } else {
      // ...
   }

   return min_IC;
}


/**
 * An AIC detector that makes a guess on the number of eigenvalues
 * that are above an unknown noise power threshold. This can work 
 * reasonably but requires more than N/2 of eigenvalues are indeed 
 * from noise space.
 * @param[in]    channel   Which channel of multi-channel data to analyse
 * @param[inout] rank      Storage for final determined rank (1..N), 0 means not found
 * @return Returns the minimum detected MDL value.
 */
double DecompositionAnalyzer::getAIC(int channel, int& rank) const
{
   double min_IC = 1.0/0.0; //infinity();
   arma::Col<double> eigs_unsorted;
   arma::Col<double> eigs;

   rank = 0;
   channel %= (_deco.N_chan + 1);

   /* Handle SVD and EVD */
   if (_deco_type == 0 || _deco_type == 1) {

      // Get lambdas and make sure lamda(1)>lambda(2)>..>lambda(N)
      if (_deco.N_chan <= 1) {
         eigs_unsorted = _deco._single_out_vector;
      } else {
         eigs_unsorted = _deco._batch_out_vectors.col(channel);
      }
      if (eigs_unsorted(0) <= eigs_unsorted(eigs_unsorted.n_elem-1)) {
        eigs = arma::sort((arma::Col<double> const)eigs_unsorted, /*0=asc,1=desc*/1);
      } else {
        eigs = eigs_unsorted;
      }

      // Compute AIC for varying number of retained elements
      for (unsigned int ii=0; ii<eigs.n_elem; ii++) {

         double AIC, MDL;
         this->compute_IC_k(ii, eigs, AIC, MDL);

         if (std::isnan(AIC) || std::isinf(AIC)) {
            std::cout << "Warning: AIC(k=" << ii << "/" << (eigs.n_elem-1) << ") is NaN or INF\n";
            continue;
         }
         if (AIC < min_IC) {
            min_IC = AIC;
            rank = ii + 1;
         }
      }
   } else {
      // ...
   }

   return min_IC;
}


double DecompositionAnalyzer::getBIC(int channel, int& rank) const
{
   double ic = -1.0f;

   // SVD and EVD
   if (_deco_type == 0 || _deco_type == 1) {

      arma::Col<double> eigs;
      if (_deco.N_chan <= 1) {
         eigs = _deco._single_out_vector;
      } else {
         eigs = _deco._batch_out_vectors.col(channel);
      }

   } 

   return ic;
}


/**
 * Compute AIC and MDL information criterion for subset of eigenvalues.
 * @param[in]    k     Criterion parameter, must be 0 <= k < (N=len(eigs))
 * @param[in]    eigs  Vector containing eigenvalues, pre-sorted descendingly, eigs(0)>=eigs(1)>=...>=eigs(N-1)
 * @param[inout] AIC   Result of computing AIC(k)
 * @param[inout] MDL   Result of computing MDL(k)
 * @return Criterion values by reference
 */
void DecompositionAnalyzer::compute_IC_k(const unsigned int k, arma::Col<double> const& eigs, double& AIC, double& MDL) const
{
   double arith = 0;
   double geo = 1.0f;
   double Q = double(eigs.n_elem - k);
   const double M = 1024.0;   // TODO: actual number matrix averaging steps done before determining its decomposition

   // Arithmetic and geometric means
   // Note1: geometric tends to overflow easily for large n_elem and RFI presence
   // Note2: eigenvalues must be sorted descendingly, e(0) >= e(1) >= ... >= e(n_elem-1)
   // Note2: k=0 to use all (N-k)=N smallest eigenvalues e(0..n_elem-1), k=1 to use eigs e(1..n_elem-1) and so on
   for (unsigned int i=k; i<eigs.n_elem; i++) {
      arith += eigs(i);
      geo *= eigs(i);
   }
   arith /= Q;
   geo = std::pow(geo, 1/Q);
      
   MDL = -Q*M*std::log(geo/arith) + 0.5*double(k)*double(2*eigs.n_elem - k)*std::log(M);
   AIC = -2*Q*M*std::log(geo/arith) + 2*double(k)*double(2*eigs.n_elem - k);
   std::cout << "IC(k=" << k << ") : MDL=" << MDL << " AIC=" << AIC << "\n";
   return;
}

