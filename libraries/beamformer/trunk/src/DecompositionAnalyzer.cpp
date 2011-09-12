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

namespace bf {

/**
 * @file DecompositionAnalyzer.cpp
 * Class for analyzing array covariance matrix decompositions.
 *
 * The rank of the signal (RFI) subspace i.e. the unknown number of 
 * interferers is estimated using some information criterion. The
 * same AIC, MDL work on the decomposition of time-integrated
 * covariance data from both time domain as well as Fourier domain.
 *
 * [1] M. Wax, T. Kailath, "Detection of Signals by Information Theoretic Criteria", 
 * IEEE, Vol. ASSP-33, No. 2, April 1985, http://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=1164557
 */


/**
 * C'stor
 * @param[in] deco Reference to decomposition results to analyze
 */
DecompositionAnalyzer::DecompositionAnalyzer(Decomposition const& deco) : _deco(deco)
{
   // generic
}


/**
 * C'stor
 * @param[in] deco Reference to decomposition results to analyze
 */
DecompositionAnalyzer::DecompositionAnalyzer(SVDecomposition const& deco) : _deco(deco)
{
   // future special setups for SVD
}


/**
 * C'stor
 * @param[in] deco Reference to decomposition results to analyze
 */
DecompositionAnalyzer::DecompositionAnalyzer(EVDecomposition const& deco) : _deco(deco)
{
   // future special setups for EVD
}


/**
 * An MDL detector that makes a guess at the number of eigenvalues
 * that are above an unknown noise power threshold. This can work  
 * reasonably but requires more than N/2 of eigenvalues are indeed    
 * from noise space. When it is known that some elements of the array 
 * have no signal, the corresponding lowest eigenvalues can be ignored 
 * using Ndiscard.
 * Finds rank = arg min(MDL(k)|k=0..Nant-Ndiscard-1).
 * @param[in]     channel   Which channel of multi-channel data to analyse 
 * @param[in]     M_smp     Number of samples (x(t)'*x(t) matrices) that were averaged before decomposition
 * @param[in]     Ndiscard  Number of smallest eigenvalues to ignore in MDL.
 * @param[in,out] rank      Final determined interference space rank (0..Nch-1), 0 for no RFI found
 * @return Returns the minimum detected MDL value.
 */
double DecompositionAnalyzer::getMDL(int channel, const int M_smp, const int Ndiscard, int& rank) const
{
   double min_IC = 1.0/0.0; //infinity();
   arma::Col<bf::real> eigs_unsorted;
   arma::Col<bf::real> eigs;
   arma::Col<double> MDL_k;

   rank = 0;
   channel %= (_deco.N_chan + 1);

   /* Handle SVD and EVD */
   if (_deco._deco_type == Decomposition::SVD || _deco._deco_type == Decomposition::EVD) {

      // Get lambdas and make sure lamda(1)>lambda(2)>..>lambda(N)
      if (_deco.N_chan <= 1) {
         eigs_unsorted = _deco._single_out_vector;
      } else {
         eigs_unsorted = _deco._batch_out_vectors.col(channel);
      }

      eigs = arma::sort((arma::Col<bf::real> const)eigs_unsorted, /*0=asc,1=desc*/1);
      if (Ndiscard > 0) {
         eigs = eigs.subvec(0, eigs.n_elem - Ndiscard - 1);
      }

      // Find k = arg min(MDL(k)|k=0..Nant-1)
      MDL_k.zeros(eigs.n_elem);
      for (unsigned int ii=0; ii<MDL_k.n_elem; ii++) {

         double AIC, MDL;
         this->compute_IC_k(ii, M_smp, eigs, AIC, MDL);
         MDL_k(ii) = MDL;

         if (std::isnan(MDL) || std::isinf(MDL)) {
            continue;
         }
         if (MDL < min_IC) {
            min_IC = MDL;
            rank = ii;
         }
      }

      // std::cout << "Ch " << channel << " MDL(M=" << M_smp << ") = " << arma::strans(MDL_k);

   } else {
      // ...
   }

   return min_IC;
}


/**
 * An AIC detector that makes a guess at the number of eigenvalues
 * that are above an unknown noise power threshold. This can work 
 * reasonably but requires more than N/2 of eigenvalues are indeed 
 * from noise space. When it is known that some elements of the array
 * have no signal, the corresponding lowest eigenvalues can be ignored   
 * using Ndiscard.
 * Finds rank = arg min(AIC(k)|k=0..Nant-Ndiscard-1).
 * @param[in]     channel  Which channel of multi-channel data to analyse
 * @param[in]     M_smp    Number of samples (x(t)'*x(t) matrices) that were averaged before decomposition
 * @param[in]     Ndiscard Number of smallest eigenvalues to ignore in MDL.
 * @param[in,out] rank     Final determined interference space rank (0..Nch-1), 0 for no RFI found
 * @return Returns the minimum detected MDL value.
 */
double DecompositionAnalyzer::getAIC(int channel, const int M_smp, const int Ndiscard, int& rank) const
{
   double min_IC = 1.0/0.0; //infinity();
   arma::Col<bf::real> eigs_unsorted;
   arma::Col<bf::real> eigs;
   arma::Col<double> AIC_k;

   rank = 0;
   channel %= (_deco.N_chan + 1);

   /* Handle SVD and EVD */
   if (_deco._deco_type == Decomposition::SVD || _deco._deco_type == Decomposition::EVD) {

      // Get lambdas and make sure lamda(1)>lambda(2)>..>lambda(N)
      if (_deco.N_chan <= 1) {
         eigs_unsorted = _deco._single_out_vector;
      } else {
         eigs_unsorted = _deco._batch_out_vectors.col(channel);
      }

      eigs = arma::sort((arma::Col<bf::real> const)eigs_unsorted, /*0=asc,1=desc*/1);
      if (Ndiscard > 0) {
         eigs = eigs.subvec(0, eigs.n_elem - Ndiscard - 1);
      }

      // Find k = arg min(AIC(k)|k=0..Nant-1)
      AIC_k.zeros(eigs.n_elem);
      for (unsigned int ii=0; ii<AIC_k.n_elem; ii++) {

         double AIC, MDL;
         this->compute_IC_k(ii, M_smp, eigs, AIC, MDL);
         AIC_k(ii) = AIC;
 
         if (std::isnan(AIC) || std::isinf(AIC)) {
            continue;
         }
         if (AIC < min_IC) {
            min_IC = AIC;
            rank = ii;
         }
      }

      // std::cout << "Ch " << channel << " AIC(M=" << M_smp << ") = " << arma::strans(AIC_k);

   } else {
      // ...
   }

   return min_IC;
}


/**
 * Three sigma thresholding detector to make a guess at the number of
 * eigenvalues that are above an unknown noise power threshold. When it 
 * is known that some elements of the array have no signal, the corresponding 
 * lowest eigenvalues can be ignored using Ndiscard.
 * @param[in]      channel  Which channel of multi-channel data to analyse 
 * @param[in]      Ndiscard Number of smallest eigenvalues to ignore in 3sigma.
 * @param[in,out]  rank     Final determined interference space rank (0..Nch-1), 0 for no RFI found
 * @return Returns the estimated number of interferers.
 */
double DecompositionAnalyzer::get3Sigma(int channel, const int Ndiscard, int& rank) const
{
   arma::Col<bf::real> eigs_unsorted;
   arma::Col<bf::real> eigs;

   rank = 0;
   channel %= (_deco.N_chan + 1);

   /* Handle SVD and EVD */
   if (_deco._deco_type == Decomposition::SVD || _deco._deco_type == Decomposition::EVD) {

      // Get lambdas and make sure lamda(1)>lambda(2)>..>lambda(N)
      if (_deco.N_chan <= 1) {
         eigs_unsorted = _deco._single_out_vector;
      } else {
         eigs_unsorted = _deco._batch_out_vectors.col(channel);
      }

      // Sort and discard some of the lowest values if requested
      eigs = arma::sort((arma::Col<bf::real> const)eigs_unsorted, /*0=asc,1=desc*/1);
      if (Ndiscard > 0) {
         eigs = eigs.subvec(0, eigs.n_elem - Ndiscard - 1);
      }

      // Count values that exceed 3sigma threshold above mean
      arma::uvec ithresh;
      bf::real thresh;
      thresh  = arma::mean(eigs) + 3.0 * arma::stddev(eigs);
      ithresh = arma::find(eigs > thresh, 0, "first"); // => indices of all matching elems
      rank    = ithresh.n_elem;

   } else {
      // ...
   }

   return rank;
}


/**
 * Three MAD thresholding detector to make a guess at the number of
 * eigenvalues that are above an unknown noise power threshold. When it 
 * is known that some elements of the array have no signal, the corresponding 
 * lowest eigenvalues can be ignored using Ndiscard.
 * Uses median and three times median absolute deviation for the threshold.
 * @param[in]      channel  Which channel of multi-channel data to analyse 
 * @param[in]      Ndiscard Number of smallest eigenvalues to ignore in 3sigma.
 * @param[in,out]  rank     Final determined interference space rank (0..Nch-1), 0 for no RFI found
 * @return Returns the estimated number of interferers.
 */
double DecompositionAnalyzer::get3MAD(int channel, const int Ndiscard, int& rank) const
{
   arma::Col<bf::real> eigs_unsorted;
   arma::Col<bf::real> eigs;

   rank = 0;
   channel %= (_deco.N_chan + 1);

   /* Handle SVD and EVD */
   if (_deco._deco_type == Decomposition::SVD || _deco._deco_type == Decomposition::EVD) {

      // Get lambdas and make sure lamda(1)>lambda(2)>..>lambda(N)
      if (_deco.N_chan <= 1) {
         eigs_unsorted = _deco._single_out_vector;
      } else {
         eigs_unsorted = _deco._batch_out_vectors.col(channel);
      }

      // Sort and discard some of the lowest values if requested
      eigs = arma::sort((arma::Col<bf::real> const)eigs_unsorted, /*0=asc,1=desc*/1);
      if (Ndiscard > 0) {
         eigs = eigs.subvec(0, eigs.n_elem - Ndiscard - 1);
      }

      // Count values that exceed 3MAD threshold above median
      arma::uvec ithresh;
      bf::real thresh;
      bf::real medianval;
      medianval = arma::median(eigs);
      thresh    = medianval + 3.0 * arma::as_scalar(arma::median(arma::abs(eigs - medianval)));
      ithresh   = arma::find(eigs > thresh, 0, "first"); // => indices of all matching elems
      rank      = ithresh.n_elem;

   } else {
      // ...
   }

   return rank;
}


/**
 * Compute AIC and MDL information criterion for subset of eigenvalues.
 * @param[in]    k     Criterion parameter, must be 0 <= k < (N=len(eigs))
 * @param[in]    M_smp Number of samples (x(t)'*x(t) matrices) that were averaged before decomposition
 * @param[in]    eigs  Vector containing eigenvalues, pre-sorted descendingly, eigs(0)>=eigs(1)>=...>=eigs(N-1)
 * @param[in,out] AIC  Result of computing AIC(k)
 * @param[in,out] MDL  Result of computing MDL(k)
 * @return Criterion values by reference
 */
void DecompositionAnalyzer::compute_IC_k(const unsigned int k, const int M_smp, arma::Col<bf::real> const& eigs, double& AIC, double& MDL) const
{
   double arith = 0;
   double geo = 1.0f;

   // MDL and AIC using likelihood L(k)=geo(k)/arith(k), the ratio of the means

   // Note1: geometric tends to overflow easily for large n_elem and RFI presence
   // Note2: eigenvalues must be sorted descendingly, e(0) >= e(1) >= ... >= e(n_elem-1)
   // Note2: k=0 to use all (N-k)=N smallest eigenvalues e(0..n_elem-1), k=1 to use eigs e(1..n_elem-1) and so on

   const double N = eigs.n_elem;
   const double Q = double(eigs.n_elem - k);
   const double M = double(M_smp);

   arma::Col<bf::real> data = eigs.rows(k, eigs.n_elem-1);
   arma::Col<bf::real> eigsSqrt = arma::pow(data, 1.0/Q);
   arma::Col<bf::real> eigsNorml = data / bf::real(Q);
   arith = arma::sum(eigsNorml);
   geo = arma::prod(eigsSqrt);

   MDL = -Q*M*std::log(geo/arith) + 0.5*double(k)*double(2*N - k)*std::log(M);
   AIC = -2*Q*M*std::log(geo/arith) + 2*double(k)*double(2*N - k);

   #ifdef _VERBOSE_DBG
   if (0) { std::cout << "IC(k=" << k << ") : MDL=" << MDL << " AIC=" << AIC << "\n"; }
   #endif

   return;
}

/** Unit test */
bool DecompositionAnalyzer::utest()
{
   arma::Col<bf::real> eigs("21.2359 2.1717 1.4279 1.0979 1.0544 0.9432 0.7324");
   arma::Col<bf::real> aics_exp("1180.8 100.5 71.4 75.5 86.8 93.2 96");
   arma::Col<bf::real> mdls_exp("590.4 67.2 66.9 80.7 95.5 105.2 110.5");
   arma::Col<bf::real> aics(eigs.n_elem);
   arma::Col<bf::real> mdls(eigs.n_elem);
   bool pass = true;
   const double ee = 1e-3; // error limit 0.1%

   for (int kk=0; kk<7; kk++) {
      double mdl, aic;
      compute_IC_k(kk, 100, eigs, aic, mdl);
      mdls(kk) = mdl;
      aics(kk) = aic;
   }

   std::cout << "DecompositionAnalyzer::utest\nUsing eigs=\n" << arma::trans(eigs);
   std::cout << "aics=\n" << arma::trans(aics);
   std::cout << "mdls=\n" << arma::trans(mdls);
   std::cout << "(aics-expected)=\n" << arma::trans(aics-aics_exp);
   std::cout << "(mdls-expected)=\n" << arma::trans(mdls-mdls_exp);

   for (int kk=0; kk<7; kk++) {
      double err1 = std::abs( (mdls_exp(kk)-mdls(kk))/mdls_exp(kk) );
      double err2 = std::abs( (aics_exp(kk)-aics(kk))/aics_exp(kk) );
      if (err1>ee) {
         std::cout << "MDL(" << kk << ")=" << mdls(kk) << " != expected=" << mdls_exp(kk) << "\n";
         pass = false;
      }
      if (err2>ee) {
         std::cout << "AIC(" << kk << ")=" << aics(kk) << " != expected=" << aics_exp(kk) << "\n";
         pass = false;
      }
   }

   std::cout << "DecompositionAnalyzer::utest " << (pass ? "PASS" : "FAIL") << "\n";

   return pass;
}

} // namespace bf

