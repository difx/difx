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

#include "Decomposition.h"

#include <armadillo>

/**
 * Allocate output matrices or output cubes.
 * @param[in]  NdecoM  Number of matrices to store decomposition (1 for Eig, 2 for QR, 2 for SVD, etc)
 * @param[in]  NdecoV  Number of vectors to store decomposition (1 for Eig, 0 for QR, 1 for SVD, etc)
 * If Nchan<=1, only the _single_out_matrices[] is allocated.
 * Otherwise, only the _batch_out_cubes[] is allocated.
 */
void Decomposition::cstor_alloc(const int NdecoM, const int NdecoV)
{

   // Note: class-const N_ant, N_chan are already set before c'stor base code is invoked

   // Matrice(s) with eigenvectors from decomposition
   for (int decoMat=0; (decoMat<NdecoM) && (decoMat<3); decoMat++) {
      if (N_chan <= 1) {
         _single_out_matrices[decoMat] = arma::zeros<arma::Mat<arma::cx_double> >(N_ant,N_ant);
      } else {   
         _batch_out_matrices[decoMat] = arma::zeros<arma::Cube<arma::cx_double> >(N_ant,N_ant, N_chan);
      }
   }

   // Vector(s) with the eigenvalues from diagonal of decomposition
   if (NdecoV >= 1) {
      if (N_chan <= 1) {
         _single_out_vector = arma::zeros<arma::Col<double> >(N_ant);
      } else {
         _batch_out_vectors = arma::zeros<arma::Mat<double> >(N_ant, N_chan);
      }
   }
}


/**
 * Perform batch decomposition of all covariances in the argument class.
 * @param[in]  cov  The covariance class with one or more matrices.
 * @return 0 on success
 */
int Decomposition::decompose(Covariance const& cov)
{
   arma::Cube<arma::cx_double> const& allRxx = cov.get();

   return decompose(cov, 0, (allRxx.n_slices-1));
}


/**
 * Perform batch decomposition of a range of covariances in the argument class.
 * @param[in]  cov      The covariance class with one or more matrices.
 * @param[in]  startch  Channel at which to start decomposition, 0 is first
 * @param[in]  endch    Last channel (inclusive) to decompose
 * @return 0 on success.
 */
int Decomposition::decompose(Covariance const& cov, const int startch, const int endch)
{
   arma::Cube<arma::cx_double> const& allRxx = cov.get();
   arma::Mat<arma::cx_double> Rxx;
 
   if (unsigned(startch) >= allRxx.n_slices) {
      return -1;
   }

   if (allRxx.n_slices == 1) {
      return this->do_decomposition(0, allRxx.slice(0));
   }
   
   for (unsigned int chan=startch; (chan<allRxx.n_slices) && (chan<=unsigned(endch)); chan++) {
      int sliceNr = chan + 1;
      Rxx = allRxx.slice(chan);
      int rc = this->do_decomposition(sliceNr, Rxx);
      if (rc != 0) {
         return rc;
      }
   }
 
   return 0;
}


/**
 * Perform batch recomposition of all covariance matrices based on the
 * decomposition data stored internally in this object.
 * Internal and output object data cube sizes must be identical.
 * @param[inout]  cov  Output covariance class for the resulting matrices.
 * @return 0 on success
 */
int Decomposition::recompose(Covariance& cov)
{
   arma::Cube<arma::cx_double> const& allRxx = cov.get();       
   return recompose(cov, 0, (allRxx.n_slices-1));
}


/**
 * Batch recompute a range of main covariance matrice(s) based on the
 * decomposition data stored internally in this object.
 * Internal and output object data cube sizes must be identical.
 * @param[inout] cov    Output covariance class for the resulting matrices.
 * @param[in]  startch  Channel at which to start recomposition, 0 is first
 * @param[in]  endch    Last channel (inclusive) to recompose   
 * @return 0 on success
 */
int Decomposition::recompose(Covariance& cov, const int startch, const int endch)
{
   arma::Cube<arma::cx_double>& allRxx = cov.getWriteable();
   arma::Mat<arma::cx_double> Rxx;

   if (unsigned(startch) >= allRxx.n_slices) {
      return -1;
   }

   if (allRxx.n_slices == 1) {
      return this->do_recomposition(0, allRxx.slice(0));
   }

   for (unsigned int chan=startch; (chan<allRxx.n_slices) && (chan<=unsigned(endch)); chan++) {
      int sliceNr = chan + 1;
      Rxx = allRxx.slice(chan);
      int rc = this->do_recomposition(sliceNr, Rxx);
      if (rc != 0) {
         return rc;
      }
   }

   return 0;
}


/**
 * Decompose covariance matrix and store results into output array
 * specified by index 'sliceNr'. Dummy template function only.
 * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
 * @param[in]  Rxx       Matrix to decompose
 * @return 0 on success
 */
int Decomposition::do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx)
{
   // DERIVED CLASS MUST IMPLEMENT THIS
   return 0;
}


/**
 * Recompose covariance matrix and store results into output covariance, in the
 * subresult location specified by index 'sliceNr'. Dummy template function only.
 * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
 * @param[inout]  Rxx    Output matrix to overwrite with recomposed result
 * @return 0 on success
 */
int Decomposition::do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx)
{
   // DERIVED CLASS MUST IMPLEMENT THIS
   return 0;
}


/**   
 * Human-readable data output to stream
 */
std::ostream &operator<<(std::ostream& os, Decomposition d)
{
   os << "Base class Decomposition without any data\n";
   return os;
}

