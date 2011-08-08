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
// $Id: $
// $HeadURL: $
// $LastChangedRevision: $
// $Author: $
// $LastChangedDate: $
//
//============================================================================

#include "Analyzer.h"
#include "Analyzers.h"

#include <armadillo>

/**
 * Make QR decomposition of covariance matrix and store results into output array
 * specified by index 'sliceNr'.
 * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
 * @param[in]  Rxx       Matrix to decompose
 * @return 0 on success
 */
int QRDecomposition::do_decomposition(int sliceNr, arma::Mat<arma::cx_double>& Rxx)
{
   return -1;
}

/**
 * Make Eigenvalue decomposition of covariance matrix and store results into output array
 * specified by index 'sliceNr'.
 * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
 * @param[in]  Rxx       Matrix to decompose
 * @return 0 on success
 */
int EVDecomposition::do_decomposition(int sliceNr, arma::Mat<arma::cx_double>& Rxx)
{
   arma::Mat<arma::cx_double> eigvecs;
   arma::Col<double> eigvals;
   if (sliceNr==0) {
      eigvecs = _single_out_matrices[0];
      eigvals = _single_out_vector;
   } else {
      eigvals = _batch_out_vectors.col(sliceNr-1);
      eigvecs = _batch_out_matrices[0].slice(sliceNr-1);
   }
   arma::eig_sym(eigvals, eigvecs, Rxx);
   std::cout << eigvecs << std::endl;
   std::cout << eigvals << std::endl;
   return 0;
}

/**
 * Make Singular Value Decomposition of covariance matrix and store results into output array
 * specified by index 'sliceNr'.
 * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
 * @param[in]  Rxx       Matrix to decompose
 * @return 0 on success
 */
int SVDecomposition::do_decomposition(int sliceNr, arma::Mat<arma::cx_double>& Rxx)
{
   arma::Mat<arma::cx_double> U;
   arma::Col<double> s;
   arma::Mat<arma::cx_double> V;

   if (sliceNr==0) {
      U = _single_out_matrices[0]; 
      s = _single_out_vector;
      V = _single_out_matrices[1]; 
   } else {
      U = _batch_out_matrices[0].slice(sliceNr-1);
      s = _batch_out_vectors.col(sliceNr-1);
      V = _batch_out_matrices[1].slice(sliceNr-1);
   }
   arma::svd(U, s, V, (const arma::Mat<arma::cx_double>)Rxx);
   std::cout << U << std::endl;
   std::cout << s << std::endl;
   std::cout << V << std::endl;
   return 0;
}
