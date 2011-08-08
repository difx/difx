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
#include "Decompositions.h"

#include <armadillo>


//////////////////////////////////////////////////////////////////////////////
// QR DECOMPOSITION
//////////////////////////////////////////////////////////////////////////////

/**
 * Make QR decomposition of covariance matrix and store results into output array
 * specified by index 'sliceNr'.
 * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
 * @param[in]  Rxx       Matrix to decompose
 * @return 0 on success
 */
int QRDecomposition::do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx)
{
   bool ok;

   #ifdef _VERBOSE_DBG
   std::cout << "Before QRD:\n" << Rxx << std::endl;
   #endif
  
   if (sliceNr == 0) {

      ok = arma::qr(_single_out_matrices[0], _single_out_matrices[1], Rxx);

      #ifdef _VERBOSE_DBG
      std::cout << "QR return ok = " << ok << std::endl;
      std::cout << "Q =\n" << _single_out_matrices[0];
      std::cout << "R =\n" << _single_out_matrices[1];
      #endif

   } else {

      int c = sliceNr - 1;
      ok = arma::qr(_batch_out_matrices[0].slice(c), _batch_out_matrices[1].slice(c), Rxx);

      #ifdef _VERBOSE_DBG
      std::cout << "QR return ok = " << ok << std::endl;
      std::cout << "Q =\n" << _batch_out_matrices[0].slice(c);
      std::cout << "R =\n" << _batch_out_matrices[1].slice(c);
      #endif
   }

   return (ok ? 0 : -1);
}


/**
 * Revert QR decomposition and store results into input array specified by index 'slicenr'.
 * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
 * @param[inout]  Rxx    Output matrix to overwrite with recomposed result
 * @return 0 on success
 */
int QRDecomposition::do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx)
{

   // Rxx = Q * R

   if (sliceNr == 0) {
      Rxx = _single_out_matrices[0] * _single_out_matrices[1];
   } else {
      int c = sliceNr - 1;
      Rxx = _batch_out_matrices[0].slice(c) * _batch_out_matrices[1].slice(c);
   }

   #ifdef _VERBOSE_DBG
   std::cout << "After QRD:\n" << Rxx;
   #endif

   return 0;
}



//////////////////////////////////////////////////////////////////////////////
// EIGENVALUE DECOMPOSITION
//////////////////////////////////////////////////////////////////////////////

/**
 * Make Eigenvalue decomposition of covariance matrix and store results into output array
 * specified by index 'sliceNr'.
 * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
 * @param[in]  Rxx       Matrix to decompose
 * @return 0 on success
 */
int EVDecomposition::do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx)
{
   bool ok;

   #ifdef _VERBOSE_DBG
   std::cout << "Before EVD:\n" << Rxx;
   #endif

   if (sliceNr == 0) {

      ok = arma::eig_sym(_single_out_vector, _single_out_matrices[0], Rxx);

      #ifdef _VERBOSE_DBG
      std::cout << "Eig_Sym return ok = " << ok << std::endl;
      std::cout << "EigVals =\n" << _single_out_vector;
      std::cout << "EigVectors =\n" << _single_out_matrices[0];
      #endif

   } else {

      int c = sliceNr - 1;

      // Bug: no matching function for call to ‘eig_sym(arma::subview_col<double>, arma::Mat<std::complex<double> >&, const arma::Mat<std::complex<double> >&)’
      // ok = arma::eig_sym(_batch_out_vectors.col(c), _batch_out_matrices[0].slice(c), Rxx); 

      arma::Col<double> eigvals;
      ok = arma::eig_sym(eigvals, _batch_out_matrices[0].slice(c), Rxx);
      _batch_out_vectors.col(c) = eigvals;

      #ifdef _VERBOSE_DBG
      std::cout << "Eig_Sym return ok = " << ok << std::endl;
      std::cout << "EigVals =\n" << _batch_out_vectors.col(c);
      std::cout << "EigVectors =\n" << _batch_out_matrices[0].slice(c);
      #endif

   }

   return (ok ? 0 : -1);
}


/**
 * Revert Eigenvalue decomposition and store results into input array specified by index 'slicenr'.
 * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
 * @param[inout]  Rxx    Output matrix to overwrite with recomposed result
 * @return 0 on success
 */
int EVDecomposition::do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx)
{

   // Rxx = eigvecs * diagmat(eigvals) * inv(eigvecs)

   if (sliceNr == 0) {
      Rxx = _single_out_matrices[0] * arma::diagmat(_single_out_vector) * arma::inv(_single_out_matrices[0]);
   } else {
      int c = sliceNr - 1;
      Rxx = _batch_out_matrices[0].slice(c) * arma::diagmat(_batch_out_vectors.col(c)) * arma::inv(_batch_out_matrices[0].slice(c));
   }

   #ifdef _VERBOSE_DBG
   std::cout << "After Inv(EVD):\n" << Rxx;
   #endif

   return 0;
}



//////////////////////////////////////////////////////////////////////////////
// SINGULAR VALUE DECOMPOSITION
//////////////////////////////////////////////////////////////////////////////

/**
 * Make Singular Value Decomposition of covariance matrix and store results into output array
 * specified by index 'sliceNr'.
 * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
 * @param[in]  Rxx       Matrix to decompose
 * @return 0 on success
 */
int SVDecomposition::do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx)
{
   bool ok;

   #ifdef _VERBOSE_DBG
   std::cout << "Before SVD:\n" << Rxx;
   #endif

   if (sliceNr == 0) {

      ok = arma::svd(_single_out_matrices[0], _single_out_vector, _single_out_matrices[1], Rxx);

      #ifdef _VERBOSE_DBG
      std::cout << "SVD return ok = " << ok << std::endl;
      std::cout << "U = \n" << _single_out_matrices[0];
      std::cout << "s = \n" << _single_out_vector;
      std::cout << "V = \n" << _single_out_matrices[1];
      #endif

   } else {

      int c = sliceNr - 1;

      // Bug: no matching function for call to ‘svd(..., arma::subview_col<double>, ...)'
      // ok = arma::svd(_batch_out_matrices[0].slice(c), _batch_out_vectors.col(c), _batch_out_matrices[1].slice(c), Rxx);

      arma::Col<double> s;
      ok = arma::svd(_batch_out_matrices[0].slice(c), s, _batch_out_matrices[1].slice(c), Rxx);
      _batch_out_vectors.col(c) = s;

      #ifdef _VERBOSE_DBG
      std::cout << "SVD return ok = " << ok << std::endl;
      std::cout << "U = \n" << _batch_out_matrices[0].slice(c);
      std::cout << "s = \n" << _batch_out_vectors.col(c);
      std::cout << "V = \n" << _batch_out_matrices[1].slice(c);
      #endif
   }

   return (ok ? 0 : -1);
}


/**
 * Revert SV decomposition and store results into input array specified by index 'slicenr'.
 * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
 * @param[inout]  Rxx    Output matrix to overwrite with recomposed result
 * @return 0 on success
 */
int SVDecomposition::do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx)
{

   // Rxx = U * s * conjtrans(V)

   if (sliceNr == 0) {
      Rxx = _single_out_matrices[0] * arma::diagmat(_single_out_vector) * arma::trans(_single_out_matrices[1]);
   } else {
      int c = sliceNr - 1;
      Rxx = _batch_out_matrices[0].slice(c) * arma::diagmat(_batch_out_vectors.col(c)) * arma::trans(_batch_out_matrices[1].slice(c));
   }

   #ifdef _VERBOSE_DBG
   std::cout << "After Inv(SVD):\n" << Rxx;
   #endif

   return 0;
}
