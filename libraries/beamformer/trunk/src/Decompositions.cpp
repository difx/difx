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

namespace bf {

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
int QRDecomposition::do_decomposition(const int sliceNr, arma::Mat<bf::complex> const& Rxx)
{
   bool ok;

   if (sliceNr == 0) {
      ok = arma::qr(_single_out_matrices[0], _single_out_matrices[1], Rxx);
   } else {
      int c = sliceNr - 1;
      ok = arma::qr(_batch_out_matrices[0].slice(c), _batch_out_matrices[1].slice(c), Rxx);
   }

   return (ok ? 0 : -1);
}


/**
 * Revert QR decomposition and store results into input array specified by index 'slicenr'.
 * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
 * @param[in,out]  Rxx   Output matrix to overwrite with recomposed result
 * @return 0 on success
 */
int QRDecomposition::do_recomposition(const int sliceNr, arma::Mat<bf::complex>& Rxx)
{

   // Rxx = Q * R

   if (sliceNr == 0) {
      Rxx = _single_out_matrices[0] * _single_out_matrices[1];
   } else {
      int c = sliceNr - 1;
      Rxx = _batch_out_matrices[0].slice(c) * _batch_out_matrices[1].slice(c);
   }

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
int EVDecomposition::do_decomposition(const int sliceNr, arma::Mat<bf::complex> const& Rxx)
{
   bool ok;

   if (sliceNr == 0) {
      ok = arma::eig_sym(_single_out_vector, _single_out_matrices[0], Rxx);
   } else {

      int c = sliceNr - 1;

      // Bug: no matching function for call to ‘eig_sym(arma::subview_col<double>, arma::Mat<std::complex<double> >&, const arma::Mat<std::complex<double> >&)’
      // ok = arma::eig_sym(_batch_out_vectors.col(c), _batch_out_matrices[0].slice(c), Rxx); 

      arma::Col<bf::real> eigvals;
      ok = arma::eig_sym(eigvals, _batch_out_matrices[0].slice(c), Rxx);
      _batch_out_vectors.col(c) = eigvals;

   }

   return (ok ? 0 : -1);
}


/**
 * Revert Eigenvalue decomposition and store results into input array specified by index 'slicenr'.
 * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
 * @param[in,out]  Rxx   Output matrix to overwrite with recomposed result
 * @return 0 on success
 */
int EVDecomposition::do_recomposition(const int sliceNr, arma::Mat<bf::complex>& Rxx)
{

   // Rxx = eigvecs * diagmat(eigvals) * inv(eigvecs)

   if (sliceNr == 0) {
      Rxx = _single_out_matrices[0] * arma::diagmat(_single_out_vector) * arma::inv(_single_out_matrices[0]);
   } else {
      int c = sliceNr - 1;
      Rxx = _batch_out_matrices[0].slice(c) * arma::diagmat(_batch_out_vectors.col(c)) * arma::inv(_batch_out_matrices[0].slice(c));
   }

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
int SVDecomposition::do_decomposition(const int sliceNr, arma::Mat<bf::complex> const& Rxx)
{
   bool ok;

   if (sliceNr == 0) {
      ok = arma::svd(_single_out_matrices[0], _single_out_vector, _single_out_matrices[1], Rxx);
   } else {

      int c = sliceNr - 1;

      // Bug: no matching function for call to ‘svd(..., arma::subview_col<double>, ...)'
      // ok = arma::svd(_batch_out_matrices[0].slice(c), _batch_out_vectors.col(c), _batch_out_matrices[1].slice(c), Rxx);

      arma::Col<bf::real> s;
      ok = arma::svd(_batch_out_matrices[0].slice(c), s, _batch_out_matrices[1].slice(c), Rxx);
      _batch_out_vectors.col(c) = s;

   }

   return (ok ? 0 : -1);
}


/**
 * Revert SV decomposition and store results into input array specified by index 'slicenr'.
 * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
 * @param[in,out]  Rxx   Output matrix to overwrite with recomposed result
 * @return 0 on success
 */
int SVDecomposition::do_recomposition(const int sliceNr, arma::Mat<bf::complex>& Rxx)
{

   // Rxx = U * s * conjtrans(V)

   if (sliceNr == 0) {
      Rxx = _single_out_matrices[0] * arma::diagmat(_single_out_vector) * arma::trans(_single_out_matrices[1]);
   } else {
      int c = sliceNr - 1;
      Rxx = _batch_out_matrices[0].slice(c) * arma::diagmat(_batch_out_vectors.col(c)) * arma::trans(_batch_out_matrices[1].slice(c));
   }

   return 0;
}


/**
 * Human-readable data output to stream
 */
std::ostream &operator<<(std::ostream& os, QRDecomposition const& d)
{
   if (d.N_chan <= 1) {
      os << "Single channel QR decomposition\n";
      os << "Q=\n" << d._single_out_matrices[0]
         << "R=\n" << d._single_out_matrices[1];
   } else {
      os << "Multi-channel QR decomposition with " << d.N_chan << " channels\n";
      for (int cc=0; cc<d.N_chan; cc++) {
         os << "Q[" << cc << "]=\n" << d._batch_out_matrices[0].slice(cc)
            << "R[" << cc << "]=\n" << d._batch_out_matrices[1].slice(cc);
      }
   }
   return os;
}

/**
 * Human-readable data output to stream
 */
std::ostream &operator<<(std::ostream& os, EVDecomposition const& d)
{
   if (d.N_chan <= 1) {
      os << "Single channel EVD decomposition\n";
      os << "U=\n" << d._single_out_matrices[0]
         << "l=\n" << arma::trans(d._single_out_vector);
   } else {
      os << "Multi-channel EVD decomposition with " << d.N_chan << " channels\n";
      for (int cc=0; cc<d.N_chan; cc++) {
         os << "U[" << cc << "]=\n" << d._batch_out_matrices[0].slice(cc)
            << "l[" << cc << "]=\n" << arma::trans(d._batch_out_vectors.col(cc));
      }
   }
   return os;
}

/**
 * Human-readable data output to stream
 */
std::ostream &operator<<(std::ostream& os, SVDecomposition const& d)
{
   if (d.N_chan <= 1) {
      os << "Single channel EVD decomposition\n";
      os << "U=\n" << d._single_out_matrices[0]
         << "V=\n" << d._single_out_matrices[1]
         << "D=\n" << arma::trans(d._single_out_vector);
   } else {
      os << "Multi-channel EVD decomposition with " << d.N_chan << " channels\n";
      for (int cc=0; cc<d.N_chan; cc++) {
         os << "U[" << cc << "]=\n" << d._batch_out_matrices[0].slice(cc)
            << "V[" << cc << "]=\n" << d._batch_out_matrices[1].slice(cc)
            << "D[" << cc << "]=\n" << arma::trans(d._batch_out_vectors.col(cc));
      }
   }
   return os;
}

} // namespace bf

