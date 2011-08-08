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

#ifndef _ANALYZERS_H
#define _ANALYZERS_H

#include "Analyzer.h"

/**
 * Derived class for computing QR matrix decompositions of a 3D data cube or single 2D matrix.
 */
class QRDecomposition : public Decomposition {

   private:
      QRDecomposition();

   public:

      /** C'stor. See parent class for documentation. */
      QRDecomposition(Covariance& Rxx) : Decomposition(Rxx) { 
         const int numMat=2, numVec=0; // X=Q_mat*R_mat
         cstor_alloc(Rxx.N_ant, Rxx.N_chan, numMat, numVec); 
      }

      /** C'stor. See parent class for documentation. */
      QRDecomposition(arma::Mat<arma::cx_double>& Rxx) : Decomposition(Rxx) { 
         const int numMat=2, numVec=0; // X=Q_mat*R_mat
         cstor_alloc(Rxx.n_cols, 1, numMat, numVec); 
      }

      ~QRDecomposition() { }

   private:

      /**
       * Make QR decomposition of covariance matrix and store results into output array
       * specified by index 'sliceNr'.
       * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
       * @param[in]  Rxx       Matrix to decompose
       * @return 0 on success
       */
      int do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx);

      /**
       * Revert QR decomposition and store results into input array specified by index 'slicenr'.
       * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
       * @param[inout]  Rxx    Output matrix to overwrite with recomposed result
       * @return 0 on success
       */
      int do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx);

};

/**
 * Derived class for computing eigen decompositions of a 3D data cube or single 2D matrix.
 */
class EVDecomposition : public Decomposition {

   private:
      EVDecomposition();

   public:

      /** C'stor. See parent class for documentation. */
      EVDecomposition(Covariance& Rxx) : Decomposition(Rxx) { 
         const int numMat=1, numVec=1; // X=E_mat*e_vec*inv(E_mat)
         cstor_alloc(Rxx.N_ant, Rxx.N_chan, numMat, numVec); 
      }

      /** C'stor. See parent class for documentation. */
      EVDecomposition(arma::Mat<arma::cx_double>& Rxx) : Decomposition(Rxx) { 
         const int numMat=1, numVec=1; // X=E_mat*e_vec*inv(E_mat)
         cstor_alloc(Rxx.n_cols, 1, numMat, numVec); 
      }

      ~EVDecomposition() { }

   private:

      /**
       * Make Eigenvalue decomposition of covariance matrix and store results into output array
       * specified by index 'sliceNr'.
       * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
       * @param[in]  Rxx       Matrix to decompose
       * @return 0 on success
       */
      int do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx);

      /**
       * Revert Eigenvalue decomposition and store results into input array specified by index 'slicenr'.
       * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
       * @param[inout]  Rxx    Output matrix to overwrite with recomposed result
       * @return 0 on success
       */
      int do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx);

};

/**
 * Derived class for computing SVD matrix decompositions of a 3D data cube or single 2D matrix.
 */
class SVDecomposition : public Decomposition {

   private:
      SVDecomposition();

   public:

      /** C'stor. See parent class for documentation. */
      SVDecomposition(Covariance& Rxx) : Decomposition(Rxx) { 
         const int numMat=2, numVec=1; // X=U_mat*s_vec*V_mat
         cstor_alloc(Rxx.N_ant, Rxx.N_chan, numMat, numVec); 
      }

      /** C'stor. See parent class for documentation. */
      SVDecomposition(arma::Mat<arma::cx_double>& Rxx) : Decomposition(Rxx) { 
         const int numMat=2, numVec=1; // X=U_mat*s_vec*V_mat
         cstor_alloc(Rxx.n_cols, 1, numMat, numVec); 
      }

      ~SVDecomposition() { }

   private:

      /**
       * Make Singular Value Decomposition of covariance matrix and store results into output array
       * specified by index 'sliceNr'.
       * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
       * @param[in]  Rxx       Matrix to decompose
       * @return 0 on success
       */
      int do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx);

      /**
       * Revert SVD decomposition and store results into input array specified by index 'slicenr'.
       * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
       * @param[inout]  Rxx    Output matrix to overwrite with recomposed result
       * @return 0 on success
       */
      int do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx);

};

#endif // _ANALYZERS_H
