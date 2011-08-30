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

#ifndef _DECOMPOSITION_H
#define _DECOMPOSITION_H

#include "Covariance.h"

#include <armadillo>
#include <iostream>

namespace bf {

/**
 * Base class for computing matrix decompositions of a 3D data cube or single 2D matrix.
 * The base class provides only the generic interface implementation and allocation functions,
 * it does not perform an actual matrix decomposition. Functions allow to process subsets of the 
 * 3D data cube, this allows multithreading and threads to work on different subsets in parallel.
 */
class Decomposition {

   friend class DecompositionAnalyzer;
   friend class DecompositionModifier;
   friend std::ostream &operator<<(std::ostream&, Decomposition const&);

   private:

      Decomposition();
      Decomposition(const Decomposition&);
      Decomposition& operator= (const Decomposition&);

   public:

      //! Identifier for type of decomposition (eigenvalue, singular value or QR decomposition)
      enum Type { None=-1, EVD=0, SVD=1, QR=2 };

   public:

      /**
       * Base class C'stor for batch decomposition. Allocates sufficient internal space
       * to compute and store decomposition results of multiple covariance matrices
       * that are found in the Rxx Covariance object. The dimensions and properties of the
       * specified Rxx determine the required amount of internal memory.
       *
       * Calls to other member functions should either use the same Rxx passed to this
       * c'stor, or must use some other Covariance data of equal size.
       *
       * @param[in] Rxx Reference to covariance class
       */
      Decomposition(Covariance& Rxx) : N_ant(Rxx.N_ant()), N_chan(Rxx.N_chan()), M_smp(Rxx.M_smp()), _deco_type(Decomposition::None) { 
         /*derived should call: cstor_alloc(Rxx.N_ant, Rxx.N_chan, numMat, numVec);*/
      }

      /**
       * Base class C'stor for decomposition. Allocates sufficient internal space
       * to compute and store decomposition results of a single covariance matrix.
       *
       * Calls to other member functions should either use the same Rxx passed to this
       * c'stor, or must use some other matrix of equal size.
       *
       * @param[in] Rxx Reference to raw covariance data
       */
      Decomposition(arma::Mat<arma::cx_double>& Rxx) : N_ant(Rxx.n_cols), N_chan(1), M_smp(1), _deco_type(Decomposition::None) { 
         /*derived should call: cstor_alloc(Rxx.n_cols, 1, numMat, numVec);*/ 
      }

      /**
       * Base class D'stor to free internal allocations.
       */
      ~Decomposition() { }

      /**
       * Accessors
       */
      void set_M_smp(int M_smp_new) { M_smp = M_smp_new; }

  protected:

       /**
        * Allocate output matrices or output cubes.
        * @param[in]  NdecoM  Number of matrices to store decomposition (1 for Eig, 2 for QR, 2 for SVD, etc)
        * @param[in]  NdecoV  Number of vectors to store decomposition (1 for Eig, 0 for QR, 1 for SVD, etc)
        *
        * If class-const N_chan<=1, only the _single_out_matrices[] N_ant x N_ant are allocated.
        * Otherwise, the data cubes in _batch_out_cubes[] are allocated.
        */
       void cstor_alloc(const int NdecoM, const int NdecoV);

   public:

      /**
       * Perform batch decomposition of all covariances in the argument class.
       * @param[in]  cov  The covariance class with one or more matrices.
       * @return 0 on success.
       */
      int decompose(Covariance const& cov);

      /**
       * Perform batch decomposition of a range of covariances in the argument class.
       * @param[in]  cov      The covariance class with one or more matrices.
       * @param[in]  startch  Channel at which to start decomposition, 0 is first
       * @param[in]  endch    Last channel (inclusive) to decompose
       * @return 0 on success.
       */
      int decompose(Covariance const& cov, const int startch, const int endch);

      /**
       * Perform single decomposition of given covariance matrix class.
       * @param[in]  Rxx  The covariance matrix to decompose.
       * @return 0 on success.
       */
      int decompose(arma::Mat<arma::cx_double> const& Rxx) {
         return do_decomposition(0, Rxx);
      }

   public:

      /**
       * Perform batch recomposition of all covariance matrices based on the
       * decomposition data stored internally in this object.
       * Internal and output object data cube sizes must be identical.
       * @param[in,out] cov  Output covariance class for the resulting matrices.
       * @return 0 on success
       */
      int recompose(Covariance& cov);

      /**
       * Perform batch recomposition of a range of main covariance matrice(s) based on the
       * decomposition data stored internally in this object.
       * Internal and output object data cube sizes must be identical.
       * @param[in,out] cov   Output covariance class for the resulting matrices.
       * @param[in]  startch  Channel at which to start recomposition, 0 is first
       * @param[in]  endch    Last channel (inclusive) to recompose
       * @return 0 on success
       */
      int recompose(Covariance& cov, const int startch, const int endch);

      /**
       * Recompute one covariance matrix based on the first (0-channel)
       * decomposition data stored internally in this object.
       * Internal and output object data cube sizes must be identical.
       * @param[in,out] Rxx  Output covariance matrix.
       * @return 0 on success
       */
      int recompose(arma::Mat<arma::cx_double>& Rxx) { 
         return do_recomposition(0, Rxx);
      }

   private:
      /**
       * Child classes need to implement this function.
       * Decomposes covariance matrix and store results into output array
       * specified by index 'sliceNr'. Dummy template function only.
       * @param[in]  sliceNr   Index into output cube (0=single matrix, 1..N+1=cube storage)
       * @param[in]  Rxx       Matrix to decompose
       * @return 0 on success
       */
      virtual int do_decomposition(const int sliceNr, arma::Mat<arma::cx_double> const& Rxx) = 0;

      /**
       * Child classes need to implement this function.
       * Recomposes covariance matrix and store results into output covariance, in the
       * subresult location specified by index 'sliceNr'. Dummy template function only.
       * @param[in]  sliceNr   Index into internal source data (0=single matrix, 1..N+1=cube storage)
       * @param[in,out]  Rxx   Output matrix to overwrite with recomposed result
       * @return 0 on success
       */
      virtual int do_recomposition(const int sliceNr, arma::Mat<arma::cx_double>& Rxx) = 0;

   protected:

      // Storage when processing several decompositions
      arma::Mat<double>           _batch_out_vectors;       // Nant x Nchannels
      arma::Cube<arma::cx_double> _batch_out_matrices[3];  // Nant x Nant x Nchannels

      // Storage when processing only one decomposition
      arma::Col<double>           _single_out_vector;       // Nant x 1
      arma::Mat<arma::cx_double>  _single_out_matrices[3]; // Nant x Nant x 1

   public:

      const int N_ant;
      const int N_chan;
      int M_smp;
      int _deco_type;

};

extern std::ostream &operator<<(std::ostream&, Decomposition const&);

} // namespace bf

#endif // _DECOMPOSITION_H
