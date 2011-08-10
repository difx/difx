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

#ifndef _COVARIANCE_H
#define _COVARIANCE_H

#include <armadillo>
#include <iostream>

/** 
 * Storage for time-integrated covariance data in the
 * form of a 3D data cube (Nantennas x Nantennas x Nchannels).
 *
 * In case of Fourier domain data, the stored data would be
 * called spectral density matrices instead of covariances.
 */
class Covariance {

   friend std::ostream &operator<<(std::ostream&, Covariance);

   private:

      Covariance();

   public:
      /**
       * C'stor, allocate space for covariances and set their starting timestamp
       * as well as integration time.
       * @param[in]   Nant       Number of elements or antennas.
       * @param[in]   Nchannels  Number of frequency channels.
       * @param[in]   Msmp       Number of sample vectors (x(t)'*x(t) matrices) that were averaged
       * @param[in]   timestamp  Starting time of the data cube.
       * @param[in]   Tint       Integration time used for the data cube.
       */
      Covariance(int Nant, int Nchannels, int Msmp, double timestamp, double Tint) : N_ant(Nant), N_chan(Nchannels), M_smp(Msmp) { 
         _Rxx = arma::zeros<arma::Cube<arma::cx_double> >(Nant,Nant, Nchannels);
         _timestamp = timestamp;
         _Tint = Tint;
      }

      /**
       * D'stor
       */
      ~Covariance() { }

   public:

      /**
       * Const accessor to data cube.
       * @return  Const reference to covariance data cube.
       */
      const arma::Cube<arma::cx_double>& get() const { return _Rxx; }

      /**
       * Writeable reference to data cube.
       * @return Reference to covariance data cube.
       */
      arma::Cube<arma::cx_double>& getWriteable() { return _Rxx; }
 
   public:

      /**
       * Load data cube contents from a memory location and
       * reorganize the memory layout if necessary.
       * @param[in]  raw_data  Pointer to data to load
       * @param[in]  format    Data format (0..N, to be defined)
       */
      void load(double* raw_data, const int format);

   public:

      /**
       * Operator += for summing the data from another covariance
       * object into the data cube contained in this object.
       */
      Covariance& operator+= (const Covariance &rhs) {
         _Rxx += rhs._Rxx;
         _Tint += rhs._Tint;
         return *this;
      }

   public:
      const int N_ant;
      const int N_chan;
      const int M_smp;

   private:
      arma::Cube<arma::cx_double> _Rxx;
      double _timestamp;
      double _Tint;
};

extern std::ostream &operator<<(std::ostream&, Covariance);

#endif // _COVARIANCE_H
