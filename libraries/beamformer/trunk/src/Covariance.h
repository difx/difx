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

#include "BeamformerTypeDefs.h"

#include "ArrayElements.h"

#include <armadillo>
#include <iostream>

namespace bf {

/** 
 * Provides storage for time-integrated covariance data in the
 * form of a 3D data cube (Nantennas x Nantennas x Nchannels).
 * In case of Fourier domain data, the stored data would be
 * called spectral density matrices instead of covariances.
 *
 * The covariance data contained in this class can be
 * analyzed and modified by other classes, for example
 * for performing adaptive beamforming, RFI template subtraction,
 * or decomposition-based RFI mitigation of the covariance
 * data itself.
 */
class Covariance {

   friend std::ostream &operator<<(std::ostream&, Covariance const&);
   //friend int CovarianceModifier::templateSubtraction(Covariance&, arma::Col<int> const&, const int);
   friend class CovarianceModifier;

   private:

      Covariance();
      Covariance& operator= (const Covariance&);

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
      Covariance(int Nant, int Nchannels, int Msmp, double timestamp, double Tint) : _N_ant(Nant), _N_chan(Nchannels), _M_smp(Msmp) { 
         _Rxx.zeros(Nant, Nant, Nchannels);
         _freqs.zeros(Nchannels);
         _timestamp = timestamp;
         _Tint = Tint;
      }

      /**
       * D'stor
       */
      ~Covariance() { }

      /**
       * Copy c'stor, make new covariance object with copy of data from another.
       */
      Covariance(const Covariance& o) : _N_ant(o._N_ant), _N_chan(o._N_chan), _M_smp(o._M_smp) {
         _Rxx = o._Rxx;
         _freqs = o._freqs;
         _timestamp = o._timestamp;
         _Tint = o._Tint;
      }

   public:

      /**
       * Const accessor to data cube.
       * @return  Const reference to covariance data cube.
       */
      const arma::Cube<bf::complex>& get() const { return _Rxx; }

      /**
       * Writeable reference to data cube.
       * @return Reference to covariance data cube.
       */
      arma::Cube<bf::complex>& getWriteable() { return _Rxx; }
 
   public:

      /**
       * Load data cube contents from a memory location and
       * reorganize the memory layout if necessary. Currently a
       * stub that generates test data.
       * Modify this for your custom data e.g. for receiving GPU  
       * beamforming cluster covariance output.
       * @param[in]  raw_data  Pointer to data to load
       * @param[in]  format    Data format (0..N, to be defined)
       */
      void load(double* raw_data, const int format);

      /**
       * Load data cube contents from a file and
       * reorganize the memory layout if necessary.
       * @param[in]  fn        Input file name and path
       * @param[in]  format    Data format (0..N, to be defined)
       */
      void load(const char* fn, const int format);

      /**
       * Store data cube contents into a file.
       * @param[in]  fn        Output file name and path
       * @param[in]  format    Data format (0..N, to be defined)
       */
      void store(const char* fn, const int format) const;

   public:
    
      /**
       * Add an artificial signal to the covariance matrix
       * @param[in]  ch     Channel number
       * @param[in]  lambda Wavelength in meters
       * @param[in]  ae     ArrayElement object with element positions
       * @param[in]  phi    Azimuth angle of signal
       * @param[in]  theta  Tilt angle of plane wave normal from zenith
       * @param[in]  P      Signal power
       * @param[in]  Pna    Internal noise power (added to autocorrelations)
       * @param[in]  Pnc    Correlated noise power (added to cross and auto)
       */
      void addSignal(int ch, double lambda, ArrayElements const& ae, double phi, double theta, double P, double Pna, double Pnc);

      /**
       * Add an artificial signal to the covariance matrix, separating the
       * array into a set of normal elements and a set of RFI-only reference
       * antennas.
       * @param[in]  ch     Channel number
       * @param[in]  lambda Wavelength in meters
       * @param[in]  ae     ArrayElement object with element positions
       * @param[in]  phi    Azimuth angle of signal
       * @param[in]  theta  Tilt angle of plane wave normal from zenith
       * @param[in]  P      Signal power
       * @param[in]  Pna    Internal noise power (added to autocorrelations)
       * @param[in]  Pnc    Correlated noise power (added to cross and auto)
       * @param[in]  Gref   Reference antenna gain over array element gain
       * @param[in]  Iref   Vector with reference antenna indices between 0:(Nant-1)
       */
      void addSignal(int ch, double lambda, ArrayElements const& ae, double phi, double theta, double P, double Pna, double Pnc, 
                     double Gref, arma::Col<int> const& Iref);

   public:

      /**
       * Sums the data of another covariance object into the data cube contained in this object.
       */
      Covariance& operator+= (const Covariance &rhs) {
         _Rxx += rhs._Rxx;
         _Tint += rhs._Tint;
         return *this;
      }

      /**
       * Given a signal vector of Nant elements, calculates covariances
       * and integrates this into the current covariance data of the
       * specified channel (Rxx[ch] = Rxx[ch] + x'*x).
       * @param[in] ch Target covariance channel 0..Nch-1
       * @param[in] x  Column vector with data from Nant elements
       */
      void add(const int ch, arma::Col<bf::complex> const& x) {
         if (0) {
            // Formal style
            _Rxx.slice(ch) += (x * arma::trans(x));
         } else {
            // Matlab-style
            _Rxx.slice(ch) += (arma::conj(x) * arma::strans(x));
         }
      }


   private:
      int _N_ant;
      int _N_chan;
      int _M_smp;

   public:
      //! Accessor to get number of antennas in contained data
      const int N_ant(void)  const { return _N_ant; }

      //! Accessor to get number of channels in contained data
      const int N_chan(void) const { return _N_chan; }

      //! Accessor to get count of time-integrated covariances in contained data
      const int M_smp(void)  const { return _M_smp; }

      /**
       * Accessor to get frequency of one channel.
       * @param[in] ch Number of channel 0..Nchan-1
       * @return Frequency in Hertz
       */
      const double channel_freq(const int ch) { return _freqs(ch); }

   private:
      arma::Cube<bf::complex> _Rxx;
      arma::Col<double> _freqs;
      double _timestamp;
      double _Tint;
};

extern std::ostream &operator<<(std::ostream&, Covariance const&);

} // namespace bf

#endif // _COVARIANCE_H
