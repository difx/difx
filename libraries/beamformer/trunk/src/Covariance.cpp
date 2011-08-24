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

#include "Covariance.h"

#include <armadillo>

#include <cmath>
#include <fstream>
#include <iostream>

/**
 * Load data cube contents from a memory location and
 * reorganize the memory layout if necessary.
 * @param[in]  raw_data  Pointer to data to load
 * @param[in]  format    Data format (0..N, to be defined)
 */

void Covariance::load(double* raw_data, int format)
{

#if 1
   arma::cx_mat tmp = arma::randu<arma::cx_mat>(_N_ant,_N_ant);
   tmp = arma::trans(tmp) * tmp; // make symmetric
   for (unsigned int cc=0; cc<_Rxx.n_slices; cc++) {
      _Rxx.slice(cc) = tmp;
      _freqs(cc) = 1409e3;
   }
#endif

}


/**
 * Load data cube contents from a file and           
 * reorganize the memory layout if necessary.
 * @param[in]  fn        Input file name and path
 * @param[in]  format    Data format (0..N, to be defined)
 */
void Covariance::load(const char* fn, const int format)
{
   int Nch, Nant, Nmatrices, Ndummy;
   std::ifstream in(fn, std::ifstream::in | std::ifstream::binary);
   in.read((char*)&Nch, 4);
   in.read((char*)&Nant, 4);
   in.read((char*)&Nmatrices, 4);
   in.read((char*)&Ndummy, 4);

   if (Nch>1024 || Nant>512) {
      std::cout << "Data 1x" << Nant << "x" << Nant << "x" << Nch << " exceeds max dimensions of 1x512x512x1024\n";
      return;
   }

   std::cout << "Loading 1x" << Nant << "x" << Nant << "x" << Nch << " data...\n";

   _N_ant = Nant;
   _N_chan = Nch;

   // Load frequencies
   _freqs.zeros(_N_ant);
   for (int cc=0; cc<_N_chan; cc++) {
      double f;
      in.read((char*)&f, 8);
      _freqs[cc] = f;
   }

   // Load complex double data
   _Rxx.zeros(_N_ant,_N_ant, _N_chan);
   for (int cc=0; cc<_N_chan; cc++) {
      for (int ii=0; ii<_N_ant; ii++) {
         for (int jj=0; jj<_N_ant; jj++) {
            double re, im;
            in.read((char*)&re, sizeof(double));
            in.read((char*)&im, sizeof(double));
            _Rxx(jj,ii,cc) = std::complex<double>(re, im);
            if (in.eof()) {
               std::cout << "Early EOF in file " << fn << " while reading channel " << cc << "\n";
               in.close();
               return;
            }
         }
      }
   }

   in.close();
}


/**
 * Store data cube contents into a file.
 * @param[in]  fn        Output file name and path
 * @param[in]  format    Data format (0..N, to be defined)
 */
void Covariance::store(const char* fn, const int format) const
{
   int Ndummy = 0;
   int Nmatrices = 1; // currently just 1 time snapshot
   std::ofstream out(fn, std::ofstream::out | std::ofstream::binary | std::ofstream::trunc);
   out.write((char*)&_N_chan, 4);
   out.write((char*)&_N_ant, 4);
   out.write((char*)&Nmatrices, 4);
   out.write((char*)&Ndummy, 4);

   for (int cc=0; cc<_N_chan; cc++) {
      double f = _freqs[cc];
      out.write((char*)&f, 8);
   }

   for (int cc=0; cc<_N_chan; cc++) {
      for (int ii=0; ii<_N_ant; ii++) {
         for (int jj=0; jj<_N_ant; jj++) {
            std::complex<double> cx = _Rxx(jj,ii,cc);
            double cr = cx.real();
            double ci = cx.imag();
            out.write((char*)&cr, sizeof(double));
            out.write((char*)&ci, sizeof(double));
         }
      }
   }

   out.close();
}


/**
 * Add an artificial signal to the covariance matrix
 * @param[in]  ch     Channel number
 * @param[in]  lambda Wavelength in meters
 * @param[in]  ar     ArrayElement object with element positions
 * @param[in]  phi    Azimuth angle of signal
 * @param[in]  theta  Tilt angle of plane wave normal from zenith
 * @param[in]  p      Signal power
 * @param[in]  Pna    Internal noise power (added to autocorrelations)
 * @param[in]  Pnc    Correlated noise power (added to cross and auto)
 */
void Covariance::addSignal(int ch, double lambda, ArrayElements const& ae, double phi, double theta, double P, double Pna, double Pnc)
{
   arma::Col<int> empty_refAnt_list;
   addSignal(ch, lambda, ae, phi, theta, P, Pna, Pnc, 0, empty_refAnt_list);
   return;
}


/**
 * Add an artificial signal to the covariance matrix, separating the
 * array into a set of normal elements and a set of RFI-only reference
 * antennas.
 * @param[in]  ch     Channel number
 * @param[in]  lambda Wavelength in meters
 * @param[in]  ae     ArrayElement object with element positions
 * @param[in]  phi    Azimuth angle of signal
 * @param[in]  theta  Tilt angle of plane wave normal from zenith
 * @param[in]  p      Signal power
 * @param[in]  Pna    Internal noise power (added to autocorrelations)
 * @param[in]  Pnc    Correlated noise power (added to cross and auto)
 * @param[in]  Gref   Reference antenna gain over array element gain
 * @param[in]  Iref   Vector with reference antenna indices between 0:(Nant-1)         
 */
void Covariance::addSignal
(int ch, double lambda, ArrayElements const& ae, double phi, double theta, double P, double Pna, double Pnc, 
 double Gref, arma::Col<int> const& Iref)
{
   double K = 2*M_PI / lambda;
   double k_src[3] = { std::sin(theta)*std::cos(phi), std::sin(theta)*std::sin(phi), std::cos(theta) };

   const ElementXYZ_t xyz = ae.getPositionSet();
   arma::Col<arma::cx_double> sigs;
   sigs.zeros(xyz.Nant); 

   P = std::sqrt(P);
   Pna = std::sqrt(Pna);
   Pnc = std::sqrt(Pnc);

   for (int a=0; a<xyz.Nant; a++) {
      double phase = K * (k_src[0]*xyz.x[a] + k_src[1]*xyz.y[a] + k_src[2]*xyz.z[a]);
      sigs(a) = std::complex<double>(std::cos(phase), -std::sin(phase)); // =exp(-i*xyz*k_src)
      sigs(a) *= P;
   }

   // Apply additional gain to the reference antenna signals
   for (unsigned int i=0; i<Iref.n_elem; i++) {
      int a = Iref(i);
      sigs(a) *= Gref;
   }

   // Store frequency and accumulate covariance (use one of two options for handedness)
   _freqs(ch) = 299792458.0 / lambda;
   if (0) {
      _Rxx.slice(ch) += (sigs * arma::trans(sigs));
   } else {
      _Rxx.slice(ch) += (arma::conj(sigs) * arma::strans(sigs));
   }

   // Add noise
   if (Pna>0) {
      _Rxx.slice(ch) += (arma::eye<arma::cx_mat>(xyz.Nant, xyz.Nant))*Pna;
   }
   if (Pnc>0) {
      // add Hermitian noise matrix
      arma::cx_mat noise = Pnc * arma::randu<arma::cx_mat>(_N_ant,_N_ant);
      _Rxx.slice(ch) += arma::trans(noise)*noise;
   }
}

/**   
 * Human-readable data output to stream
 */
std::ostream &operator<<(std::ostream& os, Covariance const& c)
{
   os << "Covariance " << c._N_ant << "x" << c._N_ant << "x" << c._N_chan 
      << ", M=" << c._M_smp << ", timestamp=" << c._timestamp << "\n";
   for (unsigned int cc=0; cc<c._Rxx.n_slices; cc++) {
      os << "Cov[" << cc << "] =\n"
         << c._Rxx.slice(cc);
   }
   return os;
}

