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

#include <cmath>

/**
 * Load data cube contents from a memory location and
 * reorganize the memory layout if necessary.
 * @param[in]  raw_data  Pointer to data to load
 * @param[in]  format    Data format (0..N, to be defined)
 */

void Covariance::load(double* raw_data, int format)
{

#if 1
   arma::cx_mat tmp = arma::randu<arma::cx_mat>(N_ant,N_ant);
   tmp = trans(tmp) * tmp; // make symmetric
   for (unsigned int cc=0; cc<_Rxx.n_slices; cc++) {
      _Rxx.slice(cc) = tmp;
      _freqs(cc) = 1409e3;
   }
#endif

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
   double K = 2*M_PI/lambda;
   double k_src[3] = { K*std::sin(theta)*std::cos(phi), K*std::sin(theta)*std::sin(phi), K*std::cos(theta) };

   const ElementXYZ_t xyz = ae.getPositionSet();
   arma::Col<arma::cx_double> sigs = arma::zeros<arma::Col<arma::cx_double> >(xyz.Nant);

   for (int a=0; a<xyz.Nant; a++) {
      double gain = cos(theta)*cos(theta);
      double phase = k_src[0]*xyz.x[a] + k_src[1]*xyz.y[a] + k_src[2]*xyz.z[a];
      sigs(a) = std::complex<double>(cos(phase), -sin(phase)); // =exp(-i*xyz*k_src)
      sigs(a) *= gain*P;
   }

   _freqs(ch) = 299792458.0 / lambda;
   _Rxx.slice(ch) += (sigs*arma::trans(sigs)) + (arma::eye<arma::cx_mat>(xyz.Nant, xyz.Nant))*Pna;

   if (Pnc>0) {
      arma::cx_mat noise = Pnc * arma::randu<arma::cx_mat>(N_ant,N_ant);
      _Rxx.slice(ch) += trans(noise)*noise;
   }
}


/**   
 * Human-readable data output to stream
 */
std::ostream &operator<<(std::ostream& os, Covariance c)
{
   os << "Covariance " << c.N_ant << "x" << c.N_ant << "x" << c.N_chan 
      << ", M=" << c.M_smp << ", timestamp=" << c._timestamp << "\n";
   for (unsigned int cc=0; cc<c._Rxx.n_slices; cc++) {
      os << "Cov[" << cc << "] =\n"
         << c._Rxx.slice(cc);
   }
   return os;
}

