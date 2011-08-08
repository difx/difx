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

#include "DecompositionAnalyzer.h"

#include <armadillo>

#include <limits>
#include <cmath>

DecompositionAnalyzer::DecompositionAnalyzer(SVDecomposition const& deco) : _deco(deco)
{
   _deco_type = 0;
}

DecompositionAnalyzer::DecompositionAnalyzer(EVDecomposition const& deco) : _deco(deco)
{
   _deco_type = 1;
}

double DecompositionAnalyzer::getMDL(int channel, int& rank) const
{
   double ic = -1.0f;
   double M = 1024;   // number of samples integrated

   // SVD and EVD
   if (_deco_type == 0 || _deco_type == 1) {

      channel %= (_deco.N_chan + 1);

      arma::Col<double> eigs;
      if (_deco.N_chan <= 1) {
         eigs = _deco._single_out_vector;
      } else {
         eigs = _deco._batch_out_vectors.col(channel);
      }

      double min_MDL = 1.0/0.0; //infinity();

      for (unsigned int ii=0; ii<(eigs.n_elem-1); ii++) {

         double arith = 0;
         double geo = 1.0f;
         double L = double(eigs.n_elem - ii);

         for (unsigned int i=(ii+1); i<eigs.n_elem; i++) {
            arith += eigs(i);
            geo *= eigs(i);
         }

         arith /= L;
         geo = std::pow(geo, 1/L);

         double MDL = -L*M*std::log(geo/arith) + 0.5*ii*(2*eigs.n_elem - ii + 1)*std::log(M);
         if (std::isnan(MDL) || std::isinf(MDL)) {
            std::cout << "Warning: MDL(n=" << ii << "/" << eigs.n_elem << ") is NaN or INF\n";
            continue;
         }

         if (MDL < min_MDL) {
            min_MDL = MDL;
            rank = ii;
         }
      }

      ic = min_MDL;
   } 

   return ic;
}

double DecompositionAnalyzer::getAIC(int channel, int& rank) const
{
   double ic = -1.0f;

   // SVD and EVD
   if (_deco_type == 0 || _deco_type == 1) {

      arma::Col<double> eigs;
      if (_deco.N_chan <= 1) {
         eigs = _deco._single_out_vector;
      } else {
         eigs = _deco._batch_out_vectors.col(channel);
      }

   } 

   return ic;
}

double DecompositionAnalyzer::getBIC(int channel, int& rank) const
{
   double ic = -1.0f;

   // SVD and EVD
   if (_deco_type == 0 || _deco_type == 1) {

      arma::Col<double> eigs;
      if (_deco.N_chan <= 1) {
         eigs = _deco._single_out_vector;
      } else {
         eigs = _deco._batch_out_vectors.col(channel);
      }

   } 

   return ic;
}

