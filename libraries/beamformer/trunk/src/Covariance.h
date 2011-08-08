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

#ifndef _COVARIANCE_H
#define _COVARIANCE_H

#include <armadillo>

/** Time-integrated covariance matrix */
class Covariance {

   private:
      Covariance();
   public:
      Covariance(int Nant, int Nchannels, double timestamp, double Tint) { 
         _Rxx = arma::zeros<arma::Cube<arma::cx_double> >(Nchannels, Nant,Nant);
         _timestamp = timestamp;
         _Tint = Tint;
      }
      ~Covariance() { }

   public:
      const arma::Cube<arma::cx_double>& get() const { return _Rxx; }
      void load(double* raw_data);

   public:
      Covariance& operator+= (const Covariance &rhs) {
         _Rxx += rhs._Rxx;
         _Tint += rhs._Tint;
         return *this;
      }

   private:
      arma::Cube<arma::cx_double> _Rxx;
      double _timestamp;
      double _Tint;
};

#endif // _COVARIANCE_H
