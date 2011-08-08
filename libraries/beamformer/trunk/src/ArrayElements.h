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

#ifndef _ARRAY_ELEMENTS_H
#define _ARRAY_ELEMENTS_H

#include <armadillo>

/** Phased array element positions */
typedef struct ElementXYZ_tt {
   int Nant;
   arma::Col<double> x;
   arma::Col<double> y;
   arma::Col<double> z;
} ElementXYZ_t;

class ArrayElements {
   public:
      ArrayElements();
      ~ArrayElements();

   public:
      const ElementXYZ_t& getPositionSet() const { return elems; }

   public:
      void generateLinear(int Nant, double spacing);
      void generateGrid(int Nant, double spacing);

   private:
      ElementXYZ_t elems;
};


#endif // _ARRAY_ELEMENTS_H
