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
   int Ldim[3];
   arma::Col<double> x;
   arma::Col<double> y;
   arma::Col<double> z;
} ElementXYZ_t;


/**
 * Class for creating and storing the (x,y,z) coordinates of all
 * elements in a phased array.
 */
class ArrayElements {

   public:
      /**
       * C'stor for initialization.
       */
      ArrayElements() { elems.Nant = 0; }

      /**
       * D'stor
       */
      ~ArrayElements() { }

   public:
      /**
       * Accessor function.
       * @return ref to struct that contains antenna element coordinates
       */
      const ElementXYZ_t& getPositionSet() const { return elems; }

   public:
      /**
       * Generate a 1D equispaced linear antenna array centered around origo
       * with antennas placed along the first dimension (x axis).
       * @param[in]  Nant     Total number of antennas
       * @param[in]  spacing  Antenna spacing in meters
       */
      void generateLinear(int Nant, double spacing);

      /**
       * Generate a 2D equispaced square grid antenna array centered around origo
       * with antennas placed onto first 2 dimensions (x,y plane)
       * @param[in]  Nant     Total number of antennas
       * @param[in]  spacing  Antenna spacing in meters
       */
      void generateGrid(int Nant, double spacing);

   private:
      ElementXYZ_t elems;
};


#endif // _ARRAY_ELEMENTS_H
