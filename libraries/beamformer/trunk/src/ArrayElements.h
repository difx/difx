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

#ifndef _ARRAY_ELEMENTS_H
#define _ARRAY_ELEMENTS_H

#include <armadillo>
#include <ostream>

namespace bf {

/** Phased array element positions */
typedef struct ElementXYZ_tt {
   int Nant;
   int Ldim[3];
   arma::Col<double> x;
   arma::Col<double> y;
   arma::Col<double> z;
   arma::Col<int> flag;
} ElementXYZ_t;


/**
 * Class for creating and storing the (x,y,z) coordinates of all
 * elements in a phased array.
 */
class ArrayElements {

   friend std::ostream &operator<<(std::ostream&, ArrayElements const&);

   public:
      enum Pointing { POINT_ASTRO=0, POINT_RFI_REFERENCE=128 };
      enum Polarization { POL_LCP=0, POL_RCP=1 };

   private:
      ArrayElements(const ArrayElements&);
      ArrayElements& operator= (const ArrayElements&);

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
       * Accessor function to element data struct
       * @return ref to struct that contains antenna element coordinates
       */
      const ElementXYZ_t& getPositionSet() const { return elems; }

      /**
       * Assemble and return a list of reference antenna indices.
       * @return Column vector with indices of all antennas currently flagged as RFI reference.
       */
      arma::Col<int> listReferenceAntennas();

      /**
       * Set flags on element, overwrites earlier flags.
       * @param[in] ielem The index of the element 0..N-1
       * @param[in] flags The value to set, consisting of OR'ed flags 
       * @return Old flag value before it was overwritten or -1 on error
       * Flags are POINT_ASTRO, POINT_RFI_REFERENCE, POL_LCP and POL_LCP.
       */
      int setFlags(const int ielem, const int flags);

      /**
       * Accessor to array element flags
       * @param[in] ielem The index of the element 0..N-1
       * @return Flag value of the element or -1 on error
       */
      const int getFlags(const int ielem) const;

      /**
       * Accessor to get number of elements in array.
       */
      const int Nant() const { return elems.Nant; }

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

extern std::ostream &operator<<(std::ostream&, ArrayElements const&);

} // namespace bf

#endif // _ARRAY_ELEMENTS_H
