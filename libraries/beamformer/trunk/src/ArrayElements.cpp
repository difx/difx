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

#include <armadillo>
#include <cmath>

#include "ArrayElements.h"

/**
 * Generate a 1D equispaced linear antenna array centered around origo
 * with antennas placed along the first dimension (x axis).
 * @param[in]  Nant     Total number of antennas
 * @param[in]  spacing  Antenna spacing in meters
 */
void ArrayElements::generateLinear(int Nant, double spacing)
{

   // Init
   elems.Nant = Nant;
   elems.Ldim[0] = Nant;
   elems.Ldim[1] = 0;
   elems.Ldim[2] = 0;
   elems.x.set_size(Nant);
   elems.y.set_size(Nant);
   elems.z.set_size(Nant);
   elems.x.zeros();
   elems.y.zeros();
   elems.z.zeros();

   // Center the elements around (0,0,0)
   double shift;
   if ((Nant % 2) == 0) {
      shift = double(Nant/2) - 0.5;
   } else {
      shift = double((Nant-1)/2);
   }

   // Calculate element positions
   for (int n=0; n<Nant; n++) {
      elems.x(n) = spacing * (double(n) - shift);
   }

   return;
}

/**
 * Generate a 2D equispaced square grid antenna array centered around origo 
 * with antennas placed onto first 2 dimensions (x,y plane)
 * @param[in]  Nant     Total number of antennas 
 * @param[in]  spacing  Antenna spacing in meters
 */
void ArrayElements::generateGrid(int Nant, double spacing)
{

   // Insure that the array will be square
   int Lbox = std::floor(sqrt(Nant));
   if ((Lbox*Lbox) != Nant) {
       Nant = Lbox*Lbox;
   }

   // Init
   elems.Nant = Nant;
   elems.Ldim[0] = Lbox;
   elems.Ldim[1] = Lbox;
   elems.Ldim[2] = 0;
   elems.x.set_size(Nant);
   elems.y.set_size(Nant);
   elems.z.set_size(Nant);
   elems.x.zeros();
   elems.y.zeros();
   elems.z.zeros();

   // Center the elements around (0,0,0)
   double shift;
   if ((Lbox % 2) == 0) {
      shift = double(Lbox/2) - 0.5;
   } else {
      shift = double((Lbox-1)/2);
   }

   // Calculate element positions
   for (int row=0; row<Lbox; row++) {
      for (int col=0; col<Lbox; col++) {
         int ii = row*Lbox + col;
         if (ii>=Nant) { continue; }
         elems.x(ii) = spacing * (col - shift);
         elems.y(ii) = spacing * (row - shift);
      }
   }
   
   return;
}
