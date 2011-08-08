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

#include "Covariance.h"

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
   }
#endif

}

