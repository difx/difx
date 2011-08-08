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

#include <iostream>
#include <armadillo>

#include "Beamformer.h"

using namespace std;
using namespace arma;

int main(int argc, char** argv)
{
        const int    DIGESTIF_Nant = 4;       // 64 elements, 60 in use
        const double DIGESTIF_spacing = 10e-2; // element separation 10cm
        const int    DIGESTIF_Nch = 2;        // 71 channels
        const double DIGESTIF_Tint = 0.1;      // guessing 0.1s integration time for covariance matrices

        ArrayElements ae;
        ae.generateGrid(DIGESTIF_Nant, DIGESTIF_spacing);

        const ElementXYZ_t xyz = ae.getPositionSet();
        cout << "Number of antennas = " << xyz.Nant << "\n";
        if (0) {
           cout << "Antenna layout:\n";
           int antcount = 0;
           for (int yy=0; yy<xyz.Ldim[1]; yy++) {
              for (int xx=0; xx<xyz.Ldim[0]; xx++) {
                 cout << antcount << "=(" << xyz.x(antcount) << "," << xyz.y(antcount) << "," << xyz.z(antcount) << ")\t\t";
                 antcount++;
              }
              cout << "\n";
           }
        }

        Covariance rxxDataBlock(DIGESTIF_Nant, DIGESTIF_Nch, 0.0f, DIGESTIF_Tint);
        rxxDataBlock.load(NULL, 0);

        EVDecomposition evd(rxxDataBlock);
        evd.decompose(rxxDataBlock);

#if 0
	cx_mat A = randu<cx_mat>(10,10);
	cx_mat B = trans(A)*A;
	vec eigvals;
	cx_mat eigvecs;
	eig_sym(eigvals, eigvecs, B);

        // nulling
        eigvals.set_size(eigvals.n_elem + 1);
        eigvals(eigvals.n_elem-1) = 0;
	cout << eigvals << endl;
#endif
    
	return 0;
}

