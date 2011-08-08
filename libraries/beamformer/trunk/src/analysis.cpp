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

#include "ArrayElements.h"
#include "BeamformerData.h"
#include "Covariance.h"
#include "Analyzer.h"

using namespace std;
using namespace arma;

int main(int argc, char** argv)
{
        const int    DIGESTIF_Nant = 64;       // 64 elements, 60 in use
        const double DIGESTIF_spacing = 10e-2; // element separation 10cm
        const int    DIGESTIF_Nch = 71;        // 71 channels
        const double DIGESTIF_Tint = 0.1;      // guessing 0.1s integration time for covariance matrices

        ArrayElements ae;
        ae.generateGrid(DIGESTIF_Nant, DIGESTIF_spacing);

        const ElementXYZ_t xyz = ae.getPositionSet();
        cout << "Number of antennas = " << xyz.Nant << "\n";
        #if 0
        cout << xyz.x << "\n";
        cout << xyz.y << "\n";
        cout << xyz.z << "\n";
        #endif

        Covariance rxxDataBlock(DIGESTIF_Nant, DIGESTIF_Nch, 0.0f, DIGESTIF_Tint);

	mat A = randu<mat>(10,10);
	mat B = trans(A)*A;

	vec eigvals;
	mat eigvecs;
	eig_sym(eigvals, eigvecs, B);

#if 0
	cout << eigvals << endl;
	cout << eigvecs << endl;

	mat U;
	vec s;
	mat V;
	svd(U,s,V, B);
	cout << U << endl;
	cout << s << endl;
	cout << V << endl;
#endif

        eigvals.set_size(eigvals.n_elem + 1);
        eigvals(eigvals.n_elem-1) = -1;
//	cout << eigvals << endl;
    
	return 0;
}

