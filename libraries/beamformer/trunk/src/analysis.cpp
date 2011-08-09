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

#include <iostream>
#include <armadillo>

#include "Beamformer.h"

using namespace std;
using namespace arma;

int main(int argc, char** argv)
{
        const int    DIGESTIF_Nant = 4;       // 64 elements, 60 in use
        const double DIGESTIF_spacing = 10e-2; // element separation 10cm
        const int    DIGESTIF_Nch = 1;        // 71 channels
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

        // Load/generate covariance matrices. 
        // Note: all covariance matrices must be Hermitian!

        srand(time(NULL));
        Covariance rxxDataBlock(DIGESTIF_Nant, DIGESTIF_Nch, 0.0f, DIGESTIF_Tint);
        rxxDataBlock.load(NULL, 0);

        Covariance outDataBlock(DIGESTIF_Nant, DIGESTIF_Nch, 0.0f, DIGESTIF_Tint);

        //////////////////////////////////////////
        // DECOMPOSITIONS
        /////////////////////////////////////////

        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        SVDecomposition edc(rxxDataBlock);
        edc.decompose(rxxDataBlock);
        edc.recompose(outDataBlock);
        cout << "--------------------------------------------------------------------\n";

        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        EVDecomposition edc2(rxxDataBlock);
        edc2.decompose(rxxDataBlock);
        edc2.recompose(outDataBlock);
        cout << "--------------------------------------------------------------------\n";

        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        QRDecomposition edc3(rxxDataBlock);
        edc3.decompose(rxxDataBlock);
        edc3.recompose(outDataBlock);
        cout << "--------------------------------------------------------------------\n";

        //////////////////////////////////////////
        // ANALYZE SVD DECOMPOSITION
        /////////////////////////////////////////

        SVDecomposition info(rxxDataBlock);
        info.decompose(rxxDataBlock);

        DecompositionAnalyzer da(info);
        da.utest();

        int mdl_rank, aic_rank;
        double mdl, aic;
        mdl = da.getMDL(0, mdl_rank);
        aic = da.getAIC(0, aic_rank);
        cout << "DecompositionAnalyzer returned channel 0 "
             << "MDL={IC=" << mdl << ",rank=" << mdl_rank << "}, "
             << "AIC={IC=" << aic << ",rank=" << aic_rank << "}\n";

        //////////////////////////////////////////
        // Reference
        /////////////////////////////////////////

	cx_mat A = randu<cx_mat>(DIGESTIF_Nant, DIGESTIF_Nant);

#if 0
        A = A * trans(A);

	cx_mat eigvecs;
	vec eigvals;
	if (!eig_sym(eigvals, eigvecs, A)) cout << "-- EIG FAILED --\n";

        cx_mat C = eigvecs * diagmat(eigvals) * inv(eigvecs);
        cout << "Before EIG:\n" << A << "\n";
        cout << "After EIG:\n"  << C << "\n";

#endif

#if 0
	cx_mat U;
	cx_mat V;
	vec s;
        if (!svd(U, s, V, A)) cout << "-- SVD FAILED --\n"; else cout << "SVD OK!\n";

        // nulling
        // s.set_size(s.n_elem + 1);
        // s(s.n_elem-1) = 0;

        cx_mat C = U * diagmat(s) * trans(V);
        cout << "Before SVD:\n" << A << "\n";
        cout << "After SVD:\n"  << C << "\n";
#endif
    
	return 0;
}

