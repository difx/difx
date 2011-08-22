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
#include <cmath>

using namespace std;
using namespace arma;

inline double deg2rad(double d) { return M_PI*d/180.0; }

int main(int argc, char** argv)
{
        const int    DIGESTIF_Nant = 4;       // 64 elements, 60 in use
        const double DIGESTIF_spacing = 10e-2; // element separation 10cm
        const int    DIGESTIF_Msmp = 100;     // time slices averaged into one covariance matrix
        const int    DIGESTIF_Nch = 2;        // 71 channels
        const double DIGESTIF_Tint = 0.1;      // guessing 0.1s integration time for covariance matrices

        //////////////////////////////////////////
        // GENERATE STANDARD ARRAY LAYOUT
        /////////////////////////////////////////

        ArrayElements ae;

        ae.generateGrid(DIGESTIF_Nant, DIGESTIF_spacing);
        const ElementXYZ_t xyz = ae.getPositionSet();

        // Flag some elements as RFI reference antennas
        ae.setFlags(0,          ArrayElements::POL_LCP | ArrayElements::POINT_RFI_REFERENCE);
        ae.setFlags(xyz.Nant-1, ArrayElements::POL_LCP | ArrayElements::POINT_RFI_REFERENCE);

        cout << "Number of antennas = " << xyz.Nant << "\n";
        std::cout << ae;

        //////////////////////////////////////////
        // PREPARE INPUT COVARIANCES
        /////////////////////////////////////////

        // Note: all covariance matrices must be Hermitian!

        Covariance rxxDataBlock(xyz.Nant, DIGESTIF_Nch, DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);

        if (0) {

           std::cout << "Data source = external virgoA_on.raw\n";

           rxxDataBlock.load("virgoA_on.raw", 0);
           rxxDataBlock.store("out.raw", 0); // for test

        } else if (0) {

           std::cout << "Data source = self-generated, array, no reference antennas, 1 astro and 3 RFI signals\n";

           for (int ch=0; ch<DIGESTIF_Nch; ch++) {
              rxxDataBlock.addSignal(ch, 0.2202, ae, deg2rad(10), deg2rad(25), 1, 0, 0);
              rxxDataBlock.addSignal(ch, 0.2202, ae, deg2rad(10), deg2rad(25), 1, 0, 0);
              rxxDataBlock.addSignal(ch, 0.2202, ae, deg2rad(40), deg2rad(25), 1, 0, 0);
              rxxDataBlock.addSignal(ch, 0.2202, ae, deg2rad(0),  deg2rad(0),  1e-3, 5e-5, 5e-11);
           }

        } else {

           std::cout << "Data source = self-generated, array, 2 reference antennas, 1 astro and 1 RFI signal\n";

           double Gref = 1e3;    // +30dB gain to RFI compared to sky-pointed array elements
           arma::Col<int> Iref;  // reference antenna indices (2 refs at 0 and DIGESTIF_Nant-1)

           Iref = ae.listReferenceAntennas();
           std::cout << "List of detected reference antennas:\n" << Iref;

           for (int ch=0; ch<DIGESTIF_Nch; ch++) {
              // add astro signal and 1st RFI signal
              rxxDataBlock.addSignal(ch, 0.2202, ae, deg2rad(0),  deg2rad(0),  1e-3, 5e-5, 5e-11);
              rxxDataBlock.addSignal(ch, 0.2202, ae, deg2rad(30), deg2rad(-15), 2, 0, 0, Gref, Iref);

              // add 2nd rfi signal; note that mitigation is not supported by Briggs-Kesteven method
              //rxxDataBlock.addSignal(ch, 0.2202, ae, deg2rad(40), deg2rad(-45), 3, 0, 0, Gref, Iref);
           }
        }

        std::cout << rxxDataBlock;
return 0;

        Covariance outDataBlock(rxxDataBlock.N_ant(), rxxDataBlock.N_chan(), DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);

        //////////////////////////////////////////
        // DECOMPOSITIONS and RECOMPOSITIONS
        /////////////////////////////////////////

#if 0
        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        SVDecomposition edc(rxxDataBlock);
        edc.decompose(rxxDataBlock);
        edc.recompose(outDataBlock);
        std::cout << outDataBlock;
        cout << "--------------------------------------------------------------------\n";

        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        EVDecomposition edc2(rxxDataBlock);
        edc2.decompose(rxxDataBlock);
        edc2.recompose(outDataBlock);
        std::cout << outDataBlock;
        cout << "--------------------------------------------------------------------\n";

        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        QRDecomposition edc3(rxxDataBlock);
        edc3.decompose(rxxDataBlock);
        std::cout << edc3;
        edc3.recompose(outDataBlock);
        std::cout << outDataBlock;
        cout << "--------------------------------------------------------------------\n";
#endif

        //////////////////////////////////////////
        // ANALYZE SVD DECOMPOSITION
        /////////////////////////////////////////

        SVDecomposition info(rxxDataBlock);
        info.decompose(rxxDataBlock);
        //cout << info;

        DecompositionAnalyzer da(info);
        //da.utest();

        int mdl_rank, aic_rank;
        double mdl, aic;
        for (int cc=0; cc<rxxDataBlock.N_chan(); cc++) {
           mdl = da.getMDL(0, mdl_rank);
           aic = da.getAIC(0, aic_rank);
           cout << "DecompositionAnalyzer on channel " << cc << " returned "
                << "MDL={IC=" << mdl << ",rank=" << mdl_rank << "}, "
                << "AIC={IC=" << aic << ",rank=" << aic_rank << "}\n";
        }

        // Nulling with at most 2 interferers
        DecompositionModifier dm(info, ae);
        dm.interfererNulling(2, false, 0, rxxDataBlock.N_chan());

        // Recompute the RFI-filtered covariance matrix
        info.recompose(outDataBlock);

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

