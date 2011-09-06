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
using namespace bf;

#define TEST_DECOMPOSITIONS      0
#define TEST_INFOCRITERIA        0
#define TEST_RFI_SUBTRACTION     0
#define TEST_BEAMFORMING         1
#define TEST_NULLED_BEAMFORMING  1

inline double deg2rad(double d) { return (3.141592653589793238462643/180.0)*d; }

int main(int argc, char** argv)
{
        const double C_LAMBDA = 0.2021;        // default wavelength in test code
        const int    DIGESTIF_Nant = 4;        // 64 elements, 60 in use
        const double DIGESTIF_spacing = 10e-2; // element separation 10cm
        const int    DIGESTIF_Msmp = 768;      // time slices averaged into one covariance matrix
        const int    DIGESTIF_Nch = 2;         // 71 channels
        const double DIGESTIF_Tint = 0.1;      // guessing 0.1s integration time for covariance matrices
        const int    DIGESTIF_Ndisc = 5;       // number of discardable, disconnected antenna elements (required for MDL/AIC/3sig)
        bool data_is_synthetic = false; // is updated later

        //////////////////////////////////////////
        // GENERATE STANDARD ARRAY LAYOUT
        /////////////////////////////////////////

        ArrayElements ae;

        ae.generateGrid(DIGESTIF_Nant, DIGESTIF_spacing);
        const ElementXYZ_t xyz = ae.getPositionSet();

        // Flag some elements as RFI reference antennas
        // Note: for efficiency, subtraction algorithm requires reference antennas
        // reside at the beginning of the antenna element array.
        ae.setFlags(0, ArrayElements::POL_LCP | ArrayElements::POINT_RFI_REFERENCE);
        ae.setFlags(1, ArrayElements::POL_LCP | ArrayElements::POINT_RFI_REFERENCE);

        cout << "Number of antennas = " << xyz.Nant << "\n";
        std::cout << ae;

        //////////////////////////////////////////
        // PREPARE INPUT, OUTPUT COVARIANCES
        /////////////////////////////////////////

        // Note: all covariance matrices must be Hermitian!

        int Nrfi = 1;
        Covariance rxxDataBlock(xyz.Nant, DIGESTIF_Nch, DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);

        if (0) {

           std::cout << "Data source = external virgoA_on.raw\n";

           rxxDataBlock.load("virgoA_on.raw", 0);
           rxxDataBlock.store("out.raw", 0); // for test

           ae.generateGrid(rxxDataBlock.N_ant(), DIGESTIF_spacing);
           cout << "Updated and generated uniform grid array for loaded data N_ant = " << ae.Nant() << "\n";

           data_is_synthetic = false;
           Nrfi = 3;

        } else if (0) {

           std::cout << "Data source = self-generated, array, no reference antennas, 1 astro and 3 RFI signals\n";

           for (int ch=0; ch<DIGESTIF_Nch; ch++) {
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(10.0), deg2rad(25.0), 1.0, 0, 0);
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(10.0), deg2rad(25.0), 1.0, 0, 0);
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(40.0), deg2rad(25.0), 1.0, 0, 0);
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(0.0),  deg2rad(0.0),  1e-3, 5e-5, 5e-11);
           }
           data_is_synthetic = true;
           Nrfi = 3;

           std::cout << rxxDataBlock;

        } else {

           std::cout << "Data source = self-generated, array, 2 reference antennas, 1 astro and 1 RFI signal\n";

           double Gref = 1e3;    // +30dB gain to RFI compared to sky-pointed array elements
           arma::Col<int> Iref;  // reference antenna indices (2 refs at 0 and 1)

           Iref = ae.listReferenceAntennas();
           std::cout << "List of detected reference antennas:\n" << Iref;

           for (int ch=0; ch<DIGESTIF_Nch; ch++) {
              // add astro signal
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(45.0), deg2rad(-90.0), 1e-2, 1e-7, 0);
              Nrfi = 0;

              // add 1st RFI signal, with reference antennas specified
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(30.0), deg2rad(-15.0), 2.0, 0, 0, Gref, Iref);
              Nrfi = 1;

#if 0
              // add 2nd rfi signal; note that mitigation is not supported by Briggs-Kesteven method
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(40.0), deg2rad(-45.0), 3.0, 0, 0, Gref, Iref);
              Nrfi = 2;
#endif
           }

           data_is_synthetic = true;

           std::cout << rxxDataBlock;
        }

        // Output storage
        Covariance outDataBlock(rxxDataBlock.N_ant(), rxxDataBlock.N_chan(), DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);


        //////////////////////////////////////////
        // DECOMPOSITIONS and RECOMPOSITIONS
        /////////////////////////////////////////

#if TEST_DECOMPOSITIONS
        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        SVDecomposition edc(rxxDataBlock);
        edc.decompose(rxxDataBlock);
        std::cout << edc;
        edc.recompose(outDataBlock);
        std::cout << outDataBlock;
        cout << "--------------------------------------------------------------------\n";

        // -- PASS --
        cout << "--------------------------------------------------------------------\n";
        EVDecomposition edc2(rxxDataBlock);
        edc2.decompose(rxxDataBlock);
        std::cout << edc2;
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
        // REFERENCE ANTENNA METHODS
        /////////////////////////////////////////

#if TEST_RFI_SUBTRACTION
        arma::Col<int> Iref;  // reference antenna indices
        Iref = ae.listReferenceAntennas();

        std::cout << "before=\n" << rxxDataBlock;

        CovarianceModifier cm(rxxDataBlock);
        if (cm.templateSubtraction(Iref, Nrfi) < 0) {
           std::cout << "Template subtraction failed\n";
        }

        std::cout << "after_sub=\n" << rxxDataBlock;
#endif


        //////////////////////////////////////////
        // ANALYZE EVD/SVD DECOMPOSITION
        /////////////////////////////////////////

        SVDecomposition info(rxxDataBlock);
        info.decompose(rxxDataBlock);

        DecompositionAnalyzer da(info);
        //da.utest();

#if TEST_INFOCRITERIA
        for (int cc=0; cc<rxxDataBlock.N_chan(); cc++) {
           int mdl_rank, aic_rank;
           double mdl, aic;
           mdl = da.getMDL(0, mdl_rank);
           aic = da.getAIC(0, aic_rank);
           cout << "DecompositionAnalyzer on channel " << cc << " returned "
                << "MDL={IC=" << mdl << ",rank=" << mdl_rank << "}, "
                << "AIC={IC=" << aic << ",rank=" << aic_rank << "}\n";
        }
#endif

        // Nulling with at most Nrfi interferers and autodetect=true
        DecompositionModifier dm(info);
        if (data_is_synthetic) {
           dm.interfererNulling(Nrfi, true, 0, rxxDataBlock.N_chan()-1);
        } else {
           // Virgo A data set has 5 disconnected elements
           dm.interfererNulling(Nrfi, true, 0, rxxDataBlock.N_chan()-1, DIGESTIF_Ndisc);
        }

        // Recompute the RFI-filtered covariance matrix
        info.recompose(outDataBlock);


        //////////////////////////////////////////
        // BEAMFORMING
        /////////////////////////////////////////

        Beams_t beams;

        // Configure the beams and channel frequencies
        beams.init(/*beams*/2, rxxDataBlock.N_ant(), rxxDataBlock.N_chan());

        beams.phi(0) = 0;  beams.theta(0) = 0;
        beams.phi(1) = 45; beams.theta(1) = -90;
        for (int cc=0; cc<rxxDataBlock.N_chan(); cc++) {
           beams.freqs(cc) = rxxDataBlock.channel_freq(cc);
        }

        // Create steerings for all beams	
        BeamformerWeights bw;
        bw.generateSteerings(beams, ae);


#if (TEST_BEAMFORMING||TEST_NULLED_BEAMFORMING)
        std::cout << beams;
#endif

#if TEST_BEAMFORMING
        // Original data: Compute classical beamformer weights, ignores covariances
        bw.generateMVDR(beams, rxxDataBlock, 0.0f);
        std::cout << "Classical non-adaptive beamformer: " << bw;

        // Original data: Compute MVDR beamformer weights from (perhaps nulled) covariance matrix
        bw.generateMVDR(beams, rxxDataBlock, 1.0f);
        std::cout << "MVDR beamformer: " << bw;

        // Original data: Compute RB-MVDR beamformer weights with Cox Projection WNGC
        bw.generateMVDR(beams, rxxDataBlock, 1.0f + 1e-5);
        std::cout << "RB-MVDR beamformer with factor b=" << (1.0f + 1e-5) << ": " << bw;
#endif


#if TEST_NULLED_BEAMFORMING
        // Original data: Compute classical beamformer weights, ignores covariances
        bw.generateMVDR(beams, outDataBlock, 0.0f);
        std::cout << "Classical non-adaptive beamformer: " << bw;

        // Original data: Compute MVDR beamformer weights from (perhaps nulled) covariance matrix
        bw.generateMVDR(beams, outDataBlock, 1.0f);
        std::cout << "MVDR beamformer: " << bw;

        // Original data: Compute RB-MVDR beamformer weights with Cox Projection WNGC
        bw.generateMVDR(beams, outDataBlock, 1.0f + 1e-5);
        std::cout << "RB-MVDR beamformer with factor b=" << (1.0f + 1e-5) << ": " << bw;
#endif

	return 0;
}

