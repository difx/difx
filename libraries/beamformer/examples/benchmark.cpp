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

#include "Beamformer.h"
#include "Timing.h"

#include <iostream>
#include <cmath>

#include <unistd.h>

using namespace std;
using namespace bf;

inline double deg2rad(double d) { return (3.141592653589793238462643/180.0)*d; }

int main(int argc, char** argv)
{
        const int    DIGESTIF_Nant = 64;       // 64 elements, 60 in use
        const double DIGESTIF_spacing = 10e-2; // element separation 10cm
        const int    DIGESTIF_Msmp = 100;      // time slices averaged into one covariance matrix
        const int    DIGESTIF_Nch = 71;        // 71 channels
        const double DIGESTIF_Tint = 0.1;      // guessing 0.1s integration time for covariance matrices
        const double C_LAMBDA = 0.2021;        // default wavelength in test code

        const int    N_ITER = 10; // benchmark iterations

        //////////////////////////////////////////
        // COMPILE INFO
        //////////////////////////////////////////

        std::cout << "\n --- Library and test program compiled for ";
        if (sizeof(bf::real) >= 8) {
           std::cout << "double-precision complex floating point arithmetic ---\n\n";
        } else {
           std::cout << "single-precision complex floating point arithmetic ---\n\n";
        }

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

        cout << "Final number of antennas on square grid = " << xyz.Nant << "\n";

        //////////////////////////////////////////
        // PREPARE INPUT, OUTPUT COVARIANCES
        /////////////////////////////////////////

        // Note: all covariance matrices must be Hermitian!

        int Nrfi = 0;
        Covariance rxxDataBlock(xyz.Nant, DIGESTIF_Nch, DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);
        Covariance refDataBlock_1RFI(xyz.Nant, DIGESTIF_Nch, DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);
        Covariance refDataBlock_2RFI(xyz.Nant, DIGESTIF_Nch, DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);
        Covariance outDataBlock(rxxDataBlock.N_ant(), rxxDataBlock.N_chan(), DIGESTIF_Msmp, 0.0f, DIGESTIF_Tint);

        if (0) {

           std::cout << "Data source = external virgoA_on.raw\n";
           rxxDataBlock.load("virgoA_on.raw", 0);
           Nrfi = 3;

        } else {

           std::cout << "Data source = self-generated, array, no reference antennas, 1 astro and 3 RFI signals\n";
           for (int ch=0; ch<DIGESTIF_Nch; ch++) {
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(10.0), deg2rad(25.0), 1.0, 0, 0);
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(10.0), deg2rad(25.0), 1.0, 0, 0);
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(40.0), deg2rad(25.0), 1.0, 0, 0);
              rxxDataBlock.addSignal(ch, C_LAMBDA, ae, deg2rad(0.0),  deg2rad(0.0),  1e-3, 5e-5, 5e-11);
           }
           Nrfi = 3;

        }


        std::cout << "Data source II = self-generated, array, 2 reference antennas, 1 astro and 1..2 RFI signals\n";
        arma::Col<int> Iref = ae.listReferenceAntennas();
        std::cout << "List of detected reference antennas: " << arma::trans(Iref);

        for (int ch=0; ch<DIGESTIF_Nch; ch++) {
           // add astro signal
           refDataBlock_1RFI.addSignal(ch, C_LAMBDA, ae, deg2rad(45.0), deg2rad(-90.0), 1e-2, 1e-7, 0);
           refDataBlock_2RFI.addSignal(ch, C_LAMBDA, ae, deg2rad(45.0), deg2rad(-90.0), 1e-2, 1e-7, 0);

           // add 1st RFI signal, with reference antennas specified
           double Gref = 1e3; // +30dB gain to RFI compared to sky-pointed array elements
           refDataBlock_1RFI.addSignal(ch, C_LAMBDA, ae, deg2rad(30.0), deg2rad(-15.0), 2.0, 0, 0, Gref, Iref);
           refDataBlock_2RFI.addSignal(ch, C_LAMBDA, ae, deg2rad(30.0), deg2rad(-15.0), 2.0, 0, 0, Gref, Iref);

           // add 2nd RFI signal
           refDataBlock_2RFI.addSignal(ch, C_LAMBDA, ae, deg2rad(10.0), deg2rad(-5.0), 2.1, 0, 0, Gref, Iref);
        }


        // Test the test
        if (0) {
           std::cout << "Testing the Timing() class with 5-second wait and 5 elements\n";
           Timing speed(5);
           usleep(5*1e6);
        }


        //////////////////////////////////////////
        // COVARIANCE GENERATION
        /////////////////////////////////////////
     
        if (1) {

           std::cout << "\nTiming performance of channels/second for computing and time-integrating covariance of " << xyz.Nant << "-element signal\n";

           arma::Col<bf::complex> random_signal;

           int Nch = outDataBlock.N_chan();

           Timing speed(Nch*N_ITER*256);
           for (int i=0; i<N_ITER*256; i++) {
              random_signal.randn(xyz.Nant);
              for (int cc=0; cc<Nch; cc++) {
                 outDataBlock.add(cc, random_signal);
              }
           }
        }

        //////////////////////////////////////////
        // DECOMPOSITIONS and RECOMPOSITIONS
        /////////////////////////////////////////

        if (1) {

           double Ncomplex = rxxDataBlock.N_ant() * rxxDataBlock.N_ant();
           double Nelem = rxxDataBlock.N_chan();

           std::cout << "\nNOTE\n"
                     << "Decomposition benchmark 'elements' are covariance matrices with " << Ncomplex << " complex values each.\n"
                     << "Speeds should be interpreted as 'channels/second' for some set of processing steps.\n"
                     << "NOTE\n\n";

           if (1) {
              std::cout << "Timing performance of c'stor Decomposition(&cov) and d'stor of SVD, EVD and QR\n";
              { Timing speed(Nelem); SVDecomposition decSVD(rxxDataBlock); }
              { Timing speed(Nelem); EVDecomposition decEVD(rxxDataBlock); }
              { Timing speed(Nelem); QRDecomposition decQR(rxxDataBlock); }
            }

           if (1) {
              std::cout << "\nTiming performance of { evd(),svd(),qr(); evd^-1(),svd^-1,qr^-1() }, average de&recomposition speed of each:\n";
              EVDecomposition dec_EVD(rxxDataBlock);
              SVDecomposition dec_SVD(rxxDataBlock);
              QRDecomposition dec_QR(rxxDataBlock);
              Timing speed(3 * Nelem*int(1 + N_ITER/10)); // 3 ops (EVD,SVD,QR)
              for (int i=0; i<(1 + N_ITER/10); i++) {
                 dec_EVD.decompose(rxxDataBlock);
                 dec_SVD.decompose(rxxDataBlock);
                 dec_QR.decompose(rxxDataBlock);
                 dec_QR.recompose(outDataBlock);
                 dec_SVD.recompose(outDataBlock);
                 dec_EVD.recompose(outDataBlock);
              }
           }

           if (1) {
              std::cout << "\nTiming performance of SVD decomposition and recomposition\n";
              SVDecomposition dec(rxxDataBlock);
              Timing speed(Nelem*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 dec.decompose(rxxDataBlock);
                 dec.recompose(outDataBlock);
              }
           }

           if (1) {
              std::cout << "\nTiming performance of EVD decomposition and recomposition\n";
              EVDecomposition dec(rxxDataBlock);
              Timing speed(Nelem*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 dec.decompose(rxxDataBlock);
                 dec.recompose(outDataBlock);
              }
           }

           if (1) {
              std::cout << "\nTiming performance of QR decomposition and recomposition\n";
              QRDecomposition dec(rxxDataBlock);
              Timing speed(Nelem*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 dec.decompose(rxxDataBlock);
                 dec.recompose(outDataBlock);
              }
           }
        }

        /////////////////////////////////////////////
        // DECOMPOSITION, NULLING and RECOMPOSITION
        /////////////////////////////////////////////

        if (1) {
           double Nelem = rxxDataBlock.N_chan();

           if (1) {
              std::cout << "\nTiming performance of SVD decomposition, RFI detection and nulling, recomposition\n";
              SVDecomposition dec(rxxDataBlock);
              DecompositionModifier dm(dec);
              Timing speed(Nelem*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 dec.decompose(rxxDataBlock);
                 dm.interfererNulling(Nrfi, true, 0, rxxDataBlock.N_chan()-1);
                 dec.recompose(outDataBlock);
              }
           }

           if (1) {
              std::cout << "\nTiming performance of EVD decomposition, RFI detection and nulling, recomposition\n";
              EVDecomposition dec(rxxDataBlock);
              DecompositionModifier dm(dec);
              Timing speed(Nelem*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 dec.decompose(rxxDataBlock);
                 dm.interfererNulling(Nrfi, true, 0, rxxDataBlock.N_chan()-1);
                 dec.recompose(outDataBlock);
              }
           }

           if (1) {
              // ...
              // QRDecomposition: not yet supported by DecompositionModifier.interfererNulling()
           }
        }

        ///////////////////////////////////////////////////////
        // REFERENCE ANTENNA METHODS
        //////////////////////////////////////////////////////

        if (1) {

           std::cout << "\nTiming performance of Covariance copying and Briggs CovarianceModofier::templateSubtraction() with 2 ant, 1 RFI\n";

           double Nelem = refDataBlock_1RFI.N_chan();
           arma::Col<int> Iref = ae.listReferenceAntennas();
           int Nrfi_ref = 1;

           Timing speed(Nelem*N_ITER);
           for (int i=0; i<N_ITER; i++) {
              Covariance temp_copy(refDataBlock_1RFI);
              CovarianceModifier cm(temp_copy);
              if (cm.templateSubtraction(Iref, Nrfi_ref) < 0) { 
                 std::cout << "Some failure in templateSubtraction(), iteration=" << i << ", cancelling!\n";
                 break; 
              }
           }
        }

        if (1) {

           std::cout << "\nTiming performance of Covariance copying and Generic CovarianceModofier::templateSubtraction() with 2 ant, 2 RFI\n";

           double Nelem = refDataBlock_2RFI.N_chan();
           arma::Col<int> Iref = ae.listReferenceAntennas();
           int Nrfi_ref = 2;

           Timing speed(Nelem*N_ITER);
           for (int i=0; i<N_ITER; i++) {
              Covariance temp_copy(refDataBlock_2RFI);
              CovarianceModifier cm(temp_copy);
              if (cm.templateSubtraction(Iref, Nrfi_ref) < 0) { 
                 std::cout << "Some failure in templateSubtraction(), iteration=" << i << ", cancelling!\n";
                 break; 
              }
           }
        }


        ///////////////////////////////////////////////////////
        // BEAMFORMING
        //////////////////////////////////////////////////////

        if (1) {
           Beams_t beams;
           BeamformerWeights bw;
           const int Nbeams = 64;

           std::cout << "\nTiming beams/sec performance of " << Nbeams << "-beam adaptive beamforming weights\n";

           beams.init(Nbeams, rxxDataBlock.N_ant(), rxxDataBlock.N_chan());
           for (int bb=0; bb<Nbeams; bb++) {
              beams.phi(bb) = 0.0f + bb;  beams.theta(0) = 0.0f - bb;
           }
           for (int cc=0; cc<rxxDataBlock.N_chan(); cc++) {
              beams.freqs(cc) = rxxDataBlock.channel_freq(0) + cc*16384.0f;
           }

           bw.generateSteerings(beams, ae);
 
           if (1) {
              std::cout << "Classic beamformer, b=0.0f\n";
              Timing speed(Nbeams*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 bw.generateMVDR(beams, rxxDataBlock, 0.0f);
              }
           }

           if (1) {
              std::cout << "MVDR beamformer, b=1.0f\n";
              Timing speed(Nbeams*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 bw.generateMVDR(beams, rxxDataBlock, 1.0f);
              }
           }

           if (1) {
              std::cout << "RB-MVDR beamformer, b=1.0f+1e-4\n";
              Timing speed(Nbeams*N_ITER);
              for (int i=0; i<N_ITER; i++) {
                 bw.generateMVDR(beams, rxxDataBlock, 1.0f+1e-4);
              }
           }

        }

	return 0;
}
