/*****************************************************************************
*    <DataSim: VLBI data simulator>                                          * 
*    Copyright (C) <2015> <Zheng Meyer-Zhao>                                 *
*                                                                            *
*    This file is part of DataSim.                                           *
                                                                             *
*    DataSim is free software: you can redistribute it and/or modify         *
*    it under the terms of the GNU General Public License as published by    *
*    the Free Software Foundation, either version 3 of the License, or       *
*    (at your option) any later version.                                     *
*                                                                            *
*    DataSim is distributed in the hope that it will be useful,              *
*    but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*    GNU General Public License for more details.                            *
*                                                                            *
*    You should have received a copy of the GNU General Public License       *
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.   *
*****************************************************************************/

#ifndef __Subband_H__
#define __Subband_H__

#include <string>
#include <fstream>
#include <stdint.h>
#include "architecture.h"
#include "model.h"

class Subband{
  public:
    // constructor and destructor
    Subband(size_t const &startIdx, size_t const &blksize, size_t const &length, size_t const &antIdx, unsigned int const &antSEFD, size_t const &sbIdx,
          size_t const &vpbytes, size_t const &vpsamps, f64* const &delaycoeffs, float const &bandwidth,
          std::string const &antname, int const &mjd, int const &seconds, float const &freq, size_t const &verbose);
    ~Subband();

    // access methods
    inline size_t const getstartIdx() const { return d_startIdx; }
    inline size_t const getblksize() const { return d_blksize; }
    inline size_t const getlength() const { return d_length; }
    inline size_t const getantIdx() const { return d_antIdx; }
    inline size_t const getsbIdx() const { return d_sbIdx; }
    inline size_t const getvpbytes() const { return d_vpbytes; }
    inline size_t const getvpsamps() const { return d_vpsamps; }
    inline double const getstarttime() const { return d_starttime; }
    inline float const getbandwidth() const { return d_bandwidth; }
    inline std::string const getantname() const { return d_antname; }
    inline size_t const getcptr() const { return d_cptr; }
    inline size_t const getprocptr() const { return d_procptr; }
    inline std::string const getfilename() const { return d_filename; }
    inline uint8_t* const getvdifbuf() const {return d_vdifbuf; }
    inline size_t const getnearestsample() const {return d_nearestsample; }
    inline double const getfracsamperror() const {return d_fracsamperror; } 

    inline void setcptr(size_t cptr) {d_cptr = cptr; }
    inline void setprocptr(size_t procptr) { d_procptr = procptr; }

    /* select data from common signal
     * add station noise
     * apply Ormsby filter
     * inverse DFT/FFT
     * copy time domain data to arr 
     */
    void fabricatedata(Ipp32fc* commFreqSig, gsl_rng *rng_inst, float sfluxdensity);
    /*
     * move data from the second half of the array to the first half
     */
    void movedata();
    /*
     * Fill in the process buffer
     */ 
    void fillprocbuffer();
    /*
     * Process data in the proc buffer
     * DFT
     * apply fractional sample correction
     * apply looffset correction
     * inverse DFT
     * apply fringe rotation
     * complex to real conversion
     */
    void processdata();
 
    /*
     * update package counter
     * update nearest sample
     * update fractional sample error
     */
    void updatevalues(Model* model);

    /*
     * Quantization
     */
    void quantize();
    /*
     * Pack the processed data to VDIF packet
     */
    void writetovdif();

    /*
     * Close the output vdif stream
     */ 
    void closevdif();
 
  private:
    size_t d_startIdx;        // start index to copy from common signal
    size_t d_blksize;         // block size to copy from startIdx
    size_t d_length;          // size of the subband array for 2*TDUR time
    size_t d_antIdx;          // antenna index the subband belongs to
    size_t d_antSEFD;         // antenna SEFD in Jansky
    size_t d_sbIdx;           // subband index
    size_t d_vpbytes;         // number of bytes in a vdif packet, including VDIF header
    size_t d_vpsamps;          // number of samples in a vdif packet
    double d_starttime;       // delay offset in microsecond
    float d_bandwidth;        // bandwidth of the subband
    std::string d_antname;    // name of the antenna the subband belongs to
    int d_mjd;                // start MJD
    int d_seconds;            // start seconds
    float d_freq;             // start frequency of the subband
    size_t d_verbose;         // verbose status

    size_t d_cptr;            // current pointer position
    size_t d_procptr;         // process pointer in d_arr in terms of number of samples
    std::string d_filename;   // vdif filename
    std::ofstream d_vdiffile; // vdif output stream to write to

    double d_sampletime;      // complex sampling, sample time is 1/bw
    double d_vptime;          // vdif packet time window
    int d_nearestsample;      // nearest sample w.r.t. delay offset
    double d_fracsamperror;   // fractional sample error of the nearest sample
    size_t d_pkcounter;       // vdif packet counter
    int d_shift;              // number of sample shift due to updating the fractional sample error

    uint8_t* d_vdifbuf;       // vdif packet buffer
    f64* d_delaycoeffs;       // delay coefficients
    Ipp32fc* d_fracsamperrbuf;// fractional sample error buffer
    Ipp32fc* d_fringerotbuf;  // fringe rotation buffer

    Ipp32fc* d_arr;           // array of time domain signal with size of d_length
    Ipp32fc* d_temp;          // temporary array of frequency domain signal with size of d_blksize
    Ipp32fc* d_tempt;         // temporary array of time domain signal with size of d_blksize

    Ipp32fc* d_procbuffer;    // process buffer with size N, where N is the number of complex samples of a vdif packet    
    Ipp32fc* d_procbuffreq;   // process buffer with size N in frequency domain
    Ipp32fc* d_procbufferrot; // process buffer with size N, where N is the number of complex samples of a vdif packet, after fringe rotation 
    Ipp32fc* d_procbuffreqcorr;// process buffer with size N in frequency domain after fractional sample error correction
    Ipp32fc* d_buffreqtemp;   // temporary frequency domain signal array with size 2N, which possesses Hermitian property    
    Ipp32fc* d_realC;         // complex signal array with size 2N, where imaginary part is 0    
    float* d_real;            // real signal array with size of 2N

    int d_bufsigsize;         // size of temporary signal DFT buffer
    int d_bufprocsize;        // size of process DFT buffer
    int d_bufCToRsize;        // sise of complex_to_real DFT buffer
    u8* d_bufsig;             // temporary signal DFT buffer
    u8* d_bufproc;            // process DFT buffer
    u8* d_bufCToR;            // complex_to_real DFT buffer
    vecDFTSpecC_cf32* d_pDFTSpecCsig;   // pointer to DFT specification structure for signal
    vecDFTSpecC_cf32* d_pDFTSpecCproc;  // pointer to DFT specification structure for process data
    vecDFTSpecC_cf32* d_pDFTSpecCCToR;  // pointer to DFT specification structure for complex_to_real

    size_t d_sampcount;       // sample count for quantization
    float d_tmul;             // threshhold multiplier for quantization
    float d_square;           // accumulated value of sample*sample

    // support functions for fabricatedata()
    void copyToTemp(Ipp32fc* commFreqSig);
    void mulsfluxdensity(float sfluxdensity);
    void addstationnoise(gsl_rng *rng_inst);
    void normalizesignal(float sfluxdensity);
    void applyfilter();
    void copyToArr();

    // support functions for processdata()
    void fillBuffreqtemp();
    void complex_to_real();
    void applyfracsamperrcorrection();
    void applyfringerotation();
    double fraction_of(double val);

    // DFT and inverseDFT
    void DFT(Ipp32fc* pSrc, Ipp32fc* pDst, vecDFTSpecC_cf32* pDFTSpecC, u8* buf);
    void inverseDFT(Ipp32fc* pSrc, Ipp32fc* pDst, vecDFTSpecC_cf32* pDFTSpecC, u8* buf);
};

#endif /* __Subband_H__ */

/*
 * eof
 */
