/********************************************************************************************************
 * @file PCal.cpp
 * Multi-tone Phase Cal Extraction
 *
 * @brief Extracts and integrates multi-tone phase calibration signal information from an input signal.
 *
 * The extractor factory chooses one out of three currently available methods for signal extraction.
 * The choice depends on the parameters which are:
 *   spacing:   frequency step between one tone and the next
 *   offset:    frequency offset between the first tone and the baseband signal start at 0 Hz
 *              note that the first tone can be at DC; for real-valued signals that tone is discarded
 *   data type: real-valued or complex-valued signal
 *   band type: single sideband with LO at an edge, or double sideband with LO at the center
 *   sideband:  only used for complex signals
 *
 * Spacing is assumed to be constant throughout the band.
 * A tone spacing is "integer" if it divides the sampling rate evenly
 * with a "small" value for the quotient.
 *
 * Basic extractor:
 *   When tone spacing is "integer" and offset is zero, i.e. tones start
 *   from DC and are at multiples of the spacing, then the extraction of
 *   amplitude and phase information is quite easy.
 *
 *   The input sample sequence can be segmented into N-length pieces according to
 *   the period (in N samples) of the lowest tone frequency. A r2c FFT of the time
 *   integrated segments gives the amplitude and phase of every tone. The 0th bin
 *   contains a tone but carries no phase information and is discarded.
 *
 * Shifting extractor:
 *   When the offset is non-zero, the signal can be counter-rotated with a
 *   precomputed complex sine to restore the offset back to zero. The basic
 *   extractor is then applied.
 *   After time integration a c2c FFT gives the amplitude and phase of every tone.
 *   In this case the 0th bin contains meaningful phase info of the first shifted tone.
 *
 * Implicit shifting extractor:
 *   Another method when the offset frequency is non-zero can be used.
 *   The offset frequency should have a period M=gcd(fs,foffset) which is "small"
 *   and which divides the sampling rate evenly. The signal is then split into M-sized
 *   segments for time integration. A final r2c FFT returns an extended spectrum.
 *   Only certain bins of this spectrum contain the tones.
 *   These bins are gathered together for the final output values.
 *
 * All extractors return amplitude and phase of the tones.
 *
 * The output data could also be analyzed in the time domain where the relative,
 * average instrumental delay can be found directly by estimating the position
 * of the peak in the time-domain data.
 *
 * @author   Jan Wagner
 * @author   Sergei Pogrebenko
 * @author   Walter Brisken
 * @version  1.1/2009
 * @license  GNU GPL v3
 *
 * Changelog:
 *   29jun2020 - extended the support for extraction from complex samples
 *   18Mar2014 - added support for extraction from complex samples
 *   27Mar2012 - better count of tones in band, added corner cases like no tones in band, zero spacing
 *   05Oct2009 - added support for arbitrary input segment lengths
 *   08Oct2009 - added Briskens rotationless method
 *   02Nov2009 - added sub-subintegration sample offset, DFT for f-d results, tone bin copying to user buf
 *   03Nov2009 - added unit test, included DFT in extractAndIntegrate_reference(), fix rotation direction
 *
 ********************************************************************************************************/

#include "architecture.h"
#ifndef UNIT_TEST
#include "alert.h"
#endif
#include "mathutil.h"
#include "pcal.h"
#include "pcal_impl.h"
#include <iostream>
#include <cmath>
using std::cerr;
using std::endl;

using namespace std;

#ifdef UNIT_TEST
    // remove dependency on Alert.cpp
    #define csevere std::cout
    #define cerror  std::cout
    #define cwarn   std::cout
    #define cdebug  std::cout
    const char startl[] = "";
    // #define VERBOSE_UNIT_TEST
#endif

#ifndef PCAL_DEBUG
    #define PCAL_DEBUG 0
#else
    #undef PCAL_DEBUG
    #define PCAL_DEBUG 1
#endif

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// IMPL class: implementation specific storage and variables
/////////////////////////////////////////////////////////////////////////////////////////////////////////

class pcal_config_pimpl {
  public:
    pcal_config_pimpl()  {};
    ~pcal_config_pimpl() {};
  public:
    double   dphi;
    cf32*    rotator;        // pre-cooked oscillator values
    cf32*    rotated;        // temporary
    cf32*    pcal_complex;   // temporary unassembled output, later final output
    f32*     pcal_real;      // temporary unassembled output for the pcaloffsethz==0.0f case
    size_t   rotatorlen;
    size_t   pcal_index;     // zero, changes when extract() is called at least once with "leftover" samples
    size_t   rotator_index;  // zero, changes when extract() is called at least once with "leftover" samples
  public:
    vecDFTSpecC_cf32* dftspec;
    u8* dftworkbuf;
    cf32*  dft_out; // unnecessary once Intel implements inplace DFTFwd_CtoC_32fc_I (counterpart of inplace DFTFwd_RtoC)
};

double PCal::_min_freq_resolution_hz = 1e9;

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// BASE CLASS: Factory Method
/////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Factory that returns a new PCal extractor object. The optimal implementation
 * is selected based on the input parameters.
 * @param bandwidth_hz     Bandwidth of the input signal in Hertz
 * @param pcal_spacing_hz  Spacing of the PCal signal, comb spacing, typically 1e6 Hertz.
 * @param pcal_offset_hz   Offset of the first PCal signal from 0Hz/DC, typically 10e3 Hertz
 * @param sampleoffset     Offset of the first sample as referenced to start of subintegration interval
 * @param data_type        COMPLEX or REAL
 * @param band_type        SINGLE (single sideband) or DOUBLE (dual sideband)
 * @return new PCal extractor class instance
 */
PCal* PCal::getNew(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz,
                   const size_t sampleoffset, Configuration::datasampling data_type, Configuration::complextype band_type)
{
    int No, Np, Nt;
    double fs;

    fs = (data_type == Configuration::COMPLEX) ? bandwidth_hz : 2.0 * bandwidth_hz;

    if (pcal_offset_hz == -1)    // Test for flag value indicating no real tones
    {
        No = Np = Nt = 0;
    }
    else
    {
        // Compute the repetition periods and number of tones in the band
        No = (int)(fs / gcd(fs, (double)pcal_offset_hz));
        Np = (int)(fs / gcd(fs, pcal_spacing_hz));
        Nt = calcNumTones(bandwidth_hz, (double)pcal_offset_hz, pcal_spacing_hz);

        // Consider user-specified minimum spectral resolution
        while (fs/(2*No) > PCal::_min_freq_resolution_hz)
            No *= 2;
    }

    if (PCAL_DEBUG)
        cdebug << startl << "PCal Factory: "
               << "bw " << bandwidth_hz << ", spacing " << pcal_spacing_hz << ", "
               << "offset " << pcal_offset_hz << ", "
               << Nt << " tones fit including band edges"
               << endl;

    // If no tones: placeholder class
    if (Nt == 0)
        return new PCalExtractorDummy(bandwidth_hz, pcal_spacing_hz, pcal_offset_hz, sampleoffset);

    // Currently only one extraction method implemented for complex data
    if (data_type == Configuration::COMPLEX) {
        if ((No % Np) == 0)
            return new PCalExtractorComplexImplicitShift(bandwidth_hz, pcal_spacing_hz, pcal_offset_hz, band_type);
        else
            return new PCalExtractorComplex(bandwidth_hz, pcal_spacing_hz, pcal_offset_hz, band_type);
    }

    // First tone in DC bin: smallest footprint extractor
    if (pcal_offset_hz == 0.0f) {
        //cwarn << startl << "Warning: non-standard pcal mode (0 offset)" << endl;
        return new PCalExtractorTrivial(bandwidth_hz, (int)pcal_spacing_hz, sampleoffset);
    }

    // First tone in higher frequency bin: moderate footprint extractor
    if ((No % Np) == 0 /* && (!want_timedomain_delay) */) {
        return new PCalExtractorImplicitShift(bandwidth_hz, pcal_spacing_hz, pcal_offset_hz, sampleoffset);
    }

    // Complicated setup: need spectral shift to reduce FFT length to manageable size
    return new PCalExtractorShifting(bandwidth_hz, pcal_spacing_hz, pcal_offset_hz, sampleoffset);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// BASE CLASS: Static helper funcs
/////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Return number of tones that fit the band, including any
 * tones that fall onto DC or the upper band edge.
 */
int PCal::calcNumTones(double bw, double offset, double step)
{
    int n = 0;
    if (bw <= 0.0f || step <= 0.0f) { return 0; }

    // loop is better than "n = 1 + floor((bw - offset) / step); n=n-1 if offset==0;"
    double ftone = offset;
    while (ftone <= bw) {
        n++;
        ftone += step;
        if (step <= 0.0f) { break; }
    }
    // count a tone at both bottom and top edge of band as one
    if (offset == 0.0 && ftone - step == bw)
        n--;
    return n;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// BASE CLASS: c'stor
/////////////////////////////////////////////////////////////////////////////////////////////////////////

PCal::PCal()
{
    // defaults, in case base class is created via "PCal pcobj;"
    _samplecount = 0;
    _fs_hz = 0.0f;
    _pcaloffset_hz = 0;
    _pcalspacing_hz = 0.0f;
    _N_bins = 0;
    _N_tones = 0;
    _finalized = false;
    _cfg = NULL;
    _estimatedbytes = 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// BASE CLASS: reference extractor, very slow but should be accurate
/////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Processes samples and accumulates the detected phase calibration tone vector.
 * Computation uses the slowest thinkable direct method. This function is
 * intended for testing and comparison only!
 * @param  data          pointer to input sample vector
 * @param  len           length of input vector
 * @param  out           output array of sufficient size to store extracted PCal
 */
bool PCal::extractAndIntegrate_reference(f32 const* data, const size_t len, cf32* out, const uint64_t sampleoffset)
{
    size_t Nbins = 2*_N_tones;
    size_t maxtoneperiod = (size_t) (_fs_hz/gcd(_pcaloffset_hz,(long int)(_fs_hz)));
    double dphi = 2*M_PI * (-_pcaloffset_hz/_fs_hz);
    vecStatus s;
    cf32 pcalout[Nbins];
    s = vectorZero_cf32(pcalout, Nbins);
    if (s != vecNoErr)
        csevere << startl <<"Error in vectorZero in PCal::extractAndIntegrate_reference " << vectorGetStatusString(s) << endl;

    for (size_t n=0; n<len; n++) {
        size_t no = n + sampleoffset;
        int bin = (no % Nbins);
        double phi = dphi * ((_samplecount + no) % maxtoneperiod);
        pcalout[bin].re += cos(phi) * data[n];
        pcalout[bin].im += sin(phi) * data[n];
    }
    _samplecount += len;

    if (PCAL_DEBUG)
        cdebug << startl << "PCal::extractAndIntegrate_reference Ntones=" << _N_tones << " Nbins=" << Nbins
               << " toneperiod=" << maxtoneperiod << endl;

    int wbufsize = 0;
    vecDFTSpecC_cf32* dftspec;
    //s = vectorInitDFTC_cf32(&dftspec, Nbins, vecFFT_NoReNorm, vecAlgHintAccurate);
    //if (s != vecNoErr) 
    //    csevere << startl <<"Error in DFTInitAlloc in PCal::extractAndIntegrate_reference " << vectorGetStatusString(s) << endl;
    //    s = vectorGetDFTBufSizeC_cf32(dftspec, &wbufsize);
    //if (s != vecNoErr)
    //    csevere << startl << "Error in DFTGetBufSize in PCal::extractAndIntegrate_reference " << vectorGetStatusString(s) << endl;
    //u8* dftworkbuf = vectorAlloc_u8(wbufsize);

    // Alloc DFT buffers
    //int sizeDFTSpec, sizeDFTInitBuf;
    //Ipp8u *dftInitBuf, *dftworkbuf;
    //ippsDFTGetSize_C_32fc(Nbins, IPP_FFT_NODIV_BY_ANY, ippAlgHintAccurate, 
    //			  &sizeDFTSpec, &sizeDFTInitBuf, &wbufsize);
    //dftspec = (IppsDFTSpec_C_32fc*)ippsMalloc_8u(sizeDFTSpec);
    //dftInitBuf = ippsMalloc_8u(sizeDFTInitBuf);
    //dftworkbuf = ippsMalloc_8u(wbufsize);

    // Initialize DFT

    //ippsDFTInit_C_32fc(Nbins, IPP_FFT_NODIV_BY_ANY, ippAlgHintAccurate, dftspec, dftInitBuf);
    //if (dftInitBuf) ippFree(dftInitBuf);

    u8* dftworkbuf;
    s = vectorInitDFTC_cf32(&dftspec, Nbins, vecFFT_NoReNorm, vecAlgHintAccurate, &wbufsize, &dftworkbuf);
    if (s != vecNoErr)
      csevere << startl <<"Error in DFTInitAlloc in PCal::extractAndIntegrate_reference " << vectorGetStatusString(s) << endl;

    cf32 dftout[Nbins];
    s = vectorDFT_CtoC_cf32(pcalout, dftout, dftspec, dftworkbuf);
    if (s != vecNoErr)
        csevere << startl << "Error in DFTFwd in PCal::extractAndIntegrate_reference " << vectorGetStatusString(s) << endl;

    if (_pcaloffset_hz == 0) {
        // The "DC" bin has no phase information, discard
        for (int i=0; i<_N_tones; i++) {
            out[i] = dftout[i+1];
        }
    } else {
        // Spectrum was shifted, DC bin has phase information, keep
        for (int i=0; i<_N_tones; i++) {
            out[i] = dftout[i+0];
        }
    }

    vectorFree(dftspec);
    if (dftworkbuf) vectorFree(dftworkbuf);
    return true;
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of Phase Cal signals with first tone at 0 Hertz
/////////////////////////////////////////////////////////////////////////////////////////////////////////

PCalExtractorTrivial::PCalExtractorTrivial(double bandwidth_hz, double pcal_spacing_hz, const size_t sampleoffset)
{
    vecStatus s;
    pcal_spacing_hz = std::abs(pcal_spacing_hz);

    /* Derive config */
    _fs_hz          = 2*bandwidth_hz;
    _pcaloffset_hz  = 0;
    _pcalspacing_hz = pcal_spacing_hz;
    _N_bins         = (int)(_fs_hz / gcd(_fs_hz, pcal_spacing_hz));
    _N_tones        = calcNumTones(bandwidth_hz, 0.0f, pcal_spacing_hz) - 1; // -1 is to exclude tone at 0 Hertz
    _tone_step      = (int)(pcal_spacing_hz / gcd(_fs_hz, pcal_spacing_hz));
    _cfg = new pcal_config_pimpl();
    _estimatedbytes = 0;

    /* Prep for FFT/DFT */
    int wbufsize = 0;
    s = vectorInitDFTC_cf32(&_cfg->dftspec, _N_bins, vecFFT_NoReNorm, vecAlgHintAccurate, &wbufsize, &_cfg->dftworkbuf);
    if (s != vecNoErr)
        csevere << startl << "Error in DFTInitAlloc in PCalExtractorTrivial::PCalExtractorTrivial " << vectorGetStatusString(s) << endl;
    _estimatedbytes += wbufsize;

    /* Allocate twice the amount strictly necessary */
    _cfg->pcal_complex = vectorAlloc_cf32(_N_bins * 2);
    _cfg->pcal_real    = vectorAlloc_f32(_N_bins * 2);
    _cfg->dft_out      = vectorAlloc_cf32(_N_bins * 1);
    _estimatedbytes   += _N_bins*4*(4+2+2);
    this->clear();

    if (PCAL_DEBUG)
        cdebug << startl << "PCalExtractorTrivial: _Ntones=" << _N_tones << ", _N_bins=" << _N_bins << ", wbufsize=" << wbufsize << endl;
}

PCalExtractorTrivial::~PCalExtractorTrivial()
{
    vectorFree(_cfg->pcal_complex);
    vectorFree(_cfg->pcal_real);
    vectorFreeDFTC_cf32(_cfg->dftspec);
    vectorFree(_cfg->dftworkbuf);
    vectorFree(_cfg->dft_out);
    delete _cfg;
}

/**
 * Clear the extracted and accumulated PCal data by setting it to zero.
 */
void PCalExtractorTrivial::clear()
{
    _samplecount = 0;
    _finalized   = false;
    vectorZero_cf32(_cfg->pcal_complex, _N_bins * 2);
    vectorZero_f32 (_cfg->pcal_real,    _N_bins * 2);
}

/**
 * Adjust the sample offset.
 *
 * The extractor needs a time-continuous input sample stream. Samples
 * are numbered 0...N according to their offset from the start of the
 * stream. The stream can be split into smaller chunks that
 * are added individually through several extractAndIntegrate() calls.
 *
 * If for some reason two chunks are not continuous in time,
 * some internal indices need to be corrected by calling this
 * function and specifying at what sample number the next
 * chunk passed to extractAndIntegrate() starts.
 *
 * @param sampleoffset sample offset of chunk passed to next extractAndIntegrate() call
 */
void PCalExtractorTrivial::adjustSampleOffset(const size_t sampleoffset)
{
    _cfg->rotator_index = 0; // unused
    _cfg->pcal_index = (sampleoffset) % _N_bins;
}

/**
 * Process a new chunk of time-continuous single channel data.
 * Time-integrates the data into the internal result buffer.
 *
 * If this function is called several times to integrate additional data
 * and these multiple pieces of data are not continuous in time,
 * please see adjustSampleOffset().
 *
 * @param samples Chunk of the input signal consisting of 'float' samples
 * @param len     Length of the input signal chunk
 * @return true on success, false if results were frozen by calling getFinalPCal()
 */
bool PCalExtractorTrivial::extractAndIntegrate(f32 const* samples, const size_t len)
{
    if (_finalized) { return false; }

    f32 const* src = samples;
    f32* dst = &(_cfg->pcal_real[_cfg->pcal_index]);
    size_t tail = (len % _N_bins);
    size_t end  = len - tail;

    /* Process the first part that fits perfectly */
    for (size_t n = 0; n < end; n+=_N_bins, src+=_N_bins) {
        vectorAdd_f32_I(src, dst, _N_bins);
    }

    /* Handle any samples that didn't fit */
    if (tail != 0) {
        vectorAdd_f32_I(src, dst, tail);
        _cfg->pcal_index = (_cfg->pcal_index + tail) % _N_bins;
    }

    /* Done! */
    _samplecount += len;
    return true;
}

/**
 * Computes the final extraction result. No more sample data can be added.
 * The PCal extraction results are copied into the specified output array.
 *
 * @param out Pointer to user PCal array with getLength() values
 * @return number of samples that were integrated for the result
 */
uint64_t PCalExtractorTrivial::getFinalPCal(cf32* out)
{
    vecStatus s;
    invariant();

    if (!_finalized) 
    {
        _finalized = true;
        vectorAdd_f32_I(/*src*/&(_cfg->pcal_real[_N_bins]), /*srcdst*/&(_cfg->pcal_real[0]), _N_bins);
        vectorRealToComplex_f32(/*srcRe*/_cfg->pcal_real, /*srcIm*/NULL, _cfg->pcal_complex, _N_bins);
        s = vectorDFT_CtoC_cf32(/*src*/_cfg->pcal_complex, _cfg->dft_out, _cfg->dftspec, _cfg->dftworkbuf);
        if (s != vecNoErr)
            csevere << startl << "Error in DFTFwd PCalExtractorTrivial::getFinalPCal " << vectorGetStatusString(s) << endl;
    }

    // Show all bin phases
    #ifdef VERBOSE_UNIT_TEST
    cout << "PCalExtractorTrivial::getFinalPCal phases are: ";
    for (int i = 0; i < _N_bins; i++)
    {
        f32 phi = (180/M_PI)*std::atan2(_cfg->dft_out[i].im, _cfg->dft_out[i].re);
        cout << "p[" << i << "]=" << phi << " ";
    }
    cout << "deg\n";
    #endif

    // Copy only the tone bins; also discard the DC bin that has zero phase,
    // but keep highest tone even if it is at the band edge (Nyquist, zero phase)
    for (int i = 0; i < _N_tones; i++)
    {
        out[i] = _cfg->dft_out[(i+1)*_tone_step];
    }

    return _samplecount;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal signals with non-zero offset via spectral shift
/////////////////////////////////////////////////////////////////////////////////////////////////////////

PCalExtractorShifting::PCalExtractorShifting(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, const size_t sampleoffset)
{
    pcal_spacing_hz = std::abs(pcal_spacing_hz);

    /* Derive config */
    _fs_hz           = 2 * bandwidth_hz;
    _pcaloffset_hz   = pcal_offset_hz;
    _pcalspacing_hz  = pcal_spacing_hz;
    _N_bins          = (int)(_fs_hz / gcd(_fs_hz, pcal_spacing_hz));
    _N_tones         = calcNumTones(bandwidth_hz, pcal_offset_hz, pcal_spacing_hz);
    _estimatedbytes  = 0;

    _cfg = new pcal_config_pimpl();
    _cfg->rotatorlen = (size_t)(_fs_hz / gcd(_fs_hz, _pcaloffset_hz));
    _cfg->rotatorlen = _N_bins * _cfg->rotatorlen / gcd ((long int)(_cfg->rotatorlen), (long int)(_N_bins));

    /* Prep for FFT/DFT */
    int wbufsize = 0;
    vecStatus s;
    s = vectorInitDFTC_cf32(&_cfg->dftspec, _N_bins, vecFFT_NoReNorm, vecAlgHintAccurate, &wbufsize, &_cfg->dftworkbuf);
    if (s != vecNoErr)
        csevere << startl 
		<< "Error in DFTInitAlloc in PCalExtractorShifting::PCalExtractorShifting.  _N_bins="
		<< _N_bins << " error " << vectorGetStatusString(s) << endl;
    _estimatedbytes += wbufsize;

    /* Allocate twice the amount strictly necessary */
    _cfg->pcal_complex = vectorAlloc_cf32(_N_bins * 2);
    _cfg->pcal_real    = vectorAlloc_f32(_N_bins * 2);
    _cfg->rotator = vectorAlloc_cf32(_cfg->rotatorlen * 2);
    _cfg->rotated = vectorAlloc_cf32(_cfg->rotatorlen * 2);
    _cfg->dft_out = vectorAlloc_cf32(_N_bins * 1);
    _estimatedbytes += _N_bins*4*(4+2+2);
    _estimatedbytes += _cfg->rotatorlen*2*8*2;
    this->clear();

    /* Prepare frequency shifter/mixer lookup */
    _cfg->dphi = 2*M_PI * (-_pcaloffset_hz/_fs_hz);
    for (size_t n = 0; n < (2 * _cfg->rotatorlen); n++) {
        double arg = _cfg->dphi * double(n);
        _cfg->rotator[n].re = f32(cos(arg));
        _cfg->rotator[n].im = f32(sin(arg));
    }

    if (PCAL_DEBUG)
        cdebug << startl << "PcalExtractorShifting: _Ntones=" << _N_tones << ", _N_bins=" << _N_bins << ", wbufsize=" << wbufsize  << ", rotatorlen=" << _cfg->rotatorlen << endl;
}

PCalExtractorShifting::~PCalExtractorShifting()
{
    vectorFree(_cfg->pcal_complex);
    vectorFree(_cfg->pcal_real);
    vectorFree(_cfg->rotator);
    vectorFree(_cfg->rotated);
    vectorFreeDFTC_cf32(_cfg->dftspec);
    vectorFree(_cfg->dftworkbuf);
    vectorFree(_cfg->dft_out);
    delete _cfg;
}

/**
 * Clear the extracted and accumulated PCal data by setting it to zero.
 */
void PCalExtractorShifting::clear()
{
    _samplecount = 0;
    _finalized   = false;
    vectorZero_cf32(_cfg->pcal_complex, _N_bins * 2);
    vectorZero_f32 (_cfg->pcal_real,    _N_bins * 2);
    vectorZero_cf32(_cfg->rotated,      _cfg->rotatorlen * 2);
}

/**
 * Adjust the sample offset.
 *
 * The extractor needs a time-continuous input sample stream. Samples
 * are numbered 0...N according to their offset from the start of the
 * stream. The stream can be split into smaller chunks that
 * are added individually through several extractAndIntegrate() calls.
 *
 * If for some reason two chunks are not continuous in time,
 * some internal indices need to be corrected by calling this
 * function and specifying at what sample number the next
 * chunk passed to extractAndIntegrate() starts.
 *
 * @param sampleoffset sample offset of chunk passed to next extractAndIntegrate() call
 */
void PCalExtractorShifting::adjustSampleOffset(const size_t sampleoffset)
{
    _cfg->rotator_index = (sampleoffset)% _cfg->rotatorlen;
    _cfg->pcal_index    = (sampleoffset)% _N_bins;
}

/**
 * Process a new chunk of time-continuous single channel data.
 * Time-integrates the data into the internal result buffer.
 *
 * If this function is called several times to integrate additional data
 * and these multiple pieces of data are not continuous in time,
 * please see adjustSampleOffset().
 *
 * @param samples Chunk of the input signal consisting of 'float' samples
 * @param len     Length of the input signal chunk
 * @return true on success, false if results were frozen by calling getFinalPCal()
 */
bool PCalExtractorShifting::extractAndIntegrate(f32 const* samples, const size_t len)
{
    if (_finalized) { return false; }

    f32 const* src = samples;
    cf32* dst = &(_cfg->pcal_complex[_cfg->pcal_index]);
    size_t tail  = (len % _cfg->rotatorlen);
    size_t end   = len - tail;

    /* This method is only marginally different from the PCalExtractorTrivial method.
     * Because now our multi-tone PCal signal tones do not reside at integer MHz frequencies,
     * or rather, not at integer multiples of the tone spacing of the comb, the first PCal
     * tone is found at some offset '_pcaloffset_hz' away from 0Hz/DC.
     * So we just use a complex oscillator to shift the input signal back into place.
     * The complex oscillator has a period of _fs_hz/gcd(_fs_hz,_pcaloffset_hz).
     * The period is usually very short, say, 1600 samples. We precomputed those
     * in the constructor and use them here.
     * If the values _fs_hz and _pcaloffset_hz are co-prime, a precomputed sin/cos vector
     * would need a lot of memory. At that point a live sin/cos computation can be
     * faster than the memory access.
     */

    /* Process the first part that fits perfectly (and note: rotatorlen modulo _N_bins is 0!) */
    for (size_t n = 0; n < end; n+=_cfg->rotatorlen, src+=_cfg->rotatorlen) {
        vectorMul_f32cf32(/*A*/src,
                        /*B*/&(_cfg->rotator[_cfg->rotator_index]),
                        /*dst*/&(_cfg->rotated[_cfg->rotator_index]),
                        /*len*/_cfg->rotatorlen);
        cf32* pulse = &(_cfg->rotated[_cfg->rotator_index]);
        for (size_t p = 0; p < (_cfg->rotatorlen/_N_bins); p++) {
            vectorAdd_cf32_I(/*src*/pulse, /*srcdst*/dst, _N_bins);
            pulse += _N_bins;
        }
    }

    /* Handle any samples that didn't fit */
    if (tail != 0) {
        vectorMul_f32cf32(
            /*A*/src,
            /*B*/&(_cfg->rotator[_cfg->rotator_index]),
            /*dst*/&(_cfg->rotated[_cfg->rotator_index]),
            /*len*/tail);
        cf32* pulse = &(_cfg->rotated[_cfg->rotator_index]);
        size_t tail2 = (tail % _N_bins);
        size_t end2  = tail - tail2;
        for (size_t p = 0; p < (end2/_N_bins); p++) {
            vectorAdd_cf32_I(/*src*/pulse, /*srcdst*/dst, _N_bins);
            pulse += _N_bins;
        }
        /* Samples that didn't fit _N_bins */
        vectorAdd_cf32_I(/*src*/pulse, /*srcdst*/dst, tail2);
        _cfg->rotator_index = (_cfg->rotator_index + tail) % _cfg->rotatorlen;
        _cfg->pcal_index    = (_cfg->pcal_index + tail2)   % _N_bins;
    }

    /* Done! */
    _samplecount += len;
    return true;
}

/**
 * Computes the final extraction result. No more sample data can be added.
 * The PCal extraction results are copied into the specified output array.
 *
 * @param out Pointer to user PCal array with getLength() values
 * @return number of samples that were integrated for the result
 */
uint64_t PCalExtractorShifting::getFinalPCal(cf32* out)
{
    vecStatus s;
    invariant();

    if (!_finalized) {
        _finalized = true;
        vectorAdd_cf32_I(/*src*/&(_cfg->pcal_complex[_N_bins]), /*srcdst*/&(_cfg->pcal_complex[0]), _N_bins);
        s = vectorDFT_CtoC_cf32(/*src*/_cfg->pcal_complex, _cfg->dft_out, _cfg->dftspec, _cfg->dftworkbuf);
        if (s != vecNoErr)
            csevere << startl << "Error in DFTFwd in PCalExtractorShifting::getFinalPCal " << vectorGetStatusString(s) << endl;
    }

    // Show all bin phases
    #ifdef VERBOSE_UNIT_TEST
    cout << "PCalExtractorShifting::getFinalPCal phases are: ";
    for (int i = 0; i < _N_bins; i++)
    {
        f32 phi = (180/M_PI)*std::atan2(_cfg->dft_out[i].im, _cfg->dft_out[i].re);
        cout << "p[" << i << "]=" << phi << " ";
    }
    cout << "deg\n";
    #endif

    // Copy only the interesting bins.
    // Note the "DC" bin has phase info in 1st shifted tone, do not discard.
    size_t step = (size_t)(std::floor(double(_N_bins)*_pcalspacing_hz/_fs_hz));
    for (size_t n=0; n<(size_t)_N_tones; n++) {
        size_t idx = n*step;
        if (idx >= (size_t)_N_bins) { break; }
        out[n].re = _cfg->dft_out[idx].re;
        out[n].im = _cfg->dft_out[idx].im;
    }
    return _samplecount;
}

///////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal signals -
//                complex samples with non-zero offset via spectral shift
///////////////////////////////////////////////////////////////////////////////////

PCalExtractorComplex::PCalExtractorComplex(double bandwidth_hz,
                                           double pcal_spacing_hz,
                                           int pcal_offset_hz,
                                           Configuration::complextype band_type)
{
    pcal_spacing_hz = std::abs(pcal_spacing_hz);

    // derive config
    _fs_hz           = bandwidth_hz;
    _pcaloffset_hz   = pcal_offset_hz;
    _pcalspacing_hz  = pcal_spacing_hz;
    _ssb             = (band_type == Configuration::SINGLE);
    _N_bins          = (int)(_fs_hz / gcd(_fs_hz, pcal_spacing_hz));
    _N_tones         = calcNumTones(bandwidth_hz, pcal_offset_hz, pcal_spacing_hz);
    _estimatedbytes  = 0;

    // Try to meet minimum spectral resolution
    while (_fs_hz/(2*_N_bins) > PCal::_min_freq_resolution_hz)
        _N_bins *= 2;

    _cfg = new pcal_config_pimpl();
    _cfg->rotatorlen = (size_t)(_fs_hz / gcd(_fs_hz, _pcaloffset_hz));
    _cfg->rotatorlen = _N_bins * _cfg->rotatorlen / gcd ((long int)(_cfg->rotatorlen), (long int)(_N_bins));

    // prep for FFT/DFT
    int wbufsize = 0;
    vecStatus s;
    s = vectorInitDFTC_cf32(&_cfg->dftspec, _N_bins, vecFFT_NoReNorm, vecAlgHintAccurate, &wbufsize, &_cfg->dftworkbuf);
    if (s != vecNoErr)
        csevere << startl
		<< "Error in DFTInitAlloc in PCalExtractorComplex::PCalExtractorComplex.  _N_bins="
		<< _N_bins << " error " << vectorGetStatusString(s) << endl;
    _estimatedbytes += wbufsize;

    // Allocate twice the amount strictly necessary
    _cfg->pcal_complex = vectorAlloc_cf32(_N_bins * 2);
    _cfg->pcal_real    = vectorAlloc_f32(_N_bins * 2);
    _cfg->rotator = vectorAlloc_cf32(_cfg->rotatorlen * 2);
    _cfg->rotated = vectorAlloc_cf32(_cfg->rotatorlen * 2);
    _cfg->dft_out = vectorAlloc_cf32(_N_bins * 1);
    _estimatedbytes += _N_bins*4*(4+2+2);
    _estimatedbytes += _cfg->rotatorlen*2*8*2;
    this->clear();

    // Prepare frequency shifter/mixer lookup
    _cfg->dphi = 2*M_PI * (-_pcaloffset_hz/_fs_hz);
    for (size_t n = 0; n < (2 * _cfg->rotatorlen); n++) {
        double arg = _cfg->dphi * double(n);
        _cfg->rotator[n].re = f32(cos(arg));
        _cfg->rotator[n].im = f32(sin(arg));
    }

    if (PCAL_DEBUG)
        cdebug << startl << "PcalExtractorComplex: _Ntones=" << _N_tones << ", _N_bins="
            << _N_bins << ", wbufsize=" << wbufsize
            << ", rotatorlen=" << _cfg->rotatorlen
            << " pcaloffset " << _pcaloffset_hz << endl;
}

PCalExtractorComplex::~PCalExtractorComplex()
{
    vectorFree(_cfg->pcal_complex);
    vectorFree(_cfg->pcal_real);
    vectorFree(_cfg->rotator);
    vectorFree(_cfg->rotated);
    vectorFreeDFTC_cf32(_cfg->dftspec);
    vectorFree(_cfg->dftworkbuf);
    vectorFree(_cfg->dft_out);
    delete _cfg;
}

// Clear the extracted and accumulated PCal data by setting it to zero.

void PCalExtractorComplex::clear()
{
    _samplecount = 0;
    _finalized   = false;
    vectorZero_cf32(_cfg->pcal_complex, _N_bins * 2);
    vectorZero_f32 (_cfg->pcal_real,    _N_bins * 2);
    vectorZero_cf32(_cfg->rotated,      _cfg->rotatorlen * 2);
}

/**
 * Adjust the sample offset.
 *
 * The extractor needs a time-continuous input sample stream. Samples
 * are numbered 0...N according to their offset from the start of the
 * stream. The stream can be split into smaller chunks that
 * are added individually through several extractAndIntegrate() calls.
 *
 * If for some reason two chunks are not continuous in time,
 * some internal indices need to be corrected by calling this
 * function and specifying at what sample number the next
 * chunk passed to extractAndIntegrate() starts.
 *
 * @param sampleoffset sample offset of chunk passed to next extractAndIntegrate() call
 */
void PCalExtractorComplex::adjustSampleOffset(const size_t sampleoffset)
{
    _cfg->rotator_index = (sampleoffset)% _cfg->rotatorlen;
    _cfg->pcal_index    = (sampleoffset)% _N_bins;
}

/**
 * Process a new chunk of time-continuous single channel data.
 * Time-integrates the data into the internal result buffer.
 *
 * If this function is called several times to integrate additional data
 * and these multiple pieces of data are not continuous in time,
 * please see adjustSampleOffset().
 *
 * @param samples Chunk of the input signal consisting of 'float' samples
 * @param len     Length of the input signal chunk
 * @return true on success, false if results were frozen by calling getFinalPCal()
 */
bool PCalExtractorComplex::extractAndIntegrate (f32 const* samples, const size_t len)
{
    if (_finalized) { return false; }

    cf32 const* src = (cf32 const*) samples;
    cf32* dst = &(_cfg->pcal_complex[_cfg->pcal_index]);
    size_t tail  = (len % _cfg->rotatorlen);
    size_t end   = len - tail;

    /* This method is only marginally different from the PCalExtractorTrivial method.
     * Because now our multi-tone PCal signal tones do not reside at integer MHz frequencies,
     * or rather, not at integer multiples of the tone spacing of the comb, the first PCal
     * tone is found at some offset '_pcaloffset_hz' away from 0Hz/DC.
     * So we just use a complex oscillator to shift the input signal back into place.
     * The complex oscillator has a period of _fs_hz/gcd(_fs_hz,_pcaloffset_hz).
     * The period is usually very short, say, 1600 samples. We precomputed those
     * in the constructor and use them here.
     * If the values _fs_hz and _pcaloffset_hz are co-prime, a precomputed sin/cos vector
     * would need a lot of memory. At that point a live sin/cos computation can be
     * faster than the memory access.
     */

    /* Process the first part that fits perfectly (and note: rotatorlen modulo _N_bins is 0!) */
    for (size_t n = 0; n < end; n+=_cfg->rotatorlen, src+=_cfg->rotatorlen) {
        vectorMul_cf32 (src, &(_cfg->rotator[_cfg->rotator_index]),
                        &(_cfg->rotated[_cfg->rotator_index]), _cfg->rotatorlen);
        cf32* pulse = &(_cfg->rotated[_cfg->rotator_index]);
        for (size_t p = 0; p < (_cfg->rotatorlen/_N_bins); p++) {
            vectorAdd_cf32_I (pulse, dst, _N_bins);
            pulse += _N_bins;
        }
    }

    /* Handle any samples that didn't fit */
    if (tail != 0) {
        vectorMul_cf32(src, &(_cfg->rotator[_cfg->rotator_index]),
                       &(_cfg->rotated[_cfg->rotator_index]), tail);
        cf32* pulse = &(_cfg->rotated[_cfg->rotator_index]);
        size_t tail2 = (tail % _N_bins);
        size_t end2  = tail - tail2;
        for (size_t p = 0; p < (end2/_N_bins); p++) {
            vectorAdd_cf32_I(pulse, dst, _N_bins);
            pulse += _N_bins;
        }
        /* Samples that didn't fit _N_bins */
        vectorAdd_cf32_I(pulse, dst, tail2);
        _cfg->rotator_index = (_cfg->rotator_index + tail) % _cfg->rotatorlen;
        _cfg->pcal_index    = (_cfg->pcal_index + tail2)   % _N_bins;
    }

    /* Done! */
    _samplecount += len;
    return true;
}

/**
 * Computes the final extraction result. No more sample data can be added.
 * The PCal extraction results are copied into the specified output array.
 *
 * @param out Pointer to user PCal array with getLength() values
 * @return number of samples that were integrated for the result
 */
uint64_t PCalExtractorComplex::getFinalPCal (cf32* out)
{
    vecStatus s;
    invariant();

    if (!_finalized) {
        _finalized = true;
        vectorAdd_cf32_I (&(_cfg->pcal_complex[_N_bins]), &(_cfg->pcal_complex[0]), _N_bins);
        s = vectorDFT_CtoC_cf32 (_cfg->pcal_complex, _cfg->dft_out, _cfg->dftspec, _cfg->dftworkbuf);
        if (s != vecNoErr)
            csevere << startl << "Error in DFTFwd in PCalExtractorComplex::getFinalPCal " << vectorGetStatusString(s) << endl;
    }

    /* For DSB, DC is in the center bin. Revert to USB/LSB-like spectrum by swapping spectral halves. */
    if(!_ssb) {
        for (size_t n=0; n<_N_bins/2; n++) {
            cf32 tmp = _cfg->dft_out[n];
            _cfg->dft_out[n] = _cfg->dft_out[_N_bins - n - 1];
            _cfg->dft_out[_N_bins - n - 1] = tmp;
        }
    }

    // Copy only the interesting bins.
    // Note the "DC" bin has phase info in 1st shifted tone, do not discard.
    size_t step = (size_t)(std::floor(double(_N_bins)*_pcalspacing_hz/_fs_hz));
    for (size_t n=0; n<(size_t)_N_tones; n++) {
        size_t idx = n*step;
        if (idx >= (size_t)_N_bins)
            break;
        out[n].re = _cfg->dft_out[idx].re;
        out[n].im = _cfg->dft_out[idx].im;
        // note: removed parts of r6027 here and in frequency shifter/mixer preparation,
        // as parts of r6027 reversed (vs existing Real LSB extractors and mpifxcorr)
        // the handedness of the -_pcaloffset_hz shift to clockwise, conjugated pcal values,
        // and reversed the tone order, not producing PCal for actual Complex LSB datasets
    }
    return _samplecount;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal signals with non-zero offset and FFT-implicit rotation possible
/////////////////////////////////////////////////////////////////////////////////////////////////////////

PCalExtractorImplicitShift::PCalExtractorImplicitShift(double bandwidth_hz, double pcal_spacing_hz,
int pcal_offset_hz, const size_t sampleoffset)
{
    /* Derive config */
    _fs_hz          = 2 * bandwidth_hz;
    _pcalspacing_hz = pcal_spacing_hz;
    _pcaloffset_hz  = pcal_offset_hz;
    _N_bins         = (int)(_fs_hz / gcd(_fs_hz, _pcaloffset_hz));
    _N_tones        = calcNumTones(bandwidth_hz, _pcaloffset_hz, _pcalspacing_hz);
    _cfg = new pcal_config_pimpl();
    _estimatedbytes = 0;

    /* Try to meet minimum spectral resolution */
    while (_fs_hz/(2*_N_bins) > PCal::_min_freq_resolution_hz)
        _N_bins *= 2;

    /* Prep for FFT/DFT */
    int wbufsize = 0;
    vecStatus s;
    s = vectorInitDFTC_cf32(&_cfg->dftspec, _N_bins, vecFFT_NoReNorm, vecAlgHintAccurate, &wbufsize,  &_cfg->dftworkbuf);
    if (s != vecNoErr)
        csevere << startl << "Error in DFTInitAlloc in PCalExtractorImplicitShift::PCalExtractorImplicitShift.  _N_bins="
                << _N_bins << " Error " << vectorGetStatusString(s) << endl;
    _estimatedbytes += wbufsize;

    /* Allocate twice the amount strictly necessary */
    _cfg->pcal_complex = vectorAlloc_cf32(_N_bins * 2);
    _cfg->pcal_real    = vectorAlloc_f32(_N_bins * 2);
    _cfg->dft_out      = vectorAlloc_cf32(_N_bins * 1);
    _estimatedbytes   += _N_bins*4*(4+2+2);
    this->clear();

    if (PCAL_DEBUG)
        cdebug << startl << "PCalExtractorImplicitShift: _Ntones = " << _N_tones << ", _N_bins = " << _N_bins << ", wbufsize = " << wbufsize << endl;
}


PCalExtractorImplicitShift::~PCalExtractorImplicitShift()
{
    vectorFree(_cfg->pcal_complex);
    vectorFree(_cfg->pcal_real);
    vectorFreeDFTC_cf32(_cfg->dftspec);
    vectorFree(_cfg->dftworkbuf);
    vectorFree(_cfg->dft_out);
    delete _cfg;
}

/**
 * Clear the extracted and accumulated PCal data by setting it to zero.
 */
void PCalExtractorImplicitShift::clear()
{
    _samplecount = 0;
    _finalized   = false;
    vectorZero_cf32(_cfg->pcal_complex, _N_bins * 2);
    vectorZero_f32 (_cfg->pcal_real,    _N_bins * 2);
}

/**
 * Adjust the sample offset.
 *
 * The extractor needs a time-continuous input sample stream. Samples
 * are numbered 0...N according to their offset from the start of the
 * stream. The stream can be split into smaller chunks that
 * are added individually through several extractAndIntegrate() calls.
 *
 * If for some reason two chunks are not continuous in time,
 * some internal indices need to be corrected by calling this
 * function and specifying at what sample number the next
 * chunk passed to extractAndIntegrate() starts.
 *
 * @param sampleoffset sample offset of chunk passed to next extractAndIntegrate() call
 */
void PCalExtractorImplicitShift::adjustSampleOffset(const size_t sampleoffset)
{
    _cfg->pcal_index = (sampleoffset)% _N_bins;
}

/**
 * Process a new chunk of time-continuous single channel data.
 * Time-integrates the data into the internal result buffer.
 *
 * If this function is called several times to integrate additional data
 * and these multiple pieces of data are not continuous in time,
 * please see adjustSampleOffset().
 *
 * @param samples Chunk of the input signal consisting of 'float' samples
 * @param len     Length of the input signal chunk
 * @return true on success, false if results were frozen by calling getFinalPCal()
 */
bool PCalExtractorImplicitShift::extractAndIntegrate(f32 const* samples, const size_t len)
{
    if (_finalized) {
        cerror << startl << "PCalExtractorImplicitShift::extractAndIntegrate on finalized class!" << endl;
        return false;
    }

    f32 const* src = samples;
    f32* dst = &(_cfg->pcal_real[_cfg->pcal_index]);
    size_t tail = (len % _N_bins);
    size_t end  = len - tail;

    /* This method is from Walter Brisken, it works perfectly for smallish 'len'
     * and when offset and tone spacing have suitable properties.
     * Instead of rotating the input to counteract the offset, we bin
     * into a long vector with size of the offset repeat length (again *2 to avoid
     * buffer wraps). After long-term integration, we copy desired FFT bins
     * into PCal. The time-domain PCal can be derived from inverse FFT.
     */

    /* Process the first part that fits perfectly */
    for (size_t n = 0; n < end; n+=_N_bins, src+=_N_bins) {
        vectorAdd_f32_I(src, /*srcdst*/dst, _N_bins);
    }

    /* Handle any samples that didn't fit */
    if (tail != 0) {
        vectorAdd_f32_I(src, /*srcdst*/dst, tail);
        _cfg->pcal_index = (_cfg->pcal_index + tail) % _N_bins;
    }

    /* Done! */
    _samplecount += len;
    return true;
}

/**
 * Computes the final extraction result. No more sample data can be added.
 * The PCal extraction results are copied into the specified output array.
 *
 * @param out Pointer to user PCal array with getLength() values
 * @return number of samples that were integrated for the result
 */
uint64_t PCalExtractorImplicitShift::getFinalPCal(cf32* out)
{
   vecStatus s;
   invariant();

   if (!_finalized)
   {
        _finalized = true;
        vectorRealToComplex_f32(/*srcRe*/_cfg->pcal_real, /*srcIm*/NULL, _cfg->pcal_complex, 2*_N_bins);
        vectorAdd_cf32_I(/*src*/&(_cfg->pcal_complex[_N_bins]), /*srcdst*/&(_cfg->pcal_complex[0]), _N_bins);
        s = vectorDFT_CtoC_cf32(/*src*/ _cfg->pcal_complex, _cfg->dft_out, _cfg->dftspec, _cfg->dftworkbuf);
        if (s != vecNoErr)
            csevere << startl << "Error in DFTFwd in PCalExtractorImplicitShift::getFinalPCal " << vectorGetStatusString(s) << endl;
    }

    // Show all bin phases
    #ifdef VERBOSE_UNIT_TEST
    cout << "PCalExtractorImplicitShift::getFinalPCal phases are: ";
    for (int i = 0; i < _N_bins; i++)
    {
        f32 phi = (180/M_PI)*std::atan2(_cfg->dft_out[i].im, _cfg->dft_out[i].re);
        cout << "p[" << i << "]=" << phi << " ";
    }
    cout << "deg\n";
    #endif

    /* Copy only the interesting bins */
    size_t step = (size_t)(std::floor(double(_N_bins)*_pcalspacing_hz/_fs_hz));
    size_t offset = (size_t)(std::floor(double(_N_bins)*_pcaloffset_hz/_fs_hz));
    if (PCAL_DEBUG)
        cdebug << startl << "PCalExtractorImplicitShift::getFinalPCal spacing_hz=" << _pcalspacing_hz << " off_hz=" << _pcaloffset_hz << " nbins=" << _N_bins << " fs_hz=" << _fs_hz << " cpy_step=" << step << " cpy_off=" << offset << endl;

    for (size_t n=0; n<(size_t)_N_tones; n++) {
        size_t idx = offset + n*step;
        if (idx >= (size_t)_N_bins) {
            out[n].re = 0;
            out[n].im = 0;
        } else {
            out[n].re = _cfg->dft_out[idx].re;
            out[n].im = _cfg->dft_out[idx].im;
        }
    }
    return _samplecount;
}

//////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal signals -
//                complex samples with non-zero offset, FFT-implicit rotation possible
//////////////////////////////////////////////////////////////////////////////////////

PCalExtractorComplexImplicitShift::PCalExtractorComplexImplicitShift(
    double bandwidth_hz,
    double pcal_spacing_hz,
    int pcal_offset_hz,
    Configuration::complextype band_type)
{
    /* Derive config */
    _fs_hz          = bandwidth_hz;
    _pcalspacing_hz = pcal_spacing_hz;
    _pcaloffset_hz  = pcal_offset_hz;
    _ssb            = (band_type == Configuration::SINGLE);
    _N_bins         = (int)(_fs_hz / gcd(_fs_hz, _pcaloffset_hz));
    _N_tones        = calcNumTones(bandwidth_hz, _pcaloffset_hz, _pcalspacing_hz);
    _estimatedbytes = 0;

    _cfg = new pcal_config_pimpl();

    /* Try to meet minimum spectral resolution */
    while (_fs_hz/(2*_N_bins) > PCal::_min_freq_resolution_hz)
        _N_bins *= 2;

    /* Prep for FFT/DFT */
    int wbufsize = 0;
    vecStatus s;
    s = vectorInitDFTC_cf32(&_cfg->dftspec, _N_bins, vecFFT_NoReNorm, vecAlgHintAccurate, &wbufsize,  &_cfg->dftworkbuf);
    if (s != vecNoErr)
        csevere << startl << "Error in DFTInitAlloc in PCalExtractorComplexImplicitShift::PCalExtractorComplexImplicitShift.  _N_bins="
                << _N_bins << " Error " << vectorGetStatusString(s) << endl;
    _estimatedbytes += wbufsize;

    /* Allocate twice the amount strictly necessary */
    _cfg->pcal_complex = vectorAlloc_cf32(_N_bins * 2);
    _cfg->dft_out      = vectorAlloc_cf32(_N_bins * 1);
    _estimatedbytes   += _N_bins*4*(4+2+2);
    this->clear();

    if (PCAL_DEBUG)
        cdebug << startl << "PCalExtractorComplexImplicitShift: _Ntones = " << _N_tones << ", _N_bins = " << _N_bins << ", wbufsize = " << wbufsize << endl;
}


PCalExtractorComplexImplicitShift::~PCalExtractorComplexImplicitShift()
{
    vectorFree(_cfg->pcal_complex);
    vectorFree(_cfg->dft_out);
    vectorFreeDFTC_cf32(_cfg->dftspec);
    vectorFree(_cfg->dftworkbuf);
    delete _cfg;
}

/**
 * Clear the extracted and accumulated PCal data by setting it to zero.
 */
void PCalExtractorComplexImplicitShift::clear()
{
    _samplecount = 0;
    _finalized   = false;
    vectorZero_cf32(_cfg->pcal_complex, _N_bins * 2);
}

/**
 * Adjust the sample offset.
 *
 * The extractor needs a time-continuous input sample stream. Samples
 * are numbered 0...N according to their offset from the start of the
 * stream. The stream can be split into smaller chunks that
 * are added individually through several extractAndIntegrate() calls.
 *
 * If for some reason two chunks are not continuous in time,
 * some internal indices need to be corrected by calling this
 * function and specifying at what sample number the next
 * chunk passed to extractAndIntegrate() starts.
 *
 * @param sampleoffset sample offset of chunk passed to next extractAndIntegrate() call
 */
void PCalExtractorComplexImplicitShift::adjustSampleOffset(const size_t sampleoffset)
{
    _cfg->pcal_index = sampleoffset % _N_bins;
}

/**
 * Process a new chunk of time-continuous single channel data.
 * Time-integrates the data into the internal result buffer.
 *
 * If this function is called several times to integrate additional data
 * and these multiple pieces of data are not continuous in time,
 * please see adjustSampleOffset().
 *
 * @param samples Chunk of the input signal consisting of 'float' samples
 * @param len     Length of the input signal chunk
 * @return true on success, false if results were frozen by calling getFinalPCal()
 */
bool PCalExtractorComplexImplicitShift::extractAndIntegrate(f32 const* samples, const size_t len)
{
    if (_finalized) {
        cerror << startl << "PCalExtractorComplexImplicitShift::extractAndIntegrate on finalized class!" << endl;
        return false;
    }

    cf32 const* src = (cf32 const*) samples;
    cf32* dst = &(_cfg->pcal_complex[_cfg->pcal_index]);
    size_t tail = (len % _N_bins);
    size_t end  = len - tail;

    /* This method is from Walter Brisken, it works perfectly for smallish 'len'
     * and when offset and tone spacing have suitable properties.
     * Instead of rotating the input to counteract the offset, we bin
     * into a long vector with size of the offset repeat length (again *2 to avoid
     * buffer wraps). After long-term integration, we copy desired FFT bins
     * into PCal. The time-domain PCal can be derived from inverse FFT.
     */

    /* Process the first part that fits perfectly */
    for (size_t n = 0; n < end; n+=_N_bins, src+=_N_bins) {
        vectorAdd_cf32_I(src, /*srcdst*/dst, _N_bins);
    }

    /* Handle any samples that didn't fit */
    if (tail != 0) {
        vectorAdd_cf32_I(src, /*srcdst*/dst, tail);
        _cfg->pcal_index = (_cfg->pcal_index + tail) % _N_bins;
    }

    /* Done! */
    _samplecount += len;
    return true;
}

/**
 * Computes the final extraction result. No more sample data can be added.
 * The PCal extraction results are copied into the specified output array.
 *
 * @param out Pointer to user PCal array with getLength() values
 * @return number of samples that were integrated for the result
 */
uint64_t PCalExtractorComplexImplicitShift::getFinalPCal(cf32* out)
{
   vecStatus s;
   invariant();

   if (!_finalized)
   {
        _finalized = true;
        vectorAdd_cf32_I(/*src*/&(_cfg->pcal_complex[_N_bins]), /*srcdst*/&(_cfg->pcal_complex[0]), _N_bins);
        s = vectorDFT_CtoC_cf32(_cfg->pcal_complex, _cfg->dft_out, _cfg->dftspec, _cfg->dftworkbuf);
        if (s != vecNoErr)
            csevere << startl << "Error in DFTFwd in PCalExtractorComplexImplicitShift::getFinalPCal " << vectorGetStatusString(s) << endl;
    }

    // Show all bin phases
    #ifdef VERBOSE_UNIT_TEST
    cout << "PCalExtractorComplexImplicitShift::getFinalPCal phases are: ";
    for (int i = 0; i < _N_bins; i++)
    {
        f32 phi = (180/M_PI)*std::atan2(_cfg->dft_out[i].im, _cfg->dft_out[i].re);
        cout << "p[" << i << "]=" << phi << " ";
    }
    cout << "deg\n";
    #endif

    /* For DSB, DC is in the center bin. Revert to USB/LSB-like spectrum by swapping spectral halves. */
    if(!_ssb) {
        for (size_t n=0; n<_N_bins/2; n++) {
            cf32 tmp = _cfg->dft_out[n];
            _cfg->dft_out[n] = _cfg->dft_out[_N_bins - n - 1];
            _cfg->dft_out[_N_bins - n - 1] = tmp;
        }
    }

    /* Copy only the interesting bins */
    size_t step = (size_t)(std::floor(double(_N_bins)*_pcalspacing_hz/_fs_hz));
    size_t offset = (size_t)(std::floor(double(_N_bins)*_pcaloffset_hz/_fs_hz));
    if (PCAL_DEBUG)
        cdebug << startl << "PCalExtractorComplexImplicitShift::getFinalPCal spacing_hz=" << _pcalspacing_hz << " off_hz=" << _pcaloffset_hz << " nbins=" << _N_bins << " fs_hz=" << _fs_hz << " cpy_step=" << step << " cpy_off=" << offset << endl;

    for (size_t n=0; n<(size_t)_N_tones; n++) {
        ssize_t bin = offset + n*step;
        if (bin >= _N_bins)
            break;
        out[n].re = _cfg->dft_out[bin].re;
        out[n].im = _cfg->dft_out[bin].im;
    }
    return _samplecount;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// FOR FUTURE REFERENCE
/////////////////////////////////////////////////////////////////////////////////////////////////////////

#if 0
/**
 * Process samples and accumulate the detected phase calibration tone vector.
 * Accumulation takes place into the internal (cf32*)pcalcfg.pcal.
 * @param  data    pointer to input sample vector
 * @param  len     length of input vector
 */
void PCal::extract_continuous(f32* data, size_t len)
{
    /* Works on almost the same principle as extract(), but
     * instead of a precomputed pcalcfg.rotatevec[], we use
     * SSE/MMX C instrinsics to compute the phase angles
     * online in registers, faster than cache access!
     */

    // Oscillator: x[1] = 2*cos(phi) * x[0] - x[-1]
    // Rewritten as two oscillators with step 2phi and offset +0,phi
    //    x1[1] = 2*cos(2phi) * x1[0] - x1[-1]
    //    x2[1] = 2*cos(2phi) * x2[0] - x2[-1]
    // Complex data layout:
    //    vec128bit = vec4float = [cos((n-1)*phi) ; sin((n-1)*phi) ; cos(n*phi)  ; sin(n*phi)]
    //    multiplicand          * [2*cos(2phi)    ; 2*cos(2phi)    ; 2*cos(2phi) ; 2*cos(2phi)]

    static __m128 mmxOldComplex_1, mmxOldComplex_0, mmxMult;

    if (sample_count == 0) {
//    if (1) {
        float dphi = pcalcfg.phase_inc;
        float p0 = sample_count * dphi;
        mmxOldComplex_0 = _mm_set_ps( sinf(p0+1*dphi), cosf(p0+1*dphi),
                                      sinf(p0+0*dphi), cosf(p0+0*dphi) );
        mmxOldComplex_1 = _mm_set_ps( sinf(p0+3*dphi), cosf(p0+3*dphi),
                                      sinf(p0+2*dphi), cosf(p0+2*dphi) );
        mmxMult = _mm_set1_ps( 2*cosf(2*dphi) );
    }

    float *accu = (float*)&pcalcfg.pcal[0];
    float *data_end = data + len;
    //cerr << " data=" << data << " data_end=" << data_end << " diff=" << (data_end-data) << " len=" << len << endl;
    //while (data < data_end) {
    for (size_t xxx=0; xxx<len; xxx+=pcalcfg.tonebins) {
        __m128 mmxData, mmxPcal, mmxTmp, mmxLoad;
        for (int bin=0; bin<int(pcalcfg.tonebins); bin+=4, data+=4, sample_count+=4) {
            /* rotate and bin first 2 samples */
            mmxLoad = _mm_load_ps(data);  // _mm_loadu_ps() if unaligned
            mmxData = _mm_unpacklo_ps(mmxLoad, mmxLoad);
            mmxPcal = _mm_load_ps(&accu[2*bin+0]);
            mmxData = _mm_mul_ps(mmxData, mmxOldComplex_0);
            mmxPcal = _mm_add_ps(mmxPcal, mmxData);
            _mm_store_ps(&accu[2*bin+0], mmxPcal);

//_mm_store_ps(v4tmp, mmxOldComplex_1);
//printf(" %f %f %f %f\n", v4tmp[0], v4tmp[1], v4tmp[2], v4tmp[3]);

            /* rotate and bin next 2 samples */
            mmxData = _mm_unpackhi_ps(mmxLoad, mmxLoad);
            mmxPcal = _mm_load_ps(&accu[2*bin+4]);
            mmxData = _mm_mul_ps(mmxData, mmxOldComplex_1);
            mmxPcal = _mm_add_ps(mmxPcal, mmxData);
            _mm_store_ps(&accu[2*bin+4], mmxPcal);
            /* compute next four complex value pairs */
            mmxTmp = _mm_mul_ps(mmxMult, mmxOldComplex_1);
            mmxTmp = _mm_sub_ps(mmxTmp, mmxOldComplex_0);
            mmxOldComplex_0 = mmxOldComplex_1;
            mmxOldComplex_1 = mmxTmp;
            mmxTmp = _mm_mul_ps(mmxMult, mmxOldComplex_1);
            mmxTmp = _mm_sub_ps(mmxTmp, mmxOldComplex_0);
            mmxOldComplex_0 = mmxOldComplex_1;
            mmxOldComplex_1 = mmxTmp;
         }
   }

//    sample_count += len;
    return;
}

#endif

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: PCal class that returns only known dummy values.
/////////////////////////////////////////////////////////////////////////////////////////////////////////

PCalExtractorDummy::PCalExtractorDummy(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz,
const size_t sampleoffset)
{
    /* Set only the absolutely required members */
    _fs_hz          = 2*bandwidth_hz;
    _pcaloffset_hz  = pcal_offset_hz;
    _pcalspacing_hz = pcal_spacing_hz;
    _N_tones        = calcNumTones(bandwidth_hz, (double)_pcaloffset_hz, _pcalspacing_hz);
    _N_bins         = 2*_N_tones;
    _estimatedbytes = 0;
    this->clear();
    if (PCAL_DEBUG)
        cdebug << startl << "PCalExtractorDummy: _Ntones=" << _N_tones << ", _N_bins=" << _N_bins << endl;
}

PCalExtractorDummy::~PCalExtractorDummy()
{
}

/**
 * Dummy.
 */
void PCalExtractorDummy::clear()
{
    _samplecount = 0;
    _finalized   = false;
}

/**
 * Adjust the sample offset. Dummy.
 * @param sampleoffset referenced back to start of subintegration interval
 */
void PCalExtractorDummy::adjustSampleOffset(const size_t sampleoffset)
{
    (void)sampleoffset;
}

/**
 * Does not do much.
 *
 * If extraction has been finalized by calling getFinalPCal() this function
 * returns False. You need to call clear() to reset.
 *
 * @param samples Chunk of the input signal consisting of 'float' samples
 * @param len     Length of the input signal chunk
 * @return true on success
 */
bool PCalExtractorDummy::extractAndIntegrate(f32 const* samples, const size_t len)
{
    if (false && _finalized) {
        cerror << startl << "Dummy::extract on finalized results!" << endl;
        return false;
    }
    _samplecount += len;
    return true;
}

/**
 * Dummy.
 * @param pointer to user PCal array with getLength() values
 * @return number of samples that were integrated for the result
 */
uint64_t PCalExtractorDummy::getFinalPCal(cf32* out)
{
    if (false && _samplecount == 0) {
        cerror << startl << "Dummy::getFinalPCal called without call to extractAndIntegrate()!";
        cerror << "                           or after call to clear()!" << endl;
    }
    _finalized = true;

    // Fill the output array with dummy values
    for (int i = 0; i < _N_tones; i++) {
        out[i].re = 0.0f;
        out[i].im = 0.0f;
    }
    return _samplecount;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// UNIT TEST (NON-AUTOMATED, MANUAL VISUAL CHECK)
/////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef UNIT_TEST

/* Example:

   $MPICXX -m64 -DUNIT_TEST -DPCAL_DEBUG -DARCH=INTEL -Wall -O3 -pthread \
       -I$IPPROOT/ipp/include/ -I.. -I$DIFXROOT/include/ \
       pcal.cpp mathutil.cpp -o test \
       -L$IPPROOT/ipp/sharedlib -L$IPPROOT/ipp/lib -L$IPPROOT/ipp/lib/intel64/ \
       -lipps -lippvm -lippcore

   Usage:   ./test <auto> | < <samplecount> <bandwidthHz> <spacingHz> <offsetHz> <sampleoffset> [<class>] >
  ./test 32000 16e6 1e6 510e3 0
  ./test 32000 16e6 200e6 4e6 0
*/

#include <cmath>
#include <iostream>
#include <iomanip>
#include <stdlib.h>
#include <cstring>

struct tcase_t {
  long bandwidth, offset, spacing;
  Configuration::datasampling data_type;
  Configuration::complextype band_type;
  const char* mode;
  const char* info;
};

void print_32f(const f32* v, const size_t len);
void print_32fc(const cf32* v, const size_t len);
void print_32fc_phase(const cf32* v, const size_t len);
bool compare_32fc_phase(const cf32* v, const size_t len, f32 angle, f32 step);
bool test_pcal_case(long samplecount, long sampleoffset, tcase_t& testcase);
void test_pcal_auto();

int main(int argc, char** argv)
{
   if (argc < 6 && argc != 2) {
      cerr << "\nUsage:   " << argv[0] << " <auto> | < <samplecount> <bandwidthHz> <spacingHz> <offsetHz> <sampleoffset> [<class>] >\n"
           << "Example: " << argv[0] << " 32000 16e6 1e6 510e3 0 implicit\n\n"
           << "Options:\n"
           << "           samplecount  : number of test samples to generate\n"
           << "           bandwidthHz  : bandwidth of test signal (half the sampling rate)\n"
           << "           spacingHz    : spacing of PCal tones in Hz\n"
           << "           offsetHz     : distance of first tone from 0 Hz\n"
           << "           sampleoffset : non-zero to test sample adjuster, 0 otherwise\n"
           << "           class        : specify extractor explicitly ('trivial', 'shift', 'implicit' or 'dummy')\n\n";
      return -1;
   }

   /* Check GCD functions */
   assert(gcd(32771.0, 32783.0) == 1);
   assert(gcd(32771.0, 32771.0*32783.0) == 32771);
   assert(gcd(32771.0*32783.0, 32771.0) == 32771);
   assert(gcd((long)0, (long)1) == 1);
   assert(gcd((long)0, (long)0) == 0);

   /* Run user-specified test */
   if (argc > 2) {
      tcase_t testcase;
      long samplecount = atof(argv[1]);
      testcase.bandwidth = atof(argv[2]);
      testcase.spacing = atof(argv[3]);
      testcase.offset = atof(argv[4]);
      testcase.data_type = Configuration::REAL;
      testcase.band_type = Configuration::SINGLE;
      long sampleoffset = atof(argv[5]);
      testcase.mode = "auto";
      if (argc > 6)
          testcase.mode = argv[6];
      testcase.info = "";
      cerr << "Settings: nsamp=" << samplecount << ", BWHz=" << testcase.bandwidth << " spcHz=" << testcase.spacing
           << ", offHz=" << testcase.offset << ", sampOff=" << sampleoffset << "\n";
      test_pcal_case(samplecount, sampleoffset, testcase);
   } else {
      cerr << "Running through several test cases\n";
      test_pcal_auto();
   }

   return 0;
}

void test_pcal_auto()
{
   long sampleoffset = 11;
   long samplecount  = 32e3;
   tcase_t cases[] = {
      // BW   1st tone  Spacing    Complex/real         Single/double sideband  Type       Description
      { 16e6,       0,      1e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "" },
      { 16e6,       0,      1e6,   Configuration::REAL, Configuration::SINGLE, "implicit", "(start at DC, implicit; ought to fail)" },
      {  3e6,       0,      2e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "" },
      {  8e6,  2.01e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "IVS Ny-Alesund 5 MHz" },
      {  8e6,  2.01e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "IVS'ish but 10 MHz" },
      { 16e6,    10e3,      1e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "" },
      { 16e6,    10e3,      3e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "spacing 3 MHz" },
      { 16e6,    10e3,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "" },
      { 16e6,       0,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "" },
      { 16e6,       0,      5e6,   Configuration::REAL, Configuration::SINGLE, "implicit", "(start at DC, implicit; ought to fail)" },
      { 16e6,    10e3,      5e6,   Configuration::REAL, Configuration::SINGLE, "implicit", "" },
      {  1e6,    10e3,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "" },
      { 32e6,   990e3,      1e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "IVS'ish LSB 10kHz offset" },
      { 32e6,  1.01e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS-like, tuning xxxx.01 MHz" },
      { 32e6,  2.01e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS-like, tuning xxxx.01 MHz" },
      { 32e6,     3e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS-like but 3 MHz spacing" },
      { 32e6,   0.6e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz USB 0.6" },
      { 32e6,   1.6e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz USB 1.6" },
      { 32e6,   2.6e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz USB 2.6" },
      { 32e6,   3.6e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz USB 3.6" },
      { 32e6,   4.6e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz USB 4.6" },
      { 32e6,   0.4e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz LSB 0.4" },
      { 32e6,   1.4e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz LSB 1.4" },
      { 32e6,   2.4e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz LSB 2.4" },
      { 32e6,   3.4e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz LSB 3.4" },
      { 32e6,   4.4e6,      5e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS tuning xxxx.04 MHz LSB 4.4" },
      { 32e6,  5.01e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.01 MHz" },
      { 32e6,     3e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, but 3 MHz spacing" },
      { 32e6,   0.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 0.6" },
      { 32e6,   1.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 1.6" },
      { 32e6,   2.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 2.6" },
      { 32e6,   3.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 3.6" },
      { 32e6,   4.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 4.6" },
      { 32e6,   5.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 5.6" },
      { 32e6,   6.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 6.6" },
      { 32e6,   7.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 7.6" },
      { 32e6,   8.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 8.6" },
      { 32e6,   9.6e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz USB 9.6" },
      { 32e6,   0.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 0.4" },
      { 32e6,   1.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 1.4" },
      { 32e6,   2.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 2.4" },
      { 32e6,   3.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 3.4" },
      { 32e6,   4.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 4.4" },
      { 32e6,   5.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 5.4" },
      { 32e6,   6.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 6.4" },
      { 32e6,   7.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 7.4" },
      { 32e6,   8.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 8.4" },
      { 32e6,   9.4e6,     10e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "VGOS Yebes 10M, tuning xxxx.40 MHz LSB 9.4" },
      { 128e6,   30e6,    200e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "KVN" },
      { 512e6,   30e6,    200e6,   Configuration::REAL, Configuration::SINGLE, "auto",     "KVN" },
      { 32e6,   2.6e6,     10e6,   Configuration::COMPLEX, Configuration::SINGLE, "auto",  "VGOS Yebes 10M, complex, xxxx.40 MHz USB 2.6" },
      { 32e6,   2.4e6,     10e6,   Configuration::COMPLEX, Configuration::SINGLE, "auto",  "VGOS Yebes 10M, complex, xxxx.40 MHz LSB 2.4" },
   };

   /* Go through test cases; doesn't yet check PASS/FAIL automatically though! */
   int Ncases = sizeof(cases) / sizeof(struct tcase_t);
   int failed[Ncases], Nfailed = 0;
   for (int i = 0; i < Ncases; i++) {
      cerr << "Auto case #" << i << " : " << cases[i].info << std::endl;
      bool ok = test_pcal_case(samplecount, sampleoffset, cases[i]);
      if (!ok)
      {
          failed[Nfailed] = i;
          ++Nfailed;
      }
   }

   cerr << "Passed " << (Ncases-Nfailed) << " cases, failed " << Nfailed << " cases." << std::endl;
   for (int i = 0; i < Nfailed; i++)
   {
      cerr << "Failed case #" << failed[i] << " : " << cases[failed[i]].info << std::endl;
   }

   return;
}

bool test_pcal_case(long samplecount, long sampleoffset, tcase_t& testcase)
{
   bool sloping_reference_data = true;  // add a phase-vs-frequency slope to be able to discern the order of tones
   bool skip_some_data = true;  // tests the adjustSampleOffset() functionality
   bool passed = false;
   uint64_t usedsamplecount;

   const float tone_phase_start = -85.0f;
   const float tone_phase_slope = 5.0f;

   /* Get an extractor */
   bool using_auto = false;
   PCal* extractor;
   extractor->setMinFrequencyResolution(10e3);
   if (!strcasecmp(testcase.mode, "trivial")) {
      extractor = new PCalExtractorTrivial(testcase.bandwidth, testcase.spacing, sampleoffset);
   } else if (!strcasecmp(testcase.mode, "shift")) {
      if (testcase.data_type == Configuration::COMPLEX)
          extractor = new PCalExtractorComplex(testcase.bandwidth, testcase.spacing, testcase.offset, testcase.band_type);
      else
          extractor = new PCalExtractorShifting(testcase.bandwidth, testcase.spacing, testcase.offset, sampleoffset);
   } else if (!strcasecmp(testcase.mode, "implicit")) {
      if (testcase.data_type == Configuration::COMPLEX)
          extractor = new PCalExtractorComplexImplicitShift(testcase.bandwidth, testcase.spacing, testcase.offset, testcase.band_type);
      else
          extractor = new PCalExtractorImplicitShift(testcase.bandwidth, testcase.spacing, testcase.offset, sampleoffset);
   } else if (!strcasecmp(testcase.mode, "dummy")) {
      extractor = new PCalExtractorDummy(testcase.bandwidth, testcase.spacing, testcase.offset, sampleoffset);
   } else {
      cerr << "Using pcal extractor factory to select suitable extractor (req: " << testcase.mode << ")\n";
      extractor = PCal::getNew(testcase.bandwidth, testcase.spacing, testcase.offset, sampleoffset, testcase.data_type, testcase.band_type);
      using_auto = true;
   }
   if (!using_auto) {
      cerr << "User-requested extractor -- may not be suitable for given input parameters\n";
   }

   /* Number of tones in the test signal bandwidth */
   int numtones_actual = PCal::calcNumTones(testcase.bandwidth, testcase.offset, testcase.spacing);
   int numtones_extracted = extractor->getLength();
   cf32* out = vectorAlloc_cf32(numtones_actual);
   cf32* ref = vectorAlloc_cf32(numtones_actual);
   vectorZero_cf32(out, numtones_actual);
   vectorZero_cf32(ref, numtones_actual);

   cerr << "Tone count: actual is " << numtones_actual
        << ", extractor keeps " << numtones_extracted << "\n";

   /* Make test signal with tones phases having a slope */
   double samplingrate = (testcase.data_type == Configuration::REAL) ? 2*testcase.bandwidth : testcase.bandwidth;
   double wtone[numtones_actual];
   for (int tone=0; tone<numtones_actual; tone++) {
       wtone[tone] = 2*M_PI * (testcase.offset + tone*testcase.spacing) / samplingrate;
   }
   float* data = vectorAlloc_f32(samplecount);
   cf32* cplxdata = vectorAlloc_cf32(samplecount);
   for (long n=0; n<samplecount; n++) {
      data[n] = 0; //rand()*1e-9;
      cplxdata[n].re = 0;
      cplxdata[n].im = 0;
      for (int tone=0; tone<numtones_actual; tone++) {
          double phi = wtone[tone] * (n+sampleoffset) + tone_phase_start*(M_PI/180);
          if (sloping_reference_data)
              phi += tone*tone_phase_slope*(M_PI/180);
          data[n] += cos(phi);
          cplxdata[n].re += cos(phi);
          cplxdata[n].im += sin(phi);
      }
   }

   /* Extract with the chosen method */
   extractor->adjustSampleOffset(sampleoffset);
   if (testcase.data_type == Configuration::REAL)
       extractor->extractAndIntegrate(data, samplecount);
   else
       extractor->extractAndIntegrate((f32*)cplxdata, samplecount);

   /* Add more data with a skip? */
   if (skip_some_data) {
       long noffset = 11;
       if (samplecount > noffset) {
           //cerr << "Adding same data but skipping first +" << noffset << " samples\n";
           extractor->adjustSampleOffset(sampleoffset + noffset);
           if (testcase.data_type == Configuration::REAL)
               extractor->extractAndIntegrate(data + noffset, samplecount - noffset);
           else
               extractor->extractAndIntegrate((f32*)(cplxdata + noffset), samplecount - noffset);
       }
   }

   /* Freeze the result */
   usedsamplecount = extractor->getFinalPCal(out);

   /* Compare extracted phase slope to expected phase slope */
   float expected_start = tone_phase_start;
   float expected_slope = (sloping_reference_data) ? tone_phase_slope : 0.0f;
   if (testcase.offset == 0 && sloping_reference_data) {
       // if first tone at DC, skip its "phase"
       expected_start += expected_slope;
   }

   if (sloping_reference_data) {
       cerr << "Expected result: tones are sloping by " << expected_slope << " deg each\n";
   } else {
       cerr << "Expected result: each tone has a fixed -90deg phase\n";
   }

   cerr << "final PCal data:\n";
   // print_32fc(out, numtones_extracted);
   // print_32fc_phase(out, numtones_extracted);
   passed = compare_32fc_phase(out, numtones_extracted, expected_start, expected_slope);

   /* Comparison with the (poorer) "reference" extracted result */
   if (0) {
       extractor->clear();
       if (testcase.data_type == Configuration::REAL)
           extractor->extractAndIntegrate_reference(data, samplecount, ref, sampleoffset);
       else {
           //extractor->extractAndIntegrate((f32*)cplxdata, samplecount);
           cerr << "Error: extractAndIntegrate_reference: complex data not supported yet" << std::endl;
       }

       cerr << "reference PCal data:\n";
       // print_32fc(ref, numtones_actual);
       // print_32fc_phase(ref, numtones_actual);

       compare_32fc_phase(ref, numtones_actual, expected_start, expected_slope);
   }

   /* Done */
   vectorFree(data);
   vectorFree(cplxdata);
   vectorFree(out);
   vectorFree(ref);
   delete extractor;
   return passed;
}

void print_32f(const f32* v, const size_t len) {
   for (size_t i=0; i<len; i++) { cerr << std::scientific << v[i] << " "; }
}

void print_32fc(const cf32* v, const size_t len) {
   for (size_t i=0; i<len; i++) { cerr << std::scientific << v[i].re << "+i" << v[i].im << " "; }
}

void print_32fc_phase(const cf32* v, const size_t len) {
   for (size_t i=0; i<len; i++) {
      float phi = (180/M_PI)*std::atan2(v[i].im, v[i].re);
      cerr << std::scientific << phi << " ";
   }
   cerr << "deg\n";
}

bool compare_32fc_phase(const cf32* v, const size_t len, f32 angle, f32 step)
{
   bool pass = true;
   const float merr = 0.1;
   int num_suspicious = 0;

   for (size_t i=0; i<len; i++) {
      f32 phi = (180/M_PI)*std::atan2(v[i].im, v[i].re);
      //f32 mag = sqrt(v[i].im*v[i].im + v[i].re*v[i].re);
      cerr << "tone #" << (i+1) << ": expect " << angle << ", got " << phi;

      if (std::abs(phi - angle) > merr) {
          // allow Nyquist or DC components to have zero phase (assumes
          // here that zero phase comps are indeed from DC/Nyq, we don't know here...)
          if ((std::abs(phi - 0.0f)>merr && std::abs(phi - 180.0f)>merr)) {
             pass = false;
             cerr << " : error>" << merr << "deg\n";
          } else {
             cerr << " : DC/Nyquist? error>" << merr << "deg\n";
             if (++num_suspicious > 1) {
                pass = false;
             }
          }
      } else {
          cerr << " : ok\n";
      }
      angle += step;
   }
   cerr << "Extracted versus expected:\n" << ((pass) ? "PASS\n" : "\u001b[31mFAIL\u001b[0m (or PASS but missed phase ambiguity)\n") << "\n";
   return pass;
}

#endif
// vim: shiftwidth=4:softtabstop=4:expandtab
