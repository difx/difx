#ifndef _PCAL_H
#define _PCAL_H
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
#include "configuration.h" // for enums datasampling, complextype
#include <cstddef>
#include <cassert>
#include <stdint.h>

using std::size_t;

class PCalExtractorTrivial;
class PCalExtractorShifting;
class PCalExtractorImplicitShift;
class PCalExtractorComplex;
class PCalExtractorComplexImplicitShift;
class PCalExtractorDummy; //NOTE added for testing
class pcal_config_pimpl;


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// BASE CLASS : Factory method
/////////////////////////////////////////////////////////////////////////////////////////////////////////

class PCal {

   public:
      PCal();
      virtual ~PCal() {};
   private:
      PCal(const PCal& o); /* no copy */
      PCal& operator= (const PCal& o); /* no assign */

   public:
      /**
       * Factory that returns a new PCal extractor object. The optimal implementation
       * is selected based on the input parameters.
       * @param bandwidth_hz     Bandwidth of the input signal in Hertz
       * @param pcal_spacing_hz  Spacing of the PCal signal, comb spacing, typically 1e6 Hertz
       * @param pcal_offset_hz   Offset of the first PCal signal from 0Hz/DC, typically 10e3 Hertz
       * @param sampleoffset     Offset of the first sample as referenced to start of subintegration interval
       * @param data_type        COMPLEX or REAL
       * @param band_type        SINGLE (single sideband) or DOUBLE (dual sideband)
       * @return new PCal extractor class instance
       */
      static PCal* getNew(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, const size_t sampleoffset, Configuration::datasampling data_type, Configuration::complextype band_type);

     /**
      * Return number of tones that fit the band, including any
      * tones that fall onto DC or the upper band edge.
      */
      static int calcNumTones(double bw, double offset, double step);

   public:
      /**
       * Clear the extracted and accumulated PCal data by setting it to zero.
       */
      virtual void clear() = 0;

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
      virtual void adjustSampleOffset(const size_t sampleoffset) = 0;
 
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
      virtual bool extractAndIntegrate(f32 const* samples, const size_t len) = 0;

      /**
       * Get data-seconds contributing to the current PCal results.
       * @return amount of integrated data in seconds
       */
      double getSeconds() const { return ((_fs_hz==0.0f) ? 0.0f : (double(_samplecount)/_fs_hz)); }

      /**
       * Get length of vector the user should reserve for getFinalPCal() output copy.
       * @return vector length in complex tones
       */
      int getLength() const { return _N_tones; }

      /**
       * Get length of folding buffer the user should use for ensuring consistency of results.
       * @return length in complex samples of folding buffer (N bins)
       */
      int getNBins() const { return _N_bins; }

      /**
       * Get estimated size of this object in bytes;
       * @return estimated size in bytes
       */
      long long getEstimatedBytes() const { return _estimatedbytes; }

      /**
       * Computes the final extraction result. No more sample data can be added.
       * The PCal extraction results are copied into the specified output array.
       *
       * @param out Pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
       */
      virtual uint64_t getFinalPCal(cf32* out) = 0;

      /**
       * Specifies a minimum frequency resolution (spectral response around
       * the PCal tone) that the extractors should try to meet. Currently assured
       * for the PCalExtractorImplicitShift extractor type only.
       * @param spectral resolution in hertz
       */
      static void setMinFrequencyResolution(double hz) { PCal::_min_freq_resolution_hz = hz; }

      /**
       * Processes samples and accumulates the detected phase calibration tone vector.
       * Computation uses the slowest thinkable direct method. This function is
       * intended for testing and comparison only!
       * @param  data          pointer to input sample vector
       * @param  len           length of input vector
       * @param  pcalout       output array of sufficient size to store extracted PCal
       * @param  sampleoffset
       */
      bool extractAndIntegrate_reference(f32 const* data, const size_t len, cf32* pcalout, const uint64_t sampleoffset);

   private:
      /* Testing */
      void invariant() { 
          assert(_samplecount>=0);
          assert(_fs_hz>0.0f);
          assert(_pcaloffset_hz>=0.0f);
          assert(_pcalspacing_hz>=0.0f);
          assert((_pcalspacing_hz==0.0f) || (_pcalspacing_hz!=0.0f && _pcaloffset_hz<=_pcalspacing_hz));
          assert(_N_bins>=0);
          assert(_N_tones>=0);
          assert(_cfg != NULL);
      }

   private:
      uint64_t _samplecount;
      double _fs_hz;
      int _pcaloffset_hz;
      double _pcalspacing_hz;
      int _ssb;
      int _N_bins;
      int _N_tones;
      bool _finalized;
      long long _estimatedbytes;

      pcal_config_pimpl* _cfg;

      static double _min_freq_resolution_hz;

   friend class PCalExtractorTrivial;
   friend class PCalExtractorShifting;
   friend class PCalExtractorImplicitShift;
   friend class PCalExtractorComplex;
   friend class PCalExtractorComplexImplicitShift;

   //NOTE added for testing
   friend class PCalExtractorDummy;
};

#endif // _PCAL_H
// vim: shiftwidth=2:softtabstop=2:expandtab
