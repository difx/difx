#ifndef _PCAL_H
#define _PCAL_H
/********************************************************************************************************
 * @file PCal.h
 * Multi-tone Phase Cal Extraction
 *
 * @brief Extracts and integrates multi-tone phase calibration signal information from an input signal.
 *
 * The principle relies on the fact that with a comb spacing of say 1 MHz and a sampling rate of say
 * 32 MHz the single 1 MHz and also all of its multiples (1, 2, .. 16 MHz in the band) have at least
 * one full sine period in 32MHz/1MHz = 32 samples. For extraction and time integration, we simply
 * have to segment the input signal into 32-sample pieces (in the above example) and integrate these
 * pieces.
 *
 * A tiny FFT performed on the integrated 32-bin result gives you the amplitude and phase
 * of every tone. As the PCal amplitude is in practice constant over a short frequency band,
 * the amplitude and phase info after the FFT directly gives you the equipment filter response.
 *
 * The extracted PCal can also be analyzed in the time domain (no FFT). The relative, average instrumental
 * delay time can be found directly by estimating the position of the peak in the time-domain data.
 *
 * @author   Jan Wagner
 * @author   Sergei Pogrebenko
 * @author   Walter Brisken
 * @version  1.1/2009
 * @license  GNU GPL v3
 *
 * Changelog:
 *   05Oct2009 - added support for arbitrary input segment lengths
 *   08oct2009 - added Briskens rotationless method
 *
 ********************************************************************************************************/

#include "architecture.h"
#include <cstddef>
#include <cassert>
#include <stdint.h>

#include "alert.h"

using std::size_t;

class PCalExtractorTrivial;
class PCalExtractorShifting;
class PCalExtractorImplicitShift;
class PCalExtractorDummy; //NOTE added for testing
class pcal_config_pimpl;


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// BASE CLASS : Factory method
/////////////////////////////////////////////////////////////////////////////////////////////////////////

class PCal {

   public:
      PCal() {};
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
       * @return new PCal extractor class instance 
       */
      static PCal* getNew(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, const size_t sampleoffset);
      //TODO remove sampleoffset arg again since we have adjustSampleOffset()

   public:
      /**
       * Set the extracted and accumulated PCal data back to zero.
       * When several PCal are run in parallel in segments of a 
       * time slice of data, and the PCal results from each segment
       * should be combined later, care must be taken to tell the PCal
       * extraction the number/offset of the first sample in the segment
       * Typically 0 for the first segment, len(segment) for the second
       * segment, and so on, until offset of the last segment
       * which is len(subintegration_subslice)-len(segment).
       * @param sampleoffset referenced back to start of subintegration interval
       */
      virtual void clear() = 0;
      //TODO remove sampleoffset arg again since we have adjustSampleOffset()

      /**
       * Adjust the sample offset. Should be called before extractAndIntegrate()
       * every time there is a gap or backwards shift in the otherwise contiguous
       * sample stream.
       * @param sampleoffset referenced back to start of subintegration interval
       */
      virtual void adjustSampleOffset(const size_t sampleoffset) = 0;
 
      /**
       * Extracts multi-tone PCal information from a single-channel signal segment
       * and integrates it to the class-internal PCal extraction result buffer.
       * There are no restrictions to the segment length.
       *
       * If you integrate over a longer time and several segments, i.e. perform
       * multiple calls to this function, take care to keep the input
       * continuous (i.e. don't leave out samples).
       *
       * If extraction has been finalized by calling getFinalPCal() this function
       * returns False. You need to call clear() to reset.
       *
       * @param samples Chunk of the input signal consisting of 'float' samples
       * @param len     Length of the input signal chunk
       * @return true on success
       */
      virtual bool extractAndIntegrate(f32 const* samples, const size_t len) = 0;

      /**
       * Returns the length in data-seconds of the currently integrated PCal results.
       * The seconds value is derived from sample count and input bandwidth.
       * @return current integration time in seconds
       */
      double getSeconds() { return ((_fs_hz==0.0f) ? 0.0f : (double(_samplecount)/_fs_hz)); }

      /**
       * Get length of vector the user should reserve for getFinalPCal() output copy.
       * @return vector length in complex tones
       */
      int getLength() { return _N_tones; }

      /**
       * Get length of folding buffer the user should use for ensuring consistency of results.
       * @return length in complex samples of folding buffer (N bins)
       */
      int getNBins() { return _N_bins; }

      /**
       * Performs finalization steps on the internal PCal results if necessary
       * and then copies these PCal results into the specified output array.
       * Data in the output array is overwritten with PCal results.
       *
       * @param pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
       */
      virtual uint64_t getFinalPCal(cf32* out) = 0;

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
     /**
      * Return greatest common divisor.
      */
      static long long gcd(long, long);

   private:
      /* Testing */
      void invariant() { 
          assert(_samplecount>=0);
          assert(_fs_hz>0.0f);
          assert(_pcaloffset_hz>=0.0f);
          assert(_pcalspacing_hz>=0.0f);
          assert(_pcaloffset_hz<_pcalspacing_hz);
          assert(_N_bins>0);
          assert(_N_tones>0);
          assert(_cfg != NULL);
      }

   private:
      uint64_t _samplecount;
      double _fs_hz;
      int _pcaloffset_hz;
      double _pcalspacing_hz;
      int _N_bins;
      int _N_tones;
      bool _finalized;

      pcal_config_pimpl* _cfg;

   friend class PCalExtractorTrivial;
   friend class PCalExtractorShifting;
   friend class PCalExtractorImplicitShift;

   //NOTE added for testing
   friend class PCalExtractorDummy;
};

#endif // _PCAL_H
// vim: shiftwidth=2:softtabstop=2:expandtab
