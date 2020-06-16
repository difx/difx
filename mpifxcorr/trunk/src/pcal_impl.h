#ifndef _PCAL_IMPL_H
#define _PCAL_IMPL_H
/********************************************************************************************************
 * @file PCal_impl.h
 * Multi-tone Phase Cal Extraction
 *
 * @brief Extracts and integrates multi-tone phase calibration signal information from an input signal.
 *
 * The extractor factory chooses one out of three currently available methods for signal extraction.
 * The choice depends on the parameters which are:
 *   spacing: frequency step between one tone and the next
 *   offset:  frequency offset between the first tone and the band start at 0 Hz
 *            note that the first tone can be at DC; processing will discard it
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
 *   05Oct2009 - added support for arbitrary input segment lengths
 *   08oct2009 - added Briskens rotationless method
 *   19Mar2014 - added support for extraction from complex samples
 *
 ********************************************************************************************************/

#include "architecture.h"
#include <cstddef>
#include <stdint.h>
using std::size_t;

class pcal_config_pimpl;

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of zero-offset PCal signals
/////////////////////////////////////////////////////////////////////////////////////////////////////////

class PCalExtractorTrivial : public PCal {
   public:
      PCalExtractorTrivial(double bandwidth_hz, double pcal_spacing_hz, const size_t sampleoffset);
      ~PCalExtractorTrivial();
   private:
     PCalExtractorTrivial& operator= (const PCalExtractorTrivial& o); /* no copy */
     PCalExtractorTrivial(const PCalExtractorTrivial& o); /* no copy */

   private:
     int _tone_step;

   public:
      /**
       * Clear the extracted and accumulated PCal data by setting it to zero.
       */
      void clear();

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
      void adjustSampleOffset(const size_t sampleoffset);

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
      bool extractAndIntegrate(f32 const* samples, const size_t len);

      /**
       * Computes the final extraction result. No more sample data can be added.
       * The PCal extraction results are copied into the specified output array.
       *
       * @param out Pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
       */
      uint64_t getFinalPCal(cf32* out);
};


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal signals with non-zero offset
/////////////////////////////////////////////////////////////////////////////////////////////////////////

class PCalExtractorShifting : public PCal {
   public:
      PCalExtractorShifting(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, 
                            const size_t sampleoffset);
      ~PCalExtractorShifting();
   private:
      PCalExtractorShifting& operator= (const PCalExtractorShifting& o); /* no copy */
      PCalExtractorShifting(const PCalExtractorShifting& o); /* no copy */

   public:
      /**
       * Clear the extracted and accumulated PCal data by setting it to zero.
       */
      void clear();

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
      void adjustSampleOffset(const size_t sampleoffset);

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
      bool extractAndIntegrate(f32 const* samples, const size_t len);

      /**
       * Computes the final extraction result. No more sample data can be added.
       * The PCal extraction results are copied into the specified output array.
       *
       * @param out Pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
       */
      uint64_t getFinalPCal(cf32* out);
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal signals with non-zero offset and FFT-implicit rotation possible
/////////////////////////////////////////////////////////////////////////////////////////////////////////

class PCalExtractorImplicitShift : public PCal {
   public:
      PCalExtractorImplicitShift(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, 
                                const size_t sampleoffset);
      ~PCalExtractorImplicitShift();
   private:
     PCalExtractorImplicitShift& operator= (const PCalExtractorImplicitShift& o); /* no copy */
     PCalExtractorImplicitShift(const PCalExtractorImplicitShift& o); /* no copy */     

   public:
      /**
       * Clear the extracted and accumulated PCal data by setting it to zero.
       */
      void clear();

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
      void adjustSampleOffset(const size_t sampleoffset);

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
      bool extractAndIntegrate(f32 const* samples, const size_t len);

      /**
       * Computes the final extraction result. No more sample data can be added.
       * The PCal extraction results are copied into the specified output array.
       *
       * @param out Pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
       */
      uint64_t getFinalPCal(cf32* out);
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal with non-zero offset and FFT-implicit rotation possible, complex data
//////////////////////////////////////////////////////////////////////////////////////////////////////////

class PCalExtractorComplexImplicitShift : public PCal {
   public:
      PCalExtractorComplexImplicitShift(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, int lsb);
      ~PCalExtractorComplexImplicitShift();
   private:
     PCalExtractorComplexImplicitShift& operator= (const PCalExtractorComplexImplicitShift& o); /* no copy */
     PCalExtractorComplexImplicitShift(const PCalExtractorComplexImplicitShift& o); /* no copy */     

   public:
      /**
       * Clear the extracted and accumulated PCal data by setting it to zero.
       */
      void clear();

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
      void adjustSampleOffset(const size_t sampleoffset);

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
      bool extractAndIntegrate(f32 const* samples, const size_t len);

      /**
       * Computes the final extraction result. No more sample data can be added.
       * The PCal extraction results are copied into the specified output array.
       *
       * @param out Pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
       */
      uint64_t getFinalPCal(cf32* out);
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// DERIVED CLASS: extraction of PCal signals with complex samples and non-zero offset
/////////////////////////////////////////////////////////////////////////////////////////////////////////

class PCalExtractorComplex : public PCal {
   public:
      PCalExtractorComplex(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, int lsb);
      ~PCalExtractorComplex();
   private:
      PCalExtractorComplex& operator= (const PCalExtractorComplex& o); /* no copy */
      PCalExtractorComplex(const PCalExtractorComplex& o); /* no copy */

   public:
      /**
       * Clear the extracted and accumulated PCal data by setting it to zero.
       */
      void clear();

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
      void adjustSampleOffset(const size_t sampleoffset);

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
      bool extractAndIntegrate (f32 const* samples, const size_t len);

      /**
       * Computes the final extraction result. No more sample data can be added.
       * The PCal extraction results are copied into the specified output array.
       *
       * @param out Pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
       */
      uint64_t getFinalPCal(cf32* out);
};

class PCalExtractorDummy : public PCal {
  public:
    PCalExtractorDummy(double bandwidth_hz, double pcal_spacing_hz, int pcal_offset_hz, 
                       const size_t sampleoffset);
    ~PCalExtractorDummy();
  private:
    PCalExtractorDummy& operator= (const PCalExtractorDummy& o); /* no copy */
    PCalExtractorDummy(const PCalExtractorDummy& o); /* no copy */

  public:
    /**
     * Clear the extracted and accumulated PCal data by setting it to zero.
     */
    void clear();

    /**
     * Dummy.
     * @param sampleoffset referenced back to start of subintegration interval
     */
    void adjustSampleOffset(const size_t sampleoffset);

    /**
     * Dummy.
     * @param samples Chunk of the input signal consisting of 'float' samples
     * @param len     Length of the input signal chunk
     * @return true on success
     */
    bool extractAndIntegrate(f32 const* samples, const size_t len);

    /**
       * Dummy. Returns fixed results.
       * @param out Pointer to user PCal array with getLength() values
       * @return number of samples that were integrated for the result
     */
    uint64_t getFinalPCal(cf32* out);
};


#endif // _PCAL_IMPL_H
// vim: shiftwidth=2:softtabstop=2:expandtab
