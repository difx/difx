/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
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
#ifndef MODE_H
#define MODE_H

#include "architecture.h"
#include "configuration.h"
#include "pcal.h"
#include <iostream>
#include <fstream>
#include <cstdlib>

using namespace std;

/**
@class Mode 
@brief Abstract superclass for all modes.  Provides station-based processing functionality

Possesses all methods necessary for mode functionality but should not be instantiated as it does not build a
lookup table or handle unpacking - this is particular to each subclass of mode.  Station-based processing
(FFT, fringe rotation, fraction sample correction etc) is handled via the process method, based on
the provided data and control (delay) arrays

@author Adam Deller
*/
class Mode{
public:
 /**
  * Constructor: allocates memory, extracts stream information and calculates number of lookups etc
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param confindex The index of the configuration this Mode is for
  * @param dsindex The index of the datastream this Mode is for
  * @param recordedbandchan The number of channels for each recorded subband
  * @param chanstoavg The number of channels to average for each subband
  * @param bpersend The number of FFT blocks to be processed in a message
  * @param gsamples The number of additional guard samples at the end of a message
  * @param nrecordedfreqs The number of recorded frequencies for this Mode
  * @param recordedbw The bandwidth of each of these IFs
  * @param recordedfreqclkoffs The time offsets in microseconds to be applied post-F for each of the frequencies
  * @param recordedfreqclkoffsdelta The delay offsets in microseconds between Rcp and Lcp
  * @param recordedfreqphaseoffs The phase offsets in degrees between Rcp and Lcp
  * @param recordedfreqlooffs The LO offsets in Hz for each recorded frequency
  * @param nrecordedbands The total number of subbands recorded
  * @param nzoombands The number of subbands to be taken from within the recorded bands - can be zero
  * @param nbits The number of bits per sample
  * @param sampling The bit sampling type (real/complex)
  * @param unpacksamp The number of samples to unpack in one hit
  * @param fbank Whether to use a polyphase filterbank to channelise (instead of FFT)
  * @param linear2circular Whether to do a linear to circular conversion after the FFT
  * @param fringerotorder The interpolation order across an FFT (Oth, 1st or 2nd order; 0th = post-F)
  * @param arraystridelen The number of samples to stride when doing complex multiplies to implement sin/cos operations efficiently
  * @param cacorrs Whether cross-polarisation autocorrelations are to be calculated
  * @param bclock The recorder clock-out frequency in MHz ("block clock")
  */
  Mode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling, int unpacksamp, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, double bclock);

 /**
  * Stores the FFT valid flags for this block of data
  * @param v The array of valid flags for each FFT
  */
  void setValidFlags(s32 * v);

 /**
  * Stores the raw data for the current block series
  * @param d The data array
  * @param dbytes The number of bytes in the data array
  * @param dscan The scan from which the data comes
  * @param dsec The seconds offset from the start of the scan
  * @param dns The offset in nanoseconds from the integer second
  */
  void setData(u8 * d, int dbytes, int dscan, int dsec, int dns);

 /**
  * reset all pcal objects
  */
  void resetpcal();
  void finalisepcal();

 /**
  * Calculates fringe rotation and fractional sample correction arrays and FFTs, and autocorrelates
  * @param index The index of the FFT chunk to process
  * @param subloopindex The "subloop" index to put the output in
  * @return Fraction of samples that were good for this FFT
  */
  float process(int index, int subloopindex);

 /**
  * Sets the autocorrelation arrays to contain 0's
  */
  void zeroAutocorrelations();

 /**
  * Sets the kurtosis arrays to contain 0's
  */
  void zeroKurtosis();

 /**
  * Stores the times for the first FFT chunk to be processed
  * @param scan The current scan
  * @param seconds The offset in seconds from the start of the scan
  * @param ns The offset in nanoseconds from the integer second
  */
  void setOffsets(int scan, int seconds, int ns);

 /**
  * Averages the autocorrelations down in frequency
  */
  void averageFrequency();

 /**
  * Calculates the kurtosis from intermediate products and averages down in frequency if requires
  * @param numblocks The number of FFTs that went into this average
  * @param maxchannels The number of channels to average down to
  * @return True if there are valid kurtosis data
  */
  bool calculateAndAverageKurtosis(int numblocks, int maxchannels);

 /**
  * Grabs the pointer to an autocorrelation array
  * @param crosspol Whether to return the crosspolarisation autocorrelation for this band
  * @param outputband The band index
  */
  inline cf32* getAutocorrelation(bool crosspol, int outputband) const { return autocorrelations[(crosspol)?1:0][outputband]; }

 /**
  * Grabs the pointer to a kurtosis array
  * @param outputband The band index
  */
  inline f32* getKurtosis(int outputband) const { return sk[outputband]; }

 /**
  * Grabs the weight for a given band
  * @param crosspol Whether to return the crosspolarisation autocorrelation for this band
  * @param outputband The band index
  */
  inline f32 getWeight(bool crosspol, int outputband) const { return weights[(crosspol)?1:0][outputband]; }

 /**
  * Gets the expected decorrelation ("van Vleck correction" ) for a given number of bits.
  * All cases other than 1 and 2 are approximate only!!!
  * @param nbits The number of bits
  * @return The square root of the decorrelation correction
  */
  static inline float getDecorrelationPercentage(int nbits) { return decorrelationpercentage[nbits-1]; }

 /**
  * @return Whether this Mode is writing cross-polarisation auto-corelations
  */
  inline bool writeCrossAutoCorrs() const { return calccrosspolautocorrs; }

 /**
  * @return Whether this Mode was initialied ok
  */
  inline bool initialisedOK() const { return initok; }

 /**
  * @param dk Whether to calculate the kurtosis or not
  */
  inline void setDumpKurtosis(bool dk) { dumpkurtosis = dk; }

 /**
  * Returns a pointer to the FFT'd data of the specified product
  * @param outputband The band to get
  * @param subloopindex The "subloop" index to get the visibilities from
  * @return Pointer to the FFT'd data (complex 32 bit float)
  */
  inline cf32* getFreqs(int outputband, int subloopindex) const { return fftoutputs[outputband][subloopindex]; };

 /**
  * Returns a pointer to the FFT'd and conjugated data of the specified product
  * @param outputband The band to get
  * @param subloopindex The "subloop" index to get the visibilities from
  * @return Pointer to the conjugate of the FFT'd data (complex 32 bit float)
  */
  inline const cf32* getConjugatedFreqs(int outputband, int subloopindex) const { return conjfftoutputs[outputband][subloopindex]; }

 /**
  * Returns the estimated number of bytes used by the Mode
  * @return Estimated memory size of the Mode (bytes)
  */
  inline long long getEstimatedBytes() const { return estimatedbytes; }

  virtual ~Mode();

  /** Constant for comparing two floats for equality (for freqs and bandwidths etc) */
  static const float TINY;

  /**
   * Returns a single pcal result.
   * @param outputband The band to get
   * @param tone The number of the tone to get
   * @return pcal result
   */
  inline cf32 getPcal(int outputband, int tone) const
  { 
    return pcalresults[outputband][tone]; 
  }
  
  ///constant indicating no valid data in a subint
  static const int INVALID_SUBINT = -99999999;

protected:
 /** 
  * Unpacks quantised data to float arrays.  The floating point values filled should
  * be in the range 0.0->1.0, and set appropriately to the expected input levels such that
  * the mean autocorrelation level at nominal sampler statistics is 0.??
  * @param sampleoffset The offset in number of time samples into the data array
  * @return The number of good samples unpacked scaled by the number of samples asked to unpack
  *         ie a weight in the range 0.0 to 1.0
  */
  virtual float unpack(int sampleoffset);
  
  Configuration * config;
  int configindex, datastreamindex, recordedbandchannels, channelstoaverage, blockspersend, guardsamples, fftchannels, numrecordedfreqs, numrecordedbands, numzoombands, numbits, bytesperblocknumerator, bytesperblockdenominator, currentscan, offsetseconds, offsetns, order, flag, fftbuffersize, unpacksamples, unpackstartsamples, datasamples, avgdelsamples;
  long long estimatedbytes;
  int fringerotationorder, arraystridelength, numfrstrides, numfracstrides;
  double recordedbandwidth, blockclock, sampletime; //MHz, microseconds
  double a0, b0, c0, a, b, c, quadadd1, quadadd2;
  double fftstartmicrosec, fftdurationmicrosec, intclockseconds;
  f32 dataweight;
  int samplesperblock, samplesperlookup, numlookups, flaglength, autocorrwidth;
  int datascan, datasec, datans, datalengthbytes, usecomplex;
  bool filterbank, calccrosspolautocorrs, fractionalLoFreq, initok, isfft, linear2circular;
  double * recordedfreqclockoffsets;
  double * recordedfreqclockoffsetsdelta;
  double * recordedfreqphaseoffset;
  double * recordedfreqlooffsets;
  bool deltapoloffsets, phasepoloffset;
  u8  *   data;
  s16 *   lookup;
  s16 *   linearunpacked;
  f32 **  unpackedarrays;
  cf32 **  unpackedcomplexarrays;
  cf32*** fftoutputs;
  cf32*** conjfftoutputs;
  f32 **  weights;
  s32 *   validflags;
  cf32*** autocorrelations;
  vecFFTSpecR_f32 * pFFTSpecR;
  vecFFTSpecC_cf32 * pFFTSpecC;
  vecDFTSpecR_f32 * pDFTSpecR;
  vecDFTSpecC_cf32 * pDFTSpecC;
  u8 * fftbuffer;
  vecHintAlg hint;
  Model * model;
  f64 * interpolator;

  //new arrays for strided complex multiply for fringe rotation and fractional sample correction
  cf32 * complexrotator;
  cf32 * complexunpacked;
  cf32 * fracsamprotatorA, * fracsamprotatorB;  // Allow different delay correction for each pol
  cf32 * fftd;

  // variables for pcal
  int * pcalnbins;
  cf32 ** pcalresults;
  PCal ** extractor;
  
  f64 * subxoff;
  f64 * subxval;
  f64 * subphase;
  f32 * subarg;
  f32 * subsin;
  f32 * subcos;

  f64 * stepxoff;
  f64 * stepxval;
  f64 * stepphase;
  f32 * steparg;
  f32 * stepsin;
  f32 * stepcos;
  cf32 * stepcplx;

  f32 * subchannelfreqs;
  f32 * lsbsubchannelfreqs;
  f32 * subfracsamparg;
  f32 * subfracsampsin;
  f32 * subfracsampcos;

  f32 * stepchannelfreqs;
  f32 * lsbstepchannelfreqs;
  f32 * stepfracsamparg;
  f32 * stepfracsampsin;
  f32 * stepfracsampcos;
  cf32 * stepfracsampcplx;

  //extras necessary for quadratic (order == 2)
  cf32 * piecewiserotator;
  cf32 * quadpiecerotator;

  f64 * subquadxval;
  f64 * subquadphase;
  f32 * subquadarg;
  f32 * subquadsin;
  f32 * subquadcos;

  f64 * stepxoffsquared;
  f64 * tempstepxval;

  //kurtosis-specific variables
  bool dumpkurtosis;
  f32 *  kscratch; //[recordedbandchannels]
  f32 ** s1; //[numrecordedbands][recordedbandchannels]
  f32 ** s2; //[numrecordedbands][recordedbandchannels]
  f32 ** sk; //[numrecordedbands][recordedbandchannels]

  // Linear to circular conversion

  cf32 *phasecorr, *phasecorrconj; // 90 degrees + phase correction
  cf32 * tmpvec; 

private:
  ///Array containing decorrelation percentages for a given number of bits
  static const float decorrelationpercentage[];
};

/** 
 @class LBAMode 
 @brief A mode for 'standard' LBA 2 bit data

 Assumes data has been compressed if running at 128 Mbps or lower ie no redundancy.  Assumes running on a LITTLE-ENDIAN MACHINE!!!
 @author Adam Deller
 */
class LBAMode : public Mode{
public:
 /**
  * Constructor: calls Mode constructor then creates lookup table
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param confindex The index of the configuration this Mode is for
  * @param dsindex The index of the datastream this Mode is for
  * @param nchan The number of channels per subband
  * @param chanstoavg The number of channels to average for each subband
  * @param bpersend The number of FFT blocks to be processed in a message
  * @param gblocks The number of additional guard blocks at the end of a message
  * @param nfreqs The number of frequencies for this Mode
  * @param bw The bandwidth of each of these IFs
  * @param recordedfreqclkoffs The time offsets in microseconds to be applied post-F for each of the frequencies
  * @param recordedfreqclkoffsdelta The offsets in microseconds between Rcp and Lcp
  * @param recordedfreqphaseoffs The phase offsets in degrees between Rcp and Lcp
  * @param recordedfreqlooffs The LO offsets in Hz for each recorded frequency
  * @param ninputbands The total number of subbands recorded
  * @param noutputbands The total number of subbands after prefiltering - not currently used (must be = numinputbands)
  * @param nbits The number of bits per sample
  * @param fbank Whether to use a polyphase filterbank to channelise (instead of FFT)
  * @param linear2circular Whether to do a linear to circular conversion after the FFT
  * @param fringerotorder The interpolation order across an FFT (Oth, 1st or 2nd order; 0th = post-F)
  * @param arraystridelen The number of samples to stride when doing complex multiplies to implement sin/cos operations efficiently
  * @param cacorrs Whether cross-polarisation autocorrelations are to be calculated
  * @param unpackvalues 4 element array containing floating point unpack values for the four possible two bit values
  */
  LBAMode(Configuration * conf, int confindex, int dsindex, int nchan, int chanstoavg, int bpersend, int gblocks, int nfreqs, double bw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta,double * recordedfreqphaseoffs, double * recordedfreqlooffs, int ninputbands, int noutputbands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, const s16* unpackvalues);

    ///unpack mapping for "standard" recording modes
    static const s16 stdunpackvalues[];
    ///unpack mapping for "vsop-style" recording modes
    static const s16 vsopunpackvalues[];
};

/**
 @class LBA8BitMode
 @brief A mode for 'Bruce' style LBA 8 bit data

 Assumes running on a LITTLE-ENDIAN MACHINE!!!
 Also assumes Nyquist sampled clock
 @author Adam Deller
 */
class LBA8BitMode : public Mode{
public:
  LBA8BitMode(Configuration * conf, int confindex, int dsindex, int nchan, int chanstoavg, int bpersend, int gblocks, int nfreqs, double bw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int ninputbands, int noutputbands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs);

  virtual float unpack(int sampleoffset);
};

/**
 @class LBA16BitMode
 @brief A mode for 'Bruce' style LBA 16 bit data

 Assumes running on a LITTLE-ENDIAN MACHINE and the byte order of the 16 bit samples is little endian!!!
 Also assumes Nyquist sampled clock
 @author Adam Deller
 */
class LBA16BitMode : public Mode{
public:
  LBA16BitMode(Configuration * conf, int confindex, int dsindex, int nchan, int chanstoavg, int bpersend, int gblocks, int nfreqs, double bw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int ninputbands, int noutputbands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs);

  virtual float unpack(int sampleoffset);
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
