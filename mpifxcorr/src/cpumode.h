#ifndef CPUMODE_H
#define CPUMODE_H

#include "mode.h"

class Configuration;

class CPUMode: public Mode {
  public:
  CPUMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling, Configuration::complextype tcomplex, int unpacksamp, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, double bclock);

  virtual ~CPUMode();

  void process(int index, int subloopindex);  //frac sample error is in microseconds 

private:
  vecFFTSpecC_cf32 * pFFTSpecC;
  vecDFTSpecC_cf32 * pDFTSpecC;
};

/** 
 @class LBA_CPUMode 
 @brief A mode for 'standard' LBA 2 bit data

 Assumes data has been compressed if running at 128 Mbps or lower ie no redundancy.  Assumes running on a LITTLE-ENDIAN MACHINE!!!
 @author Adam Deller
 */
class LBA_CPUMode : public CPUMode{
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
  LBA_CPUMode(Configuration * conf, int confindex, int dsindex, int nchan, int chanstoavg, int bpersend, int gblocks, int nfreqs, double bw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta,double * recordedfreqphaseoffs, double * recordedfreqlooffs, int ninputbands, int noutputbands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, const s16* unpackvalues);

    ///unpack mapping for "standard" recording modes
    static const s16 stdunpackvalues[];
    ///unpack mapping for "vsop-style" recording modes
    static const s16 vsopunpackvalues[];
};

/**
 @class LBA8Bit_CPUMode
 @brief A mode for 'Bruce' style LBA 8 bit data

 Assumes running on a LITTLE-ENDIAN MACHINE!!!
 Also assumes Nyquist sampled clock
 @author Adam Deller
 */
class LBA8Bit_CPUMode : public CPUMode{
public:
  LBA8Bit_CPUMode(Configuration * conf, int confindex, int dsindex, int nchan, int chanstoavg, int bpersend, int gblocks, int nfreqs, double bw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int ninputbands, int noutputbands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs);

  virtual float unpack(int sampleoffset);
};

/**
 @class LBA16Bit_CPUMode
 @brief A mode for 'Bruce' style LBA 16 bit data

 Assumes running on a LITTLE-ENDIAN MACHINE and the byte order of the 16 bit samples is little endian!!!
 Also assumes Nyquist sampled clock
 @author Adam Deller
 */
class LBA16Bit_CPUMode : public CPUMode{
public:
  LBA16Bit_CPUMode(Configuration * conf, int confindex, int dsindex, int nchan, int chanstoavg, int bpersend, int gblocks, int nfreqs, double bw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int ninputbands, int noutputbands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs);

  virtual float unpack(int sampleoffset);
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
