/***************************************************************************
 *   Copyright (C) 2006-2016 by Adam Deller                                *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
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
#ifndef POLYCO_H
#define POLYCO_H

#include <string>
#include <iostream>
#include <fstream>
#include "architecture.h"

using namespace std;

/** 
 @class Polyco
 @brief Provides pulse phase prediction for a given pulsar at a given time

 Uses a polynomial read from the provided polyco file to calculate pulse phase at given frequency and time.
 @author Adam Deller
 */
class Polyco{
public:
 /**
  * Constructor: Loads the provided file and allocates the weighting and phase arrays
  * @param filename The polyco file to load from
  * @param subcount The index of the polyco in the file to load
  * @param confindex The configuration index which this polyco relates to (ie which pulsar)
  * @param nbins The number of pulsar bins to use
  * @param maxchans The number of spectral points per band
  * @param bphases The phases of the endpoints of each pulsar bin
  * @param bweights The weights associated with each pulsar bin
  * @param calcmins The period of time over which a given set of phase offsets is expected to be accurate
  */
  Polyco(string filename, int subcount, int confindex, int nbins, int maxchans, double * bphases, double * bweights, double calcmins);

 /**
  * Copy constructor - performs deep copy of provided Polyco
  * @param tocopy The polyco object to copy
  */
  Polyco(const Polyco & tocopy);

  ~Polyco();

 /**
  * Loads a polyco file and stores the information in memory
  * @param filename The file to load from
  * @param subcount The index of the polyco in the file to load
  * @return True if polyco file loaded ok
  */
  bool loadPolycoFile(string filename, int subcount);

 /**
  * Checks to see if this polyco's valid range includes the specified time
  * @param incmjd The Modified Julian Date of the time to check
  * @param incmjdfraction The offset in days (as a fraction) from the specified integer MJD
  * @return True if the specified time falls in this Polyco's time range
  */
  bool includesTime(int incmjd, double incmjdfraction) const;

 /**
  * Sets the frequency band information and allocates the necessary arrays for bin calculation
  * @param nfreqs The number of frequencies to calculate for
  * @param freqs Array of frequency values, in MHz
  * @param bws Array of bandwidths, in MHz
  * @param nchans Array of number of channels
  * @param compute Array specifying which frequencies should actually be computed
  * @return Whether bin values are legal or not (should abort if not legal)
  */
  bool setFrequencyValues(int nfreqs, double * freqs, double * bws, int * nchans, bool * compute);

 /**
  * Sets the active time (from which subsequent offsets will refer to) to the given values
  * @param startmjd The reference integer Modified Julian Date
  * @param startmjdfraction The reference Modified Julian Date fractional component
  */
  void setTime(int startmjd, double startmjdfraction);

 /**
  * Calculates the bins for each spectral point of each frequency band at the given time
  * @param offsetmins The offset from the previously set reference time, in minutes
  * @param bins The array of bin values, set by this function
  */
  void getBins(double offsetmins, int ** bins) const;

 /**
  * Loops through the provided Polyco array, and returns the one that matches the specified time, or null if none match
  * @param requiredconfig The configuration required of the Polyco (ensure you don't get the wrong pulsar!!!)
  * @param mjd The Modified Julian Date of the time to get
  * @param mjdfraction The offset in days (as a fraction) from the specified integer MJD
  * @param polycos The array of Polycos to check through
  * @param npolycos The number of polycos in the array
  * @param printtimes Whether to print the comparison values to cinfo or not
  * @return The matching Polyco, or null if none match
  */
  static Polyco * getCurrentPolyco(int requiredconfig, int mjd, double mjdfraction, Polyco ** polycos, int npolycos, bool printtimes);

 /**
  * Returns whether the initial file was read ok
  * @return Whether the initial file was read ok
  */
  inline bool initialisedOK() const { return readok; }
  
// /**
//  * Returns the bin counts
//  * @return The bin counts (number of times each bin has been calculated since last cleared) for this Polyco
//  */
//  inline s32 *** getBinCounts() { return currentbincounts; }
 /**
  * Returns the configuration index
  * @return The configuration index this Polyco corresponds to
  */
  inline int getConfig() const { return configindex; }
 /**
  * Returns the bin weights
  * @return The bin weights for this Polyco
  */
  inline f64* getBinWeights() const { return binweights; }
 /**
  * Returns the weight*width product for the specified bin
  * @return The weight*width product for the specified bin
  */
  inline f64 getBinWeightTimesWidth(int bin) const { f64 w = binphases[bin]-binphases[(bin+numbins-1)%numbins]; return (w<0.0)?binweights[bin]*(1.0+w):binweights[bin]*w; }

/**
  * Returns the width of the specified bin
  * @return The width of the specified bin
  */
  inline f64 getBinWidth(int bin) const { f64 w = binphases[bin]-binphases[(bin+numbins-1)%numbins]; return (w<0.0)?(1.0+w):w; }

// /**
//  * Clears the bin counts
//  */
//  void incrementBinCount();

 /**
  * Returns the estimated number of bytes used by the Polyco
  * @return Estimated memory size of the Polyco (bytes)
  */
  inline long long getEstimatedBytes() const { return estimatedbytes; }

  inline int getMJD() const { return mjd; }
  inline double getMJDfraction() const { return mjdfraction; }
  inline double getSpanMinutes() const { return timespan; }

  ///defines how much error will be tolerated due to frequency drift since the reference time
  static const double BIN_TOLERANCE;

  ///The constant used in converting DM to delay
  static const double DM_CONSTANT_SECS;
  
protected:
 /**
  * Calculates the phase offsets for each bin of each frequency at the given time
  * @param offsetmins The offset in minutes from the reference time
  */
  void calculateDMPhaseOffsets(double offsetmins);

  string pulsarname;
  int configindex, numbins, maxchannels, numfreqs, observatory, timespan, numcoefficients, mjd;
  long long estimatedbytes;
  double mjdfraction, dt0, dm, dopplershift, logresidual, refphase, f0, obsfrequency, binaryphase, minbinwidth, calclengthmins;
  bool readok;
  double * coefficients;
  f64 * binphases;
  f64 * binweights;
  f32 * lofrequencies;
  f32 * bandwidths;
  int * numchannels;
  bool * computefor;
  f64 * freqcoefficientarray;
  f64 * phasecoefficientarray;
  f64 * timepowerarray;
  f64 * absolutephases;
  f64 ** dmPhaseOffsets;
  f64 ** channeldmdelays;
  int *** currentbincounts;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
