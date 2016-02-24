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
#ifndef VISIBILITY_H
#define VISIBILITY_H

#include <string>
#include "architecture.h"
#include "datastream.h"

/**
@class Visibility 
@brief Stores all baselines visibilities and autocorrelations for one time integration

This class stores all the visibilities (including autocorrelations) as long-term accumulation is performed,
and once all data has been gathered it is written to disk, in ascii or fits format

@author Adam Deller
*/

class Visibility{
public:
 /**
  * Constructor: Copies the information passed to it and allocates the accumulation arrays
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param id This Datastream's MPI id
  * @param numvis The number of Visibilities in the array
  * @param dbuffer A buffer to use when writing to disk (one shared between all visibilities)
  * @param dbufferlen The length of the disk writing buffer
  * @param eseconds The length of the correlation, in seconds
  * @param scan The scan on which we will start
  * @param scanstartsec The number of seconds from the start of this scan
  * @param startns The number of nanoseconds offset from the start second
  * @param pnames The names of the polarisation products eg {RR, LL, RL, LR} or {XX, YY, XY, YX}
  */

  Visibility(Configuration * conf, int id, int numvis, char * dbuffer, int dbufferlen, int eseconds, int scan, int scanstartsec, int startns, const string * pnames);

  ~Visibility();

 /**
  * Tells whether any problems have been found when configuring the Visibility (like
  * missing polycos when pulsar binning)
  * @return Boolean value which is true if no problems found
  */
  inline bool configuredOK() const { return configuredok; }

 /**
  * Adds one sub-integration to the accumulator
  * @param subintresults The sub-integration to be added
  * @return Whether this integration period is now complete
  */
  bool addData(cf32* subintresults);

 /**
  * For all datastreams with pulse cal extraction enabled, write some comments to the beginning of the pulse cal file
  */
  void initialisePcalFiles();

 /**
  * Writes this Visibility's integrated results to disk, after amplitude calibration
  */
  void writedata();

 /**
  * Clears the accumulation vectors and moves to the next time period this Visibility will be responsible for
  */
  void increment();

 /**
  * @return The configuration index for the time period this Visibility is currently integrating
  */
  inline int getCurrentConfig() const { return currentconfigindex; }

 /**
  * Returns the estimated number of bytes used by the Visibility
  * @return Estimated memory size of the Visibility (bytes)
  */
  inline long long getEstimatedBytes() const { return estimatedbytes; }

 /**
  * Returns the current scan index
  * @return Current scan index
  */
  inline int getCurrentScan() const { return currentscan; }

 /**
  * Calculates the time difference between the specified time and the start of the present integration period
  * Assumes the same scan
  * @param seconds The comparison time in whole seconds
  * @param ns The offset from the whole seconds in nanoseconds
  * @return Difference between specified time and start of current integration period, in nanoseconds
  */
  inline s64 timeDifference(int seconds, int ns) const
  { return (static_cast<s64>(seconds-currentstartseconds))*1000000000LL + static_cast<s64>(ns) - static_cast<s64>(currentstartns); }

 /**
  * @return The time at the start of the current integration period, in seconds since
  *         start of experiment
  */
  inline double getTime() const { return ((double)(model->getScanStartSec(currentscan, expermjd, experseconds) + currentstartseconds)) + double(currentstartns)/1000000000.0; }

/**
  * Send a difxmessage containing integration time and antenna weights
  */
  void multicastweights();

  ///constant for the number of bytes in a DiFX output header
  static const int HEADER_BYTES = 74;

  ///Sync word for new binary header
  static const unsigned int SYNC_WORD = 0xFF00FF00;

  ///Version of the binary header
  static const int BINARY_HEADER_VERSION = 1;

  void copyVisData(char **buf, int *bufsize, int *nbuf);

private:
 /**
  * Changes the parameters of the Visibilty object to match the specified configuration
  * @param configindex The index of the configuration to change to
  */
  void changeConfig(int configindex);

 /**
  * Changes the time period of this Visibility to the next period it will be responsible for.
  * Based on the integration time at each time period, and the number of Visibility's in the array
  */
  void updateTime();

 /**
  * Sets up the socket used to send out data to a remote monitoring socket
  * @param hostname The hostname to send data to
  * @param port The port to send through
  * @param window_size Window size to use (kB)
  * @param sock Pointer to the socket that has been opened
  */
  int openMonitorSocket(char *hostname, int port, int window_size, int *sock);

 /**
  * Sends the results array straight down the monitor socket
  */
  int sendMonitorData(bool tofollow);

 /**
  * Checks if the monitoring socket is open - if not, tries to open it
  */
  bool checkSocketStatus();

/**
  * Writes the visibilities to disk in ascii format - only used for debugging
  */
  void writeascii(int dumpmjd, double dumpseconds);

/**
  * Writes the visibilities to disk in DiFX format (binary with inserted ascii headers)
  */
  void writedifx(int dumpmjd, double dumpseconds);

/**
  * Writes the ascii header for a visibility point in a DiFX format output file
  */
  void writeDiFXHeader(ofstream * output, int baselinenum, int dumpmjd, double dumpseconds, int configindex, int sourceindex, int freqindex, const char polproduct[3], int pulsarbin, int flag, float weight, double buvw[3], int filecount);

  Configuration * config;
  int visID, expermjd, experseconds, currentscan, currentstartseconds, currentstartns, offsetns, offsetnsperintegration, subintsthisintegration, subintns, numvisibilities, numdatastreams, numbaselines, currentsubints, resultlength, currentconfigindex, maxproducts, executeseconds, autocorrwidth, todiskbufferlength, maxfiles;
  long long estimatedbytes;
  double fftsperintegration, meansubintsperintegration;
  const string * polnames;
  bool first, pulsarbinon, configuredok;
  int portnum;
  char * hostname;
  cf32 ** autocorrcalibs;
  f32 *** autocorrweights;
  f32 **** baselineweights;
  f32 ***  baselineshiftdecorrs;
  std::string * telescopenames;
  cf32 * results;
  char * todiskbuffer;
  int * todiskmemptrs;
  f32 * floatresults;
  f32 *** binweightsums;
  cf32 *** binscales;
  f32 * binweightdivisor;
  int ** pulsarbins;
  Model * model;
  Polyco * polyco;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
