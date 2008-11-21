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
#ifndef VISIBILITY_H
#define VISIBILITY_H

#include <mpi.h>
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
  * @param eseconds The length of the correlation, in seconds
  * @param skipseconds The number of seconds to skip from the start of the correlation, due to the first source(s) not being correlated
  * @param startns The number of nanoseconds to skip from the start of the correlation
  * @param pnames The names of the polarisation products eg {RR, LL, RL, LR} or {XX, YY, XY, YX}
  * @param mon Whether to send visibility data down a monitor socket
  * @param port The port number to send down
  * @param hname The socket to send monitor data down
  * @param monskip Only send 1 in every monskip visibilities to the monitor
  */
  Visibility(Configuration * conf, int id, int numvis, int eseconds, int skipseconds, int startns, const string * pnames, bool mon, int port, char * hname, int * sock, int monskip);

  ~Visibility();

 /**
  * Adds one sub-integration to the accumulator
  * @param subintresults The sub-integration to be added
  * @return Whether this integration period is now complete
  */
  bool addData(cf32* subintresults);

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
  inline int getCurrentConfig() { return currentconfigindex; }

 /**
  * Calculates the time difference between the specified time and the centre of the present integration period
  * @param seconds The comparison time in whole seconds
  * @param ns The offset from the whole seconds in nanoseconds
  * @return Difference between specified time and centre of current integration period, in seconds
  */
  inline double timeDifference(int seconds, int ns)
    { return double(seconds-currentstartseconds) + double(ns)/1000000000.0 - double(currentstartsamples - subintsamples/2)/samplespersecond; }

 /**
  * @return The time at the start of the current integration period
  */
  inline double getTime() { return double(currentstartseconds) + double(currentstartsamples)/double(samplespersecond); }

/**
  * Send a difxmessage containing integration time and antenna weights
  */
  void multicastweights();

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
  void writeascii();

/**
  * Writes the visibilities to disk in rpfits format
  */
  void writerpfits();

/**
  * Writes the visibilities to disk in DiFX format (binary with inserted ascii headers)
  */
  void writedifx();

/**
  * Writes the ascii header for a visibility point in a DiFX format output file
  */
  void writeDiFXHeader(ofstream * output, int baselinenum, int dumpmjd, double dumpseconds, int configindex, int sourceindex, int freqindex, const char polproduct[3], int pulsarbin, int flag, float weight, float buvw[3]);

  Configuration * config;
  int visID, expermjd, experseconds, integrationsamples, currentstartseconds, currentstartsamples, offset, offsetperintegration, subintsthisintegration, subintsamples, numvisibilities, numdatastreams, numbaselines, numchannels, currentsubints, resultlength, currentconfigindex, samplespersecond, maxproducts, executeseconds, autocorrincrement;
  double fftsperintegration, meansubintsperintegration;
  const string * polnames;
  bool first, monitor, pulsarbinon;
  int portnum;
  char * hostname;
  int * mon_socket;
  int monitor_skip;
  cf32 ** autocorrcalibs;
  f32 ** autocorrweights;
  f32 *** baselineweights;
  std::string * telescopenames;
  //cf32 *** results;
  cf32 * results;
#ifdef HAVE_RPFITS
  cf32 * rpfitsarray;
#endif
  f32 *** binweightsums;
  cf32 *** binscales;
  f32 * binweightdivisor;
  int ** pulsarbins;
  Polyco * polyco;
  int *** baselinepoloffsets;
  int *** datastreampolbandoffsets;
};

#endif
