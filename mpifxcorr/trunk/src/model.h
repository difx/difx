/***************************************************************************
 *   Copyright (C) 2009 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at  http://cira.ivec.org/dokuwiki/doku.php/difx/documentation for     *
 *   more details.                                                         *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: $
// $HeadURL: $
// $LastChangedRevision: $
// $Author: $
// $LastChangedDate: $
//
//============================================================================
#ifndef MODEL_H
#define MODEL_H

#include <string>
#include <fstream>
#include <cstdlib>
#include <iostream>
#include <vector>

//forward declaration of Configuration
class Configuration;

using namespace std;

/**
 @class Model
 @brief Container class for geometric model information, in scan-based form

 Contains a list station and source positions, and a list of scans.  Each scan contains a polynomial
 representation of {u,v,w} and delay terms as loaded from the .model file, and allows easy access via 
 interpolation to any point in the experiment

 @author Adam Deller
 */
class Model{
  public:
    /**
     * Constructor: Loads the model information from the supplied file into memory
     * @param conf The configuration object, containing all information about correlation setup
     * @param cfilename The calc file to load from (which contains pointer to IM file with actual polynomials)
     */
    Model(Configuration * conf, string cfilename);

    ~Model();

    /// Types of antenna axis
    enum axistype {ALTAZ=0, RADEC=1, ORB=2, XY=3};

    /// Constants
    static const int MAX_POLY_ORDER = 10;

    /// Structures that are used by other classes
    typedef struct {
      string name;
      double x,y,z,axisoffset;
      axistype mount;
    } station;

    typedef struct {
      int index;
      string name;
      double ra, dec;
      int qual;
      string calcode;
    } source;

    typedef struct {
      int mjd; //days
      int taiutc; //TAI - UTC in seconds
      double ut1utc; //UT1 - UTC in seconds
      double xpole; //offset in arcseconds
      double ypole; //offset in arcseconds
    } eop;

    typedef struct {
      string name;
      int numsamples;
      double * samplemjd;
      double * x;
      double * y;
      double * z;
      double * vx;
      double * vy;
      double * vz;
    } spacecraft;

    /**
     * Returns whether the Model file was opened and parsed successfully
     * @return Whether this Model object was successfully created
     */
    inline bool openSuccess() const { return opensuccess; }

    /**
     * Returns whether the pointing centre is correlated for this scan
     * @return Whether the pointing centre is correlated for the scan
     */
    inline bool isPointingCentreCorrelated(int scan) const { return scantable[scan].pointingcentrecorrelated; }

    /**
     * Returns the estimated number of bytes used by the Model
     * @return Estimated memory size of the Model (bytes)
     */
    inline long long getEstimatedBytes() const { return estimatedbytes; }

    /**
     * Returns the maximum geometric rate for the given antenna, in us/sec
     * @param antnum The antenna to look at
     * @return Maximum geometric rate for the given antenna, in us/sec
     */
    inline double getMaxRate(int antnum) const { return maxrate[antnum]; }

    ///accessor methods for number of scans, scan durations etc
    inline int getNumScans() const { return numscans; }
    inline int getScanStartSec(int scan, int jobmjd, int jobsec) const { return (modelmjd-jobmjd)*86400 + (modelstartseconds-jobsec) + scantable[scan].offsetseconds; }
    inline double getScanStartMJD(int scan) const { return modelmjd + (double)(modelstartseconds+scantable[scan].offsetseconds)/86400.0; }
    inline int getScanEndSec(int scan, int jobmjd, int jobsec) const { return (modelmjd-jobmjd)*86400 + (modelstartseconds-jobsec) + scantable[scan].offsetseconds + scantable[scan].durationseconds; }
    inline double getScanEndMJD(int scan) const { return modelmjd + (double)(modelstartseconds+scantable[scan].offsetseconds+scantable[scan].durationseconds)/86400.0; }
    inline string getScanIdentifier(int scan) const { return scantable[scan].identifier; }
    inline int getScanDuration(int scan) const { return scantable[scan].durationseconds; }
    inline double getModelStartMJDPlusFraction() const { return modelmjd + ((double)modelstartseconds)/86400.0; }
    inline int getMaxNSBetweenXCAvg(int scan) const { return scantable[scan].maxnsbetweenxcavg; }
    inline int getMaxNSBetweenACAvg(int scan) const { return scantable[scan].maxnsbetweenacavg; }

    ///accessor methods for source information from a scan
    inline int getPointingCentreSourceIndex(int scan) const { return scantable[scan].pointingcentre->index; }
    inline int getPhaseCentreSourceIndex(int scan, int phasecentre) const { return scantable[scan].phasecentres[phasecentre]->index; }
    inline source * getScanPointingCentreSource(int scan) const { return scantable[scan].pointingcentre; }
    inline source * getScanPhaseCentreSource(int scan, int phasecentre) const { return scantable[scan].phasecentres[phasecentre]; }

    ///accessor methods for antenna information
    inline int getNumStations() const { return numstations; }
    inline station getStation(int station) const { return stationtable[station]; }

    /**
     * Updates a clock value mid-correlation (intended for eVLBI only!!)
     * Clock reference time is forced to model start time
     * @param antennaindex The index of the antenna
     * @param order The order of the clock model to be updated
     * @param deltaclock array of clock polynomial update values
     */
     void updateClock(int antennaindex, int order, double * deltaclock);

    /**
     * Fills in the UVW values, in metres, for the specified antenna pointing centre
     * at the specified time
     * @param scanindex The scan
     * @param offsettime Offset in seconds from the start of the scan
     * @param antennaindex1 The first antenna
     * @param antennaindex2 The second antenna
     * @param scansourceindex The source index within the scan
     * @param uvw The uvw values to fill in
     * @return true = success, false = failure (offset wasn't within scan, probably)
     */
    bool interpolateUVW(int scanindex, double offsettime, int antennaindex1, int antennaindex2, int scansourceindex, double* uvw);

    /**
     * Calculates coefficients for a 0th, 1st or second order interpolation of delay for one
     * pointing centre of a given antenna at a given time
     * @param scanindex The scan
     * @param offsettime Offset in seconds from the start of the scan, to start of timespan
     * @param timespan Timespan the interpolation should attempt to match, in seconds
     * @param increments The number of increments across timespan (ie the x value range, 0->numincrements)
     * @param antennaindex The antenna
     * @param scansourceindex The source index within the scan
     * @param order The order of the interpolator (0, 1, or 2)
     * @param delaycoeffs The coefficients, to be filled in
     * @return true = success, false = failure (offset wasn't within scan, probably)
     */
    bool calculateDelayInterpolator(int scanindex, f64 offsettime, f64 timespan, int increments, int antennaindex, int scansourceindex, int order, f64 * delaycoeffs);

    /**
     * Adds the clock model for a given antenna
     * @param antennaname The name of the antenna to add the clock model for
     * @param refmjd The reference time for this clock model (MJD, with fraction)
     * @param order The order of the clock polynomial model (0, 1, or more)
     * @param terms The clock model coefficients (in us, us/s, us/s^2 ...)
     * @param isupdate True if this is a mid-correlation clock update (eVLBI); false will zero clock first
     * @return True unless the antenna was not found
     */
    bool addClockTerms(string antennaname, double refmjd, int order, double * terms, bool isupdate);

    /**
     * Returns the number of phase centres to be correlated for this scan
     * @return The number of phase centres to be correlated for this scan
     */
    inline int getNumPhaseCentres(int scanindex) { return scantable[scanindex].numphasecentres; }

    /**
     * Returns whether the specified phase centre is also the pointing centre
     * @return whether the specified phase centre is also the pointing centre
     */
    inline bool isPointingCentre(int scan, int source) const { return (scantable[scan].phasecentres[source] == scantable[scan].pointingcentre); }

  private:
    typedef struct {
      int offsetseconds, durationseconds, nummodelsamples, polystartmjd, polystartseconds;
      string obsmodename, identifier;
      source * pointingcentre;
      int numphasecentres;
      int maxnsbetweenxcavg;
      int maxnsbetweenacavg;
      bool pointingcentrecorrelated;
      source ** phasecentres;
      f64 **** u;
      f64 **** v;
      f64 **** w;
      f64 **** delay;
      f64 **** wet;
      f64 **** dry;
      f64 **** adj;
      f64 **** az;
      f64 **** elcorr;
      f64 **** elgeom;
      f64 **** parang;
      f64 *** clock;
    } scan;

    bool readInfoData(istream * input);
    bool readCommonData(istream * input);
    bool readStationData(istream * input);
    bool readSourceData(istream * input);
    bool readEOPData(istream * input);
    bool readSpacecraftData(istream * input);
    bool readScanData(istream * input);
    bool readPolynomialSamples(istream * input);
    bool fillPolyRow(f64* vals, string line, int npoly);
    axistype getMount(string mount);

    int modelmjd, modelstartseconds, numstations, numsources, numscans, numeops, numspacecraft;
    int polyorder, modelincsecs;
    long long estimatedbytes;
    bool opensuccess;
    Configuration * config;
    string calcfilename, imfilename;
    double ** binomialcoeffs;
    double * maxrate;
    station * stationtable;
    source * sourcetable;
    eop * eoptable;
    spacecraft * spacecrafttable;
    scan * scantable;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
