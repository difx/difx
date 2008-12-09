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
#ifndef UVW_H
#define UVW_H

#include <string>
#include <fstream>
#include <cstdlib>
#include <iostream>
#include <vector>

//forward declaration of Configuration
class Configuration;

using namespace std;

/**
 @class Uvw
 @brief Container class for geometric model information

 Contains all u,v and w terms as loaded from the .uvw file and allows easy access via interpolation to any point in the experiment
 @author Adam Deller
 */
class Uvw{
public:
 /**
  * Constructor: Loads the uvw information from the supplied file into memory
  * @param config The configuration object, containing all information about the duration and setup of this correlation
  * @param uvwfilename The file to load from
  * @param nameonly Whether to only load the scan source names to save memory (if the model info will not be needed)
  */
  Uvw(Configuration * config, string uvwfilename, bool nameonly);

  ~Uvw();

 /**
  * Calculates the projected baseline vector (u,v,w) from two telescopes at a given point in time
  * @param t1name The first telescope's name
  * @param t2name The second telescope's name
  * @param mjd The integer Modified Julian Date of the desired time
  * @param seconds The offset in seconds from this MJD
  * @param buvw The baseline u, v and w in metres, set by this function
  */
  void interpolateUvw(string t1name, string t2name, int mjd, float seconds, float buvw[]);

 /**
  * Finds the source corresponding to the active scan at a given time
  * @param mjd The integer Modified Julian Date of the desired time
  * @param sec The offset in seconds from this MJD
  * @param toset The string which is set to the source name for this time
  */
  void getSourceName(int mjd, int sec, string & toset);

 /**
  * Returns whether the UVW file was opened and parsed successfully
  * @return Whether this UVW object was successfully created
  */
  inline bool openSuccess() { return opensuccess; }

 /**
  * Gets the index of the source corresponding to the active scan at a given time
  * @param mjd The integer Modified Julian Date of the desired time
  * @param sec The offset in seconds from this MJD
  * @return Index for the source active at this time, referring to the order they are listed in the UVW file header
  */
  inline int getSourceIndex(int mjd, int sec) { return scanindices[scannumbers.at(int(((mjd-expermjd)*86400 + sec-experstartseconds)/uvwincrementsecs))]; }

 /**
  * Gets the number of sources in the UVW file
  * @return The number of sources in the UVW file
  */
  inline int getNumSources() { return numsources; }

 /**
  * Gets the number of stations in the UVW file
  * @return The number of stations in the UVW file
  */
  inline int getNumStations() { return numstations; }

 /**
  * Gets the number of points in the UVW file
  * @return The number of points in the UVW file
  */
  inline int getNumUVWPoints() { return numuvwpoints; }

 /**
  * Gets the name of the requested station
  * @param index The index of the station required
  * @return The name of the requested station
  */
  inline string getStationName(int index) { return stations[index].name; }

 /**
  * Gets the X coordinate, in metres, of the requested station
  * @param index The index of the station required
  * @return The X coordinate, in metres, of the requested station
  */
  inline double getStationX(int index) { return stations[index].x; }

 /**
  * Gets the Y coordinate, in metres, of the requested station
  * @param index The index of the station required
  * @return The Y coordinate, in metres, of the requested station
  */
  inline double getStationY(int index) { return stations[index].y; }

 /**
  * Gets the Z coordinate, in metres, of the requested station
  * @param index The index of the station required
  * @return The Z coordinate, in metres, of the requested station
  */
  inline double getStationZ(int index) { return stations[index].z; }

 /**
  * Gets the mount type of the requested station
  * @param index The index of the station required
  * @return The mount type of the requested station, RPFITS mapping (0=az/el,1=ra/dec,2=orbital,3=xy)
  */
  inline int getStationMount(int index) { return stations[index].mount; }

 /**
  * Gets the name of the requested source
  * @param index The index of the source required
  * @return The name of the requested station
  */
  inline string getSourceName(int index) { return scansources[index].name; }

 /**
  * Gets the right ascension of the requested source
  * @param index The index of the source required
  * @return The right ascension of the requested station, in radians
  */
  inline double getSourceRA(int index) { return scansources[index].ra; }

 /**
  * Gets the declination of the requested source
  * @param index The index of the source required
  * @return The declination of the requested station, in radians
  */
  inline double getSourceDec(int index) { return scansources[index].dec; }

private:
  typedef struct {
    string name;
    double x,y,z;
    int mount; //0=altaz, 1 = radec, 2=orbital, 3=xy
  } telescope;

  typedef struct {
    string name;
    double ra, dec;
  } source;

  int getMountInt(string mount);

  int expermjd, experstartseconds, numstations, numscans, numuvwpoints, numsources;
  double uvwincrementsecs;
  bool uvwread, opensuccess;
  vector<int> scannumbers;
  source * scansources;
  telescope * stations;
  int * scanindices;
  int * numpoints;
  int * scanstartpoints;
  double **** uvw;
};

#endif
