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
#include "uvw.h"
#include "configuration.h"
#include <difxmessage.h>

Uvw::Uvw(Configuration * config, string uvwfilename, bool nameonly)
  : uvwread(!nameonly)
{
  int year, month, day, hour, minute, second, at, next;
  string line;
  bool found;
  char message[80];

  ifstream input(uvwfilename.c_str());
  if(!input.is_open() || input.bad()) {
    sprintf(message, "Error opening uvw file %s - aborting!!!", uvwfilename.c_str());
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_FATAL);
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  config->getinputline(&input, &line, "START YEAR");
  year = atoi(line.c_str());
  config->getinputline(&input, &line, "START MONTH");
  month = atoi(line.c_str());
  config->getinputline(&input, &line, "START DAY");
  day = atoi(line.c_str());
  config->getinputline(&input, &line, "START HOUR");
  hour = atoi(line.c_str());
  config->getinputline(&input, &line, "START MINUTE");
  minute = atoi(line.c_str());
  config->getinputline(&input, &line, "START SECOND");
  second = atoi(line.c_str());

  config->getMJD(expermjd, experstartseconds, year, month, day, hour, minute, second);

  config->getinputline(&input, &line, "INCREMENT");
  uvwincrementsecs = atof(line.c_str());

  //read in the telescope info
  config->getinputline(&input, &line, "NUM TELESCOPES");
  numstations = atoi(line.c_str());
  stations = new telescope[numstations];
  for(int i=0;i<numstations;i++)
  {
    config->getinputline(&input, &stations[i].name, "TELESCOPE ", i);
    //trim the whitespace off the end
    while((stations[i].name).at((stations[i].name).length()-1) == ' ')
      stations[i].name = (stations[i].name).substr(0, (stations[i].name).length()-1);
    config->getinputline(&input, &line, "TELESCOPE ", i);
    stations[i].mount = getMountInt(line);
    config->getinputline(&input, &line, "TELESCOPE ", i);
    stations[i].x = atof(line.c_str());
    config->getinputline(&input, &line, "TELESCOPE ", i);
    stations[i].y = atof(line.c_str());
    config->getinputline(&input, &line, "TELESCOPE ", i);
    stations[i].z = atof(line.c_str());
  }

  //read in scan information
  numuvwpoints = 0;
  numsources = 0;
  config->getinputline(&input, &line, "NUM SCANS");
  numscans = atoi(line.c_str()); 
  numpoints = new int[numscans];
  scansources = new source[numscans];
  scanindices = new int[numscans];
  scanstartpoints = new int[numscans];
  if(uvwread) //only create the uvw information if we've been asked to, otherwise save time and memory
    uvw = new double***[numscans];

  if(!nameonly)
    cout << "Scan information has been read in - numscans is " << numscans << endl;

  for(int i=0;i<numscans;i++)
  {
    config->getinputline(&input, &line, "SCAN ", i);
    numpoints[i] = atoi(line.c_str());
    numuvwpoints += numpoints[i];
    if(uvwread)
      uvw[i] = new double**[numpoints[i] + 3]; //since we have -1, numpoints and numpoints+1 as well
    config->getinputline(&input, &line, "SCAN ", i);
    scanstartpoints[i] = atoi(line.c_str());
    config->getinputline(&input, &(scansources[numsources].name), "SCAN ", i); 

    //work out if we already have this source in the list
    found = false;
    for(int j=0;j<numsources;j++)
    {
      if((scansources[numsources].name).compare(scansources[j].name) == 0)
      {
        found = true;
        scanindices[i] = j;
      }
    }

    if(!found)
    {
      //grab the info for this source and store it
      config->getinputline(&input, &line, "SCAN ", i);
      scansources[numsources].ra = atof(line.c_str());
      config->getinputline(&input, &line, "SCAN ", i);
      scansources[numsources].dec = atof(line.c_str());
      scanindices[i] = numsources;
      numsources++;
    }
    else
    {
      //still have to skip over the ra and dec lines
      config->getinputline(&input, &line, "SCAN ", i);
      config->getinputline(&input, &line, "SCAN ", i);
    }

    for(int j=0;j<numpoints[i]+3;j++)
    {
      at = 0;
      config->getinputline(&input, &line, "RELATIVE INC ", j-1);
      if(uvwread)
        uvw[i][j] = new double*[numstations];
      for(int k=0;k<numstations;k++)
      {
        if(uvwread)
          uvw[i][j][k] = new double[3]; //u, v, w
        for(int l=0;l<3;l++)
        {
          next = line.find_first_of('\t', at);
          if(uvwread)
            uvw[i][j][k][l] = atof((line.substr(at, next-at)).c_str());
          at = next+1;
        }
      }
      if(j<numpoints[i])
        scannumbers.push_back(i);
    }
  }
}


Uvw::~Uvw()
{
  int count, subcount;

  count = 0;
  if(uvwread)
  {
    for(int i=0;i<numscans;i++)
    {
      subcount = 0;
      while((count < numuvwpoints) && (scannumbers.at(count) == i))
      {
        for(int k=0;k<numstations;k++)
        {
          delete [] uvw[i][subcount][k];
        }
        delete [] uvw[i][subcount];
        subcount++;
        count++;
      }
      for(int j=0;j<3;j++)
      {
        for(int k=0;k<numstations;k++)
        {
          delete [] uvw[i][subcount+j][k];
        }
        delete [] uvw[i][subcount+j];
      }
      delete [] uvw[i];
    }
    delete [] uvw;
  }

  delete [] numpoints;
  delete [] stations;
  delete [] scansources;
  delete [] scanindices;
  delete [] scanstartpoints;
}

void Uvw::interpolateUvw(string t1name, string t2name, int mjd, float seconds, float buvw[])
{
  double offsetsec, distance, distancesquared;
  float tuvw[2][3]; //individual telescope u, v ,w
  int scannumber, nearestindex;
  double a, b, c;
  int stationindex[2];
  stationindex[0] = -1;
  stationindex[1] = -1;
  
  for(int i=0;i<numstations;i++)
  {
    if(stations[i].name == t1name)
      stationindex[0] = i;
    if(stations[i].name == t2name)
      stationindex[1] = i;
  }
  if(stationindex[0] < 0 || stationindex[1] < 0)
  {
    char message[128];
    sprintf(message, "Error - one of the telescope %s or %s could not be found in the uvw file!!!", t1name.c_str(), t2name.c_str());
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
    buvw[0] = 0;
    buvw[1] = 0;
    buvw[2] = 0;
    return;
  }

  //work out the offsets from known points so we can interpolate
  offsetsec = (mjd-expermjd)*86400 + seconds - experstartseconds;
  scannumber = scannumbers.at(int(offsetsec/uvwincrementsecs + 0.5));
  nearestindex = int(offsetsec/uvwincrementsecs + 0.5) - scanstartpoints[scannumber] + 1;
  if(nearestindex <= 0)
    nearestindex = 1;
  if(nearestindex > numpoints[scannumber]+1)
    nearestindex = numpoints[scannumber]+1;
  distance = offsetsec/uvwincrementsecs - (nearestindex + scanstartpoints[scannumber] - 1);
  distancesquared = distance*distance;

  for(int i=0;i<2;i++)
  {
    for(int j=0;j<3;j++)
    {
      //calculate the quadratic parameters for this telescope axis
      a = (uvw[scannumber][nearestindex+1][stationindex[i]][j] + uvw[scannumber][nearestindex-1][stationindex[i]][j])/2 - uvw[scannumber][nearestindex][stationindex[i]][j];
      b = (uvw[scannumber][nearestindex+1][stationindex[i]][j] - uvw[scannumber][nearestindex-1][stationindex[i]][j])/2;
      c = uvw[scannumber][nearestindex][stationindex[i]][j];

      tuvw[i][j] = float(a*distancesquared + b*distance + c);
    }
  }
  
  for(int i=0;i<3;i++)
  {
    buvw[i] = tuvw[0][i] - tuvw[1][i];
  }
}

void Uvw::getSourceName(int mjd, int sec, string & toset)
{
  int index = int(((mjd-expermjd)*86400 + sec-experstartseconds)/uvwincrementsecs);
  if(index < 0)
  {
    //NOTE -- the following error is commented out since this case seems to happen fairly frequently
    //cerr << "Error - attempting to get a source name from mjd " << mjd << "." << double(sec)/86400.0 << ", when the uvw file begins at " << expermjd << "." << double(experstartseconds)/86400.0 << ", will take first source" << endl;
    index = 0;
  }
  else if (index >= numuvwpoints)
  {
    //cerr << "Error - attempting to get a source name from mjd " << mjd << "." << double(sec)/86400.0 << ", when the uvw file ends at " << expermjd << "." << double(experstartseconds + numuvwpoints*uvwincrementsecs)/86400.0 << ", will take last source" << endl;
    index = numuvwpoints-1;
  }
  toset = scansources[scanindices[scannumbers.at(index)]].name;
}

//utility routine which returns an integer which FITS expects based on the type of mount
int Uvw::getMountInt(string mount)
{
  if(mount.compare("azel") == 0 || mount.compare("altz") == 0) //its an azel mount
    return 0;
    
  if(mount.compare("equa") == 0 || mount.compare("hadec") == 0) //equatorial mount
    return 1;
    
  if(mount.compare("orbi") == 0) //orbital mount
    return 2;
    
  if((mount.substr(0,2)).compare("xy") == 0) //xy mount
    return 3;
    
  //otherwise unknown
  difxMessageSendDifxAlert("Unknown mount type: Assuming Az-El", DIFX_ALERT_LEVEL_WARNING);
  return 0;
}
