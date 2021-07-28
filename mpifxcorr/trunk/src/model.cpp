/***************************************************************************
 *   Copyright (C) 2009-2021 by Adam Deller                                *
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

#include <sstream>
#include <strings.h>
#include "architecture.h"
#include "configuration.h"
#include "alert.h"
#include "model.h"
#include "sysutil.h"

Model::Model(Configuration * conf, string cfilename)
  : config(conf), calcfilename(cfilename)
{
  opensuccess = true;

  istream * input = config->mpiGetFileContent(calcfilename.c_str());
  if (input == NULL)
  {
    cfatal << startl << "Error opening model file " << calcfilename << " - aborting!!!" << endl;
    opensuccess = false;
  }

  estimatedbytes = 0;
  stationtable = 0;
  sourcetable = 0;
  eoptable = 0;
  spacecrafttable = 0;
  scantable = 0;
  binomialcoeffs = 0;
  maxrate = 0;

  //read the files
  if(opensuccess) 
    opensuccess = readInfoData(input);
  if(opensuccess)
    opensuccess = readCommonData(input);
  if(opensuccess)
    opensuccess = readStationData(input);
  if(opensuccess)
    opensuccess = readSourceData(input);
  if(opensuccess)
    opensuccess = readScanData(input);
  if(opensuccess)
    opensuccess = readEOPData(input);
  if(opensuccess)
    opensuccess = readSpacecraftData(input);
  if(opensuccess)
    opensuccess = readPolynomialSamples(input);

  if(opensuccess && polyorder > MAX_POLY_ORDER) {
    cfatal << startl << "Polynomial order " << polyorder << " is greater than the maximum allowed (" << MAX_POLY_ORDER << ") - aborting!" << endl;
    opensuccess = false;
  }

  //allocate some arrays
  if(opensuccess) {
    binomialcoeffs = new double*[polyorder+1];
    for(int i=0;i<polyorder+1;i++) {
      binomialcoeffs[i] = new double[i+1];
      binomialcoeffs[i][0] = 1;
      binomialcoeffs[i][i] = 1;
      for(int j=1;j<i;j++)
        binomialcoeffs[i][j] = binomialcoeffs[i-1][j-1] + binomialcoeffs[i-1][j];
    }
  }

  delete input;
}

Model::~Model()
{
  if(binomialcoeffs) {
    for(int i=0;i<polyorder+1;i++)
      delete [] binomialcoeffs[i];
    delete [] binomialcoeffs;
  }
  if(stationtable)
    delete [] stationtable;
  if(sourcetable)
    delete [] sourcetable;
  if(eoptable)
    delete [] eoptable;
  if(spacecrafttable) {
    for(int i=0;i<numspacecraft;i++) {
      delete [] spacecrafttable[i].samplemjd;
      delete [] spacecrafttable[i].x;
      delete [] spacecrafttable[i].y;
      delete [] spacecrafttable[i].z;
      delete [] spacecrafttable[i].vx;
      delete [] spacecrafttable[i].vy;
      delete [] spacecrafttable[i].vz;
    }
    delete [] spacecrafttable;
  }
  if(scantable) {
    for(int i=0;i<numscans;i++) {
      for(int j=0;j<scantable[i].nummodelsamples;j++) {
        for(int k=0;k<scantable[i].numphasecentres+1;k++) {
          for(int l=0;l<numstations;l++) {
            vectorFree(scantable[i].u[j][k][l]);
            vectorFree(scantable[i].v[j][k][l]);
            vectorFree(scantable[i].w[j][k][l]);
            vectorFree(scantable[i].delay[j][k][l]);
            vectorFree(scantable[i].wet[j][k][l]);
            vectorFree(scantable[i].dry[j][k][l]);
            vectorFree(scantable[i].adj[j][k][l]);
            vectorFree(scantable[i].az[j][k][l]);
            vectorFree(scantable[i].elcorr[j][k][l]);
            vectorFree(scantable[i].elgeom[j][k][l]);
            vectorFree(scantable[i].parang[j][k][l]);
          }
          delete [] scantable[i].u[j][k];
          delete [] scantable[i].v[j][k];
          delete [] scantable[i].w[j][k];
          delete [] scantable[i].delay[j][k];
          delete [] scantable[i].wet[j][k];
          delete [] scantable[i].dry[j][k];
          delete [] scantable[i].adj[j][k];
          delete [] scantable[i].az[j][k];
          delete [] scantable[i].elcorr[j][k];
          delete [] scantable[i].elgeom[j][k];
          delete [] scantable[i].parang[j][k];
        }
        for(int k=0;k<numstations;k++)
          delete [] scantable[i].clock[j][k];
        delete [] scantable[i].u[j];
        delete [] scantable[i].v[j];
        delete [] scantable[i].w[j];
        delete [] scantable[i].delay[j];
        delete [] scantable[i].wet[j];
        delete [] scantable[i].dry[j];
        delete [] scantable[i].adj[j];
        delete [] scantable[i].az[j];
        delete [] scantable[i].elcorr[j];
        delete [] scantable[i].elgeom[j];
        delete [] scantable[i].parang[j];
        delete [] scantable[i].clock[j];
      }
      delete [] scantable[i].u;
      delete [] scantable[i].v;
      delete [] scantable[i].w;
      delete [] scantable[i].delay;
      delete [] scantable[i].wet;
      delete [] scantable[i].dry;
      delete [] scantable[i].adj;
      delete [] scantable[i].az;
      delete [] scantable[i].elcorr;
      delete [] scantable[i].elgeom;
      delete [] scantable[i].parang;
      delete [] scantable[i].clock;
      delete [] scantable[i].phasecentres;
    }
    delete [] scantable;
  }
  if(maxrate)
    delete maxrate;
}

void Model::updateClock(int antennaindex, int order, double * deltaclock)
{
  string antennaname = stationtable[antennaindex].name;
  double refmjd = modelmjd + (double)modelstartseconds/86400.0;

  addClockTerms(antennaname, refmjd, order, deltaclock, true);
}

bool Model::interpolateUVW(int scanindex, double offsettime, int antennaindex1, int antennaindex2, int scansourceindex, double* uvw)
{
  int scansample, polyoffset;
  double deltat, tempuvw;
  double * coeffs;
  f64 tpowerarray[MAX_POLY_ORDER+1];

  //work out the correct sample and offset from that sample
  polyoffset = (modelmjd - scantable[scanindex].polystartmjd)*86400 + modelstartseconds + scantable[scanindex].offsetseconds - scantable[scanindex].polystartseconds;
  scansample = int((offsettime+polyoffset)/double(modelincsecs));
  if(scansample == scantable[scanindex].nummodelsamples && ((offsettime+polyoffset - scansample*modelincsecs) < modelincsecs/1000000.0)) {
    //Asked for a value at the exact end of a scan, which is ok, but need to adjust or check below would complain
    scansample--;
  }

  if(scansample < 0 || scansample >= scantable[scanindex].nummodelsamples)
    return false;
  deltat = offsettime+polyoffset - scansample*modelincsecs;

  tpowerarray[0] = 1.0;
  for(int i=0;i<polyorder;i++)
    tpowerarray[i+1] = tpowerarray[i]*deltat;

  //calculate the uvw values
  coeffs = scantable[scanindex].u[scansample][scansourceindex][antennaindex1];
  vectorDotProduct_f64(tpowerarray, coeffs, polyorder+1, &(uvw[0]));
  coeffs = scantable[scanindex].u[scansample][scansourceindex][antennaindex2];
  vectorDotProduct_f64(tpowerarray, coeffs, polyorder+1, &tempuvw);
  uvw[0] = uvw[0] - tempuvw;
  coeffs = scantable[scanindex].v[scansample][scansourceindex][antennaindex1];
  vectorDotProduct_f64(tpowerarray, coeffs, polyorder+1, &(uvw[1]));
  coeffs = scantable[scanindex].v[scansample][scansourceindex][antennaindex2];
  vectorDotProduct_f64(tpowerarray, coeffs, polyorder+1, &tempuvw);
  uvw[1] = uvw[1] - tempuvw;
  coeffs = scantable[scanindex].w[scansample][scansourceindex][antennaindex1];
  vectorDotProduct_f64(tpowerarray, coeffs, polyorder+1, &(uvw[2]));
  coeffs = scantable[scanindex].w[scansample][scansourceindex][antennaindex2];
  vectorDotProduct_f64(tpowerarray, coeffs, polyorder+1, &tempuvw);
  uvw[2] = uvw[2] - tempuvw;
  return true;
}

bool Model::calculateDelayInterpolator(int scanindex, f64 offsettime, f64 timespan, int numincrements, int antennaindex, int scansourceindex, int order, f64 * delaycoeffs)
{
  int scansample, status, polyoffset;
  double deltat;
  double delaysamples[3];
  f64 tpowerarray[MAX_POLY_ORDER+1];

  //check that order is ok
  if(order < 0 || order > 2) {
    csevere << startl << "Model delay interpolator asked to produce " << order << "th order output - can only do 0, 1 or 2!" << endl;
    return false;
  } 

  //work out the correct sample and offset for the midrange of the timespan
  polyoffset = (modelmjd - scantable[scanindex].polystartmjd)*86400 + modelstartseconds + scantable[scanindex].offsetseconds - scantable[scanindex].polystartseconds;
  scansample = int((offsettime+polyoffset)/double(modelincsecs));
  if(scansample == scantable[scanindex].nummodelsamples && ((offsettime+polyoffset - scansample*modelincsecs) < 1e-6)) {
    //Asked for a value at the exact end of a scan, which is ok, but need to adjust or check below would complain
    scansample--;
  }
  if(scansample < 0 || scansample >= scantable[scanindex].nummodelsamples) {
    cwarn << startl << "Model delay interpolator was asked to produce results for scan " << scanindex << " from outside the scans valid range (worked out scansample " << scansample << ", when numsamples was " << scantable[scanindex].nummodelsamples << ")" << endl;
    return false;
  }
  deltat = offsettime+polyoffset+timespan/2.0 - scansample*modelincsecs;
  tpowerarray[0] = 1.0;
  for(int i=0;i<polyorder;i++)
    tpowerarray[i+1] = tpowerarray[i]*deltat;
  
  //zero-th order interpolation - the simplest case
  if(order==0) {
    //cout << "Model is calculating a zero-th order delay, with offsettime " << offsettime << ", scansourceindex " << scansourceindex << ", scan " << scanindex << ", and polyoffset " << polyoffset << ", scansample was " << scansample << endl;
    //cout << "tpowerarray[0],[1],[2] is " << tpowerarray[0] << "," << tpowerarray[1] << "," << tpowerarray[2] << ", delay[0][1][2] is " << scantable[scanindex].delay[scansample][scansourceindex][antennaindex][0] << "," << scantable[scanindex].delay[scansample][scansourceindex][antennaindex][1] << "," << scantable[scanindex].delay[scansample][scansourceindex][antennaindex][2] << endl;
    status = vectorDotProduct_f64(tpowerarray, scantable[scanindex].delay[scansample][scansourceindex][antennaindex], polyorder+1, delaycoeffs);
    if (status != vecNoErr)
      cerror << startl << "Error calculating zero-th order interpolation in Model" << endl;
    return true; //note return
  }

  //If not 0th order interpolation, need to fill out all 3 spots
  status = vectorDotProduct_f64(tpowerarray, scantable[scanindex].delay[scansample][scansourceindex][antennaindex], polyorder+1, &(delaysamples[1]));
  if (status != vecNoErr)
    cerror << startl << "Error calculating sample 1 for interpolation in Model" << endl;
  deltat = offsettime+polyoffset - scansample*modelincsecs;
  for(int i=0;i<polyorder;i++)
    tpowerarray[i+1] = tpowerarray[i]*deltat;
  status = vectorDotProduct_f64(tpowerarray, scantable[scanindex].delay[scansample][scansourceindex][antennaindex], polyorder+1, &(delaysamples[0]));
  if (status != vecNoErr)
    cerror << startl << "Error calculating sample 0 for interpolation in Model" << endl;
  deltat = offsettime+polyoffset + timespan - scansample*modelincsecs;
  for(int i=0;i<polyorder;i++)
    tpowerarray[i+1] = tpowerarray[i]*deltat;
  status = vectorDotProduct_f64(tpowerarray, scantable[scanindex].delay[scansample][scansourceindex][antennaindex], polyorder+1, &(delaysamples[2]));
  if (status != vecNoErr)
    cerror << startl << "Error calculating sample 2 for interpolation in Model" << endl;
  //cout << "In calculateinterpolator, sample0 was " << delaysamples[0] << ", sample1 was " << delaysamples[1] << ", sample2 was " << delaysamples[2] << ", timespan was " << timespan << endl;
 
  //linear interpolation
  if(order==1) {
    delaycoeffs[0] = (delaysamples[2]-delaysamples[0])/numincrements;
    delaycoeffs[1] = delaysamples[0] + (delaysamples[1] - (delaycoeffs[0]*numincrements/2.0 + delaysamples[0]))/3.0;
    return true; //note return
  }

  //quadratic interpolation
  delaycoeffs[0] = (2.0*delaysamples[0]-4.0*delaysamples[1]+2.0*delaysamples[2])/(numincrements*numincrements);
  delaycoeffs[1] = (-3.0*delaysamples[0]+4.0*delaysamples[1]-delaysamples[2])/numincrements;
  delaycoeffs[2] = delaysamples[0];
  //cout << "Interpolator produced coefficients " << delaycoeffs[0] << ", " << delaycoeffs[1] << ", " << delaycoeffs[2] << " from samples " << delaysamples[0] << ", " << delaysamples[1] << ", " << delaysamples[2] << " for a time range " << timespan << endl;
  return true;
}

bool Model::addClockTerms(string antennaname, double refmjd, int order, double * terms, bool isupdate)
{
  double clockdistance;
  double * clockdt;

  if(scantable == 0) //hasn't been allocated yet
    return false; //note exit here

  if(order > polyorder) {
    cfatal << startl << "Clock order for antenna " << antennaname << " is greater than the model polynomial order - this cannot be supported! You must regenerate the model with a polynomial order at least " << order << endl;
    return false;
  }

  for(int i=0;i<numstations;i++)
  {
    if(stationtable[i].name.compare(antennaname) == 0)
    {
      for(int j=0;j<numscans;j++)
      {
        if(scantable[j].nummodelsamples == 0) //the IM information has not yet been read
          return false; //note exit here

        clockdt = new double[order+1];
        for(int k=0;k<scantable[j].nummodelsamples;k++)
        {
          clockdistance = 86400.0*(scantable[j].polystartmjd + ((double)scantable[j].polystartseconds)/86400.0 - refmjd) + k*double(modelincsecs);
          clockdt[0] = 1.0;
          for(int l=1;l<=order;l++)
            clockdt[l] = clockdt[l-1]*clockdistance;
          if(!isupdate) {
            for(int l=0;l<=polyorder;l++)
              scantable[j].clock[k][i][l] = 0.0;
          }
          else { //first subtract the old clock model out of delay
            for(int p=0;p<scantable[j].numphasecentres+1;p++)
            {
              for(int l=0;l<=polyorder;l++)
              {
                scantable[j].delay[k][p][i][l] -= scantable[j].clock[k][i][l];
              }
            }
          }
          for(int l=0;l<=order;l++)
          {
            for(int m=l;m<=order;m++)
              scantable[j].clock[k][i][l] += terms[m]*clockdt[m-l]*binomialcoeffs[m][m-l];
          }

          //now update the overall delay model too
          for(int p=0;p<scantable[j].numphasecentres+1;p++)
          {
            for(int l=0;l<=polyorder;l++)
            {
              scantable[j].delay[k][p][i][l] += scantable[j].clock[k][i][l];
            }
          }
        }
        delete [] clockdt;
      }
      return true; //note exit here
    }
  }

  return false; //mustn't have found the station
}

bool Model::readInfoData(istream * input)
{
  string line = "";
  string key = "";
  //nothing here is worth saving, so just skip it all
  config->getinputline(input, &line, "JOB ID");
  config->getinputline(input, &line, "JOB START TIME");
  config->getinputline(input, &line, "JOB STOP TIME");
  config->getinputkeyval(input, &key, &line);
  if(key.find("DELAY MODEL") != string::npos) { //look for the DELAY MODEL line, skip it if present
    config->getinputline(input, &line, "DUTY CYCLE");
  }
  else {
    if(key.find("DUTY CYCLE") == string::npos) {
      cerror << startl << "Went looking for DUTY CYCLE (or maybe DELAY MODEL), but got " << key << endl;
    }
  }
  config->getinputline(input, &line, "OBSCODE");
  config->setObsCode(line);
  config->getinputline(input, &line, "DIFX VERSION");
  cinfo << startl << "DIFX VERSION = " << line << endl;
  config->getinputkeyval(input, &key, &line);
  if(key.find("DIFX LABEL") != string::npos) { //look for the DIFX LABEL line, skip it if present
    cinfo << startl << "DIFX LABEL = " << line << endl;
    config->getinputline(input, &line, "SUBJOB ID");
  }
  else {
    if(key.find("SUBJOB ID") == string::npos) {
      cerror << startl << "Went looking for DIFX LABEL (or maybe SUBJOB ID), but got " << key << endl;
    }
  }
  config->getinputline(input, &line, "SUBARRAY ID");
  return true;
}

bool Model::readCommonData(istream * input)
{
  int year, month, day, hour, minute, second;
  double mjd;
  string key = "";
  string line = "";

  //Get the start time
  config->getinputkeyval(input, &key, &line);
  if(key.find("VEX") != string::npos) { //look for the VEX line, skip it if present
    config->getinputline(input, &line, "START MJD");
  }
  else {
    if(key.find("START MJD") == string::npos) {
      cerror << startl << "Went looking for START MJD (or maybe VEX FILE), but got " << key << endl;
    }
  }
  mjd = atof(line.c_str());
  config->getinputline(input, &line, "START YEAR");
  year = atoi(line.c_str());
  config->getinputline(input, &line, "START MONTH");
  month = atoi(line.c_str());
  config->getinputline(input, &line, "START DAY");
  day = atoi(line.c_str());
  config->getinputline(input, &line, "START HOUR");
  hour = atoi(line.c_str());
  config->getinputline(input, &line, "START MINUTE");
  minute = atoi(line.c_str());
  config->getinputline(input, &line, "START SECOND");
  second = atoi(line.c_str());
  config->getMJD(modelmjd, modelstartseconds, year, month, day, hour, minute, second);
  if(fabs(mjd - ((double)modelmjd+(double)modelstartseconds/86400.0)) > 0.000001)
    cwarn << startl << " START MJD does not seem to agree with START YEAR/MONTH/.../SECOND..?" << mjd-int(mjd) << ", " << (double)modelstartseconds/86400.0 << endl;

  //ignore the next two lines, not relevant to the correlator
  config->getinputline(input, &line, "SPECTRAL AVG");
  config->getinputline(input, &line, "TAPER FUNCTION");
  return true;
}

bool Model::readStationData(istream * input)
{
  string line = "";

  config->getinputline(input, &line, "NUM TELESCOPES");
  numstations = atoi(line.c_str());
  stationtable = new station[numstations];
  for(int i=0;i<numstations;i++) {
    config->getinputline(input, &(stationtable[i].name), "TELESCOPE ", i);
    //trim the whitespace off the end
    while((stationtable[i].name).at((stationtable[i].name).length()-1) == ' ')
      stationtable[i].name = (stationtable[i].name).substr(0, (stationtable[i].name).length()-1);
    config->getinputline(input, &line, "TELESCOPE ", i);
    stationtable[i].mount = getMount(line);
    config->getinputline(input, &line, "TELESCOPE ", i);
    stationtable[i].axisoffset = atoi(line.c_str());
    config->getinputline(input, &line, "TELESCOPE ", i);
    stationtable[i].x = atoi(line.c_str());
    config->getinputline(input, &line, "TELESCOPE ", i);
    stationtable[i].y = atoi(line.c_str());
    config->getinputline(input, &line, "TELESCOPE ", i);
    stationtable[i].z = atoi(line.c_str());
    config->getinputline(input, &line, "TELESCOPE ", i); //ignore this, its the shelf
  }
  maxrate = new double[numstations];
  for(int i=0;i<numstations;i++)
    maxrate[i] = 0;
  return true;
}
bool Model::readSourceData(istream * input)
{
  string line = "";

  config->getinputline(input, &line, "NUM SOURCES");
  numsources = atoi(line.c_str());
  sourcetable = new source[numsources];
  for(int i=0;i<numsources;i++) {
    sourcetable[i].index = i;
    config->getinputline(input, &(sourcetable[i].name), "SOURCE ", i);
    //trim the whitespace off the end
    while((sourcetable[i].name).at((sourcetable[i].name).length()-1) == ' ')
      sourcetable[i].name = (sourcetable[i].name).substr(0, (sourcetable[i].name).length()-1);
    config->getinputline(input, &line, "SOURCE ", i);
    sourcetable[i].ra = atof(line.c_str());
    config->getinputline(input, &line, "SOURCE ", i);
    sourcetable[i].dec = atof(line.c_str());
    config->getinputline(input, &(sourcetable[i].calcode), "SOURCE ", i);
    config->getinputline(input, &line, "SOURCE ", i);
    sourcetable[i].qual = atoi(line.c_str());
  }
  return true;
}

bool Model::readScanData(istream * input)
{
  string line = "";

  config->getinputline(input, &line, "NUM SCANS");
  numscans = atoi(line.c_str());
  scantable = new scan[numscans];
  for(int i=0;i<numscans;i++) {
    scantable[i].nummodelsamples = 0; //shows that IM info not yet read
    config->getinputline(input, &scantable[i].identifier, "SCAN ", i);
    config->getinputline(input, &line, "SCAN ", i);
    scantable[i].offsetseconds = atoi(line.c_str());
    config->getinputline(input, &line, "SCAN ", i);
    scantable[i].durationseconds = atoi(line.c_str());
    config->getinputline(input, &(scantable[i].obsmodename), "SCAN ", i);
    config->getinputline(input, &line, "SCAN ", i);
    scantable[i].maxnsbetweenxcavg = atoi(line.c_str());
    config->getinputline(input, &line, "SCAN ", i);
    scantable[i].maxnsbetweenacavg = atoi(line.c_str());
    config->getinputline(input, &line, "SCAN ", i);
    scantable[i].pointingcentre = &(sourcetable[atoi(line.c_str())]);
    config->getinputline(input, &line, "SCAN ", i);
    scantable[i].numphasecentres = atoi(line.c_str());
    scantable[i].phasecentres = new source*[scantable[i].numphasecentres];
    scantable[i].pointingcentrecorrelated = false;
    for(int j=0;j<scantable[i].numphasecentres;j++) {
      config->getinputline(input, &line, "SCAN ", i);
      scantable[i].phasecentres[j] = &(sourcetable[atoi(line.c_str())]);
      if(scantable[i].phasecentres[j] == scantable[i].pointingcentre) {
        scantable[i].pointingcentrecorrelated = true;
        if(j != 0) {
          cfatal << startl << "If pointing centre is correlated, it must be the first phase centre - aborting!" << endl;
          return false;
        }
      }
    }
  }
  return true;
}

bool Model::readEOPData(istream * input)
{
  string line = "";

  config->getinputline(input, &line, "NUM EOPS");
  numeops = atoi(line.c_str());
  eoptable = new eop[numeops];
  for(int i=0;i<numeops;i++) {
    config->getinputline(input, &line, "EOP ", i);
    eoptable[i].mjd = atoi(line.c_str());
    config->getinputline(input, &line, "EOP ", i);
    eoptable[i].taiutc = atoi(line.c_str());
    config->getinputline(input, &line, "EOP ", i);
    eoptable[i].ut1utc = atof(line.c_str());
    config->getinputline(input, &line, "EOP ", i);
    eoptable[i].xpole = atof(line.c_str());
    config->getinputline(input, &line, "EOP ", i);
    eoptable[i].ypole = atof(line.c_str());
  }
  return true;
}

bool Model::readSpacecraftData(istream * input)
{
  string line = "";
  string key = "";

  config->getinputline(input, &line, "NUM SPACECRAFT");
  numspacecraft = atoi(line.c_str());
  spacecrafttable = new spacecraft[numspacecraft];
  for(int i=0;i<numspacecraft;i++) {
    config->getinputkeyval(input, &key, &line);
    if(key.find("FRAME") != string::npos) { //look for the FRAME line, skip it if present
      config->getinputkeyval(input, &key, &line);
    }
    if(key.find("SPACECRAFT ", i) == string::npos && key.find("NAME", i) == string::npos) {
      cerror << startl << "Went looking for SPACECRAFT " << i << " NAME (or maybe FRAME), but got " << key << endl;
    }
    spacecrafttable[i].name = line;
    config->getinputkeyval(input, &key, &line);
    if(key.find("EPHEM") != string::npos) { //look for the EPHEM line, skip it if present
      config->getinputkeyval(input, &key, &line);
    }
    if(key.find("ID") != string::npos) { //look for the ID line, skip it if present
      config->getinputkeyval(input, &key, &line);
    }
    if(key.find("SPACECRAFT ", i) == string::npos && key.find("ROWS", i) == string::npos) {
      cerror << startl << "Went looking for SPACECRAFT " << i << " ROWS, but got " << key << endl;
    }
    spacecrafttable[i].numsamples = atoi(line.c_str());
    spacecrafttable[i].samplemjd = new double[spacecrafttable[i].numsamples];
    spacecrafttable[i].x  = new double[spacecrafttable[i].numsamples];
    spacecrafttable[i].y  = new double[spacecrafttable[i].numsamples];
    spacecrafttable[i].z  = new double[spacecrafttable[i].numsamples];
    spacecrafttable[i].vx = new double[spacecrafttable[i].numsamples];
    spacecrafttable[i].vy = new double[spacecrafttable[i].numsamples];
    spacecrafttable[i].vz = new double[spacecrafttable[i].numsamples];
    for(int j=0;j<spacecrafttable[i].numsamples;j++) {
      config->getinputline(input, &line, "SPACECRAFT ", i);
      spacecrafttable[i].samplemjd[j] = atof(line.substr(0,17).c_str());
      spacecrafttable[i].x[j] = atof(line.substr(18,18).c_str());
      spacecrafttable[i].y[j] = atof(line.substr(37,18).c_str());
      spacecrafttable[i].z[j] = atof(line.substr(56,18).c_str());
      spacecrafttable[i].vx[j] = atof(line.substr(75,18).c_str());
      spacecrafttable[i].vy[j] = atof(line.substr(94,18).c_str());
      spacecrafttable[i].vz[j] = atof(line.substr(103,18).c_str());
    }
  }
  return true;
}

bool Model::readPolynomialSamples(istream * calcinput)
{
  int year, month, day, hour, minute, second, mjd, daysec;
  string line, key;
  bool polyok = true;
  bool hasXYZDerivatives = false;
  bool hasLMDerivatives = false;

  config->getinputline(calcinput, &imfilename, "IM FILENAME");

  istream * input = config->mpiGetFileContent(imfilename.c_str());
  if (input == NULL)
  {
    cfatal << startl << "Error opening IM file " << imfilename << " - aborting!!!" << endl;
    return false; //note exit here
  }
  //The following data is not needed here - just skim over it
  config->getinputline(input, &line, "CALC SERVER");
  config->getinputline(input, &line, "CALC PROGRAM");
  config->getinputline(input, &line, "CALC VERSION");

  //Now we get to some stuff that should be checked
  config->getinputline(input, &line, "START YEAR");
  year = atoi(line.c_str());
  config->getinputline(input, &line, "START MONTH");
  month = atoi(line.c_str());
  config->getinputline(input, &line, "START DAY");
  day = atoi(line.c_str());
  config->getinputline(input, &line, "START HOUR");
  hour = atoi(line.c_str());
  config->getinputline(input, &line, "START MINUTE");
  minute = atoi(line.c_str());
  config->getinputline(input, &line, "START SECOND");
  second = atoi(line.c_str());
  config->getMJD(mjd, daysec, year, month, day, hour, minute, second);
  if(!((mjd == modelmjd) && (daysec == modelstartseconds))) {
    cfatal << startl << "IM file and CALC file start dates disagree - aborting!!!" << endl;
    cfatal << startl << "MJD from IM file is " << mjd << ", from CALC file is " << modelmjd << ", IM file sec is " << daysec << ", CALC file sec is " << modelstartseconds << endl;
    return false;
  }

  //some important info on the polynomials
  config->getinputline(input, &line, "POLYNOMIAL ORDER");
  polyorder = atoi(line.c_str());
  config->getinputline(input, &line, "INTERVAL (SECS)");
  modelincsecs = atoi(line.c_str());

  //Another unimportant value
  config->getinputline(input, &line, "ABERRATION CORR");

  //now check the telescope names match
  config->getinputline(input, &line, "NUM TELESCOPES");
  if(numstations != atoi(line.c_str())) {
    cfatal << startl << "IM file and CALC file disagree on number of telescopes - aborting!!!" << endl;
    return false;
  }
  for(int i=0;i<numstations;i++) {
    config->getinputline(input, &line, "TELESCOPE ", i);
    if(line.compare(stationtable[i].name) != 0) {
      cfatal << startl << "IM file and CALC file disagree on telescope " << i << " name - aborting!!!" << endl;
      return false;
    }
  }

  //now loop through scans - make sure sources match, and store polynomials
  config->getinputline(input, &line, "NUM SCANS");
  if(numscans != atoi(line.c_str())) {
    cfatal << startl << "IM file and CALC file disagree on number of scans - aborting!!!" << endl;
    return false;
  }
  for(int i=0;i<numscans;i++) {
    config->getinputline(input, &line, "SCAN ", i);
    if(line.compare((scantable[i].pointingcentre)->name) != 0) {
      cfatal << startl << "IM file and CALC file disagree on scan " << i << " pointing centre (" << line << " vs. " << (scantable[i].pointingcentre)->name << ") - aborting!!!" << endl;
      return false;
    }
    config->getinputline(input, &line, "SCAN ", i);
    if(scantable[i].numphasecentres != atoi(line.c_str())) {
      cfatal << startl << "IM file and CALC file disagree on scan " << i << " number of phase centres (" << line << " vs. " << scantable[i].numphasecentres << ") - aborting!!!" << endl;
      return false;
    }
    for(int j=0;j<scantable[i].numphasecentres;j++) {
      config->getinputline(input, &line, "SCAN ", i);
      if(line.compare((scantable[i].phasecentres[j])->name) != 0) {
        cfatal << startl << "IM file and CALC file disagree on scan " << i << " phase centre " << j << " (" << line << " vs. " << (scantable[i].phasecentres[j])->name << ") - aborting!!!" << endl;
        return false;
      }
    }
    config->getinputline(input, &line, "SCAN ", i);
    scantable[i].nummodelsamples = atoi(line.c_str());
    scantable[i].u = new f64***[scantable[i].nummodelsamples];
    scantable[i].v = new f64***[scantable[i].nummodelsamples];
    scantable[i].w = new f64***[scantable[i].nummodelsamples];
    scantable[i].delay = new f64***[scantable[i].nummodelsamples];
    scantable[i].wet = new f64***[scantable[i].nummodelsamples];
    scantable[i].dry = new f64***[scantable[i].nummodelsamples];
    scantable[i].adj = new f64***[scantable[i].nummodelsamples];
    scantable[i].az = new f64***[scantable[i].nummodelsamples];
    scantable[i].elcorr = new f64***[scantable[i].nummodelsamples];
    scantable[i].elgeom = new f64***[scantable[i].nummodelsamples];
    scantable[i].parang = new f64***[scantable[i].nummodelsamples];
    scantable[i].clock = new f64**[scantable[i].nummodelsamples];
    for(int j=0;j<scantable[i].nummodelsamples;j++) {
      config->getinputkeyval(input, &key, &line);
      if(key.find("DELTA XYZ") != string::npos)
      {
        hasXYZDerivatives = true;
        config->getinputkeyval(input, &key, &line);
      }
      else if(key.find("DELTA LM") != string::npos)
      {
        hasLMDerivatives = true;
        config->getinputkeyval(input, &key, &line);
      }
      mjd = atoi(line.c_str());
      config->getinputline(input, &line, "SCAN ", i);
      daysec = atoi(line.c_str());
      if(j==0) {
        scantable[i].polystartmjd = mjd;
        scantable[i].polystartseconds = daysec;
      }
      else if((mjd-scantable[i].polystartmjd)*86400 + daysec-scantable[i].polystartseconds != j*modelincsecs) {
        cfatal << startl << "IM file has polynomials separated by a different amount than increment - aborting!" << endl;
        return false;
      }
      scantable[i].u[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].v[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].w[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].delay[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].wet[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].dry[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].adj[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].az[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].elcorr[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].elgeom[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].parang[j] = new f64**[scantable[i].numphasecentres+1];
      scantable[i].clock[j] = new f64*[numstations];
      for(int k=0;k<numstations;k++)
        scantable[i].clock[j][k] = vectorAlloc_f64(polyorder+1);
      for(int k=0;k<scantable[i].numphasecentres+1;k++) {
        scantable[i].u[j][k] = new f64*[numstations];
        scantable[i].v[j][k] = new f64*[numstations];
        scantable[i].w[j][k] = new f64*[numstations];
        scantable[i].delay[j][k] = new f64*[numstations];
        scantable[i].wet[j][k] = new f64*[numstations];
        scantable[i].dry[j][k] = new f64*[numstations];
        scantable[i].adj[j][k] = new f64*[numstations];
        scantable[i].az[j][k] = new f64*[numstations];
        scantable[i].elcorr[j][k] = new f64*[numstations];
        scantable[i].elgeom[j][k] = new f64*[numstations];
        scantable[i].parang[j][k] = new f64*[numstations];
        for(int l=0;l<numstations;l++) {
          estimatedbytes += 6*8*(polyorder + 1);
          scantable[i].u[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].v[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].w[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].delay[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].wet[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].dry[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].adj[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].az[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].elcorr[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].elgeom[j][k][l] = vectorAlloc_f64(polyorder+1);
          scantable[i].parang[j][k][l] = vectorAlloc_f64(polyorder+1);
          config->getinputline(input, &line, "SRC ", k);
          polyok = polyok && fillPolyRow(scantable[i].delay[j][k][l], line, polyorder+1);
          if(fabs(scantable[i].delay[j][k][l][1]) > fabs(maxrate[l]) &&
             (scantable[i].delay[j][k][l][0] > 0.0 || stationtable[l].mount == ORB))
            //ignore rates from Earth-based antennas when the delay is negative - they are junk
            maxrate[l] = fabs(scantable[i].delay[j][k][l][1]);
          config->getinputkeyval(input, &key, &line);
          if(key.find("DRY") != string::npos) { //look for optional "DRY" delay subcomponent
            polyok = polyok && fillPolyRow(scantable[i].dry[j][k][l], line, polyorder+1);
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("WET") != string::npos) { //look for optional "WET" delay subcomponent
            polyok = polyok && fillPolyRow(scantable[i].wet[j][k][l], line, polyorder+1);
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("ADJ") != string::npos) { //look for optional "ADJ" delay subcomponent (usually for phased arrays)
            polyok = polyok && fillPolyRow(scantable[i].adj[j][k][l], line, polyorder+1);
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("AZ") != string::npos) { //look for optional "AZ" azimuth specification
            polyok = polyok && fillPolyRow(scantable[i].az[j][k][l], line, polyorder+1);
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("CORR") != string::npos) { //look for optional "EL CORR" refraction-corrected elevation specification
            polyok = polyok && fillPolyRow(scantable[i].elcorr[j][k][l], line, polyorder+1);
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("GEOM") != string::npos) { //look for optional "EL GEOM" geometric elevation specification
            polyok = polyok && fillPolyRow(scantable[i].elgeom[j][k][l], line, polyorder+1);
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("PAR") != string::npos) { //look for optional "PAR ANGLE" parallactic angle specification
            polyok = polyok && fillPolyRow(scantable[i].parang[j][k][l], line, polyorder+1);
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("STA X") != string::npos) { //look for optional "STA X" station position in J2000
            // Do nothing with it...
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("STA Y") != string::npos) { //look for optional "STA Y" station position in J2000
            // Do nothing with it...
            config->getinputkeyval(input, &key, &line);
          }
          if(key.find("STA Z") != string::npos) { //look for optional "STA Z" station position in J2000
            // Do nothing with it...
            config->getinputkeyval(input, &key, &line);
          }
          polyok = polyok && fillPolyRow(scantable[i].u[j][k][l], line, polyorder+1);
          config->getinputline(input, &line, "SRC ", k);
          polyok = polyok && fillPolyRow(scantable[i].v[j][k][l], line, polyorder+1);
          config->getinputline(input, &line, "SRC ", k);
          polyok = polyok && fillPolyRow(scantable[i].w[j][k][l], line, polyorder+1);
          if(!polyok) {
            cfatal << startl << "IM file has problem with polynomials - aborting!" << endl;
            return false;
          }
          if(hasXYZDerivatives)
          {
            for(int skip = 0; skip < 9; ++skip) {
              // just skip over the next 9 lines which are not used by mpifxcorr
              config->getinputkeyval(input, &key, &line);
            }
          }
          if(hasLMDerivatives)
          {
            for(int skip = 0; skip < 5; ++skip) {
              // just skip over the next 5 lines which are not used by mpifxcorr
              config->getinputkeyval(input, &key, &line);
            }
          }
        }
      }
    }
  }
  return true;
}

//Split a whitespace-separated row and store the values in an array of doubles
bool Model::fillPolyRow(f64* vals, string line, int npoly)
{
  istringstream iss(line);
  string val;

  for(int i=0;i<npoly;i++) {
    if(!iss)
      return false;
    iss >> val;
    vals[i] = atof(val.c_str());
  }
  return true;
}

//utility routine which returns an integer which FITS expects based on the type of mount
Model::axistype Model::getMount(string mount)
{
  if(strcasecmp(mount.c_str(), "azel") == 0) //its an azel mount
    return ALTAZ;
  if(strcasecmp(mount.c_str(), "equa") == 0) //equatorial mount
    return RADEC;
  if(strcasecmp(mount.c_str(), "orbi") == 0) //orbital mount
    return ORB;
  if(strcasecmp(mount.substr(0,2).c_str(), "xy") == 0) //xy mount
    return XY;

  //otherwise unknown
  cerror << startl << "Warning - unknown mount type: Assuming Az-El" << endl;
  return ALTAZ;
}
// vim: shiftwidth=2:softtabstop=2:expandtab
