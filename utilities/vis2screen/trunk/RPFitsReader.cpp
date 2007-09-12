/***************************************************************************
 *   Copyright (C) 2005 by Adam Deller                                     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <vector>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <stdio.h>
#include <stddef.h>
#include <dirent.h>
#include <stdlib.h>
#include <iomanip>
#include <math.h>
#include <complex> 
#include <RPFITS.h>

using namespace std;

//prototypes
void makeFortranString(string line, int length, char * destination);

//global variables because I'm lazy
int status, ifnum, binnum, flagval, sourcenum, bline;
float utsec, uval, vval, wval;
float * visibilities;
float * weighting;

int main(int argc, char** argv)
{
  if(argc != 2)
  {
    cout << "Invoke with RPFitsReader <inputfilename>" << endl;
    return EXIT_FAILURE;
  }
  
  //set up the few variables we need
  string outfilename = argv[1];
  makeFortranString(outfilename, 256, names_.file);
  param_.ncard = 0;
  status = -2;
  
  //open the file
  rpfitsin_(&status, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  
  status = 0;
  visibilities = new float[2*if_.if_nfreq[0]*param_.nstok];
  weighting = new float[if_.if_nfreq[0]*param_.nstok];
  cout << "The number of IFs is " << if_.n_if << ", the number of stokes parameters is " << param_.nstok << endl;
  cout << "The number of antennas is " << anten_.nant << ", the first two antenna numbers are " << anten_.ant_num[0] << ", " << anten_.ant_num[1] << endl;
  cout << "The data format is " << param_.data_format << endl;
  cout << "The number of sources is " << su_.n_su << ", source number[0] is " << su_.su_num[0] << endl;
  cout << "The number of channels is (two ways) " << if_.if_nfreq[0] << ", " << param_.nfreq << endl;
  cout << "The header integration time is " << param_.intime << endl;
  cout.precision(10);

  //read in all the data we can find
  while(status == 0)
  {
    rpfitsin_(&status, visibilities, weighting, &bline, &utsec, &uval, &vval, &wval, &flagval, &binnum, &ifnum, &sourcenum);
    cout << "At ut " << utsec << " on baseline " << bline << ", visibilities[10] was " << visibilities[10] << ", weight[10] was " << weighting[10] << ", flag was " << flagval << ", bin was " << binnum << ", ifnum was " << ifnum << ", sourcenum was " << sourcenum << ", (u,v,w) were (" << uval << "," << vval << "," << wval << ")" << ", intbase was " << param_.intbase << endl;
    /*if(bline > 258 && ifnum > 2) //its a Hobart baseline with a freq it shouldn't have
    {
      cout << "At ut " << utsec << " on baseline " << bline << ", visibilities[10] was " << visibilities[10] << ", weight[10] was " << weighting[10] << ", flag was " << flagval << ", bin was " << binnum << ", ifnum was " << ifnum << ", sourcenum was " << sourcenum << ", (u,v,w) were (" << uval << "," << vval << "," << wval << ")" << ", intbase was " << param_.intbase << endl;
    }*/
  }
  cout << "About to try again for fun..." << endl;
  status = 0;
  while(status == 0)
  {
    rpfitsin_(&status, visibilities, weighting, &bline, &utsec, &uval, &vval, &wval, &flagval, &binnum, &ifnum, &sourcenum);
    cout << "At ut " << utsec << " on baseline " << bline << ", visibilities[10] was " << visibilities[10] << ", weight[10] was " << weighting[10] << ", flag was " << flagval << ", bin was " << binnum << ", ifnum was " << ifnum << ", sourcenum was " << sourcenum << ", (u,v,w) were (" << uval << "," << vval << "," << wval << ")" << endl;
  }
}

//utility routine to turn a c string object into a char array, padded with spaces all the way
void makeFortranString(string line, int length, char * destination)
{
  int linelength = line.length();
  
  if(linelength <= length)
  {
    strcpy(destination, line.c_str());
    for(int i=0;i<length-linelength;i++)
      destination[i+linelength] = ' ';
  }
  else
  {
    strcpy(destination, (line.substr(0, length-1)).c_str());
    destination[length-1] = line.at(length-1);
  }
}










/*

RPFitsWriter::RPFitsWriter(char * infilename)
  : inputfilename(infilename), status(0), referencebaseline(-1), numsources(0)
{}

RPFitsWriter::~RPFitsWriter()
{
  int count = 0;
  int subcount;
  
  for(int i=0;i<numbaselines;i++)
  {
    for(int j=0;j<numbaselinefreqs[i];j++)
    {
      for(int k=0;k<numproducts;k++)
      {
        delete [] stokescoefficients[i][j][k];
      }
      delete [] stokescoefficients[i][j];
      delete [] stokesproducts[i][j];
    }
    delete [] stokescoefficients[i];
    delete [] stokesproducts[i];
  }
  
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
  delete [] stokescoefficients;
  delete [] stokesproducts;
  delete [] dirnames;
  delete [] numbaselinefreqs;
  delete [] stations;
  delete [] baselinenames;
  delete [] baselinenumbers;
  delete [] reffrequencies;
  delete [] refbandwidths;
  delete [] refreferencefracs;
}

void RPFitsWriter::processData()
{
  for(int i=0;i<numdirectories;i++)
  {
    cout << "About to start processing directory " << i << endl;
    processDirectory(dirnames[i]);
  }
  cout << "About to make the final call to close the file..." << endl;
  int closeflag = 1;
  rpfitsout_(&closeflag, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  cout << "Finished processing all directories!! The status of the closing operation was " << closeflag << endl;
}

void RPFitsWriter::processInputFile()
{
  float real;
  int at, next;
  string line;
  ifstream input(inputfilename, ios::in);
  if(!input.is_open() || input.bad())
    cerr << "Error opening uvw file " << uvwfilename << "!!!" << endl;

  getinputline(&input, &uvwfilename);
  getinputline(&input, &outfilename);
  getinputline(&input, &prefix);
  prefixlength = prefix.length();
  
  getinputline(&input, &line);
  numchannels = atoi(line.c_str());
  
  getinputline(&input, &line);
  maxfrequencies = atoi(line.c_str());
  reffrequencies = new float[maxfrequencies];
  refbandwidths = new float[maxfrequencies];
  refreferencefracs = new float[maxfrequencies];
  for(int i=0;i<maxfrequencies;i++)
  {
    getinputline(&input, &line);
    reffrequencies[i] = float(atof(line.c_str()));
    getinputline(&input, &line);
    refbandwidths[i] = float(atof(line.c_str()));
    getinputline(&input, &line);
    if(line.compare("U") == 0) //upper sideband, therefore reference = 0
      refreferencefracs[i] = 0.0;
    else if(line.compare("L") == 0) //lower sideband, therefore reference = 1.0
      refreferencefracs[i] = 1.0;  
    else if(line.compare("C") == 0) //specified as centre freq of band, therefore reference = 0.5
      refreferencefracs[i] = 0.5;
    else //print warning and assume upper sideband
    {
      cerr << "Warning!! Incorrect specification of sideband for frequency " << i << endl;
      refreferencefracs[i] = 0.0;
    }
  }
  
  getinputline(&input, &line);
  numproducts = atoi(line.c_str());
  productnames = new string[numproducts];
  getinputline(&input, &line);
  at = 0;
  for(int i=0;i<numproducts;i++)
  {
    next = line.find_first_of(' ', at);
    productnames[i] = line.substr(at, next-at); //eg I, Q, U, V or RR, LL etc
    at = next + 1;
  }
  
  getinputline(&input, &line);
  dumptime = atof(line.c_str());
  
  getinputline(&input, &line);
  numdirectories = atoi(line.c_str());
  dirnames = new string[numdirectories];
  for(int i=0;i<numdirectories;i++)
    getinputline(&input, &dirnames[i]);
    
  //get the baseline information, including the coefficients to convert recorded products to Stokes parameters  
  getinputline(&input, &line);
  numbaselines = atoi(line.c_str());
  numbaselinefreqs = new int[numbaselines];
  baselinenames = new string[numbaselines];
  baselinenumbers = new int[numbaselines];
  stokesproducts = new int**[numbaselines];
  stokescoefficients = new complex<float>***[numbaselines];
  
  for(int i=0;i<numbaselines;i++)
  {
    getinputline(&input, &baselinenames[i]);
    getinputline(&input, &line);
    baselinenumbers[i] = atoi(line.c_str());
    getinputline(&input, &line);
    numbaselinefreqs[i] = atoi(line.c_str());
    stokesproducts[i] = new int*[numbaselinefreqs[i]]; //these are the products used to form the stokes parameters from the input circular/linear cross products
    stokescoefficients[i] = new complex<float>**[numbaselinefreqs[i]];
    for(int j=0;j<numbaselinefreqs[i];j++)
    {
      stokesproducts[i][j] = new int[numproducts];
      stokescoefficients[i][j] = new complex<float>*[numproducts];
      getinputline(&input, &line);
      //read in the source products
      at = 0;
      for(int k=0;k<numproducts;k++)
      {
        next = line.find_first_of(' ', at);
        stokesproducts[i][j][k] = atoi((line.substr(at, next-at)).c_str());
        at = next + 1;
      }
      //now read in the actual coefficients, which are space separated complex numbers re1 im1 [re2 im2 re3 im3 re4 im4]
      for(int k=0;k<numproducts;k++)
      {
        stokescoefficients[i][j][k] = new complex<float>[numproducts];
        at = 0;
        getinputline(&input, &line);
        for(int l=0;l<numproducts;l++)
        {
          next = line.find_first_of(' ', at);
          real = atof((line.substr(at, next-at)).c_str());
          at = next + 1;
          next = line.find_first_of(' ', at);
          stokescoefficients[i][j][k][l] = complex<float>(real, atof((line.substr(at, next-at)).c_str()));
          at = next + 1;
        }
      }
    }
  }
  
  //get the user defined header cards for the rpfits file
  getinputline(&input, &line);
  numuserheadercards = atoi(line.c_str());
  userheadercards = new char[RPFITS_HEADER_LENGTH*numuserheadercards];
  for(int i=0;i<numuserheadercards;i++)
  {
    getinputline(&input, &line);
    makeFortranString(line, RPFITS_HEADER_LENGTH, &(userheadercards[RPFITS_HEADER_LENGTH*i]));
  }
}

void RPFitsWriter::processUVWFile()
{
  string line;
  bool found;
  char date[100];
  char obsdatecstr[12];
  int year, month, day, hour, minute, second, numpoints, at, next;
  ifstream input(uvwfilename.c_str(), ios::in);
  if(!input.is_open() || input.bad())
    cerr << "Error opening uvw file " << uvwfilename << "!!!" << endl;
  
  numuvwpoints = 0;  
    
  getinputline(&input, &line);
  year = atoi(line.c_str());
  getinputline(&input, &line);
  month = atoi(line.c_str());
  getinputline(&input, &line);
  day = atoi(line.c_str());
  getinputline(&input, &line);
  hour = atoi(line.c_str());
  getinputline(&input, &line);
  minute = atoi(line.c_str());
  getinputline(&input, &line);
  second = atoi(line.c_str());

  getMJD(utstartdaymjd, utdayfractionmjd, year, month, day, hour, minute, second);
  sprintf(date, "%04u%02u%02u:%02u%02u%02u", year, month, day, hour, minute, second);
  sprintf(obsdatecstr, "%04u-%02u-%02u", year, month, day);
  datestring = date;
  obsdate = obsdatecstr;
  
  getinputline(&input, &line);
  uvwincrementsecs = atof(line.c_str());
  
  //read in telescope information
  getinputline(&input, &line);
  numstations = atoi(line.c_str());
  stations = new telescope[numstations];
  for(int i=0;i<numstations;i++)
  {
    getinputline(&input, &stations[i].name);
    getinputline(&input, &line);
    stations[i].mount = getMountInt(line);
    getinputline(&input, &line);
    stations[i].x = atof(line.c_str());
    getinputline(&input, &line);
    stations[i].y = atof(line.c_str());
    getinputline(&input, &line);
    stations[i].z = atof(line.c_str());
  }
  
  //read in scan information
  getinputline(&input, &line);
  numscans = atoi(line.c_str()); 
  uvw = new double***[numscans];
  scansources = new source[numscans];
  scanindices = new int[numscans];
  scanstartpoints = new int[numscans];
  for(int i=0;i<numscans;i++)
  {
    getinputline(&input, &line);
    numpoints = atoi(line.c_str());
    numuvwpoints += numpoints;
    uvw[i] = new double**[numpoints + 3]; //since we have -1, numpoints and numpoints+1 as well
    getinputline(&input, &line);
    scanstartpoints[i] = atoi(line.c_str());
    getinputline(&input, &(scansources[numsources].name)); 
    
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
      getinputline(&input, &line);
      scansources[numsources].ra = atof(line.c_str());
      getinputline(&input, &line);
      scansources[numsources].dec = atof(line.c_str());
      numsources++;
    }
    else
    {
      //still have to skip over the ra and dec lines
      getinputline(&input, &line);
      getinputline(&input, &line);
    }
    
    
    for(int j=0;j<numpoints+3;j++)
    {
      at = 0;
      getinputline(&input, &line);
      uvw[i][j] = new double*[numstations];
      for(int k=0;k<numstations;k++)
      {
        uvw[i][j][k] = new double[3]; //u, v, w
        for(int l=0;l<3;l++)
        {
          next = line.find_first_of('\t', at);
          uvw[i][j][k][l] = atof((line.substr(at, next-at)).c_str());
          at = next+1;
        }
      }
      if(j<numpoints)
        scannumbers.push_back(i);
    }
  }
}

void RPFitsWriter::writeHeaderInfo()
{
  //set up the outputfilename
  makeFortranString(outfilename, 256, names_.file);
  
  //set up the parameters
  param_.write_wt = 0; //don't write weights
  param_.ncard = numuserheadercards;
  strncpy(userheadercards, names_.card, RPFITS_HEADER_LENGTH*numuserheadercards);
  param_.intbase = float(dumptime);
  param_.data_format = 2; //for complex visibilities, no weights
  doubles_.x_array = 0.0;
  doubles_.y_array = 0.0;
  doubles_.z_array = 0.0;
  makeFortranString("J2000", 8, names_.coord);
  makeFortranString(obsdate.c_str(), 12, names_.datobs); 
  
  //set up the antenna info
  anten_.nant = numstations;
  for(int i=0;i<numstations;i++)
  {
    anten_.ant_num[i] = i+1;
    anten_.ant_mount[i] = stations[i].mount;
    makeFortranString(stations[i].name, ANTENNA_NAME_LENGTH, &(names_.sta[i*ANTENNA_NAME_LENGTH]));
    doubles_.x[i] = stations[i].x;
    doubles_.y[i] = stations[i].y;
    doubles_.z[i] = stations[i].z;
  }
  
  //set up the IF info
  if_.n_if = maxfrequencies;
  string blank = "  ";
  for(int i=0;i<maxfrequencies;i++)
  {
    if_.if_invert[i] = 1; //should never be inverted as the correlator can invert any inverted bands
    if_.if_nfreq[i] = numchannels + 1;
    if_.if_nstok[i] = numproducts;
    if_.if_num[i] = i+1;
    if_.if_sampl[i] = 2; //temporary, assume 2 bit sampling
    if_.if_simul[i] = 1; //don't know what these do...?
    if_.if_chain[i] = 1;
    doubles_.if_bw[i] = refbandwidths[i]*1000000; //convert from MHz to Hz
    doubles_.if_ref[i] = int(numchannels*refreferencefracs[i]) + 1;
    doubles_.if_freq[i] = reffrequencies[i]*1000000; //convert from MHz to Hz
    for(int j=0;j<numproducts;j++)
      makeFortranString(productnames[j], STOKES_NAME_LENGTH, &(names_.if_cstok[(4*i + j)*STOKES_NAME_LENGTH]));
    for(int j=numproducts;j<4;j++)
      makeFortranString(blank, STOKES_NAME_LENGTH, &(names_.if_cstok[(4*i + j)*STOKES_NAME_LENGTH]));
  }
  
  //set up the source info
  su_.n_su = numsources;
  for(int i=0;i<numsources;i++)
  {
    su_.su_num[i] = i+1;
    doubles_.su_rad[i] = scansources[i].ra;
    doubles_.su_decd[i] = scansources[i].dec;
    doubles_.su_prad[i] = scansources[i].ra;
    doubles_.su_pdecd[i] = scansources[i].dec;
    makeFortranString(scansources[i].name, SOURCE_NAME_LENGTH, &(names_.su_name[i*SOURCE_NAME_LENGTH]));
    makeFortranString(blank, SOURCE_CALCODE_LENGTH, &(names_.su_cal[i*SOURCE_CALCODE_LENGTH]));
  }
  
  int openflag = -2; //open the file and write the header
  
  rpfitsout_(&openflag, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  //cout << "The open flag was set to " << openflag << endl;
}


const char * RPFitsWriter::OUTFILE_DELIMITERS = "._";

//routine to interpolate the u, v and w in metres for this baseline at the given time
void RPFitsWriter::interpolateuvw(string baseline, int scannumber, double offsetsec, float * buvw)
{
  double a, b, c;
  int stationindex[2];
  float tuvw[2][3]; //individual telescope u, v ,w
  //cout << "offsetsec is " << offsetsec << ", uvwincrementsecs is " << uvwincrementsecs << ", scanstartpoints[scannumber] is " << scanstartpoints[scannumber] << endl;
  int nearestindex = int(offsetsec/uvwincrementsecs + 0.5) - scanstartpoints[scannumber] + 1;
  double distance = offsetsec/uvwincrementsecs - (nearestindex + scanstartpoints[scannumber] - 1);
  double distancesquared = distance*distance;
  int splitindex = baseline.find_first_of('-');
  string station1name = baseline.substr(0, splitindex);
  string station2name = baseline.substr(splitindex+1);

  for(int i=0;i<numstations;i++)
  {
    if(((stations[i].name).substr(0, station1name.length())).compare(station1name) == 0) //it is this telescope
      stationindex[0] = i;
    
    if(((stations[i].name).substr(0, station2name.length())).compare(station2name) == 0) //it is this telescope
      stationindex[1] = i;
  }
  
  //cout << "About to start interpolating...  Stationindex[0] = " << stationindex[0] << ", stationindex[1] = " << stationindex[1] << ", scannumber is " << scannumber << ", nearestindex is " << nearestindex << endl;
  for(int i=0;i<2;i++)
  {
    for(int j=0;j<3;j++)
    {
      a = (uvw[scannumber][nearestindex+1][stationindex[i]][j] + uvw[scannumber][nearestindex-1][stationindex[i]][j])/2 - uvw[scannumber][nearestindex][stationindex[i]][j];
      b = (uvw[scannumber][nearestindex+1][stationindex[i]][j] - uvw[scannumber][nearestindex-1][stationindex[i]][j])/2;
      c = uvw[scannumber][nearestindex][stationindex[i]][j];
      //cout << "I have calculated a=" << a << ", b=" << b << ", c=" << c << ", and distance = " << distance << endl;
      
      tuvw[i][j] = float(a*distancesquared + b*distance + c);
    }
  }
  
  for(int i=0;i<3;i++)
  {
    buvw[i] = tuvw[0][i] - tuvw[1][i];
  }
}

//routine to multiply the input matrix by the stokes coefficients transfer matrix and store the result in the 1D visibility matrix
void RPFitsWriter::matrixMultiplyProducts(int baselinenum, int freqnum, complex<float> ** inputs, float * visibilities)
{
  complex<float> output;
  
  for(int i=0;i<numproducts;i++)
  {
    for(int j=0;j<numchannels+1;j++)
    {
      output = complex<float>(0.0,0.0);
      for(int k=0;k<numproducts;k++)
      {
        //cout << "About to multiply " << inputs[k][j].real() << "," << inputs[k][j].imag() << " by " << stokescoefficients[baselinenum][freqnum][i][k].real() << "," << stokescoefficients[baselinenum][freqnum][i][k].imag() << endl;
        output += inputs[k][j]*stokescoefficients[baselinenum][freqnum][i][k];
      }
      visibilities[(j*numproducts + i)*2]     = output.real();
      visibilities[(j*numproducts + i)*2 + 1] = output.imag();
    }
  }
}

//utility routine to find the IF number and baseline number corresponding to the given baseline and product, and store the products data in the correct location in the input array
int RPFitsWriter::storeData(complex<float> ** inputs, string filename, string baseline, int product, int & baselinenum, int freqnum)
{
  float real, imag;
  bool specificfrequency = (freqnum<0)?false:true; //if freqnum was specified, we will look only for products in that IF
  
  //locate the baseline number
  for(int i=0;i<numbaselines;i++)
  {
    if(baseline.compare(baselinenames[i]) == 0) //this is the baseline
    {
      baselinenum = i;
      for(int j=0;j<numbaselinefreqs[i];j++)
      {
        if(!specificfrequency || (freqnum == j))
        {
          for(int k=0;k<numproducts;k++)
          {
            if(stokesproducts[i][j][k] == product) // we have found the spot we want to put the data in
            {
              //open up this file and read in the data - ignore the last data point which is the nyquist channel
              ifstream binaryinput(filename.c_str(), ios::in|ios::binary);
              //cout << "About to read in the data from file " << filename << endl;
              if(!binaryinput.is_open() || binaryinput.bad())
              {
                cerr << "Error!!! Could not open file " << filename << endl;
                return -1; //note return with failure
              }
              else
              {
                for(int l=0;l<numchannels+1;l++)
                {
                  binaryinput.read(reinterpret_cast < char * > (&real), sizeof(&real));
                  binaryinput.read(reinterpret_cast < char * > (&imag), sizeof(&imag));
                  //inputs[k][l] = complex<float>(real, imag);
                  inputs[k][l] = complex<float>(real*cos(imag), real*sin(imag));
                }
              }
              binaryinput.close();
            
              return j;//note exit with success here!!!
            }
          }
        }
      }
    }
  }
  
  return -1; //return failure
}

//utility routine to check if a filename matches a baseline and time
bool RPFitsWriter::matchingfile(string filename, string baseline, int & product, int date, int timesec, int timemicrosec)
{
  char fileinfo[MAX_FILENAME_LENGTH];
  char * token;
  
  strcpy(fileinfo, (filename.substr(prefixlength)).c_str()); //skip the prefix on the filename
  if(baseline.compare(strtok(fileinfo, OUTFILE_DELIMITERS)) != 0)
    return false; //not the same baseline
  strtok(NULL, OUTFILE_DELIMITERS); //skip past the _Product_
  product = atoi(strtok(NULL, OUTFILE_DELIMITERS));
  strtok(NULL, OUTFILE_DELIMITERS); //skip past the _DateTime_
  if(date !=  atoi(strtok(NULL, OUTFILE_DELIMITERS)))
    return false; //not the same date
  if(timesec != atoi(strtok(NULL, OUTFILE_DELIMITERS)))
    return false; //not the same time
  if(timemicrosec != atoi(strtok(NULL, OUTFILE_DELIMITERS)))
    return false; //not the same time
  
  //if we got this far it must be a matching file
  return true;
}

//utility routine which returns an integer which FITS expects based on the type of mount
int RPFitsWriter::getMountInt(string mount)
{
  if(mount.compare("azel") == 0) //its an azel mount
    return 0;
    
  if(mount.compare("equa") == 0) //equatorial mount
    return 1;
    
  if(mount.compare("orbi") == 0) //orbital mount
    return 2;
    
  if((mount.substr(0,2)).compare("xy") == 0) //xy mount
    return 3;
    
  //otherwise unknown
  cerr << "Warning - unknown mount type: Assuming Az-El" << endl;
  return 0;
}

//utility routine to peek into a directory, find one binary file and grab the date, and format it as a string "yyyy-mm-dd"
string * RPFitsWriter::getObsDate(string directory)
{
  string * toReturn = new string();
  bool found = false;
  DIR * dirpointer;
  struct dirent * entrypointer;
  
  dirpointer = opendir(directory.c_str());
  if (dirpointer != NULL)
  {
    //first read in all the filenames that are the ones we want
    while ((entrypointer = readdir (dirpointer)) && !found)
    {
      if(strstr(entrypointer->d_name, ".binout") != NULL) //it is a binary output file
      {
        found = true;
        strtok(entrypointer->d_name + prefixlength, OUTFILE_DELIMITERS);
        strtok(NULL, OUTFILE_DELIMITERS);
        *toReturn = strtok(NULL, OUTFILE_DELIMITERS);
      }
    }
  }
  *toReturn = toReturn->substr(0,4) + "-" + toReturn->substr(4,2) + "-" + toReturn->substr(6,2);
  
  return toReturn;
}

//utility routine to turn a date and time made up of ints into an mjd date int and mjd fractional day double
void RPFitsWriter::getMJD(int & d, double & fracd, int year, int month, int day, int hour, int minute, int second)
{
    int a = (14 - month)/12;
    int y = year + 4800 - a;
    int m = month + 12*a - 3;

    d = day + (153*m+2)/5 + 365*y + y/4 - y/100 + y/400 - (32045+2400000);
    if(hour<12)
    {
        d -= 1;
        hour += 12;
    }
    else
    {
        hour -= 12;
    }
    
    fracd = (3600*hour + 60*minute + second)/86400.0;
}



//Utility routine which reads a line, discarding a header of constant length
void RPFitsWriter::getinputline(ifstream * input, string * line)
{
    input->get(header, HEADER_LENGTH);
    getline(*input,*line);
}*/
