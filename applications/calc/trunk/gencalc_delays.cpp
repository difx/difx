#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <iomanip>

#include "ccon.h"
#include "cphys.h"
#include "cmxst.h"
#include "situvw.h"

using namespace std;

//prototypes
void processInput(string inputfilename);

//constants for string lengths
static const int SOURCE_LENGTH = 16;
static const unsigned int SITE_LENGTH = 8;
static const int HEADER_LENGTH = 21;

//external FORTRAN call prototypes
extern "C" {
int init_calc_(char * errorstring, int strlength);
int load_sites_(char * errorstring, int strlength);
int load_sources_(char * sourcename, char * errorstring, int len1, int len2);
void eop_setup_(short * utctag);
void tocup_();
void initl_(int * kount);
int get_baseline_xyz_(int * num_ants, char * ant_name, double * ant_x, double * ant_y, double * ant_z);
bool find_source_(char * srcname, char * srcra_string, char * srcdec_string, int len1, int len2, int len3);
void star_(int * nloaded, double * srcra, double * srcdec);
void v_refcalc_(char * sitename, char * sourcename, short * utctag, double * seconds, double * delay, double * delayrate, double * uvw, double * duvw, int sitelength, int sourcelength);
}
//end of the extern statement

//global variables
short utctag[5]; //year, month, day, hour, minute
int numtelescopes, numscans, cumulativescanstart;
int status, kount, nloaded; //FORTRAN reqd variables
int * scanlength_incs;
char ** telescopenames; //telescope names are 8 character
char ** sourcenames; //source names are 16 character
char errorstring[80];
char srcra_string[20], srcdec_string[20];
char headerbuffer[20];
double timeincsec, timesec, srcra, srcdec, delay, delayrate, uvw[3], duvw[3];
string line, outfilename, tempheader;
bool sourcefound;
string * mount;
double antennax[1];
double antennay[1];
double antennaz[1];

int main(int argc, char ** argv)
{
  if(argc != 2)
    {
      cerr << "Error - invoke with gencalc_delays <inputfilename>" << endl;
      return EXIT_FAILURE; //note EXIT from program
    }

  // Modified by CWest
  // Change to the database directory and run it there.

  char currdir[256];
  getcwd(currdir, 256);
  strcat(currdir, "/");
  
  char *calcdb_env = getenv("CALCDB");
  if(calcdb_env == NULL){
    chdir(__PWD);
  }else{
    chdir(calcdb_env);
  }

  string infilename = string(argv[1]);
    
  if( strncmp(infilename.c_str(),"/",1) != 0) // check for a leading /
    infilename = currdir + infilename;
  
  processInput(infilename);
  
  //run all the necessary calc initialisation routines
  status = init_calc_(errorstring, 80); //FORTRAN call - hence trailing underscore and need length of string(s) as final parameter(s)
  if(status != 0)
  {
    cerr << "Error in init_calc!  Error string: " << errorstring << endl;
    return EXIT_FAILURE;  //note EXIT from program
  }
  
  status = load_sites_(errorstring, 80); //FORTRAN call
  if(status != 0)
  {
    cerr << "Error in load_sites!  Error string: " << errorstring << endl;
    return EXIT_FAILURE;  //note EXIT from program
  }
  
  status = load_sources_((char*)"source.tab", errorstring, 11, 80); //FORTRAN call
  if(status != 0)
  {
    cerr << "Error in load_sources!  Error string: " << errorstring << endl;
    return EXIT_FAILURE;  //note EXIT from program
  }

  // Modified by CWest to change the outfilename to the directory it was launched
  if( strncmp(outfilename.c_str(),"/",1) != 0) // check for a leading /
    outfilename = currdir + outfilename;

  //output the initial start of the delay file    
  ofstream delayoutput((outfilename + ".delay").c_str(), ios::trunc);
  ofstream uvwoutput((outfilename + ".uvw").c_str(), ios::trunc);
  
  delayoutput.setf(ios::left); 
  uvwoutput.setf(ios::left);
  delayoutput << setprecision(15);
  uvwoutput << setprecision(15);
  //delayoutput.setw(20);
  
  /*//set up the header data in the delay file - old way
  delayoutput << "FIRST SRC = " << sourcenames[0] << "  STATIONS =   ";
  for(int i=0;i<numtelescopes;i++)
  {
    delayoutput << telescopenames[i] << "   ";
  }
  delayoutput << endl;
  delayoutput << " START UT = " << utctag[0] << "  " << utctag[1] << "  " << utctag[2] << "  " << utctag[3] << "  " << utctag[4] << "   0.0 " << endl;*/
  
  //set up the delay and uvw files, the new way
  delayoutput << setw(20) << "START YEAR: " << utctag[0] << endl;
  delayoutput << setw(20) << "START MONTH: " << utctag[1] << endl;
  delayoutput << setw(20) << "START DAY: " << utctag[2] << endl;
  delayoutput << setw(20) << "START HOUR: " << utctag[3] << endl;
  delayoutput << setw(20) << "START MINUTE: " << utctag[4] << endl;
  delayoutput << setw(20) << "START SECOND: " << "0" << endl;
  delayoutput << setw(20) << "INCREMENT (SECS): " << timeincsec << endl;
  uvwoutput << setw(20) << "START YEAR: " << utctag[0] << endl;
  uvwoutput << setw(20) << "START MONTH: " << utctag[1] << endl;
  uvwoutput << setw(20) << "START DAY: " << utctag[2] << endl;
  uvwoutput << setw(20) << "START HOUR: " << utctag[3] << endl;
  uvwoutput << setw(20) << "START MINUTE: " << utctag[4] << endl;
  uvwoutput << setw(20) << "START SECOND: " << "0.0" << endl;
  uvwoutput << setw(20) << "INCREMENT (SECS): " << timeincsec << endl;
  
  //set up the header data in the uvw file - new way
  //uvwoutput << "###########SCAN INFORMATION###############" << endl;
  //uvwoutput << setw(20) << "START UT: " << utctag[0] << "\t" << utctag[1] << "\t" << utctag[2] << "\t" << utctag[3] << "\t" << utctag[4] << "\t0.0 " << endl;
  
  uvwoutput << setw(20) << "NUM TELESCOPES: " << numtelescopes << endl;
  delayoutput << setw(20) << "NUM TELESCOPES: " << numtelescopes << endl;
  for(int i=0;i<numtelescopes;i++)
  {
    int one = 1;
    sprintf(headerbuffer, "TELESCOPE %u NAME: ", i);
    uvwoutput << setw(20) << headerbuffer << telescopenames[i] << endl;
    delayoutput << setw(20) << headerbuffer << telescopenames[i] << endl;
    sprintf(headerbuffer, "TELESCOPE %u MOUNT: ", i);
    uvwoutput << setw(20) << headerbuffer << mount[i] << endl;
    sprintf(headerbuffer, "TELESCOPE %u X (m): ", i);
    get_baseline_xyz_(&one, telescopenames[i], &antennax[0], &antennay[0], &antennaz[0]);
    uvwoutput << setw(20) << headerbuffer << antennax[0] << endl;
    sprintf(headerbuffer, "TELESCOPE %u Y (m): ", i);
    uvwoutput << setw(20) << headerbuffer << antennay[0] << endl;
    sprintf(headerbuffer, "TELESCOPE %u Z (m): ", i);
    uvwoutput << setw(20) << headerbuffer << antennaz[0] << endl;
  }
  
  uvwoutput << setw(20) << "NUM SCANS: " << numscans << endl;
  delayoutput << setw(20) << "NUM SCANS: " << numscans << endl;
  /*for(int i=0;i<numscans;i++)
  {
    sourcefound = find_source_(sourcenames[i], srcra_string, srcdec_string, SOURCE_LENGTH, 20, 20);
    if(!sourcefound)
    {
      cerr << "Error in finding source " << sourcenames[i] << "!!! RA was " << srcra_string << ", dec was " << srcdec_string << endl;
      return EXIT_FAILURE;  //note EXIT from program
    }
    star_(&nloaded, &srcra, &srcdec);
    sprintf(headerbuffer, "SCAN %u ", i);
    tempheader = headerbuffer;
    uvwoutput << setw(20) << (tempheader + "POINTS: ") << int(scanlength_incs[i]) << endl;
    uvwoutput << setw(20) << (tempheader + "SRC NAME: ") << sourcenames[i] << endl;;
    uvwoutput << setw(20) << (tempheader + "SRC RA: ") << srcra << endl;
    uvwoutput << setw(20) << (tempheader + "SRC DEC: ") << srcdec << endl;
  }
  uvwoutput << "#########END SCAN INFORMATION#############" << endl;*/
  
  cumulativescanstart = 0;
  //ok, now loop through all the sources and generate the delayoutput file
  for(int i=0;i<numscans;i++)
  {
    //load the current source
    sourcefound = find_source_(sourcenames[i], srcra_string, srcdec_string, SOURCE_LENGTH, 20, 20);
    if(!sourcefound)
    {
      cerr << "Error in finding source " << sourcenames[i] << "!!! RA was " << srcra_string << ", dec was " << srcdec_string << endl;
      return EXIT_FAILURE;  //note EXIT from program
    }
    
    star_(&nloaded, &srcra, &srcdec);
    cout << "Source found ok: name " << sourcenames[i] << ", ra: " << srcra_string << " or in radians " << srcra << ", dec: " << srcdec_string << " or in radians " << srcdec << endl;
    
    eop_setup_(utctag); //FORTRAN call
  
    tocup_(); //FORTRAN call
  
    initl_(&kount); //FORTRAN call
    
    sprintf(headerbuffer, "SCAN %u ", i);
    tempheader = headerbuffer;
    delayoutput << setw(20) << (tempheader + "POINTS: ") << scanlength_incs[i] << endl;
    delayoutput << setw(20) << (tempheader + "START PT: ") << cumulativescanstart << endl;
    delayoutput << setw(20) << (tempheader + "SRC NAME: ") << sourcenames[i] << endl;;
    uvwoutput << setw(20) << (tempheader + "POINTS: ") << scanlength_incs[i] << endl;
    uvwoutput << setw(20) << (tempheader + "START PT: ") << cumulativescanstart << endl;
    uvwoutput << setw(20) << (tempheader + "SRC NAME: ") << sourcenames[i] << endl;;
    uvwoutput << setw(20) << (tempheader + "SRC RA: ") << srcra << endl;
    uvwoutput << setw(20) << (tempheader + "SRC DEC: ") << srcdec << endl;
    
    //everything is now loaded, so calculate the delays and uvs for each time in the scan
    for(int j=-1;j<=scanlength_incs[i]+1;j++)
    {
      timesec = (cumulativescanstart+j)*timeincsec;
      //delayoutput << setw(6) << timesec << "\t";
      //uvwoutput << setw(6) << timesec << "\t";
      sprintf(headerbuffer, "RELATIVE INC %i: ", j);
      tempheader = headerbuffer;
      delayoutput << setw(20) << tempheader;
      uvwoutput << setw(20) << tempheader;
      
      for(int k=0;k<numtelescopes;k++)
      {
        v_refcalc_(telescopenames[k], sourcenames[i], utctag, &timesec, &delay, &delayrate, uvw, duvw, SITE_LENGTH, SOURCE_LENGTH);
        //delayoutput << setw(17) << delay << "\t" << setw(17) << delayrate << "\t";
        delayoutput << setw(17) << delay << "\t";
        uvwoutput << setw(17) << uvw[0] << "\t" << setw(17) << uvw[1] << "\t" << setw(17) << uvw[2] << "\t";
      }
      
      delayoutput << endl;
      uvwoutput << endl;
      //timesec += timeincsec;
    }
    
    cumulativescanstart += scanlength_incs[i];
  }
  
  delayoutput.close();
  uvwoutput.close();
  
  cout << "Finished" << endl;
  
  return EXIT_SUCCESS;
}

void processInput(string inputfilename)
{
  char header[HEADER_LENGTH];
  
  ifstream input(inputfilename.c_str(), ios::in);
  if(input.fail())
    cerr << "Problem opening " << inputfilename << endl;
    
  //process the input file
  input.get(header, HEADER_LENGTH);
  getline(input, outfilename);
  
  input.get(header, HEADER_LENGTH);
  getline(input, line);
  numtelescopes = atoi(line.c_str());
  
  telescopenames = new char*[numtelescopes];
  mount = new string[numtelescopes];
  for(int i=0;i<numtelescopes;i++)
  {
    input.get(header, HEADER_LENGTH);
    getline(input, line);
    telescopenames[i] = new char[SITE_LENGTH+1];
    if(line.length() > SITE_LENGTH)
      cerr << "Error!!! Site name " << line << " is too long - max " << SITE_LENGTH << " characters" << endl;
    else
    {
      line.copy(telescopenames[i], line.length());
      for(unsigned int j=line.length();j<SITE_LENGTH;j++) {
	telescopenames[i][j] = ' '; //make it look like a FORTRAN string
        telescopenames[i][SITE_LENGTH] = 0; //make it look like a C string
      }
      //strcpy(telescopenames[i], line.c_str());
      //telescopenames[i][line.length()] = ' '; //stomp on the trailing null char
    }
    input.get(header, HEADER_LENGTH);
    getline(input, mount[i]);
  }
  
  //getline(input, line, ' ');
  input.get(header, HEADER_LENGTH);
  getline(input, line, ' ');
  utctag[0] = short(atoi(line.c_str()));
  getline(input, line, ' ');
  utctag[1] = short(atoi(line.c_str()));
  getline(input, line, ' ');
  utctag[2] = short(atoi(line.c_str()));
  getline(input, line, ' ');
  utctag[3] = short(atoi(line.c_str()));
  getline(input, line);
  utctag[4] = short(atoi(line.c_str()));
  
  input.get(header, HEADER_LENGTH);
  getline(input, line);
  timeincsec = atof(line.c_str());
  
  input.get(header, HEADER_LENGTH);
  getline(input, line);
  numscans = atoi(line.c_str());
  
  sourcenames = new char*[numscans];
  scanlength_incs = new int[numscans];
  
  for(int i=0;i<numscans;i++)
  {
    input.get(header, HEADER_LENGTH);
    getline(input,line);
    sourcenames[i] = new char[SOURCE_LENGTH];
    for(int j=0;j<SOURCE_LENGTH;j++) 
      sourcenames[i][j] = ' '; //make it look like a FORTRAN string
    strcpy(sourcenames[i], line.c_str());
    sourcenames[i][line.length()] = ' '; //stomp on the trailing null char
    input.get(header, HEADER_LENGTH);
    getline(input,line);
    scanlength_incs[i] = atoi(line.c_str());
  }
}
