/***************************************************************************
 *   Copyright (C) 2006-2016 Adam Deller, Walter Brisken & Chris Phillips  *
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
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <plplot/plplot.h>
#include <string.h>
#include <sstream>
#include <fstream>
#include "architecture.h"
#include "configuration.h"
#include "monserver.h"
#include <iomanip>

using namespace std;

#define MAXPOL 4

//prototypes
void plot_results(Configuration * config, Model * model);
void change_config(Configuration * config, int configindex, const char*);
void maxmin(f32 **vals, int nchan, int npol, float *max, float *min);

int socketnumber, currentconfig, maxresultlength, nav, atseconds, currentscan;
int resultlength, numchannels, nprod=0, mjd;
double intseconds = 1;
IppsFFTSpec_R_32f* fftspec=NULL;
Ipp8u *wbuf=NULL;
cf32 ** productvis=NULL;
f32  *phase[MAXPOL], *amplitude[MAXPOL];
f32 *lags[MAXPOL];
PLFLT *xval=NULL;

int main(int argc, const char * argv[])
{
  int i, status = 1, numchan;
  int prod, nvis, startsec;
  cf32 *vis;
  struct monclient monserver;
  Configuration * config;
  Model * model;
  vector<DIFX_ProdConfig> products;
  struct product_offset *plotprod;

  for (i=0; i<MAXPOL; i++) {
    phase[i] = NULL;
    amplitude[i] = NULL;
    lags[i] = NULL;
  }

  if(argc < 3 || argc > 4)
  {
    cerr << "Error - invoke with difx_monitor <inputfile> <host> [int time (s)]" << endl;
    return EXIT_FAILURE;
  }

  //work out the config stuff
  config = new Configuration(argv[1], 0);
  if (!config->consistencyOK()) {
    cerr << "Aborting" << endl;
    exit(1);
  }
  model = config->getModel();
  startsec = config->getStartSeconds();
  mjd = config->getStartMJD();

  if(argc == 4)
    intseconds = atof(argv[3]);

  status  = monserver_connect(&monserver, (char*)argv[2],  Configuration::MONITOR_TCP_WINDOWBYTES);
  if (status) exit(1);

  currentscan = 0;
  currentconfig = config->getScanConfigIndex(currentscan);
  products = monserver_productconfig(config, currentconfig);
  plotprod = NULL;
  status = set_productoffsets_all(&nprod, &plotprod, products);
  if (status) exit(1);
  cout << "Got " << nprod << " products" << endl;

  status = monserver_requestproducts_byoffset(monserver, plotprod, nprod);
  if (status) exit(1);

  //get into the loop of receiving, processing and plotting!
  atseconds = 0;
  change_config(config, currentconfig, argv[1]);

  umask(002);


  nvis=0;
  while (1) {

    status = monserver_readvis(&monserver);
    if (status) break;
    nvis++;

    cout << "Got visibility # " << monserver.timestamp << endl;

    if (monserver.timestamp==-1) continue;

    atseconds = monserver.timestamp-startsec;

    if (atseconds < model->getScanStartSec(currentscan, mjd, startsec)
	|| atseconds > model->getScanStartSec(currentscan, mjd, startsec)) {

      int scan = 0;
      while (scan<model->getNumScans()) {
	if (atseconds>model->getScanEndSec(scan, mjd, startsec)) {
	  scan++;
	} else {
	  break;
	}
      }
      if (scan>=model->getNumScans()) scan = model->getNumScans()-1;
      currentscan = scan;
      int newconfig = config->getScanConfigIndex(currentscan);

      if(newconfig != currentconfig) {
	currentconfig = newconfig;

	products = monserver_productconfig(config, currentconfig);
	status = set_productoffsets_all(&nprod, &plotprod, products);
	if (status) exit(1);
	cout << "Got " << nprod << " products" << endl;
  
	status = monserver_requestproducts_byoffset(monserver, plotprod, nprod);
	if (status) exit(1);

	change_config(config, currentconfig, argv[1]);
      }
    }

    while (!monserver_nextvis(&monserver, &prod, &numchan, &vis)) {
      if (prod>=nprod) {
	cerr << "Got product larger than expected - aborting" << endl;
	exit(1);
      }

      status = vectorAdd_cf32_I(vis, productvis[prod], numchannels);

      if(status != vecNoErr) {
	cerr << "Error trying to add to accumulation buffer - aborting!" << endl;
	exit(1);
      }
    }

    //plot
    if(nvis==nav) {
      plot_results(config, model);
      nvis=0;
      for (i=0; i<nprod; i++) {
	status = vectorZero_cf32(productvis[i], numchannels);
	if(status != vecNoErr) {
	  cerr << "Error trying to zero visibility buffer - aborting!" << endl;
	  exit(1);
	}
      }
    }
  }

  //close the socket
  monserver_close(&monserver);
}

void plot_results(Configuration * config, Model * model)
{
  int i, j, k, npol;
  char plplotname[256];
  char polpair[MAXPOL][3];
  char timestr[10];
  string sourcename;
  ostringstream ss;
  f32 temp;
  cf32 div;
  int status;
  int colours[MAXPOL] = {2,3,4,5};

  for (i=0; i<MAXPOL; i++)  polpair[i][2] = 0;

  div.re = 1.0/nav;
  div.im = 0;
  for(int i=0;i<nprod;i++) {

    status = vectorMulC_cf32_I(div, productvis[i], numchannels);
    if(status != vecNoErr) {
          cerr << "Error trying to add to accumulation buffer - aborting!" << endl;
	  exit(1);
    }
  }

  int binloop = 1;
  if(config->pulsarBinOn(currentconfig) && !config->scrunchOutputOn(currentconfig))
    binloop = config->getNumPulsarBins(currentconfig);

  int at = 0;
  for(i=0;i<config->getNumBaselines();i++)  {

    int ds1index = config->getBDataStream1Index(currentconfig, i);
    int ds2index = config->getBDataStream2Index(currentconfig, i);

    for(j=0;j<config->getBNumFreqs(currentconfig,i);j++)    {
      int freqindex = config->getBFreqIndex(currentconfig, i, j);
      for (int b=0;b<binloop;b++) {
	npol = config->getBNumPolProducts(currentconfig,i,j);
	if (npol>MAXPOL) {
	  cerr << "Too many polarisation products - aborting" << endl;
	  exit(1);
	}
        for(k=0;k<npol;k++) {
          config->getBPolPair(currentconfig,i,j,k,polpair[k]);

	  // Calculate amplitude, phase and lags
	  status = vectorPhase_cf32(productvis[at], phase[k], numchannels);
	  if(status != vecNoErr) {
	    cerr << "Error trying to calculate phase - aborting!" << endl;
	    exit(1);
	  }
	  vectorMulC_f32_I(180/M_PI, phase[k], numchannels);

	  status = vectorMagnitude_cf32(productvis[at], amplitude[k], numchannels);
	  if(status != vecNoErr) {
	    cerr << "Error trying to calculate amplitude - aborting!" << endl;
	    exit(1);
	  }

	  ippsFFTInv_CCSToR_32f((Ipp32f*)productvis[at], lags[k], fftspec, wbuf);
	  //rearrange the lags into order
	  for(int l=0;l<numchannels;l++) {
            temp = lags[k][l];
            lags[k][l] = lags[k][l+numchannels];
	    lags[k][l+numchannels] = temp;
          }
          at++;
	}

	//plot something - data is from resultbuffer[at] to resultbuffer[at+numchannels+1]
	sprintf(plplotname, "lba-%d-f%d-b%d.png", i, j, b);
	plsdev("png");
	plsfnam(plplotname);
	plstart("png",1,3);

	//FIXME
	status = 1;

	if (status != 1) {
	  cout << "Error opening plplot device: " << plplotname << endl;
	} else {
	  float max, min;
	  PLFLT *pllags;
	  PLFLT *plamp;
	  PLFLT *plphase;

	  // FIXME
	  //cpgscr(0,1,1,1);
	  //cpgscr(1,0,0,0);

	  // Plot lags
	  maxmin(lags, numchannels*2, npol, &max, &min);

	  plschr(0,1.3);
	  plcol0(1);
	  plenv(0,numchannels*2,min,max,0,0);
	  pllab("Channel", "Correlation coefficient", "");
	  plcol0(2);


	  pllags = new PLFLT[numchannels*2];
	  for (k=0; k<npol; k++) {
	    for(int q = 0; q < numchannels*2; ++q) pllags[q] = lags[k][q];
	    plcol0(colours[k]);
	    plline(numchannels*2, xval, pllags);
	  }
	  delete [] pllags;

	  // Annotate
	  //sourcename = model->getScanIdentifier(currentscan);
	  sourcename = model->getScanPointingCentreSource(currentscan)->name;

	  plcol1(4);
	  plschr(0, 1.5);

	  ss << config->getTelescopeName(ds1index) 
	     << "-" 
	     <<  config->getTelescopeName(ds2index)
	     << "  " << sourcename; 

	  plmtex("T", -1.5,0.02, 0, ss.str().c_str());	    
	  ss.str("");

	  ss << config->getFreqTableFreq(freqindex) << " MHz";
	  plmtex("T", -2.6,0.98,1,ss.str().c_str());	    
	  ss.str("");

	  int seconds = atseconds+config->getStartSeconds();
	  int hours = seconds/3600;
	  seconds -= hours*3600;
	  int minutes = seconds/60;
	  seconds %= 60;
	  sprintf(timestr, "%02d:%02d:%02d", hours, minutes, seconds);
	  plmtex("T", -1.5,0.98,1,timestr);

	  for (k=0; k<npol; k++) {
	    int p = npol-k-1;
	    plcol0(colours[p]);
	    plmtex("B", -1,0.97-0.03*k,1,polpair[p]);
	  }

	  plschr(0, 1.5);
	  plcol0(1);

	  // Plot Amplitude
	  maxmin(amplitude, numchannels, npol, &max, &min);

	  plcol0(1);
	  plenv(0,numchannels,min,max,0,0);
	  pllab("Channel", "Amplitude (Jy)", "");
	  plcol0(2);

	  plamp = new PLFLT[numchannels];
	  for (k=0; k<npol; k++) {
	    plcol0(colours[k]);
	    for(int q = 0; q < numchannels; ++q) plamp[q] = amplitude[k][q];
	    plline(numchannels, xval, plamp);
	  }
	  delete [] plamp;


	  // Plot Phase
	  plcol0(1);
	  plenv(0,numchannels,-180,180,0,0);
	  pllab("Channel", "Phase (deg)", "");
	  plcol0(2);
	  plschr(0, 1);

	  plphase = new PLFLT[numchannels];
	  for (k=0; k<npol; k++) {
	    plcol0(colours[k]);
	    for(int q = 0; q < numchannels; ++q) plphase[q] = phase[k][q];
	    plsym(numchannels, xval, plphase, 17);
	  }
	  delete [] plphase;
	  plcol0(1);

	  plend();

        }
      }
    }
  }

  int autocorrwidth = (config->getMaxProducts()>1)?2:1;
  for(int i=0;i<config->getNumDataStreams();i++) {
    for(int j=0;j<autocorrwidth;j++) {
      for(int k=0;k<config->getDNumRecordedBands(currentconfig, i); k++) {

	if (j==0) {
	  sprintf(plplotname, "lba-auto%d-f%d-b0.png",
		  i, k);
	  plsdev("png");
	  plsfnam(plplotname);
	  plstart("png",1,1);
	} else {
	  sprintf(plplotname, "lba-autocross%d-b%d.png",
		  i, k);
	  plsdev("png");
	  plsfnam(plplotname);
	  plstart("png",1,2);
	}
	{
	  float max, min;
	  PLFLT *plamp;

	  //FIXME
	  //cpgscr(0,1,1,1);
	  //cpgscr(1,0,0,0);

	  // Plot Amplitude

	  status = vectorMagnitude_cf32(productvis[at], amplitude[0], numchannels);
	  if(status != vecNoErr) {
	    cerr << "Error trying to calculate amplitude - aborting!" << endl;
	    exit(1);
	  }

	  max = amplitude[0][0];
	  min = max;
	  for (int n=1; n<numchannels; n++) {
	    if (amplitude[0][n] > max) max = amplitude[0][n];
	    if (amplitude[0][n] < min) min = amplitude[0][n];
	  }
	  if (min==max) {
	    min-=1;
	    max+=1;
	  }

	  plcol0(1);
	  plenv(0,numchannels+1,min,max,0,0);
	  pllab("Channel", "Amplitude (Jy)", "");
	  plcol0(2);
	  plamp = new PLFLT[numchannels];
	  for(int q = 0; q < numchannels; ++q) plamp[q] = amplitude[0][q];
	  plline(numchannels, xval, plamp);
	  delete [] plamp;
	  // Annotate
	  plcol0(4);

	  if (j==0) {
	    plschr(0, 1);
	  } else {
	    plschr(0, 1.5);
	  }

	  ss << config->getDStationName(currentconfig, i) 
	     << "  " << sourcename; 
	  plmtex("T", -1.5,0.02, 0, ss.str().c_str());	    
	  ss.str("");

	  int freqindex = config->getDRecordedFreqIndex(currentconfig, i, k);
	  ss << config->getFreqTableFreq(freqindex) << " MHz";
	  plmtex("T", -2.6,0.98,1,ss.str().c_str());	    
	  ss.str("");

	  ss << timestr << "     " 
	     << config->getDRecordedBandPol(currentconfig, i, k);
	  if (j==0)
	    ss << config->getDRecordedBandPol(currentconfig, i, k);
	  else
	    ss << ((config->getDRecordedBandPol(currentconfig, i, k) == 'R')?'L':'R');
	  plmtex("T", -1.5,0.98,1,ss.str().c_str());
	  ss.str("");

	  if (j!=0) {
	    PLFLT *plphase;
	    status = vectorPhase_cf32(productvis[at], phase[0], numchannels);
	    if(status != vecNoErr) {
	      cerr << "Error trying to calculate phase - aborting!" << endl;
	      exit(1);
	    }
	    vectorMulC_f32_I(180/M_PI, phase[0], numchannels);

	    // Plot Phase
	    plcol0(1);
	    plschr(0, 1);
	    plenv(0,numchannels,-180,180,0,0);
	    pllab("Channel", "Phase (deg)", "");
	    plcol0(2);
	    
	    plphase = new PLFLT[numchannels];
	    for(int q = 0; q < numchannels; ++q) plphase[q] = phase[0][q];
	    plline(numchannels, xval, plphase);
	    delete [] plphase;
	  }

	  plend();
	}
	at++;
      }
    }
  }
}

void change_config(Configuration * config, int configindex, const char *inputfile) {
  int status, i;
  ofstream html;
  ostringstream ss;

  numchannels = config->getFNumChannels(currentconfig);  /* This is the wrong way to do things, but seems
							    to work for now. */

  cout << "New config " << currentconfig << " at " << atseconds << endl;

  if(xval) delete [] xval;
  if(fftspec) ippFree(fftspec);

  for (i=0; i<MAXPOL; i++) {
    if(lags[i]) vectorFree(lags[i]);
    if(phase[i]) vectorFree(phase[i]);
    if(amplitude[i]) vectorFree(amplitude[i]);
  }
  if (wbuf) ippFree(wbuf);
  
  if (productvis) {
    for (i=0; i<nprod; i++) {
      if (productvis[i]) vectorFree(productvis[i]);
    }
    delete [] productvis;
  }

  xval = new PLFLT[numchannels*2];
  for(int i=0;i<numchannels*2;i++)
    xval[i] = i;

  cout << "Allocate arrays" << endl;
  for (i=0; i<MAXPOL; i++) {
    lags[i] = vectorAlloc_f32(numchannels*2);
    phase[i] = vectorAlloc_f32(numchannels);
    amplitude[i] = vectorAlloc_f32(numchannels);
  }

  int order = 0;
  while(((numchannels*2) >> order) > 1)
    order++;
#ifdef IPP9
  int sizeFFTSpec, sizeFFTInitBuf, wbufsize;
  u8 *fftInitBuf, *fftSpecBuf;
  
  ippsFFTGetSize_R_32f(order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast,  &sizeFFTSpec, &sizeFFTInitBuf, &wbufsize);
  fftSpecBuf = ippsMalloc_8u(sizeFFTSpec);
  if (sizeFFTInitBuf>0)
    fftInitBuf = ippsMalloc_8u(sizeFFTInitBuf);
  else
    fftInitBuf=NULL;
  if (wbufsize>0)
    wbuf = ippsMalloc_8u(wbufsize);
  else
    wbuf=NULL;

  // Initialize FFT
  ippsFFTInit_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast, fftSpecBuf, fftInitBuf);
  if (fftInitBuf) ippFree(fftInitBuf);
#else
  ippsFFTInitAlloc_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);
#endif
  
  nav = (int)ceil(intseconds/config->getIntTime(currentconfig));
  cout << "#Integrations to average = " << nav << endl;

  resultlength = config->getCoreResultBWeightOffset(currentconfig, 0, 0);

  productvis = new cf32*[nprod];
  for (i=0; i<nprod; i++) {
    productvis[i] = vectorAlloc_cf32(numchannels);

    status = vectorZero_cf32(productvis[i], numchannels);
    if(status != vecNoErr) {
      cerr << "Error trying to zero visibility buffer - aborting!" << endl;
      exit(1);
    }
  }

  // Generate HTML
  vector <string> filebase;
  vector <string> title;

  html.open("index.html", ios::out|ios::trunc);
  if (!html.is_open()) {
    cerr << "Warning: Failed to open index.html for writing" << endl;
    return;
  }

  html << "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" << endl;
  html << "<html>" << endl;
  html << "<head>" << endl;
  html << "<meta content=\"text/html; charset=ISO-8859-1\" http-equiv=\"content-type\">" << endl;
  html << "<title>" << inputfile << "</title>" << endl;
  html << "</head>" << endl;
  html << "<body>" << endl;
  html << "<h2 style=\"text-align: center;\">" << inputfile << "</h2>" << endl;
  html << "<table style=\"text-align: left; width: 100%;\" border=\"1\" cellpadding=\"2\" cellspacing=\"2\">" << endl;
  html << "<tbody>" << endl;

  int binloop = 1;
  if(config->pulsarBinOn(currentconfig) && !config->scrunchOutputOn(currentconfig))
    binloop = config->getNumPulsarBins(currentconfig);

  for (int i=0;i<config->getNumBaselines();i++) {
    int ds1index = config->getBDataStream1Index(currentconfig, i);
    int ds2index = config->getBDataStream2Index(currentconfig, i);

    html << "<tr>" << endl;
    html << "<th style=\"text-align: center; background-color: rgb(204, 255, 255);\">" 
	 << config->getTelescopeName(ds1index) << "-" 
	 << config->getTelescopeName(ds2index) << "</th>" << endl;

    for(int j=0;j<config->getBNumFreqs(currentconfig,i);j++) {
      int freqindex = config->getBFreqIndex(currentconfig, i, j);

      // Skip over pulsar bins and polarisation
      for(int b=0;b<binloop;b++) {
	for(int k=0;k<config->getBNumPolProducts(currentconfig, i, j);k++) {
	}
      }
      
      html <<  "<td style=\"text-align: center;\">"
	   << "<a href=\"lba-" << i << "-f" << j << ".html\">" 
	   << config->getFreqTableFreq(freqindex) << " MHz" << "</a></td>" << endl;
      
      ss << config->getTelescopeName(ds1index) << "-" 
	 << config->getTelescopeName(ds2index) << "   "
	 << config->getFreqTableFreq(freqindex) << " MHz";
      title.push_back(ss.str());
      ss.str("");

      ss << "lba-" << i << "-f" << j;
      filebase.push_back(ss.str());
      ss.str("");
	    
    }
    html << "</tr>" << endl;
  }

  html << "</tbody>" << endl;
  html << "</table>" << endl;
  html << "<br><br>" << endl;

  // Autocorrelations
  html << "<table style=\"text-align: left; width: 100%;\" border=\"1\" cellpadding=\"2\" cellspacing=\"2\">" << endl;
  html << "<tbody>" << endl;

  //int autocorrwidth = (config->getMaxProducts()>2)?2:1;

  for(int i=0;i<config->getNumDataStreams();i++) {

    html << "<tr>" << endl;
    html << "<th style=\"text-align: center; background-color: rgb(204, 255, 255);\">" 
	 << config->getDStationName(currentconfig, i)
	 << "</th>" << endl;


    //for(int j=0;j<autocorrwidth;j++) {
    for(int k=0;k<config->getDNumRecordedBands(currentconfig, i); k++) {

      char pol = config->getDRecordedBandPol(currentconfig, i, k);

      int freqindex = config->getDRecordedFreqIndex(currentconfig, i, k);

      html << "<td style=\"text-align: center;\">" <<   config->getFreqTableFreq(freqindex) 
	   << " MHz <a href=\"lba-auto" << i << "-f" << k << ".html\"> " << pol << "cp </a></td>" 
	   << endl;

      ss << config->getDStationName(currentconfig,i) << "  " 
	 << config->getFreqTableFreq(freqindex) << " MHz";
      title.push_back(ss.str());
      ss.str("");

      ss << "lba-auto" << i << "-f" << k;
      filebase.push_back(ss.str());
      ss.str("");
    }
    html << "<tr>" << endl;
  }

  html << "</tbody>" << endl;
  html << "</table>" << endl;
  html << "</body>" << endl;
  html << "</html>" << endl;

  html.close();

  for (unsigned int i=0; i< title.size(); i++) {
    string filename;
    filename = filebase[i]+".html";
    html.open(filename.c_str(), ios::out|ios::trunc);
    if (!html.is_open()) {
      cerr << "Warning: Failed to open " << filename <<  "for writing" << endl;
      return;
    }

    html << "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" << endl;
    html << "<html>" << endl;
    html << "<head>" << endl;
    html << "<meta content=\"text/html; charset=ISO-8859-1\" http-equiv=\"content-type\">" << endl;
    html << "<meta http-equiv=\"Refresh\" content=\"2\">" << endl;
    html << "<title> " << title[i] << "</title>" << endl;
    html << "</head>" << endl;
    html << "<body>" << endl;
    html << "<img src=\"" << filebase[i] << "-b0.png\">" << endl;
    html << "</body>" << endl;
    html << "</html>" << endl;
    html.close();
  }
}



void maxmin(f32 **vals, int nchan, int npol, float *max, float *min) {
  float delta;

  ippsMax_32f(vals[0], nchan, max);
  ippsMin_32f(vals[0], nchan, min);

  for (int i=1; i<npol; i++) {
    float thismax, thismin;

    ippsMax_32f(vals[i], nchan, &thismax);
    ippsMin_32f(vals[i], nchan, &thismin);
    if (thismax>*max) *max = thismax;
    if (thismin<*min) *min = thismin;
  }

  delta = (*max-*min)*0.05;
  if (delta<0.5) delta = 1;
  *min -= delta;
  *max += delta;

  return;
}
