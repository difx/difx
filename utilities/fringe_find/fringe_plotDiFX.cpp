#include <iostream>
#include <cstdlib>
#include <string>
#include <stdio.h>
#include <fstream>
#include <vector>
#include <math.h>
#include <ipps.h>
#include <cmath>
#include "configuration.h"

using namespace std;

int main(int argc, char** argv)
{
  if(argc != 3 && argc !=4 && argc !=5)
  {
    cerr << "Usage: fringe_plotDiFX <difx data file> <config file> [sourcename] [-D (for debug on)]" << endl;
    return EXIT_FAILURE;
  }
  bool debugon = false;
  string targetsource = "";
  if(argc == 5) {
    debugon = true;
    targetsource = string(argv[3]);
  }
  else if(argc == 4) {
    if(argv[3][0] == '-' && argv[3][1] == 'D')
      debugon = true;
    else
      targetsource = string(argv[3]);
  }

  static const int SEARCH_LENGTH = 16;
  static const float PI = 3.14159265358979323846264338;
  string pols[] = {"RR","LL","RL","LR"};
  string line, pol, srcname;
  int baseline, mjd, sec, confindex, srcindex, freqindex, pbin, flag, weight_add, numchannels, lastnumchannels, order, status, coarseindex, fineindex, basicindex, baselineindex, polindex, minindex, maxindex;
  float stddev, minval, maxval, basicsnr, maxamp;
  Ipp32fc bandsum;
  double u,v,w;
  bool weights_used;
  string filename = argv[1];
  int sepindex = filename.find_last_of('.');
  int numvispoints = atoi(filename.substr(sepindex+1).c_str());
  Configuration * config = new Configuration(argv[2]);
  config->loaduvwinfo(true);
  int maxnumchannels = 0;
  for(int i=0;i<config->getNumConfigs();i++) {
    if(config->getNumChannels(i) > maxnumchannels)
      maxnumchannels = config->getNumChannels(i);
  }
  lastnumchannels = -1;
  float * visibilities = ippsMalloc_32f(maxnumchannels*3); //worst case
  if(visibilities == NULL)
    cerr << "Error - could not allocate memory for visibility array!" << endl;
  status = ippsZero_32f(visibilities, maxnumchannels*3);
  if (status != ippStsNoErr)
    cerr << "Error trying to zero visibilities!!!" << endl;
  float * timedomain = ippsMalloc_32f(maxnumchannels*2);
  float * lags = ippsMalloc_32f(maxnumchannels*2);
  float * rotated = ippsMalloc_32f(maxnumchannels*2);
  float *** rotator = new float**[config->getNumConfigs()];
  float *** finerotator = new float**[config->getNumConfigs()];
  vector<float> **** amplitudes = new vector<float>***[config->getNumConfigs()];
  vector<float> **** phases = new vector<float>***[config->getNumConfigs()];
  vector<float> **** delays = new vector<float>***[config->getNumConfigs()];
  vector<float> **** snrs = new vector<float>***[config->getNumConfigs()];
  cout << "About to start going through the configs creating vectors.." << endl;
  for(int i=0;i<config->getNumConfigs();i++) {
    rotator[i] = new float*[SEARCH_LENGTH+1];
    finerotator[i] = new float*[SEARCH_LENGTH+1];
    for (int j=-SEARCH_LENGTH/2;j<=SEARCH_LENGTH/2;j++) {
      rotator[i][j+SEARCH_LENGTH/2] = ippsMalloc_32f(config->getNumChannels(i)*2);
      finerotator[i][j+SEARCH_LENGTH/2] = ippsMalloc_32f(config->getNumChannels(i)*2);
      if(rotator[i][j+SEARCH_LENGTH/2] == NULL || finerotator[i][j+SEARCH_LENGTH/2] == NULL)
	cerr << "Error - could not allocate memory!!!" << endl;
      if(debugon)
	cout << "Doing rotator " << i << ", " << j << endl;
      for (int k=0;k<config->getNumChannels(i);k++) {
	rotator[i][j+SEARCH_LENGTH/2][2*k] = sin(PI*float(j*k)/float(config->getNumChannels(i)*SEARCH_LENGTH/2));
	rotator[i][j+SEARCH_LENGTH/2][2*k+1] = cos(PI*float(j*k)/float(config->getNumChannels(i)*SEARCH_LENGTH/2));
	finerotator[i][j+SEARCH_LENGTH/2][2*k] = sin(PI*float(j*k)/float(SEARCH_LENGTH*config->getNumChannels(i)*SEARCH_LENGTH/2));
	finerotator[i][j+SEARCH_LENGTH/2][2*k+1] = cos(PI*float(j*k)/float(SEARCH_LENGTH*config->getNumChannels(i)*SEARCH_LENGTH/2));
      }
    }
  }
  if(debugon)
    cout << "Finished initialising rotators" << endl;
  int numbaselines = 0;
  bool found;
  int * baselinenumbers = new int[config->getNumBaselines()];
  for(int i=0;i<config->getNumBaselines();i++) {
    found = false;
    for(int j=0;j<numbaselines;j++) {
      if(baselinenumbers[j] == config->getBNumber(0, i))
	found = true;
    }
    if(!found)
      baselinenumbers[numbaselines++] = config->getBNumber(0, i);
  }
  for(int i=0;i<config->getNumConfigs();i++) {
    amplitudes[i] = new vector<float>**[numbaselines];
    phases[i] = new vector<float>**[numbaselines];
    delays[i] = new vector<float>**[numbaselines];
    snrs[i] = new vector<float>**[numbaselines];
    for (int j=0;j<numbaselines;j++) {
      amplitudes[i][j] = new vector<float>*[config->getFreqTableLength()];
      phases[i][j] = new vector<float>*[config->getFreqTableLength()];
      delays[i][j] = new vector<float>*[config->getFreqTableLength()];
      snrs[i][j] = new vector<float>*[config->getFreqTableLength()];
    }
    for (int j=0;j<config->getNumBaselines();j++) {
      for (int k=0;k<config->getBNumFreqs(i,j);k++) {
        int findex = config->getBFreqIndex(i,j,k);
	int bindex = 0;
	for(int l=0;l<numbaselines;l++) {
	  if(config->getBNumber(i,j) == baselinenumbers[l])
	    bindex = l;
	}
	amplitudes[i][bindex][findex] = new vector<float>[config->getBNumPolProducts(i,j,k)];
	phases[i][bindex][findex] = new vector<float>[config->getBNumPolProducts(i,j,k)];
	delays[i][bindex][findex] = new vector<float>[config->getBNumPolProducts(i,j,k)];
	snrs[i][bindex][findex] = new vector<float>[config->getBNumPolProducts(i,j,k)];
      }
    }
  }
  IppsFFTSpec_R_32f* fftspec;

  ifstream difxin(argv[1]);
  if(!difxin.is_open())
    cerr << "Error opening file " << argv[1] << endl;

  if(difxin.eof())
    cerr << "End of file!" << endl;

  if(difxin.bad())
    cerr << "Some kind of failure for input file!!!" << endl;

  for(int i=0;i<numvispoints;i++) {
    if(debugon)
      cout << "About to read header for vis point " << i << "/" << numvispoints << endl;
    getline(difxin, line);
    if(line == "") {
      cout << "Warning - blank line?? difxin.eof() is " << difxin.eof() << ", difxin.bad() is " << difxin.bad() << endl;
      if(difxin.eof())
	exit(0);
    }
    baseline = atoi(line.substr(20).c_str());
    getline(difxin, line);
    mjd = atoi(line.substr(20).c_str());
    getline(difxin, line);
    sec = atoi(line.substr(20).c_str());
    getline(difxin, line);
    confindex = atoi(line.substr(20).c_str());
    getline(difxin, line);
    srcindex = atoi(line.substr(20).c_str());
    getline(difxin, line);
    freqindex = atoi(line.substr(20).c_str());
    getline(difxin, line);
    pol = line.substr(20);
    getline(difxin, line);
    pbin = atoi(line.substr(20).c_str());
    getline(difxin, line);
    flag = atoi(line.substr(20).c_str());
    getline(difxin, line);
    weight_add = atoi(line.substr(20).c_str());
    weights_used = (weight_add == 0)?false:true;
    getline(difxin, line);
    u = atof(line.substr(20).c_str());
    getline(difxin, line);
    v = atof(line.substr(20).c_str());
    getline(difxin, line);
    w = atof(line.substr(20).c_str());

    if(debugon)
      cout << "About to read the actual visibilities for time " << mjd << "/" << sec << endl;
    numchannels = config->getNumChannels(confindex);
    difxin.read((char*)visibilities, numchannels*(2+weight_add)*sizeof(float));

    (config->getUVW())->getSourceName(mjd, sec, srcname);
    srcname.erase(srcname.find(' '));
    if (debugon)
      cout << "Sourecname is " << srcname << ", desired is " << targetsource << endl;
    if(targetsource != "" && !(targetsource == srcname)) //don't want this source
      continue;

    if (baseline % 257 == 0) {//autocorrelation, skip
      if (debugon)
	cout << "Skipping autocorrelation for baseline " << baseline << endl;
      continue;
    }

    baselineindex = 0;
    while(baselinenumbers[baselineindex] != baseline)
      baselineindex++;

    polindex = 0;
    for(int j=0;j<4;j++) {
      if (pols[j] == pol)
	polindex = j;
    }
    while(polindex > config->getBNumPolProducts(confindex,baselineindex,freqindex))
      polindex--;

    //recreate FFT spec if needed
    if (numchannels != lastnumchannels) {
      if (lastnumchannels > 0)
	ippsFFTFree_R_32f(fftspec);
      lastnumchannels = numchannels;
      order = 0;
      while(((numchannels*2) >> order) > 1)
	order++;
      ippsFFTInitAlloc_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);
      if(debugon)
	cout << "Allocated an FFT spec of length " << numchannels << endl;
    }

    //re-order if weights were used
    if (weights_used) {
      if(debugon)
	cout << "Re-ordering to excise weights from vis array" << endl;
      for(int j=1;j<numchannels;j++) {
	visibilities[j*2] = visibilities[j*3];
	visibilities[j*2+1] = visibilities[j*3+1];
      }
    }

    //inverse FFT to lagspace
    status = ippsFFTInv_CCSToR_32f(visibilities, timedomain, fftspec, 0);
    if(status != ippStsNoErr)
      cerr << "Error doing the inverse FFT to lagspace!!!" << endl;

    //rearrange the lags into order
    for(int j=0;j<numchannels*2;j++)
      lags[(j+numchannels)%(2*numchannels)] = timedomain[j];

    //search for max
    ippsStdDev_32f(lags, numchannels*2, &stddev, ippAlgHintFast);
    if(status != ippStsNoErr)
      cerr << "Error doing the stddev of the lags!!!" << endl;
    ippsMinMaxIndx_32f(lags, numchannels*2, &minval, &minindex, &maxval, &maxindex);
    if(status != ippStsNoErr)
      cerr << "Error doing the min/max of the lags!!!" << endl;
    basicsnr = ((abs(minval)>maxval)?(abs(minval)/stddev):(maxval/stddev));
    basicindex = ((abs(minval)>maxval)?minindex:maxindex) - numchannels;
    if(isnan(basicsnr)) {
      if(debugon)
	cout << "Basicsnr is NaN, most likely the visibilities were all zero - so I'm skipping to the next entry!" << endl;
      continue;
    }
    if(debugon)
      cout << "Basicsnr is " << basicsnr << ". basicindex is " << basicindex << endl;

    if(basicindex != 0) {
      //get the visibilities to the nearest sample
      for(int j=0;j<numchannels;j++) {
	rotated[2*j] = sin(2*PI*float(j*basicindex)/float(numchannels));
	rotated[2*j+1] = cos(2*PI*float(j*basicindex)/float(numchannels));
      }
      status = ippsMul_32fc_I((Ipp32fc*)rotated, (Ipp32fc*)visibilities, numchannels);
      if(status != ippStsNoErr)
	cerr << "Error trying to rotate to nearest sample!!!" << endl;

      if(debugon)
	cout << "Rotated to the nearest sample" << endl;
    }
    else {
      if (debugon)
	cout << "Not rotating to nearest sample - already there!" << endl;
    }

    //do a coarse search of +- half a sample delay at 1/16 incs
    maxamp = 0;
    for (int j=-SEARCH_LENGTH/2;j<=SEARCH_LENGTH/2;j++) {
      status = ippsMul_32fc((Ipp32fc*)visibilities, (Ipp32fc*)(rotator[confindex][j+SEARCH_LENGTH/2]), (Ipp32fc*)rotated, numchannels);
      if(status != ippStsNoErr)
        cerr << "Error trying to rotate!!!" << endl;
      status = ippsSum_32fc((Ipp32fc*)rotated, numchannels, &bandsum, ippAlgHintFast);
      if(status != ippStsNoErr)
        cerr << "Error trying to sum for coarse guess!!!" << endl;
      if(debugon)
	cout << "Coarse rotate " << j << ": Bandsum was " << bandsum.re << " + " << bandsum.im << " i, maxamp is " << maxamp << endl;
      if(sqrt(bandsum.re*bandsum.re + bandsum.im*bandsum.im) > maxamp) {
	maxamp = sqrt(bandsum.re*bandsum.re + bandsum.im*bandsum.im);
	coarseindex = j;
      }
    }

    if(debugon)
      cout << "Done the coarse search - will rotate to coarseindex " << coarseindex << endl;

    //rotate the visibilities to the nearest coarse value
    status = ippsMul_32fc_I((Ipp32fc*)(rotator[confindex][coarseindex+SEARCH_LENGTH/2]), (Ipp32fc*)visibilities, numchannels);
    if(status != ippStsNoErr)
      cerr << "Error trying to rotate to nearest coarse delay!!!" << endl;

    if(debugon)
      cout << "Rotated to the nearest coarse index" << endl;

    //do a fine search around the best result
    maxamp = 0;
    for (int j=-SEARCH_LENGTH/2;j<=SEARCH_LENGTH/2;j++) {
      status = ippsMul_32fc((Ipp32fc*)visibilities, (Ipp32fc*)(finerotator[confindex][j+SEARCH_LENGTH/2]), (Ipp32fc*)rotated, numchannels);
      if(status != ippStsNoErr)
        cerr << "Error trying to rotate!!!" << endl;
      status = ippsSum_32fc((Ipp32fc*)rotated, numchannels, &bandsum, ippAlgHintFast);
      if(status != ippStsNoErr)
        cerr << "Error trying to sum after fine rotate!!!" << endl;
      if(debugon)
	cout << "Fine rotate " << j << ": Bandsum was " << bandsum.re << " + " << bandsum.im << " i, maxamp is " << maxamp << endl;
      if(sqrt(bandsum.re*bandsum.re + bandsum.im*bandsum.im) > maxamp) {
	maxamp = sqrt(bandsum.re*bandsum.re + bandsum.im*bandsum.im);
	fineindex = j;
      }
    }
    
    if(debugon)
      cout << "Done the fine search! Now will store results.  Confindex is " << confindex << ", baselineindex is " << baselineindex << ", freqindex is " << freqindex << ", polindex is " << polindex << endl;
    
    //store the results
    amplitudes[confindex][baselineindex][freqindex][polindex].push_back(maxamp/numchannels);
    phases[confindex][baselineindex][freqindex][polindex].push_back(180.0*(float(coarseindex) + float(fineindex)/float(SEARCH_LENGTH))/float(SEARCH_LENGTH));
    delays[confindex][baselineindex][freqindex][polindex].push_back((((float(coarseindex) + float(fineindex)/float(SEARCH_LENGTH))/float(SEARCH_LENGTH))+basicindex)*(1000.0/config->getConfigBandwidth(confindex)));
    snrs[confindex][baselineindex][freqindex][polindex].push_back(basicsnr);

    if(debugon)
      cout << "For baseline " << baseline << ", at time " << mjd << "/" << sec << ", the source was " << srcindex << " and the config was " << confindex << ", with uvw (" << u << "," << v << "," << w << ").  For freq " << freqindex << ", pol " << pol << ", pulsar bin " << pbin << ", we have snr " << basicsnr << ", amplitude " << maxamp/numchannels << ", delay " << delays[confindex][baselineindex][freqindex][polindex][delays[confindex][baselineindex][freqindex][polindex].size() - 1] << ", basicindex was " << basicindex << endl;
    if(basicsnr > 6)
      cout << "Fringe found: b/line " << baseline << ", time " << mjd << "/" << sec << ": snr " << basicsnr << ", index (" << basicindex << "/" << coarseindex << "/" << fineindex << "), delay " << delays[confindex][baselineindex][freqindex][polindex][delays[confindex][baselineindex][freqindex][polindex].size() - 1] << "ns" << endl;
  }
}
