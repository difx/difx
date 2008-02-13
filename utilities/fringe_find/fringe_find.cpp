//my bash at a quick stats package for the text dump output of fxcorr

#include <vector>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <cmath>
#include <ipps.h>

static const float SIGMA_LIMIT = 4.0;

using namespace std;

int main(int argc, char *argv[])
{
  if(argc != 3)
    {
      cerr << "Must have 2 parameters: correct invocation: fringe_find <filename> <numchannels>" << endl;
      return EXIT_FAILURE;
    }

  string line;
  int numchannels = atoi(argv[2]);
  Ipp32f* amplitudes = ippsMalloc_32f(numchannels + 1);
  Ipp32f* phases = ippsMalloc_32f(numchannels + 1);
  Ipp32f* realcomponent = ippsMalloc_32f(numchannels + 1);
  Ipp32f* imagcomponent = ippsMalloc_32f(numchannels + 1);
  Ipp32fc* complexfrequency = ippsMalloc_32fc(numchannels + 1);
  Ipp32f* lags = ippsMalloc_32f(numchannels*2);
  Ipp32f* timedomain = ippsMalloc_32f(numchannels*2);
  Ipp32f stddev, minval, maxval, basicsnr, derivsnr, bestsnr;
  int minindex, maxindex, basicindex, bestindex, otherindex;
  IppsFFTSpec_R_32f* fftspec;
  string commentstring;
  ifstream input;
  int order = 0;
  //float sumx = 0.0;
  //float sumy = 0.0;
  //float sumxsquared = 0.0;
  //float sumysquared = 0.0;
  //float sumxy = 0.0;
  //float a = 0.0;
  //float b = 0.0;
  //Ipp32f rms, meanval, snr;
  //Ipp32f * residuals = ippsMalloc_32f(maxchan-minchan);

  while(((numchannels*2) >> order) > 1)
    order++;

  ippsFFTInitAlloc_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);

  //work out if its a binary or ascii file
  int index = string(argv[1]).find_first_of('.');
  string extension = string(argv[1]).substr(index+1);
  if(extension == "binout") //its a binary file
  {
    cout << "Processing binary file" << endl;
    input.open(argv[1],ios::in|ios::binary);
    if(!input.is_open() || input.fail())
    {
      cerr << "Error - could not open file " << argv[1] << endl;
      exit(1);
    }
    for(int i=0;i<numchannels+1;i++)
    {
      input.read((char*)&realcomponent[i], sizeof(float));
      input.read((char*)&imagcomponent[i], sizeof(float));
    }
  }
  else
  {
    input.open(argv[1],ios::in);
    if(!input.is_open() || input.fail())
    {
      cerr << "Error - could not open file " << argv[1] << endl;
      exit(1);
    }

    //ignore the channel number
    getline(input, line, ' ');

    //read in the data
    for(int i=0;i<numchannels+1;i++)
    {
      //read the amplitude
      getline(input, line, ' ');
      amplitudes[i] = atof(line.c_str());
      
      //read the phase
      getline(input, line);
      phases[i] = atof(line.c_str());

      //ignore the channel number
      getline(input, line, ' ');
    }

    ippsPolarToCart_32f(amplitudes, phases, realcomponent, imagcomponent, numchannels + 1);
  }

  input.close();

  ippsRealToCplx_32f(realcomponent, imagcomponent, complexfrequency, numchannels + 1);
  ippsFFTInv_CCSToR_32f((Ipp32f*)complexfrequency, timedomain, fftspec, 0);
  
  //rearrange the lags into order
  for(int i=0;i<numchannels*2;i++)
    lags[(i+numchannels)%(2*numchannels)] = timedomain[i];
  
  //search for max
  ippsStdDev_32f(lags, numchannels*2, &stddev, ippAlgHintFast);
  ippsMinMaxIndx_32f(lags, numchannels*2, &minval, &minindex, &maxval, &maxindex);
  basicsnr = ((abs(minval)>maxval)?(abs(minval)/stddev):(maxval/stddev));
  basicindex = ((abs(minval)>maxval)?minindex:maxindex);
    
  //print out the lags
  ofstream output("lagspace.out", ios::out);

  for(int i=0;i<numchannels*2;i++)
    output << i-numchannels << " " << lags[i] << endl;

  output.close();
    
  //smooth - 3 point hanning window
  for(int i=1;i<numchannels*2-1;i++)
    lags[i] = (0.5*lags[i-1] + lags[i] + 0.5*lags[i+1]);
    
  //take derivative
  for(int i=numchannels*2-2;i>1;i--)
    lags[i] = lags[i]-lags[i-1];

  //smooth again
  for(int i=3;i<numchannels*2-2;i++)
    lags[i] = (0.5*lags[i-1] + lags[i] + 0.5*lags[i+1]);
      
  //search for max
  ippsStdDev_32f(lags+3, numchannels*2-5, &stddev, ippAlgHintFast);
  ippsMaxIndx_32f(lags+3, numchannels*2-5, &maxval, &maxindex);
  derivsnr = maxval/stddev;
    
  if(derivsnr > basicsnr)
  {
    bestsnr = derivsnr;
    bestindex = maxindex-numchannels;
    otherindex = basicindex-numchannels;
    commentstring = " via DERIV method";
  }
  else
  {
    bestsnr = basicsnr;
    bestindex = basicindex-numchannels;
    otherindex = maxindex-numchannels;
    commentstring = " via BASIC method";
  }
  
  if((abs(maxindex-basicindex)%(2*numchannels)) < 5) //the two methods agree
    commentstring += " AND THEY LINE UP!!";
  else
  {
    if(otherindex<0)
      otherindex = -otherindex;
    commentstring += ", alternate channel was ";
    commentstring += char(otherindex/1000 + '0');
    commentstring += char((otherindex%1000)/100 + '0');
    commentstring += char((otherindex%100)/10 + '0');
    commentstring += char((otherindex%10) + '0');
  }
      
  if(bestsnr > SIGMA_LIMIT)
    cout << "Fringe found at " << bestindex << " with S/N " << bestsnr << commentstring  << endl;

  ofstream output2("lagderivative.out", ios::out);

  for(int i=3;i<numchannels*2-2;i++)
    output2 << i-numchannels << " " << lags[i] << endl;

  output2.close();

  return EXIT_SUCCESS;
}
