//my bash at a quick stats package for the text dump output of fxcorr

#include <vector>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <cmath>
#include <ipps.h>
#include <ippcore.h>

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
  IppsDFTSpec_R_32f* dftspec;
  IppStatus status;
  int sizeDFTSpec, sizeDFTInitBuf, wbufsize, i;
  string commentstring;
  ifstream input;
  int fftchannels = numchannels*2;
  Ipp8u *dftInitBuf, *dftWorkBuf;
  
  // Initialize DFT  
  ippsDFTGetSize_R_32f(fftchannels, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast,  &sizeDFTSpec, &sizeDFTInitBuf, &wbufsize);
  dftspec = (IppsDFTSpec_R_32f*)ippsMalloc_8u(sizeDFTSpec);
  dftInitBuf = ippsMalloc_8u(sizeDFTInitBuf);
  dftWorkBuf = ippsMalloc_8u(wbufsize);
  ippsDFTInit_R_32f(fftchannels, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast, dftspec, dftInitBuf);
  if (dftInitBuf) ippFree(dftInitBuf);

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


    //read in the data
    for(int i=0;i<numchannels;i++)
    {
      //ignore the channel number
      getline(input, line, ' ');

      //read the amplitude
      getline(input, line, ' ');
      amplitudes[i] = atof(line.c_str());
      
      //read the phase
      getline(input, line);
      phases[i] = atof(line.c_str());
    }
    // Add a dummy value to the end
    amplitudes[numchannels] = 0;
    phases[numchannels] = 0;

    ippsPolarToCart_32f(amplitudes, phases, realcomponent, imagcomponent, numchannels+1);
  }

  input.close();

  ippsRealToCplx_32f(realcomponent, imagcomponent, complexfrequency, numchannels + 1);
  
  ippsDFTInv_CCSToR_32f((Ipp32f*)complexfrequency, timedomain, dftspec, dftWorkBuf);

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
