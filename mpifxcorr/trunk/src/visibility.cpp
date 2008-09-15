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
#include "config.h"
#include "visibility.h"
#include "core.h"
#include "datastream.h"
#include <dirent.h>
#include <cmath>
#include <string>
#include <string.h>
#include <stdio.h>
#ifdef HAVE_RPFITS
#include <RPFITS.h>
#endif
#include <iomanip>
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#ifdef HAVE_DIFXMESSAGE
#include <difxmessage.h>
#endif
#include "alert.h"

Visibility::Visibility(Configuration * conf, int id, int numvis, int eseconds, int skipseconds, int startns, const string * pnames, bool mon, int port, char * hname, int * sock, int monskip)
  : config(conf), visID(id), numvisibilities(numvis), executeseconds(eseconds), polnames(pnames), monitor(mon), portnum(port), hostname(hname), mon_socket(sock), monitor_skip(monskip)
{
  int status;

  cinfo << "About to create visibility " << id << "/" << numvis << endl;

  if(visID == 0)
    *mon_socket = -1;
  maxproducts = config->getMaxProducts();
  autocorrincrement = (maxproducts>1)?2:1;
  first = true;
  currentsubints = 0;
  numdatastreams = config->getNumDataStreams();
  resultlength = config->getMaxResultLength();
  results = vectorAlloc_cf32(resultlength);
  status = vectorZero_cf32(results, resultlength);
  if(status != vecNoErr)
    csevere << "Error trying to zero when incrementing visibility " << visID << endl;
  numbaselines = config->getNumBaselines();
  currentconfigindex = config->getConfigIndex(skipseconds);
  expermjd = config->getStartMJD();
  experseconds = config->getStartSeconds();
  changeConfig(currentconfigindex);

  //set up the initial time period this Visibility will be responsible for
  offset = 0;
  currentstartsamples = (int)((((double)startns)/1000000000.0)*((double)samplespersecond) + 0.5);
  currentstartseconds = skipseconds;
  offset = offset+offsetperintegration;
  subintsthisintegration = integrationsamples/subintsamples;
  if(offset >= subintsamples/2)
  {
    offset -= subintsamples;
    subintsthisintegration++;
  }
  for(int i=0;i<visID;i++)
    updateTime();
}


Visibility::~Visibility()
{
  vectorFree(results);
#ifdef HAVE_RPFITS
  vectorFree(rpfitsarray);
#endif
  for(int i=0;i<numdatastreams;i++)
    delete [] autocorrcalibs[i];
  delete [] autocorrcalibs;

  for(int i=0;i<numbaselines;i++)
  {
    for(int j=0;j<config->getBNumFreqs(currentconfigindex, i);j++)
      delete [] baselinepoloffsets[i][j];
    delete [] baselinepoloffsets[i];
  }
  delete [] baselinepoloffsets;

  for(int i=0;i<numdatastreams;i++)
  {
    for(int j=0;j<config->getDNumFreqs(currentconfigindex, i);j++)
      delete [] datastreampolbandoffsets[i][j];
    delete [] datastreampolbandoffsets[i];
  }
  delete [] datastreampolbandoffsets;

  if(pulsarbinon) {
    for(int i=0;i<config->getFreqTableLength();i++) {
      for(int j=0;j<config->getNumChannels(currentconfigindex)+1;j++)
        vectorFree(binweightsums[i][j]);
      for(int j=0;j<config->scrunchOutputOn(currentconfigindex)?1:config->getNumPulsarBins(currentconfigindex);j++)
        vectorFree(binscales[i][j]);
      delete [] binweightsums[i];
      delete [] binscales[i];
    }
    delete [] binweightsums;
    delete [] binscales;
    vectorFree(binweightdivisor);
  }
}

bool Visibility::addData(cf32* subintresults)
{
  int status;

  status = vectorAdd_cf32_I(subintresults, results, resultlength);
  if(status != vecNoErr)
    csevere << "Error copying results in Vis. " << visID << endl;
  currentsubints++;

  return (currentsubints==subintsthisintegration); //are we finished integrating?
}

void Visibility::increment()
{
  int status;
  cinfo << "Vis. " << visID << " is incrementing, since currentsubints = " << currentsubints << endl;

  currentsubints = 0;
  for(int i=0;i<numvisibilities;i++) //adjust the start time and offset
    updateTime();

  status = vectorZero_cf32(results, resultlength);
  if(status != vecNoErr)
    csevere << "Error trying to zero when incrementing visibility " << visID << endl;

  if(pulsarbinon) {
    for(int i=0;i<config->getFreqTableLength();i++) {
      for(int j=0;j<config->getNumChannels(currentconfigindex);j++) {
        if(config->scrunchOutputOn(currentconfigindex))
        {
          binweightsums[i][j][0] = 0.0;
        }
        else
        {
          status = vectorZero_f32(binweightsums[i][j], config->getNumPulsarBins(currentconfigindex));
          if(status != vecNoErr)
            csevere << "Error trying to zero binweightsums when incrementing visibility " << visID << endl;
        }
      }
    }
  }
}

void Visibility::updateTime()
{
  int configindex;
  offset = offset+offsetperintegration;
  subintsthisintegration = integrationsamples/subintsamples;
  if(offset >= subintsamples/2)
  {
    offset -= subintsamples;
    subintsthisintegration++;
  }
  currentstartsamples += integrationsamples;
  currentstartseconds += currentstartsamples/samplespersecond;
  currentstartsamples %= samplespersecond;
  configindex = config->getConfigIndex(currentstartseconds);
  while(configindex < 0 && currentstartseconds < executeseconds)
  {
    configindex = config->getConfigIndex(++currentstartseconds);
    currentstartsamples = 0;
    offset = offsetperintegration;
    subintsthisintegration = integrationsamples/subintsamples;
    if(offset >= subintsamples/2)
    {
      offset -= subintsamples;
      subintsthisintegration++;
    }
  }
  if(configindex != currentconfigindex && currentstartseconds < executeseconds)
  {
    changeConfig(configindex);
  }
}

//setup monitoring socket
int Visibility::openMonitorSocket(char *hostname, int port, int window_size, int *sock) {
  int status;
  int err=0;
  unsigned long ip_addr;
  struct hostent     *hostptr;
  struct linger      linger = {1, 1};
  struct sockaddr_in server;    /* Socket address */
  int saveflags,ret,back_err;
  fd_set fd_w;
  struct timeval timeout;

  timeout.tv_sec = 0;
  timeout.tv_usec = 100000;

  hostptr = gethostbyname(hostname);
  if (hostptr==NULL) {
    printf("Failed to look up hostname %s\n", hostname);
    return(1);
  }
  
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons((unsigned short)port); 
  server.sin_addr.s_addr = ip_addr;
  
  printf("Connecting to %s\n",inet_ntoa(server.sin_addr));
    
  *sock = socket(AF_INET, SOCK_STREAM, 0);
  if (*sock==-1) {
    perror("Failed to allocate socket");
    return(1);
  }

  /* Set the window size to TCP actually works */
  status = setsockopt(*sock, SOL_SOCKET, SO_SNDBUF,
                      (char *) &window_size, sizeof(window_size));
  if (status!=0) {
    close(*sock);
    perror("Setting socket options");
    return(1);
  }

  saveflags=fcntl(*sock,F_GETFL,0);
  if(saveflags<0) {
    perror("fcntl1");
    err=errno;
    return 1;
  }

  /* Set non blocking */
  if(fcntl(*sock,F_SETFL,saveflags|O_NONBLOCK)<0) {
    perror("fcntl2");
    err=errno;
    return 1;
  }

  // try to connect    
  status = connect(*sock, (struct sockaddr *) &server, sizeof(server));
  back_err=errno;

  /* restore flags */
  if(fcntl(*sock,F_SETFL,saveflags)<0) {
    perror("fcntl3");
    err=errno;
    return 1;
  }

  /* return unless the connection was successful or the connect is
           still in progress. */

  if(status<0) {
    if (back_err!=EINPROGRESS) {
      perror("connect");
      err=errno;
      return 1;
    } else {

      FD_ZERO(&fd_w);
      FD_SET(*sock,&fd_w);

      status = select(FD_SETSIZE,NULL,&fd_w,NULL,&timeout);
      if(status < 0) {
	perror("select");
	err=errno;
	return 1;
      }

      /* 0 means it timeout out & no fds changed */
      if(status==0) {
	close(*sock);
	status=ETIMEDOUT;
	return 1;
      }

      /* Get the return code from the connect */
      socklen_t len=sizeof(ret);
      status=getsockopt(*sock,SOL_SOCKET,SO_ERROR,&ret,&len);
      if(status<0) {
	perror("getsockopt");
	status = errno;
	return 1;
      }

      /* ret=0 means success, otherwise it contains the errno */
      if(ret) {
	status=ret;
	return 1;
      }
    }
  }

  return 0;
} /* Setup Net */

int Visibility::sendMonitorData(bool tofollow) {
  char *ptr;
  int ntowrite, nwrote, atsec;

  //ensure the socket is open
  if(checkSocketStatus())
  {
    if(!tofollow)
      atsec = -1;
    else
      atsec = currentstartseconds;

    ptr = (char*)(&atsec);
    nwrote = send(*mon_socket, ptr, 4, 0);
    if (nwrote < 4)
    {
      cerror << "Error writing to network - will try to reconnect next Visibility 0 integration!" << endl;
      return 1;
    }

    if(tofollow)
    {
      ptr = (char*)results;
      ntowrite = resultlength*sizeof(cf32);

      while (ntowrite>0) {
        nwrote = send(*mon_socket, ptr, ntowrite, 0);
        if(errno == EPIPE)
        {
          printf("Network seems to have dropped out!  Will try to reconnect shortly...!\n");
          return(1);
        }
        if (nwrote==-1) {
          if (errno == EINTR) continue;
          perror("Error writing to network");

          return(1);
        } else if (nwrote==0) {
          printf("Warning: Did not write any bytes!\n");
          return(1);
        } else {
          ntowrite -= nwrote;
          ptr += nwrote;
        }
      }
    }
  }
  return(0);
}

bool Visibility::checkSocketStatus()
{
  if(*mon_socket < 0)
  {
    if(visID != 0)
    {
      //don't even try to connect, unless you're the first visibility.  Saves trying to reconnect too often
      cerror << "Visibility " << visID << " won't try to reconnect monitor - waiting for vis 0..." << endl;
      return false;
    }
    if(openMonitorSocket(hostname, portnum, Configuration::MONITOR_TCP_WINDOWBYTES, mon_socket) != 0)
    {
      *mon_socket = -1;
      cerror << "WARNING: Monitor socket could not be opened - monitoring not proceeding! Will try again after " << numvisibilities << " integrations..." << endl;
      return false;
    }
  }
  return true;
}

void Visibility::writedata()
{
  f32 scale, divisor;
  int status, ds1, ds2, ds1bandindex, ds2bandindex, binloop;
  int dumpmjd;
  double dumpseconds;

  cdebug << "Vis. " << visID << " is starting to write out data" << endl;

  dumpmjd = expermjd + (experseconds + currentstartseconds)/86400;
  dumpseconds = double((experseconds + currentstartseconds)%86400) + double((currentstartsamples+integrationsamples/2)/(2000000.0*config->getDBandwidth(currentconfigindex,0,0)));
  
  if(currentstartseconds >= executeseconds)
  {
    return; //NOTE EXIT HERE!!!
  }

  if(currentsubints == 0) //nothing to write out
  {
    //if required, send a message to the monitor not to expect any data this integration - 
    //if we can't get through to the monitor, close the socket
    if(monitor && sendMonitorData(false) != 0) {
      cerror << "tried to send a header only to monitor and it failed - closing socket!" << endl;
      close(*mon_socket);
      *mon_socket = -1;
    }
    return; //NOTE EXIT HERE!!!
  }

  int skip = 0;
  int count = 0;
  int nyquistchannel;
  if(config->pulsarBinOn(currentconfigindex) && !config->scrunchOutputOn(currentconfigindex))
    binloop = config->getNumPulsarBins(currentconfigindex);
  else
    binloop = 1;

  for(int i=0;i<numbaselines;i++)
  {
    //skip through the baseline visibilities, grabbing the weights as you go and zeroing
    //that cheekily used Nyquist channel imaginary component of the results array
    for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++) {
      //the Nyquist channel referred to here is for the *first* datastream of the baseline, in the event 
      //that one datastream has USB and the other has LSB
      nyquistchannel = numchannels;
      if(config->getFreqTableLowerSideband(config->getBFreqIndex(currentconfigindex, i, j)))
        nyquistchannel = 0;
      for(int k=0;k<config->getBNumPolProducts(currentconfigindex, i, j);k++) {
        baselineweights[i][j][k] = results[skip + nyquistchannel].im/fftsperintegration;
        results[skip + nyquistchannel].im = 0.0;
        skip += numchannels+1;
      }
      skip += (numchannels+1)*config->getBNumPolProducts(currentconfigindex, i, j)*(binloop-1);
    }
  }
  for(int i=0;i<numdatastreams;i++)
  {
    for(int j=0;j<config->getDNumOutputBands(currentconfigindex, i); j++)
    {
      nyquistchannel = numchannels;
      if(config->getDLowerSideband(currentconfigindex, i, config->getDLocalFreqIndex(currentconfigindex, i, j)))
        nyquistchannel = 0;
      //Grab the weight for this band and then remove it from the resultsarray
      autocorrweights[i][j] = results[skip+count+nyquistchannel].im/fftsperintegration;
      results[skip+count+nyquistchannel].im = 0.0;
      
      //work out the band average, for use in calibration (allows us to calculate fractional correlation)
      status = vectorMean_cf32(&results[skip + count], numchannels+1, &autocorrcalibs[i][j], vecAlgHintFast);
      if(status != vecNoErr)
        csevere << "Error in getting average of autocorrelation!!!" << status << endl;
      count += numchannels + 1;
    }
    if(config->writeAutoCorrs(currentconfigindex) && autocorrincrement > 1)  { 
      //need to grab weights for the cross autocorrs that are also present in the array
      for(int j=0;j<config->getDNumOutputBands(currentconfigindex, i); j++)
      {
        nyquistchannel = numchannels;
        if(config->getDLowerSideband(currentconfigindex, i, config->getDLocalFreqIndex(currentconfigindex, i, j)))
          nyquistchannel = 0;
        autocorrweights[i][j+config->getDNumOutputBands(currentconfigindex, i)] = results[skip+count+nyquistchannel].im/fftsperintegration;
        results[skip+count+nyquistchannel].im = 0.0;
        count += numchannels + 1;
      }
    }
  }
  count = 0;
  for(int i=0;i<numbaselines;i++) //do each baseline
  {
    ds1 = config->getBOrderedDataStream1Index(currentconfigindex, i);
    ds2 = config->getBOrderedDataStream2Index(currentconfigindex, i);
    for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++) //do each frequency
    {
      for(int k=0;k<config->getBNumPolProducts(currentconfigindex, i, j);k++) //do each product of this frequency eg RR,LL,RL,LR
      {
        ds1bandindex = config->getBDataStream1BandIndex(currentconfigindex, i, j, k);
        ds2bandindex = config->getBDataStream2BandIndex(currentconfigindex, i, j, k);
        divisor = (Mode::getDecorrelationPercentage(config->getDNumBits(currentconfigindex, ds1)))*(Mode::getDecorrelationPercentage(config->getDNumBits(currentconfigindex, ds2)))*autocorrcalibs[ds1][ds1bandindex].re*autocorrcalibs[ds2][ds2bandindex].re;
        if(divisor > 0.0) //only do it if there is something to calibrate with
          scale = sqrt(config->getDTsys(currentconfigindex, ds1)*config->getDTsys(currentconfigindex, ds2)/divisor);
        else
          scale = 0.0;
        for(int b=0;b<binloop;b++)
        {
          //amplitude calibrate the data
          if(scale > 0.0)
          {
            status = vectorMulC_f32_I(scale, (f32*)(&(results[count])), 2*(numchannels+1));
            if(status != vecNoErr)
              csevere << "Error trying to amplitude calibrate the baseline data!!!" << endl;
          }
          else
          {
            //We want normalised correlation coefficients, so scale by number of contributing
            //samples rather than datastream tsys and decorrelation correction
            if(baselineweights[i][j][k] > 0.0)
            {
              //scale = 1.0/(((float)(subintsthisintegration))*((float)(config->getBlocksPerSend(currentconfigindex)*2*numchannels)));
              scale = 1.0/(baselineweights[i][j][k]*meansubintsperintegration*((float)(config->getBlocksPerSend(currentconfigindex)*2*numchannels)));
              status = vectorMulC_f32_I(scale, (f32*)(&(results[count])), 2*(numchannels+1));
              if(status != vecNoErr)
                csevere << "Error trying to amplitude calibrate the baseline data!!!" << endl;
            }
          }
          count += numchannels+1;
        }
      }
    }
  }

  if(config->writeAutoCorrs(currentconfigindex)) //if we need to, calibrate the autocorrs
  {
    for(int i=0;i<numdatastreams;i++) //do each datastream
    {
      for(int j=0;j<autocorrincrement;j++) //the parallel, (and the cross if needed) product for which this band is the first
      {
        for(int k=0;k<config->getDNumOutputBands(currentconfigindex, i); k++) //for each band
        {
          //calibrate the data
          divisor = (Mode::getDecorrelationPercentage(config->getDNumBits(currentconfigindex, i))*sqrt(autocorrcalibs[i][k].re*autocorrcalibs[i][(j==0)?k:config->getDMatchingBand(currentconfigindex, i, k)].re));
          if(divisor > 0.0)
          {
            scale = config->getDTsys(currentconfigindex, i)/divisor;
	    if(scale > 0.0)
	    {
              status = vectorMulC_f32_I(scale, (f32*)(&(results[count])), 2*(numchannels+1));
              if(status != vecNoErr)
                csevere << "Error trying to amplitude calibrate the datastream data!!!" << endl;
            }
            else
            {
              //We want normalised correlation coefficients, so scale by number of contributing            
              //samples rather than datastream tsys and decorrelation correction            
              if(autocorrweights[i][k+j*config->getDNumOutputBands(currentconfigindex, i)] > 0.0)
              {
                //scale = 1.0/(((float)(subintsthisintegration))*((float)(config->getBlocksPerSend(currentconfigindex)*2*numchannels)));
                scale = 1.0/(autocorrweights[i][k+j*config->getDNumOutputBands(currentconfigindex, i)]*meansubintsperintegration*((float)(config->getBlocksPerSend(currentconfigindex)*2*numchannels)));
                status = vectorMulC_f32_I(scale, (f32*)(&(results[count])), 2*(numchannels+1));
                if(status != vecNoErr)
                  csevere << "Error trying to amplitude calibrate the datastream data for the correlation coefficient case!!!" << endl;
              }
            }
          }
          count += numchannels+1;
        }
      }
    }
  }

  //if necessary, work out the scaling factors for pulsar binning
  if(pulsarbinon) {
    int numsubints;
    int mjd = expermjd + (experseconds + currentstartseconds)/86400;
    double mjdfrac = (double((experseconds + currentstartseconds)%86400) + double((currentstartsamples)/(2000000.0*config->getDBandwidth(currentconfigindex,0,0))))/86400.0;
    double fftminutes = double(config->getNumChannels(currentconfigindex))/(60000000.0*config->getDBandwidth(currentconfigindex,0,0));
    polyco = Polyco::getCurrentPolyco(currentconfigindex, mjd, mjdfrac, config->getPolycos(currentconfigindex), config->getNumPolycos(currentconfigindex));
    double * binweights = polyco->getBinWeights();

    //SOMEWHERE IN HERE, CHECK THE AMPLITUDE SCALING FOR MULTIPLE SCRUNCHED BINS!

    numsubints = integrationsamples/subintsamples;
    if(offset + offsetperintegration >= subintsamples/2) numsubints++;
    polyco->setTime(mjd, mjdfrac);
    for(int i=0;i<numsubints*config->getBlocksPerSend(currentconfigindex);i++) {
      polyco->getBins(i*fftminutes, pulsarbins);
      for(int j=0;j<config->getFreqTableLength();j++) {
        for(int k=0;k<config->getNumChannels(currentconfigindex)+1;k++) {
          if(config->scrunchOutputOn(currentconfigindex)) {
            binweightsums[j][k][0] += binweights[pulsarbins[j][k]];
          }
          else {
            binweightsums[j][k][pulsarbins[j][k]] += binweights[pulsarbins[j][k]];
          }
        }
      }
    }
    cinfo << "Done calculating weight sums" << endl;
    for(int i=0;i<config->getFreqTableLength();i++) {
      for(int j=0;j<config->getNumChannels(currentconfigindex)+1;j++) {
        for(int k=0;k<(config->scrunchOutputOn(currentconfigindex))?1:config->getNumPulsarBins(currentconfigindex);k++)
            binscales[i][k][j].re = binscales[i][k][j].im = binweightsums[i][j][k] / binweightdivisor[k];
      }
    }
    cinfo << "Done calculating scales" << endl;

    //do the calibration - should address the weight here as well!
    count = 0;
    for(int i=0;i<numbaselines;i++) //do each baseline
    {
      for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++) //do each frequency
      {
        for(int k=0;k<config->getBNumPolProducts(currentconfigindex, i, j);k++) //do each product of this frequency eg RR,LL,RL,LR
        {
          for(int b=0;b<binloop;b++)
          {
            status = vectorMul_f32_I((f32*)(binscales[config->getBFreqIndex(currentconfigindex, i, j)][b]), (f32*)(&(results[count])), 2*(numchannels+1));
            if(status != vecNoErr)
              csevere << "Error trying to pulsar amplitude calibrate the baseline data!!!" << endl;
            count += numchannels+1;
          }
        }
      }
    }
    cinfo << "Done the in-place multiplication" << endl;
  }
  
  //all calibrated, now just need to write out
  if(config->getOutputFormat() == Configuration::RPFITS)
    writerpfits();
  else if(config->getOutputFormat() == Configuration::DIFX)
    writedifx();
  else
    writeascii();

  //send monitoring data, if we don't have to skip this one
  if(monitor) {
    if (visID % monitor_skip == 0) {
      if (sendMonitorData(true) != 0){ 
	cerror << "Error sending monitoring data - closing socket!" << endl;
	close(*mon_socket);
	*mon_socket = -1;
      }
    } else {
    }
  }

  cdebug << "Vis. " << visID << " has finished writing data" << endl;
}

void Visibility::writeascii()
{
  ofstream output;
  int binloop;
  char datetimestring[26];

  int count = 0;
  int samples = currentstartsamples + integrationsamples/2;
  int seconds = experseconds + currentstartseconds + samples/samplespersecond;
  int microseconds = int((double(samples%samplespersecond)/double(samplespersecond))*1000000 + 0.5);
  int hours = seconds/3600;
  int minutes = (seconds-hours*3600)/60;
  int mjd = expermjd;
  seconds = seconds - (hours*3600 + minutes*60);
  while(hours >= 24)
  {
     hours -= 24;
     mjd++;
  }
  sprintf(datetimestring, "%05u_%02u%02u%02u_%06u", mjd, hours, minutes, seconds, microseconds);
  cinfo << "Mjd is " << mjd << ", hours is " << hours << ", minutes is " << minutes << ", seconds is " << seconds << endl;
  
  if(config->pulsarBinOn(currentconfigindex) && !config->scrunchOutputOn(currentconfigindex))
    binloop = config->getNumPulsarBins(currentconfigindex);
  else
    binloop = 1;

  for(int i=0;i<numbaselines;i++)
  {
    for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++)
    {
      for(int b=0;b<binloop;b++)
      {
        for(int k=0;k<config->getBNumPolProducts(currentconfigindex, i, j);k++)
        {
          //write out to a naive filename
          output.open(string(string("baseline_")+char('0' + i)+"_freq_"+char('0' + j)+"_product_"+char('0'+k)+"_"+datetimestring+"_bin_"+char('0'+b)+".output").c_str(), ios::out|ios::trunc);
          for(int l=0;l<numchannels+1;l++)
              output << l << " " << sqrt(results[count + l].re*results[count + l].re + results[count + l].im*results[count + l].im) << " " << atan2(results[count + l].im, results[count + l].re) << endl;
          output.close();
          count += numchannels+1;
        }
      }
    }
  }

  if(config->writeAutoCorrs(currentconfigindex)) //if we need to, write out the autocorrs
  {
    for(int i=0;i<numdatastreams;i++)
    {
      for(int j=0;j<autocorrincrement;j++)
      {
        for(int k=0;k<config->getDNumOutputBands(currentconfigindex, i); k++)
        {
          //write out to naive filename
          output.open(string(string("datastream_")+char('0' + i)+"_crosspolar_"+char('0' + j)+"_product_"+char('0'+k)+"_"+datetimestring+"_bin_"+char('0'+0)+".output").c_str(), ios::out|ios::trunc);
          for(int l=0;l<numchannels+1;l++)
            output << l << " " << sqrt(results[count + l].re*results[count + l].re + results[count + l].im*results[count + l].im) << " " << atan2(results[count + l].im, results[count + l].re) << endl;
          output.close();
          count += numchannels+1;
        }
      }
    }
  }
}

void Visibility::writerpfits()
{
#ifdef HAVE_RPFITS

  int baselinenumber, freqnumber, sourcenumber, numpolproducts, binloop;
  int firstpolindex;
  int count = 0;
  int status = 0;
  int flag = 0;
  int bin = 0;
  float buvw[3]; //the u,v and w for this baseline at this time
  f32 * visibilities = (f32*)rpfitsarray;
  float offsetstartdaysec = currentstartseconds + experseconds + float((currentstartsamples+integrationsamples/2)/(2000000.0*config->getDBandwidth(currentconfigindex,0,0)));
  
  if(config->pulsarBinOn(currentconfigindex) && !config->scrunchOutputOn(currentconfigindex))
    binloop = config->getNumPulsarBins(currentconfigindex);
  else
    binloop = 1;
  sourcenumber = config->getSourceIndex(expermjd, experseconds + currentstartseconds) + 1;

  //ensure the intbase parameter is set correctly
  param_.intbase = config->getIntTime(currentconfigindex);

  for(int i=0;i<numbaselines;i++)
  {
    baselinenumber = config->getBNumber(currentconfigindex, i);

    //interpolate the uvw
    (config->getUVW())->interpolateUvw(config->getDStationName(currentconfigindex, config->getBOrderedDataStream1Index(currentconfigindex, i)), config->getDStationName(currentconfigindex, config->getBOrderedDataStream2Index(currentconfigindex, i)), expermjd, offsetstartdaysec, buvw);
    for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++)
    {
      for(int b=0;b<binloop;b++)
      {
        //clear the rpfits array, which is a specially ordered array of all products for this frequency
        status = vectorZero_cf32(rpfitsarray, maxproducts*(numchannels+1));
        if(status != vecNoErr)
          csevere << "Error trying to zero the rpfitsarray!!!" << endl;
        freqnumber = config->getBFreqIndex(currentconfigindex, i, j) + 1;
        numpolproducts = config->getBNumPolProducts(currentconfigindex, i, j);

        //put the stuff in the rpfitsarray in order
        for(int k=0;k<numpolproducts;k++)
        {
          for(int l=0;l<numchannels+1;l++)
            rpfitsarray[l*maxproducts + baselinepoloffsets[i][j][k]] = results[count + l];
          count += numchannels + 1;
        }

        //write out the rpfits data
        rpfitsout_(&status/*should be 0 = writing data*/, visibilities, 0/*not using weights*/, &baselinenumber, &offsetstartdaysec, &buvw[0], &buvw[1], &buvw[2], &flag/*not flagged*/, &b, &freqnumber, &sourcenumber);
      }
    }
  }
  //zero the uvw for autocorrelations
  buvw[0] = 0.0;
  buvw[1] = 0.0;
  buvw[2] = 0.0;
  if(config->writeAutoCorrs(currentconfigindex)) //if we need to, write out the autocorrs
  {
    for(int i=0;i<numdatastreams;i++)
    {
      baselinenumber = 257*(config->getDTelescopeIndex(currentconfigindex, i)+1);
      for(int j=0;j<config->getDNumFreqs(currentconfigindex, i); j++)
      {
        firstpolindex = -1;
        //find a product that is active, so we know where to look to work out which frequency this is
        for(int k=maxproducts-1;k>=0;k--) {
          if(datastreampolbandoffsets[i][j][k] >= 0)
            firstpolindex = k;
        }
        if(firstpolindex >= 0)
        {
          //zero the rpfitsarray
          status = vectorZero_cf32(rpfitsarray, maxproducts*(numchannels+1));
          if(status != vecNoErr)
            csevere << "Error trying to zero the rpfitsarray!!!" << endl;
          freqnumber = config->getDFreqIndex(currentconfigindex, i, datastreampolbandoffsets[i][j][firstpolindex]%config->getDNumOutputBands(currentconfigindex, i)) + config->getIndependentChannelIndex(currentconfigindex)*config->getFreqTableLength() + 1;

          for(int k=0;k<maxproducts; k++)
          {
            if(datastreampolbandoffsets[i][j][k] >= 0)
            {
              //put the stuff in the rpfitsarray in order
              for(int l=0;l<numchannels+1;l++)
                rpfitsarray[l*maxproducts + k] = results[count + l + (numchannels+1)*datastreampolbandoffsets[i][j][k]];
            }
          }

          //write out the rpfits data
          rpfitsout_(&status/*should be 0 = writing data*/, visibilities, 0/*not using weights*/, &baselinenumber, &offsetstartdaysec, &buvw[0], &buvw[1], &buvw[2], &flag/*not flagged*/, &bin, &freqnumber, &sourcenumber);
        }
        else
          cerror << "WARNING - did not find any bands for frequency " << j << " of datastream " << i << endl;
      }
      count += autocorrincrement*(numchannels+1)*config->getDNumOutputBands(currentconfigindex, i);
    }
  }
  if(status != 0)
    cerror << "Error trying to write visibilities for " << currentstartseconds << " seconds plus " << currentstartsamples << " samples" << endl;

#endif
}


void Visibility::writedifx()
{
  ofstream output;
  char filename[256];
  int dumpmjd, binloop, sourceindex, freqindex, numpolproducts, firstpolindex, baselinenumber, lsboffset;
  double dumpseconds;
  int count = 0;
  float buvw[3]; //the u,v and w for this baseline at this time
  char polpair[3]; //the polarisation eg RR, LL

  if(config->pulsarBinOn(currentconfigindex) && !config->scrunchOutputOn(currentconfigindex))
    binloop = config->getNumPulsarBins(currentconfigindex);
  else
    binloop = 1;
  sourceindex = config->getSourceIndex(expermjd, experseconds + currentstartseconds);

  //work out the time of this integration
  dumpmjd = expermjd + (experseconds + currentstartseconds)/86400;
  dumpseconds = double((experseconds + currentstartseconds)%86400) + double((currentstartsamples+integrationsamples/2)/(2000000.0*config->getDBandwidth(currentconfigindex,0,0)));

  sprintf(filename, "%s/DIFX_%05d_%06d", config->getOutputFilename().c_str(), expermjd, experseconds);
  
  //work through each baseline visibility point
  for(int i=0;i<numbaselines;i++)
  {
    baselinenumber = config->getBNumber(currentconfigindex, i);

    //interpolate the uvw
    (config->getUVW())->interpolateUvw(config->getDStationName(currentconfigindex, config->getBOrderedDataStream1Index(currentconfigindex, i)), config->getDStationName(currentconfigindex, config->getBOrderedDataStream2Index(currentconfigindex, i)), expermjd, dumpseconds + (dumpmjd-expermjd)*86400.0, buvw);
    for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++)
    {
      freqindex = config->getBFreqIndex(currentconfigindex, i, j);
      numpolproducts = config->getBNumPolProducts(currentconfigindex, i, j);
      lsboffset = 0;
      if(config->getFreqTableLowerSideband(config->getBFreqIndex(currentconfigindex, i, j)))
        lsboffset = 1;

      for(int b=0;b<binloop;b++)
      {
        for(int k=0;k<numpolproducts;k++) 
        {
          config->getBPolPair(currentconfigindex, i, j, k, polpair);

          //open the file for appending in ascii and write the ascii header
          output.open(filename, ios::app);
          writeDiFXHeader(&output, baselinenumber, dumpmjd, dumpseconds, currentconfigindex, sourceindex, freqindex, polpair, b, 0, baselineweights[i][j][k], buvw);

          //close, reopen in binary and write the binary data, then close again
          output.close();
          output.open(filename, ios::app|ios::binary);
          //For both USB and LSB data, the Nyquist channel is excised.  Thus, the numchannels that are written out represent the
          //the valid part of the band in both cases, and run from lowest frequency to highest frequency in both cases.  For USB
          //data, the first channel is the DC - for LSB data, the last channel is the DC
          output.write((char*)(results + (count*(numchannels+1)) + lsboffset), numchannels*sizeof(cf32));
          output.close();

          count++;
        }
      }
    }
  }

  //now each autocorrelation visibility point if necessary
  if(config->writeAutoCorrs(currentconfigindex)) // FIXME -- bug lurks within?
  {
    buvw[0] = 0.0;
    buvw[1] = 0.0;
    buvw[2] = 0.0;
    for(int i=0;i<numdatastreams;i++)
    {
      baselinenumber = 257*(config->getDTelescopeIndex(currentconfigindex, i)+1);
      for(int j=0;j<config->getDNumFreqs(currentconfigindex, i); j++)
      {
        firstpolindex = -1;
        lsboffset = 0;
        if(config->getDLowerSideband(currentconfigindex, i, j))
          lsboffset = 1;
        //find a product that is active, so we know where to look to work out which frequency this is
        for(int k=maxproducts-1;k>=0;k--) {
          if(datastreampolbandoffsets[i][j][k] >= 0)
            firstpolindex = k;
        }
        if(firstpolindex >= 0)
        {
          freqindex = config->getDFreqIndex(currentconfigindex, i, datastreampolbandoffsets[i][j][firstpolindex]%config->getDNumOutputBands(currentconfigindex, i));

          for(int k=0;k<maxproducts; k++)
          {
            if(datastreampolbandoffsets[i][j][k] >= 0)
            {
              //open, write the header and close
              output.open(filename, ios::app);
              writeDiFXHeader(&output, baselinenumber, dumpmjd, dumpseconds, currentconfigindex, sourceindex, freqindex, polnames[k].c_str(), 0, 0, autocorrweights[i][datastreampolbandoffsets[i][j][k]], buvw);
              output.close();

              //open, write the binary data and close
              output.open(filename, ios::app|ios::binary);
              //see baseline writing section for description of treatment of USB/LSB data and the Nyquist channel
              output.write((char*)(results + lsboffset + (count+datastreampolbandoffsets[i][j][k])*(numchannels+1)), numchannels*sizeof(cf32));
              output.close();
            }
          }
        }
        else
          cerror << "WARNING - did not find any bands for frequency " << j << " of datastream " << i << endl;
      }
      count += autocorrincrement*config->getDNumOutputBands(currentconfigindex, i);
    }
  }
}

void Visibility::multicastweights()
{
  int i, j, k, pboffset;
  float *weight;
  double mjd;
  int dumpmjd;
  double dumpseconds;

  weight = new float[numdatastreams];
  
  //work out the time of this integration
  dumpmjd = expermjd + (experseconds + currentstartseconds)/86400;
  dumpseconds = double((experseconds + currentstartseconds)%86400) + double((currentstartsamples+integrationsamples/2)/(2000000.0*config->getDBandwidth(currentconfigindex,0,0)));

  for(int i=0;i<numdatastreams;i++)
  {
    pboffset = -1;
    for(int j=0;j<config->getDNumFreqs(currentconfigindex, i); j++)
    {
      //find a product that is active, so we know where to look to work out which frequency this is
      for(int k=maxproducts-1;k>=0;k--) {
        if(datastreampolbandoffsets[i][j][k] >= 0)
          pboffset = datastreampolbandoffsets[i][j][k];
      }
      if(pboffset >= 0)
        break;
    }
    if(pboffset >= 0)
      weight[i] = autocorrweights[i][pboffset];
    else
      weight[i] = -1.0;
  }

  mjd = dumpmjd + dumpseconds/86400.0;

#ifdef HAVE_DIFXMESSAGE
  difxMessageSendDifxStatus(DIFX_STATE_RUNNING, "", mjd, numdatastreams, weight);
#endif

  delete[] weight;
} 


void Visibility::writeDiFXHeader(ofstream * output, int baselinenum, int dumpmjd, double dumpseconds, int configindex, int sourceindex, int freqindex, const char polproduct[3], int pulsarbin, int flag, float weight, float buvw[3])
{
  *output << setprecision(15);
  *output << "BASELINE NUM:       " << baselinenum << endl;
  *output << "MJD:                " << dumpmjd << endl;
  *output << "SECONDS:            " << dumpseconds << endl;
  *output << "CONFIG INDEX:       " << configindex << endl;
  *output << "SOURCE INDEX:       " << sourceindex << endl;
  *output << "FREQ INDEX:         " << freqindex << endl;
  *output << "POLARISATION PAIR:  " << polproduct[0] << polproduct[1] << endl;
  *output << "PULSAR BIN:         " << pulsarbin << endl;
  *output << "FLAGGED:            " << flag << endl;
  *output << "DATA WEIGHT:        " << weight << endl;
  *output << "U (METRES):         " << buvw[0] << endl;
  *output << "V (METRES):         " << buvw[1] << endl;
  *output << "W (METRES):         " << buvw[2] << endl;
}

void Visibility::changeConfig(int configindex)
{
  char polpair[3];
  bool found;
  polpair[2] = 0;
  
  if(first) 
  {
    //can just allocate without freeing all the old stuff
    first = false;
    autocorrcalibs = new cf32*[numdatastreams];
    autocorrweights = new f32*[numdatastreams];
    baselineweights = new f32**[numbaselines];
    baselinepoloffsets = new int**[numbaselines];
    datastreampolbandoffsets = new int**[numdatastreams];
    binweightsums = new f32**[config->getFreqTableLength()];
    binscales = new cf32**[config->getFreqTableLength()];
    pulsarbins = new s32*[config->getFreqTableLength()];
  }
  else
  {
    //need to delete the old arrays before allocating the new ones
    for(int i=0;i<numdatastreams;i++) {
      for(int j=0;j<config->getDNumFreqs(currentconfigindex, i);j++)
        delete [] datastreampolbandoffsets[i][j];
      delete [] datastreampolbandoffsets[i];
      delete [] autocorrcalibs[i];
    }
    for(int i=0;i<autocorrincrement*config->getDNumFreqs(currentconfigindex, i);i++) {
      delete [] autocorrweights[i];
    }
    for(int i=0;i<numbaselines;i++)
    {
      for(int j=0;j<config->getBNumFreqs(currentconfigindex, i);j++) {
        delete [] baselinepoloffsets[i][j];
        delete [] baselineweights[i][j];
      }
      delete [] baselinepoloffsets[i];
      delete [] baselineweights[i];
    }
#ifdef HAVE_RPFITS
    vectorFree(rpfitsarray);
#endif
    if(pulsarbinon) {
      for(int i=0;i<config->getFreqTableLength();i++) {
        for(int j=0;j<config->getNumChannels(currentconfigindex)+1;j++)
          vectorFree(binweightsums[i][j]);
        for(int j=0;j<config->scrunchOutputOn(currentconfigindex)?1:config->getNumPulsarBins(currentconfigindex);j++)
          vectorFree(binscales[i][j]);
        vectorFree(pulsarbins[i]);
        delete [] binweightsums[i];
        delete [] binscales[i];
      }
      vectorFree(binweightdivisor);
    }
  }

  //get the new parameters for this configuration from the config object
  currentconfigindex = configindex;
  pulsarbinon = config->pulsarBinOn(configindex);
  numchannels = config->getNumChannels(configindex);
  samplespersecond = int(2000000*config->getDBandwidth(configindex, 0, 0) + 0.5);
  integrationsamples = int(2000000*config->getDBandwidth(configindex, 0, 0)*config->getIntTime(configindex) + 0.5);
  subintsamples = config->getBlocksPerSend(configindex)*numchannels*2;
  offsetperintegration = integrationsamples%subintsamples;
  fftsperintegration = ((double)integrationsamples)*config->getBlocksPerSend(configindex)/((double)subintsamples);
  meansubintsperintegration = fftsperintegration/config->getBlocksPerSend(configindex);
  cinfo << "For Visibility " << visID << ", offsetperintegration is " << offsetperintegration << ", subintsamples is " << subintsamples << ", and configindex is " << configindex << endl;
  resultlength = config->getResultLength(configindex);
  for(int i=0;i<numdatastreams;i++)
    autocorrcalibs[i] = new cf32[config->getDNumOutputBands(configindex, i)];

  //work out the offsets we need to use to put things in the rpfits array in the right order
  for(int i=0;i<numbaselines;i++)
  {
    baselinepoloffsets[i] = new int*[config->getBNumFreqs(configindex, i)];
    baselineweights[i] = new f32*[config->getBNumFreqs(configindex, i)];
    for(int j=0;j<config->getBNumFreqs(configindex, i);j++)
    {
      baselinepoloffsets[i][j] = new int[config->getBNumPolProducts(configindex, i, j)];
      baselineweights[i][j] = new f32[config->getBNumPolProducts(configindex, i, j)];
      for(int k=0;k<config->getBNumPolProducts(configindex, i, j);k++)
      {
        config->getBPolPair(configindex, i, j, k, polpair);
        found = false;
        for(int l=0;l<maxproducts;l++)
        {
          if(polnames[l] == string(polpair))
          {
            baselinepoloffsets[i][j][k] = l;
            found = true;
          }
        }
        if(!found)
        {
          cerror << "Could not find a polarisation pair, will be put in position " << maxproducts << "!!!" << endl;
          baselinepoloffsets[i][j][k] = maxproducts-1;
        }
      }   
    }
  }

  //do the same for the datastreams, so we can order things properly in the rpfits array
  for(int i=0;i<numdatastreams;i++)
  {
    autocorrcalibs[i] = new cf32[config->getDNumOutputBands(configindex, i)];
    autocorrweights[i] = new f32[autocorrincrement*config->getDNumOutputBands(configindex, i)];
    cinfo << "Creating datastreampolbandoffsets, length " << config->getDNumFreqs(configindex,i) << endl;
    datastreampolbandoffsets[i] = new int*[config->getDNumFreqs(configindex,i)];
    for(int j=0;j<config->getDNumFreqs(configindex, i);j++)
    {
      datastreampolbandoffsets[i][j] = new int[maxproducts];
      for(int k=0;k<maxproducts;k++)
        datastreampolbandoffsets[i][j][k] = -1;
      for(int k=0;k<config->getDNumOutputBands(configindex, i);k++)
      {
        if(config->getDLocalFreqIndex(configindex, i, k) == j)
        {
          //work out the index in the polnames array
          for(int l=0;l<maxproducts;l++)
          {
            if(config->getDBandPol(configindex, i, k) == polnames[l].data()[0])
            {
              if(config->getDBandPol(configindex, i, k) == polnames[l].data()[1])
                datastreampolbandoffsets[i][j][l] = k;
              else
                datastreampolbandoffsets[i][j][l] = k+config->getDNumOutputBands(configindex, i);
            }
          }
        }
      }
    }
  }

#ifdef HAVE_RPFITS
  //allocate the rpfits array
  rpfitsarray = vectorAlloc_cf32(maxproducts*(numchannels+1));
#endif

  //create the pulsar bin weight accumulation arrays
  if(pulsarbinon) {
    double fftsecs = double(config->getNumChannels(currentconfigindex))/(1000000.0*config->getDBandwidth(currentconfigindex,0,0));
    polyco = Polyco::getCurrentPolyco(configindex, expermjd + (experseconds + currentstartseconds)/86400, double((experseconds + currentstartseconds)%86400)/86400.0, config->getPolycos(configindex), config->getNumPolycos(configindex));
    //polyco->setTime(expermjd + (experseconds + currentstartseconds)/86400, double((experseconds + currentstartseconds)%86400)/86400.0);
    if(config->scrunchOutputOn(configindex)) {
      binweightdivisor = vectorAlloc_f32(1);
      binweightdivisor[0] = 0.0;
      for (int i=0;i<config->getNumPulsarBins(configindex);i++)
      {
        binweightdivisor[0] += polyco->getBinWeightTimesWidth(i)*config->getIntTime(configindex)/fftsecs;
      }
      binweightdivisor[0] /= double(config->getNumPulsarBins(configindex));
    }
    else {
      binweightdivisor = vectorAlloc_f32(config->getNumPulsarBins(configindex));
      for (int i=0;i<config->getNumPulsarBins(configindex);i++)
      {
        binweightdivisor[i] = polyco->getBinWeightTimesWidth(i)*config->getIntTime(configindex)/fftsecs;
      }
    }
    for(int i=0;i<config->getFreqTableLength();i++) {
      binweightsums[i] = new f32*[config->getNumChannels(configindex)+1];
      binscales[i] = new cf32*[config->scrunchOutputOn(configindex)?1:config->getNumPulsarBins(configindex)];
      pulsarbins[i] = vectorAlloc_s32(config->getNumChannels(configindex)+1);
      for(int j=0;j<config->getNumChannels(configindex)+1;j++) {
        if(config->scrunchOutputOn(configindex))
          binweightsums[i][j] = vectorAlloc_f32(1);
        else
          binweightsums[i][j] = vectorAlloc_f32(config->getNumPulsarBins(configindex));
      }
      for(int j=0;j<(config->scrunchOutputOn(configindex)?1:config->getNumPulsarBins(configindex));j++)
        binscales[i][j] = vectorAlloc_cf32(config->getNumChannels(configindex) + 1);
    }
  }
}
