/***************************************************************************
 *   Copyright (C) 2009 by Adam Deller                                     *
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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <mpi.h>
#include "pulseprofile.h"
#include "alert.h"
#include "difxmessage.h"
#include "math.h"
#include "mk5.h"
#ifdef HAVE_XLRAPI_H
#include "nativemk5.h"
#endif

using namespace std;

int main(int argc, char *argv[])
{
  cout << "About to run MPIInit" << endl;

  MPI_Init(&argc, &argv);
  cout << "Ran MPIInit!" << endl;
  world = MPI_COMM_WORLD;
  MPI_Comm_size(world, &numprocs);
  MPI_Comm_rank(world, &myid);
  MPI_Comm_dup(world, &return_comm);

  if(argc != 2)
  {
    cerr << "Error - invoke with pulseprofile <configfilename>" << endl;
    exit(1);
  }
  if(numprocs < 3) {
    cerr << "Error - must have at least 3 processes (manager, datastream, core(s)) - aborting" << endl;
    return EXIT_FAILURE;
  }

  quit = false;
  currentdsindex = 0;
  //string dsstring = argv[2];
  //string::size_type at = -1;
  //string::size_type wasat = 0;
  //while(at!=string::npos) {
  //  wasat = ++at;
  //  at = dsstring.find_first_of(",");
  //  dsindices[numdatastreams++] = atoi(dsstring.substr(wasat,at-wasat).c_str());
  //}
  controldata = new int[4];
  config = new Configuration(argv[1], myid);
  numdatastreams = config->getNumDataStreams();
  numcores = numprocs - (numdatastreams+1);
  coreids = new int[numcores];
  for(int i=0;i<numcores;i++)
    coreids[i] = numdatastreams + 1 + i;
  if(numprocs < numdatastreams + 2) {
    cerr << "Error - must have at least " << numdatastreams+2 << " processes    (manager, datastreams, core(s)) - aborting" << endl;
    return EXIT_FAILURE;
  }
  config->loaduvwinfo(true);
  numbins = config->getNumPulsarBins(0);
  numchannels = config->getNumChannels(0);
  nsincrement = int(1000.0*(config->getBlocksPerSend(0)*numchannels)/(config->getDBandwidth(0, 0, 0))+ 0.5);
  if(config->getNumConfigs() > 1 || !config->pulsarBinOn(0)) {
    cerr << "Must be a single config file, with pulsar binning on - aborting" << endl;
    return EXIT_FAILURE;
  }
  //for(int i=0;i<numdatastreams;i++) {
  //  if(dsindices[i] < 0 || dsindices[i] >= config->getNumDataStreams()) {
  //    cerr << "Dsindex " << dsindices[i] << " is outside the permitted range of datastream values (0, " << config->getNumDataStreams() << ") - aborting" << endl;
  //    return EXIT_FAILURE;
  //  }
  //}
  Configuration::dataformat df = config->getDataFormat(0,0);
  int dfreqs = config->getDNumFreqs(0,0);
  int dbands = config->getDNumInputBands(0,0);
  double bw = config->getDBandwidth(0,0,0);
  int bits = config->getDNumBits(0,0);
  double * findices = new double[dbands];
  for(int i=0;i<dbands;i++) findices[i] = config->getDFreqIndex(0,0,i);
  //check that all the modes are the same
  for(int i=0;i<numdatastreams;i++) {
    bool mismatch = false;
    if(config->getDataFormat(0,i) != df) mismatch = true;
    else if(config->getDNumFreqs(0,i) != dfreqs) mismatch = true;
    else if(config->getDNumInputBands(0,i) != dbands) mismatch = true;
    else if(config->getDBandwidth(0,i,0) != bw) mismatch = true;
    else if(config->getDNumBits(0,i) != bits) mismatch = true;
    else {
      for(int j=0;j<dbands;j++) {
        if(config->getDFreqIndex(0,i,j) != findices[j]) mismatch = true;
      }
    }
    if(mismatch) {
      cerr << "Error - not all datastreams have the same mode - aborting!" << endl;
      return EXIT_FAILURE;
    }
  }
  delete [] findices;

  if(myid == 0 || myid > numdatastreams)
    resultbuffer = vectorAlloc_f64(numbins);
  else {
    if(config->isMkV(myid-1))
      datastream = new Mk5DataStream(config, myid-1, myid, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
    else if(config->isNativeMkV(myid-1)) {
#ifdef HAVE_XLRAPI_H 
      datastream = new NativeMk5DataStream(config, myid-1, myid, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
#else
      cout << "NativeMk5 cannot be used here, you don't have the libraries compiled in! Aborting." << endl;
      MPI_Abort(world, 1);
#endif
    }
    else
      datastream = new DataStream(config, myid-1, myid, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
    datastream->initialise();
  }
  if(myid == 0) {
    profile = vectorAlloc_f64(numbins);
    for(int i=0;i<numbins;i++)
      profile[i] = 0;
  }
  polycos = config->getPolycos(0);
  MPI_Barrier(world);
  if(myid == 0)
    processManager();
  else if (myid<=numdatastreams)
    datastream->execute();
  else
    processCore();
  MPI_Finalize();
  cout << myid << ": BYE!" << endl;

  return EXIT_SUCCESS;
}

void processManager()
{
  skipseconds = 0;
  currentconfigindex = config->getConfigIndex(skipseconds);
  executetimeseconds = config->getExecuteSeconds();
  while(currentconfigindex < 0 && skipseconds < executetimeseconds)
    currentconfigindex = config->getConfigIndex(++skipseconds);
  if(skipseconds == executetimeseconds)
  {
    cfatal << startl << "Could not locate any of the specified sources in the specified time range - aborting!!!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
  controldata[1] = skipseconds;
  controldata[2] = 0;

  //start by sending a job to each core
  cout << "Manager about to fill up the initial buffer" << endl;
  for(int i=0;i<COREBUFLEN;i++)
  {
    for(int j=0;j<numcores;j++)
    {
      //cout << "About to send data from datastream " << currentdsindex+1 << " to core " << coreids[j] << endl;
      controldata[0] = coreids[j];
      sendManagerData(controldata, j);
    }
  }

  while(controldata[1] < executetimeseconds)
  {
    //cout << "About to receive some visibility and send again" << endl;
    //receive from any core, and send data straight back
    receiveVisData(true);
  }

  //now send the quit signal to each datastream and each core
  managerTerminate();
  
  //now receive the final data from each core
  for(int i=0;i<COREBUFLEN;i++)
  {
    for(int j=0;j<numcores;j++)
      receiveVisData(false);
  }

  //normalise the average pulse profile and write out
  profilemin = profile[0];
  totalprofile = 0;
  for(int i=0;i<numbins;i++)
  {
    if(profile[i] < profilemin)
      profilemin = profile[i];
    totalprofile += profile[i];
  }
  cout << "Total profile was " << totalprofile << ", profilemin was " << profilemin << endl;
  for(int i=0;i<numbins;i++)
    profile[i] = (profile[i] - profilemin)/(totalprofile-numbins*profilemin);

  cout.precision(15);
  string outname = config->getOutputFilename() + ".profileout.ascii";
  output.open(outname.c_str(), ios::trunc);
  output << "Bin#	Weight" << endl;
  for(int i=0;i<numbins;i++)
    output << i << " " << profile[i] << endl;
  output.close();
}

void managerTerminate()
{
  for(int i=0;i<numcores;i++)
    MPI_Send(controldata, 1, MPI_INT, coreids[i], CR_TERMINATE, return_comm);
  for(int i=0;i<numdatastreams;i++)
    MPI_Send(controldata, 3, MPI_INT, i+1, DS_TERMINATE, MPI_COMM_WORLD);
}

void receiveVisData(bool sendagain)
{
  MPI_Status mpistatus;
  int sourcecore, sourceid=0;
  MPI_Recv(resultbuffer, numbins, MPI_DOUBLE, MPI_ANY_SOURCE, MPI_ANY_TAG, return_comm, &mpistatus);
  sourcecore = mpistatus.MPI_SOURCE;
  for(int i=0;i<numcores;i++)
  {
    if(coreids[i] == sourcecore)
      sourceid = i;
  }
  if(sendagain)
  {
    controldata[0] = sourcecore;
    //cout << "About to send more data" << endl;
    sendManagerData(controldata, sourceid);
  }
  for(int i=0;i<numbins;i++)
    profile[i] = profile[i] + resultbuffer[i];
}

void sendManagerData(int data[], int coreindex)
{
  int configindex;
  data[3] = currentdsindex;
  MPI_Send(&data[1], 3, MPI_INT, coreids[coreindex], CR_RECEIVETIME, return_comm);
  //cout << "About to send to ds " << currentdsindex+1 << endl;
  MPI_Ssend(data, 3, MPI_INT, currentdsindex+1, DS_PROCESS, MPI_COMM_WORLD);
  //cout << "Sent to ds " << currentdsindex+1 << endl;
  currentdsindex++;
  if(currentdsindex == numdatastreams)
    currentdsindex = 0;
  data[2] += nsincrement;
  if(data[2] >= 1000000000)
  {
    data[2] -= 1000000000;
    data[1]++;
    if(data[0]%10 == 0)
      cout << "Up to execute second " << data[1] << "/" << executetimeseconds << endl;
    //check that we haven't changed configs
    configindex = config->getConfigIndex(data[1]);
    while(configindex < 0 && data[1] < executetimeseconds)
    {
      configindex = config->getConfigIndex(++data[1]); //we won't send out data for this time
      data[2] = 0;
    }
  }
}

void processCore()
{
  corecounter = 0;
  mode = config->getMode(0, 0);
  bins = new int*[config->getFreqTableLength()];
  for(int i=0;i<config->getFreqTableLength();i++)
    bins[i] = new int[numchannels + 1];
  databytes = int(config->getMaxDataBytes()*(double(config->getBlocksPerSend(0) + config->getGuardBlocks(0))/double(config->getBlocksPerSend(0)))+0.5);
  Configuration::dataformat df = config->getDataFormat(0,0);
  if(df == Configuration::MKIV || df == Configuration::VLBA || df == Configuration::MARK5B)
    databytes += config->getFrameBytes(0, 0);
  rawbuffer = new char*[COREBUFLEN];
  delaydata = new double*[COREBUFLEN];
  offsets = new int*[COREBUFLEN];
  for(int i=0;i<COREBUFLEN;i++) {
    rawbuffer[i] = new char[databytes];
    delaydata[i] = new double[config->getMaxSendBlocks() + 1];
    offsets[i] = new int[3];
  }
  for(int i=0;i<COREBUFLEN;i++) {
    coreReceiveData();    
    corecounter++;
  }

  while(!quit) {
    coreProcessData();
    coreReceiveData();
    corecounter++;
  }
  coreProcessData();
}

void coreReceiveData()
{
  MPI_Status mpistatus;
  int perr, ds;

  if(quit)
    return; //don't try to read, we've already finished

  //Get the instructions on the time offset from the FxManager node
  MPI_Recv(offsets[corecounter%COREBUFLEN], 3, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, return_comm, &mpistatus);
  if(mpistatus.MPI_TAG == CR_TERMINATE) {
    quit = true;
    return;
  }
  ds = offsets[corecounter%COREBUFLEN][2]+1;
  MPI_Recv(rawbuffer[corecounter%COREBUFLEN], databytes, MPI_UNSIGNED_CHAR, ds, CR_PROCESSDATA, MPI_COMM_WORLD, &mpistatus);
  MPI_Recv(delaydata[corecounter%COREBUFLEN], config->getMaxSendBlocks() + 1, MPI_DOUBLE, ds, CR_PROCESSCONTROL, MPI_COMM_WORLD, &mpistatus);
}

void coreProcessData()
{
  int bandfreq;
  double offsetmins;
  cf32 * autocorr;
  int dsindex = offsets[corecounter%COREBUFLEN][2];
  Polyco * polyco = Polyco::getCurrentPolyco(0, config->getStartMJD(), double(config->getStartSeconds() + offsets[corecounter%COREBUFLEN][0])/86400.0, polycos, config->getNumPolycos(0), false);
  if(polyco == NULL) {
    cerr << "Error - could not locate a polyco to cover MJD " << config->getStartMJD() << ", seconds " << config->getStartSeconds() + offsets[corecounter%COREBUFLEN][0] << " - ABORTING!" << endl;
   MPI_Abort(MPI_COMM_WORLD, 1);
  }
  polyco->setTime(config->getStartMJD(), double(config->getStartSeconds() + offsets[corecounter%COREBUFLEN][0] + double(offsets[corecounter%COREBUFLEN][1])/1000000000.0)/86400.0);
  mode->setDelays(&(delaydata[corecounter%COREBUFLEN][1]));
  mode->setData((u8*)rawbuffer[corecounter%COREBUFLEN], databytes, delaydata[corecounter%COREBUFLEN][0]);
  mode->setOffsets(offsets[corecounter%COREBUFLEN][0], offsets[corecounter%COREBUFLEN][1]);
  for(int i=0;i<numbins;i++)
    resultbuffer[i] = 0.0;
  //cout << "Core " << myid << " is about to process data, offsets[0] is " << offsets[corecounter%COREBUFLEN][0] << endl;
  for(int i=0;i<config->getBlocksPerSend(0);i++) {
    mode->zeroAutocorrelations();
    mode->process(i);
    offsetmins = double(i*numchannels)/(60.0*1000000.0*config->getConfigBandwidth(0));
    polyco->getBins(offsetmins, bins);
    for(int j=0;j<config->getDNumInputBands(0, dsindex);j++) {
      bandfreq = config->getDFreqIndex(0, dsindex, j);
      autocorr = mode->getAutocorrelation(false, j);
      for(int k=0;k<numchannels-3;k++) {
        resultbuffer[bins[bandfreq][k+2]] += autocorr[k+2].re;
      }
    }
  }

  //send the results back to the manager
  //cout << "Resultbuffer[0] was " << resultbuffer[0] << ", resultbuffer[5] was " << resultbuffer[5] << endl;
  if(delaydata[corecounter%COREBUFLEN][1] < 0.0)
    //cout << "This subint contained rubbish" << endl;
  cout << "Core " << myid << " about to send results back to manager" << endl;
  MPI_Ssend(resultbuffer, numbins, MPI_DOUBLE, fxcorr::MANAGERID, true, return_comm);    
}
