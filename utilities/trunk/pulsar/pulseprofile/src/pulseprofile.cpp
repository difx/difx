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

#include "pulseprofile.h"
#include "alert.h"
#include "math.h"
#include "mk5.h"
#include "nativemk5.h"

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

  if(argc != 3)
  {
    cerr << "Error - invoke with pulseprofile <configfilename> <targetdatastreamindex>" << endl;
    exit(1);
  }
  if(numprocs < 3) {
    cerr << "Error - must have at least 3 processes (manager, datastream, core(s)) - aborting" << endl;
    return EXIT_FAILURE;
  }

  quit = false;
  numcores = numprocs - 2;
  coreids = new int[numcores];
  for(int i=0;i<numcores;i++)
    coreids[i] = 2 + i;
  controldata = new int[3];
  config = new Configuration(argv[1], myid);
  config->loaduvwinfo(true);
  dsindex = atoi(argv[2]);
  numbins = config->getNumPulsarBins(0);
  numchannels = config->getNumChannels(0);
  nsincrement = int(1000.0*(config->getBlocksPerSend(0)*numchannels)/(config->getDBandwidth(0, 0, 0))+ 0.5);
  if(config->getNumConfigs() > 1 || !config->pulsarBinOn(0)) {
    cerr << "Must be a single config file, with pulsar binning on - aborting" << endl;
    return EXIT_FAILURE;
  }
  if(dsindex >= config->getNumDataStreams()) {
    cerr << "Dsindex " << dsindex << " is greater than the active number of datastreams " << config->getNumDataStreams() << " - aborting" << endl;
    return EXIT_FAILURE;
  }
  if(myid != 1)
    resultbuffer = vectorAlloc_f64(numbins);
  else {
    if(config->isMkV(dsindex))
      datastream = new Mk5DataStream(config, dsindex, myid, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
    else if(config->isNativeMkV(dsindex)) {
      //datastream = new NativeMk5DataStream(config, dsindex, myid, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
      cout << "NativeMk5 not yet supported - aborting!" << endl;
      MPI_Abort(world, 1);
    }
    else
      datastream = new DataStream(config, dsindex, myid, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
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
  else if (myid==1)
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
      controldata[0] = coreids[j];
      sendManagerData(controldata, j);
    }
  }

  while(controldata[1] < executetimeseconds)
  {
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
  MPI_Send(controldata, 3, MPI_INT, 1, DS_TERMINATE, MPI_COMM_WORLD);
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
    sendManagerData(controldata, sourceid);
  }
  for(int i=0;i<numbins;i++)
    profile[i] = profile[i] + resultbuffer[i];
}

void sendManagerData(int data[], int coreindex)
{
  int configindex;
  MPI_Send(&data[1], 2, MPI_INT, coreids[coreindex], CR_RECEIVETIME, return_comm);
  MPI_Ssend(data, 3, MPI_INT, 1, DS_PROCESS, MPI_COMM_WORLD);
  data[2] += nsincrement;
  if(data[2] >= 1000000000)
  {
    data[2] -= 1000000000;
    data[1]++;
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
  mode = config->getMode(0, dsindex);
  bins = new int*[config->getMaxNumFreqs()];
  for(int i=0;i<config->getMaxNumFreqs();i++)
    bins[i] = new int[numchannels + 1];
  databytes = config->getMaxDataBytes();
  if(config->isMkV(dsindex))
    databytes += config->getFrameBytes(0, dsindex);
  rawbuffer = new char*[COREBUFLEN];
  delaydata = new double*[COREBUFLEN];
  offsets = new int*[COREBUFLEN];
  for(int i=0;i<COREBUFLEN;i++) {
    rawbuffer[i] = new char[databytes];
    delaydata[i] = new double[config->getMaxSendBlocks() + 1];
    offsets[i] = new int[2];
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
  int perr;

  if(quit)
    return; //don't try to read, we've already finished

  //Get the instructions on the time offset from the FxManager node
  MPI_Recv(offsets[corecounter%COREBUFLEN], 2, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, return_comm, &mpistatus);
  if(mpistatus.MPI_TAG == CR_TERMINATE) {
    quit = true;
    return;
  }
  MPI_Recv(rawbuffer[corecounter%COREBUFLEN], databytes, MPI_UNSIGNED_CHAR, 1, CR_PROCESSDATA, MPI_COMM_WORLD, &mpistatus);
  MPI_Recv(delaydata[corecounter%COREBUFLEN], config->getMaxSendBlocks() + 1, MPI_DOUBLE, 1, CR_PROCESSCONTROL, MPI_COMM_WORLD, &mpistatus);
}

void coreProcessData()
{
  int bandfreq;
  double offsetmins;
  cf32 * autocorr;
  Polyco * polyco = Polyco::getCurrentPolyco(0, config->getStartMJD(), double(config->getStartSeconds() + offsets[corecounter%COREBUFLEN][0])/86400.0, polycos, config->getNumPolycos(0), false);
  polyco->setTime(config->getStartMJD(), double(config->getStartSeconds() + offsets[corecounter%COREBUFLEN][0] + double(offsets[corecounter%COREBUFLEN][1])/1000000000.0)/86400.0);
  mode->setDelays(&(delaydata[corecounter%COREBUFLEN][1]));
  mode->setData((u8*)rawbuffer[corecounter%COREBUFLEN], databytes, delaydata[corecounter%COREBUFLEN][0]);
  mode->setOffsets(offsets[corecounter%COREBUFLEN][0], offsets[corecounter%COREBUFLEN][1]);
  for(int i=0;i<numbins;i++)
    resultbuffer[i] = 0.0;
  for(int i=0;i<config->getBlocksPerSend(0);i++) {
    mode->zeroAutocorrelations();
    mode->process(i);
    offsetmins = double(i*numchannels)/(60.0*1000000.0*config->getConfigBandwidth(0));
    polyco->getBins(offsetmins, bins);
    for(int j=0;j<config->getDNumInputBands(0, dsindex);j++) {
      bandfreq = config->getDFreqIndex(0, dsindex, j);
      autocorr = mode->getAutocorrelation(false, j);
      for(int k=0;k<numchannels-1;k++) {
        resultbuffer[bins[bandfreq][k+1]] += autocorr[k+1].re;
      }
    }
  }

  //send the results back to the manager
  cout << "Resultbuffer[0] was " << resultbuffer[0] << ", resultbuffer[5] was " << resultbuffer[5] << endl;
  if(delaydata[corecounter%COREBUFLEN][1] < 0.0)
    cout << "This subint contained rubbish" << endl;
  MPI_Ssend(resultbuffer, numbins, MPI_DOUBLE, fxcorr::MANAGERID, true, return_comm);    
}
