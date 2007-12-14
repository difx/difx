/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken and Adam Deller                  *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/

#include <mpi.h>
#include <string.h>
#include <stdlib.h>
#include "config.h"
#include "nativemk5.h"

#define u32 uint32_t


NativeMk5DataStream::NativeMk5DataStream(Configuration * conf, int snum, 
	int id, int ncores, int * cids, int bufferfactor, int numsegments) :
		Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor, 
	numsegments)
{
	XLR_RETURN_CODE xlrRC;

	/* each data buffer segment contains an integer number of frames, 
	 * because thats the way config determines max bytes
	 */

	cout << "Opening Streamstor [" << mpiid << "]" << endl;
	xlrRC = XLROpen(1, &xlrDevice);
  
  	if(xlrRC == XLR_FAIL)
	{
		XLRClose(xlrDevice);
		cerr << "Error opening Streamstor device [" << mpiid << "]" << endl;
		cerr << "Do you have permission +rw for /dev/windrvr6?" << endl;
		exit(1);
	}
	else
	{
		cerr << "Success opening Streamstor device [" << mpiid << "]" <<  endl;
	}

	module.nscans = -1;
	readpointer = -1;
	scan = 0;
}

NativeMk5DataStream::~NativeMk5DataStream()
{
	XLRClose(xlrDevice);
}

/* Here "File" is VSN */
void NativeMk5DataStream::initialiseFile(int configindex, int fileindex)
{
	double startmjd;
	double scanstart, scanend;
	int v, i, n;
	int doUpdate = 0;
	char *mk5dirpath;

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(mk5dirpath == 0)
	{
		mk5dirpath = ".";
	}

	if(module.nscans < 0)
	{
		doUpdate = 1;
		cout << "getting module info" << endl;
		v = getCachedMark5Module(&module, xlrDevice, corrstartday, 
			datafilenames[configindex][fileindex].c_str(), mk5dirpath);

		if(v < 0)
		{
			cerr << "Module " << 
				datafilenames[configindex][fileindex] << 
				" not found in unit - aborting!!!" << endl;
			dataremaining = false;
			return;
		}
	}

	// find starting position
  
	startmjd = corrstartday + corrstartseconds/86400.0;
  
	if(scan != 0)  /* just continue by reading next scan */
	{
		cout << "NM5 : continuing read in next scan" << endl;
		scan++;
		if(scan-module.scans >= module.nscans)
		{
			cerr << "No more data on module" << endl;
			scan = 0;
			dataremaining = false;
			keepreading = false;
			return;
		}
		cout << "Before[" << mpiid << "] rs = " << readseconds << "  rns = " << readnanoseconds << endl;
		scanstart = scan->mjd + (scan->sec + scan->ns*1.e-9)/86400.0;
		scanend = scanstart + scan->duration/86400.0;
		readpointer = scan->start + scan->frameoffset;
		readseconds = (scan->mjd-corrstartday)*86400 
			+ scan->sec - corrstartseconds;
		readnanoseconds = scan->ns;
		cout << "After[" << mpiid << "]  rs = " << readseconds << "  rns = " << readnanoseconds << endl;
	}
	else 
	{
		n = 0;
		for(i = 0; i < module.nscans; i++)
		{
			double scanstart, scanend;
			scan = module.scans + i;
			scanstart = scan->mjd + (scan->sec + scan->ns*1.e-9)/86400.0;
			scanend = scanstart + scan->duration/86400.0;

			if(startmjd < scanstart)  /* obs starts before data */
			{
				cout << "NM5 : scan found(1) : " << i << endl;
				readpointer = scan->start + scan->frameoffset;
				readseconds = (scan->mjd-corrstartday)*86400 
					+ scan->sec - corrstartseconds;
				readnanoseconds = scan->ns;
				break;
			}
			else if(startmjd < scanend) /* obs starts within data */
			{
				cout << "NM5 : scan found(2) : " << i << endl;
				readpointer = scan->start + scan->frameoffset;
				n = (int)((((corrstartday - scan->mjd)*86400 
			+ (corrstartseconds - (scan->sec + scan->ns*1.e-9)))
					/ (scan->framens*1.e-9)) + 0.5);
				readpointer += n*scan->framebytes;
				readseconds = 0;
				readnanoseconds = 0;
				break;
			}
		}
		cout << "NativeMk5DataStream " << mpiid << 
			" positioned at byte " << readpointer << 
			" seconds = " << readseconds <<" ns = " << 
			readnanoseconds << " n = " << n << endl;

		if(i >= module.nscans || scan == 0)
		{
			cerr << "No valid data found - aborting\n";
			dataremaining = false;
			return;
		}

		cout << "Scan info. start = " << scan->start << " off = " << scan->frameoffset << " size = " << scan->framebytes << endl;
	}

	cout << "The frame start day is " << scan->mjd << 
		", the frame start seconds is " << (scan->sec+scan->ns*1.e-9)
		<< ", readseconds is " << readseconds << 
		", readnanoseconds is " << readnanoseconds << endl;

	/* update all the configs - to ensure that the nsincs and 
	 * headerbytes are correct
	 */
	if(doUpdate)
	{
		cout << "Updating all configs [" << mpiid << "]" << endl;
		for(i = 0; i < numdatasegments; i++)
		{
			updateConfig(i);
		}
	}
	else
	{
		cout << "NOT updating all configs [" << mpiid << "]" << endl;
	}
	
	cout << "Scan " << (scan-module.scans) <<" initialised[" << mpiid << "]" << endl;
}

void NativeMk5DataStream::openfile(int configindex, int fileindex)
{
	cout << "NativeMk5DataStream " << mpiid << 
		" is about to look at a scan" << endl;

	/* fileindex should never increase for native mark5, but
	 * check just in case. 
	 */
	if(fileindex >= confignumfiles[configindex])
	{
		dataremaining = false;
		keepreading = false;
		cout << "NativeMk5DataStream " << mpiid << 
			" is exiting because fileindex is " << fileindex << 
			", while confignumconfigfiles is " << 
			confignumfiles[configindex] << endl;
		return;
	}

	dataremaining = true;

	initialiseFile(configindex, fileindex);
}

void NativeMk5DataStream::moduleToMemory(int buffersegment)
{
	long long start;
	unsigned long *buf;
	unsigned long a, b;
	int i, t;
	S_READDESC      xlrRD;
	XLR_RETURN_CODE xlrRC;
	XLR_ERROR_CODE  xlrEC;
	XLR_READ_STATUS xlrRS;
	int bytes;
	char errStr[XLR_ERROR_LENGTH];

	bytes = readbytes;
	start = readpointer;
	buf = (unsigned long *)&databuffer[(buffersegment*bufferbytes)/
		numdatasegments];
	if(start & 4)
	{
		start += 4;
		buf++;
	}

	if(start + bytes > scan->start + scan->length)
	{
		bytes = scan->start + scan->length - start;
		cout << "[" << mpiid << "] shortening read to only " << bytes << " bytes ";
		cout << "(was " << readbytes << ")" << endl;
	}

	/* always read multiples of 8 bytes */
	bytes &= ~7;

	a = start >> 32;
	b = start & 0xFFFFFFFF; 

	waitForBuffer(buffersegment);

	// xlrRC = XLRReadData(xlrDevice, buf, a, b, bytes);

	xlrRD.AddrHi = a;
	xlrRD.AddrLo = b;
	xlrRD.XferLength = bytes;
	xlrRD.BufferAddr = buf;

	for(t = 0; t < 2; t++)
	{
		XLRReadImmed(xlrDevice, &xlrRD);
		
		/* Wait up to 10 seconds for a return */
		for(i = 1; i < 10000; i++)
		{
			xlrRS = XLRReadStatus(0);
			if(xlrRS == XLR_READ_COMPLETE)
			{
				break;
			}
			else if(xlrRS == XLR_READ_ERROR)
			{
				cerr << "XXX:" << a << ":" << b << "  rp = " << readpointer << endl;
				xlrEC = XLRGetLastError();
				XLRGetErrorMessage(errStr, xlrEC);
				cerr << "NativeMk5DataStream " << mpiid << 
					" XLRReadData error: " << errStr << endl; 
				dataremaining = false;
				keepreading = false;
				bufferinfo[buffersegment].validbytes = 0;
				return;
			}
			if(i % 1000 == 0)
			{
				cout << "[" << mpiid << "] Waited " << i << " ms   state = "; 
				if(xlrRS == XLR_READ_WAITING)
				{
					cout << "XLR_READ_WAITING" << endl;
				}
				if(xlrRS == XLR_READ_RUNNING)
				{
					cout << "XLR_READ_RUNNING" << endl;
				}
				else
				{
					cout << "XLR_READ_OTHER " << endl;
				}
			}
			usleep(1000);
		}
		if(xlrRS == XLR_READ_COMPLETE)
		{
			break;
		}
		else if(t == 0)
		{
			cout << "[" << mpiid << "]  XLRClose() being called!" << endl;
			XLRClose(xlrDevice);
			
			cout << "[" << mpiid << "]  XLRCardReset() being called!" << endl;
			xlrRC = XLRCardReset(1);
			cout << "[" << mpiid << "]  XLRCardReset() called! " << xlrRC << endl;

			cout << "[" << mpiid << "]  XLROpen() being called!" << endl;
			xlrRC = XLROpen(1, &xlrDevice);
			cout << "[" << mpiid << "]  XLROpen() called! " << xlrRC << endl;

		}
	}

#if 0
	if(xlrRC != XLR_SUCCESS)
	{
		cerr << "XXX:" << a << ":" << b << "  rp = " << readpointer << endl;
		xlrEC = XLRGetLastError();
		XLRGetErrorMessage(errStr, xlrEC);
		cerr << "NativeMk5DataStream " << mpiid << 
			" XLRReadData error: " << errStr << endl; 
		dataremaining = false;
		keepreading = false;
		bufferinfo[buffersegment].validbytes = 0;
		return;
	}
#endif
	if(xlrRS != XLR_READ_COMPLETE)
	{
		cerr << "XXX:" << a << ":" << b << "  rp = " << readpointer << endl;
		cerr << "[" << mpiid << "] Waited 10 seconds for a read and gave up" << endl;
		dataremaining = false;
		keepreading = false;
		bufferinfo[buffersegment].validbytes = 0;
		return;
	}
	else
	{
		bufferinfo[buffersegment].validbytes = bytes;
		if(bytes < readbytes)
		{
			dataremaining = false;
		}
		else
		{
			readnanoseconds += bufferinfo[buffersegment].nsinc;
			readseconds += readnanoseconds/1000000000;
			readnanoseconds %= 1000000000;
			readpointer += bytes;
		}
	}
}

void NativeMk5DataStream::loopfileread()
{
  int perr;
  int numread = 0;

  cout << "NM5 : loopfileread starting" << endl;

  //lock the first section to start reading
  openfile(bufferinfo[0].configindex, 0);
  moduleToMemory(numread++);
  moduleToMemory(numread++);
  perr = pthread_mutex_lock(&(bufferlock[numread]));
  if(perr != 0)
    cerr << "Error in initial telescope readthread lock of first buffer section!!!" << endl;
  readthreadstarted = true;
  perr = pthread_cond_signal(&initcond);
  if(perr != 0)
    cerr << "NativeMk5DataStream readthread " << mpiid << " error trying to signal main thread to wake up!!!" << endl;
  moduleToMemory(numread++);

  lastvalidsegment = (numread-1)%numdatasegments;
  while((bufferinfo[lastvalidsegment].configindex < 0 || filesread[bufferinfo[lastvalidsegment].configindex] <= confignumfiles[bufferinfo[lastvalidsegment].configindex]) && keepreading)
  {
    cout << "[" << mpiid << "] Entering while loop" << endl;
    while(dataremaining && keepreading)
    {
      lastvalidsegment = (lastvalidsegment + 1)%numdatasegments;
      
      //lock the next section
      perr = pthread_mutex_lock(&(bufferlock[lastvalidsegment]));
      if(perr != 0)
        cerr << "Error in telescope readthread lock of buffer section!!!" << lastvalidsegment << endl;

      //unlock the previous section
      perr = pthread_mutex_unlock(&(bufferlock[(lastvalidsegment-1+numdatasegments)% numdatasegments]));
      if(perr != 0)
        cerr << "Error in telescope readthread unlock of buffer section!!!" << (lastvalidsegment-1+numdatasegments)%numdatasegments << endl;

      //do the read
      moduleToMemory(lastvalidsegment);
      numread++;
    }
    cout << "Out of while loop: " << mpiid << endl;
    if(keepreading)
    {
      //if we need to, change the config
      int nextconfigindex = config->getConfigIndex(readseconds);
      cout << "old config[" << mpiid << "] = " << nextconfigindex << endl;
      while(nextconfigindex < 0 && readseconds < config->getExecuteSeconds())
      {
        nextconfigindex = config->getConfigIndex(++readseconds);
      }
      cout << "new config[" << mpiid << "] = " << nextconfigindex << endl;
      if(readseconds == config->getExecuteSeconds())
      {
        cout << "Condition1 : " << mpiid << endl;
        bufferinfo[(lastvalidsegment+1)%numdatasegments].seconds = config->getExecuteSeconds();
        bufferinfo[(lastvalidsegment+1)%numdatasegments].nanoseconds = 0;
        keepreading = false;
      }
      else
      {
        cout << "Condition2 : " << mpiid << endl;
        if(config->getConfigIndex(readseconds) != bufferinfo[(lastvalidsegment + 1)%numdatasegments].configindex)
	{
          updateConfig((lastvalidsegment + 1)%numdatasegments);
        }
	//if the datastreams for two or more configs are common, they'll all have the same 
        //files.  Therefore work with the lowest one
        int lowestconfigindex = bufferinfo[(lastvalidsegment+1)%numdatasegments].configindex;
        for(int i=config->getNumConfigs()-1;i>=0;i--)
        {
          if(config->getDDataFileNames(i, streamnum) == config->getDDataFileNames(lowestconfigindex, streamnum))
            lowestconfigindex = i;
        }
        openfile(lowestconfigindex, filesread[lowestconfigindex]);
	cout << "Condition2 : " << mpiid << "  Restarting loop" << endl;
      }
    }
  }
  perr = pthread_mutex_unlock(&(bufferlock[lastvalidsegment]));
  if(perr != 0)
    cerr << "Error in telescope readthread unlock of buffer section!!!" << lastvalidsegment << endl;

  cout << "DATASTREAM " << mpiid << "'s readthread is exiting!!! Filecount was " << filesread[bufferinfo[lastvalidsegment].configindex] << ", confignumfiles was " << confignumfiles[bufferinfo[lastvalidsegment].configindex] << ", dataremaining was " << dataremaining << ", keepreading was " << keepreading << endl;
}
