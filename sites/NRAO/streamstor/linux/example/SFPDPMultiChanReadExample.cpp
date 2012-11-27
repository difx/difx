/**************************************************************************
 * Name: SFPDPMultiChanReadExample.cpp
 * 
 * This example is for Amazon boards with a SFPDP daughter board.  It shows 
 * how to read data that was recorded on multiple channels.
 *
 * The example assumes that StreamStor device 1 has data on it that was
 * previously recorded on channel 28 (optical port 1).
 *
 * See the "Channel Description and Selection" chapter in the StreamStor SDK 
 * Users' Manual for more information.
 *
 ****************************************************************************/

//$Id: //streamstor/example/SFPDPMultiChanReadExample.cpp#4 $

#include <iostream>
#include <stdio.h>
#ifdef WIN32
#include <conio.h>
#include <windows.h>
#include <winioctl.h>
#endif

#ifndef WIN32
#define LONGLONGFMT "%llu"
#else
#define LONGLONGFMT "%I64u"
#endif

#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

#include "xlrapi.h"

void PrintXLRError();

SSHANDLE    xlrDevice = INVALID_SSHANDLE;

void cleanup(void);

int main(int argc, char *argv[])
{
   S_READDESC   	readDesc;
   UINT64         u64Length=0;
   UINT32        	readBuffer[40000];
   char           prtBuff[256];

   atexit(cleanup);

   printf ("Opening StreamStor Device 1 ...\n");
   if( XLROpen(1, &xlrDevice) != XLR_SUCCESS) 
   {
      printf ("Error:  Cannot open StreamStor device 1.\n");
      exit(1);
   }

   //
   // Find out how much data was recorded on channel 28. To get the 
   // lengths of data recorded in multi-channel mode, even if only a single 
   // channel was actually recorded, you must set the mode to multi-channel 
   // before calling XLRGetLength.
   // 
   printf ("Setting mode to multi-channel to get length of data.\n");
   printf ("\trecorded on channel 28.\n");
   if (XLRSetMode(xlrDevice, SS_MODE_MULTI_CHANNEL) != XLR_SUCCESS) 
   {
      printf ("Error: XLRSetMode to multi-channel failed.\n");
      exit(1);
   }
    
   //
   // Clear the channels once, then setup the channels to use.
   //
   if( XLRClearChannels(xlrDevice) != XLR_SUCCESS)
   {
      printf ("Error:  Clear Channels failed.\n");
      exit(1);
   }

   // 
   // Channel 28 is the channel we are interested in, so select it.
   //
   if (XLRSelectChannel(xlrDevice, 28) != XLR_SUCCESS)
   {
      printf("Error - could not select channel 28.\n");
      exit(1);
   }

   //
   // XLRGetLength will report on the currently selected channel, 
   // which is now 28.
   //
   u64Length = XLRGetLength(xlrDevice);
   if (u64Length == 0) {
      printf ("There is no data recorded on channel 28.\n");
      exit(1);
   }
   sprintf (prtBuff, "Bytes recorded on channel 28 = %s\n", LONGLONGFMT);
   printf(prtBuff, u64Length);


   //
   // We will call XLRRead to read data over the PCI bus.  You can only 
   // read one channel at a time over the PCI bus, so the StreamStor must 
   // be in single channel mode before calling XLRRead.
   //
   printf ("Switching to Single Channel Mode for reading over PCI Bus.\n");
   if( XLRSetMode(xlrDevice, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS)
   {
      printf ("Error:  XLRSetMode to single channel failed.\n");
      exit(1);
   }

   //
   // Selecting channel 28 will indicate to the StreamStor that channel 28
   // contains the data you want to read.  Since we already selected channel 
   // 28 above, we don't need to select it again here.
   //

   //
   // Note that you do not need to bind channel 28 in order to read from it 
   // with XLRRead.  Binding is used to establish the data path for a 
   // specific port, and in this example you will not be moving any 
   // data over optical port 1 (channel 28) so you don't need to bind it.
   //
   printf ("Data will be read from channel 28.\n");

   //
   // The datapath used by XLRRead is the PCI bus (channel 0).  So, bind 
   // channel 0 for output.
   //
   if( XLRBindOutputChannel(xlrDevice, 0) != XLR_SUCCESS)
   {
      printf ("Error:  Bind output channel to 0 failed.\n");
      exit(1);
   }

   //
   // Set up the read structure to be passed to XLRRead. AddrHi and AddrLo 
   // must be appropriately byte-aligned.
   //
   readDesc.AddrHi = 0;
   readDesc.AddrLo = 0;
   readDesc.XferLength = sizeof( readBuffer );
   readDesc.BufferAddr = readBuffer;

   //
   // Now read a buffer of channel 28 data into readBuffer.
   //
   if( XLRRead( xlrDevice, &readDesc ) != XLR_SUCCESS )
   {
      printf ("Error:  XLRRead failed.\n");
      exit(1);
   }

   printf ("XLRRead succeeded.\n");

   exit(0);
}


/*
 *  NAME: cleanup
 *
 *  PARAMETERS: none
 *
 *  DESCRIPTION: Called by exit to close handle and print error messages.
 *
 *  RETURNS: void
 *
 */
void cleanup()
{

   PrintXLRError();
   if (xlrDevice != INVALID_SSHANDLE) 
   {
      printf ("Closing StreamStor.\n");
      XLRClose(xlrDevice);
   }
}

/*
 ** PrintXLRError
 *
 *  PARAMETERS: none
 *
 *  DESCRIPTION: retrieves the last StreamStor error code and then prints
 *    out the corresponding error message.
 *
 *  RETURNS: void
 *
 */
void PrintXLRError() 
{
   char	 errorMessage[XLR_ERROR_LENGTH];
   UINT32 code = XLRGetLastError();
   if (code != 3) 
   {
      XLRGetErrorMessage(errorMessage, code);
      printf ("Exiting because of error:  %s\n", errorMessage);
   }
}
 

