/*******************************************************************
 *
 * Name: GetChannelLengthExample.cpp
 *    
 * This example is for StreamStor boards with multi channel
 * recordings on them. The multi channel recording could have been
 * done over the Serial FPDP daughter board or over the PCIe bus.
 * This example shows how to determine the number of channels
 * recorded and the amount of data that was recorded on each 
 * channel in multi-channel mode.
 *
 * This example assumes that you have already recorded some data in
 * multi-channel mode.
 *
 *******************************************************************/

//$Id: //streamstor/example/GetChannelLengthExample.cpp#4 $

#include <iostream>
using namespace std;

#include <stdio.h>
#include "xlrapi.h"

#ifndef WIN32
#define LONGLONGFMT "%llu"
#else
#include <windows.h>
#define LONGLONGFMT "%I64u"
#endif

#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

SSHANDLE    XlrDevice = INVALID_SSHANDLE;
BOOLEAN     printLastError = FALSE;

void cleanup();
void PrintXLRError();

int main(int argc, char *argv[])
{
   S_DIR             sDir;
   S_RECDCHANNELINFO channelInfo;
   UINT64            dwLength=0;
   UINT32            deviceToOpen=0;
   UINT32            channel=0;
   char              prtBuff[256];
   UINT32			   i;
   
   atexit(cleanup);

   printf 
      ("This program demonstrates getting lengths on multi-channel recordings.\n");
   printf ("Enter StreamStor device to query:  \n");
   cin >> deviceToOpen;

   printf("Opening StreamStor ...\n");
   if(XLROpen(deviceToOpen, &XlrDevice) != XLR_SUCCESS)
   {
      printf ("Error:  Open failed.\n");
      exit(1);
   }

   //
   // IMPORTANT:  To get the lengths from data recorded in multi-channel
   // mode, even if only a single channel was actually recorded, you 
   // must set the mode to multi-channel before attempting to get the length.
   //
   if (XLRSetMode(XlrDevice, SS_MODE_MULTI_CHANNEL) != XLR_SUCCESS) {
      printf ("Error: Could not set mode to Multi Channel.\n");
      exit(1);
   }

   if( XLRGetRecordedChannelInfo( XlrDevice, &channelInfo )!= XLR_SUCCESS )
   {
      printf ("Error: Could not get recorded channel info.\n");
      exit(1);
   }

   printf( "Total Number of Channels recorded: %u\n", 
            channelInfo.NumChannelsRecorded );


  
   //
   // Examine each channel, reporting on its length.
   //
   for( i=0; i < channelInfo.NumChannelsRecorded; i++ )
   {
      //
      // You must first select the channel you are interested in.
      //
     // Select the channel we want to get the length of
      if (XLRSelectChannel(XlrDevice, channelInfo.RecordedChannelNumber[i]) != XLR_SUCCESS)
      {
         printf("/n Error - could not select channel %u.\n", channelInfo.RecordedChannelNumber[i]);
         exit(1);
      }
      //
      // You can get the length by calling either XLRGetDirectory() or by
      // calling XLRGetLength().
      //
      if(XLRGetDirectory(XlrDevice, &sDir) != XLR_SUCCESS)
      {
         printf ("Error: XLRGetDirectory failed.\n");
         exit(1);
      }

      sprintf (prtBuff, 
         "XLRGetDirectory:  bytes on channel %u = %s\n", channelInfo.RecordedChannelNumber[i], LONGLONGFMT);
      printf(prtBuff, sDir.Length);

      // now get use XLRGetLength() to get the length
      dwLength = XLRGetLength(XlrDevice);
      sprintf (prtBuff, "XLRGetLength:  bytes on channel %u = %s\n", 
         channelInfo.RecordedChannelNumber[i], LONGLONGFMT);
      printf(prtBuff, dwLength);
    
    } 

   printLastError = FALSE;
   exit(0);
}

 
/*
 * Name: cleanup
 *
 * Parameters: none
 *
 * Description: Called by exit to close handles.
 *
 * Returns: void
 *
 */
void cleanup(void)
{
   if (printLastError == TRUE) {
      PrintXLRError();
   }
   if (XlrDevice != INVALID_SSHANDLE) {
      printf ("Closing StreamStor card.\n");
      XLRClose(XlrDevice);
   }
}


/*
 *  Name: PrintXLRError
 *
 *  Parameters: none
 *
 *  Description: retrieves the last StreamStor error code and then prints
 *    out the corresponding error message.
 *
 *  Returns: void
 *
 */
void PrintXLRError() 
{
   char	errorMessage[XLR_ERROR_LENGTH];

   XLRGetErrorMessage(errorMessage, XLRGetLastError());
   printf ("XLR error:  %s\n", errorMessage);
}
 

