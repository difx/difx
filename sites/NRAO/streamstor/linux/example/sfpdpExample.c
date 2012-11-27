/* 
 * The following C code shows how to set up the top Serial FPDP (Port 1) 
 * connector as an input channel to record, then playback the data back 
 * through the second from the top Serial FPDP (Port 2)connector. 
 * XlrStatus should always be checked to see if a failure 
 * was returned from a StreamStor API function call.
 */

/** include files **/
#include <stdio.h>
#include "xlrapi.h"

#ifdef WIN32
#define LONGLONGFMT "%I64u"
#else
#define LONGLONGFMT "%llu"
#include <stdlib.h>   // for malloc
#include <unistd.h>   // for sleep
#include <string.h>   // for strncmp
#define Sleep(x)		(sleep((unsigned int)(x/1000)))
#endif

/** internal functions **/
void PrintXLRError();

/** local definitions **/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

int main(int argc, char *argv[])
{
   SSHANDLE             hTarget;
   S_DIR                sDir;
   char                 prtBuff[256];

   if( XLROpen(1, &hTarget) != XLR_SUCCESS) 
   {
      printf ("Error:  Cannot open StreamStor device 1\n");
      PrintXLRError();
      exit(1);
   }
   
   if( XLRSetMode(hTarget, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS)
   {
      printf ("Error:  Set Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   // The following order of the calls to set up the channel is very important
   if( XLRClearChannels(hTarget) != XLR_SUCCESS)  // call this before setting up the channel for record
   {
      printf ("Error:  Clear Channels failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select Channel 28 as channel to record
   if( XLRSelectChannel(hTarget, 28) != XLR_SUCCESS) // Always call XLRSelectChannel() before
                                                     //   calling XLRBindInputChannel()
   {
      printf ("Error:  Select Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Bind the Serial FPDP Port 1 (channel 28), the top SFPDP port, as the input channel.
   if( XLRBindInputChannel(hTarget, 28) != XLR_SUCCESS)
   {
      printf ("Error:  Bind Input Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Enable CRC option on SFPDP Port 1
   if( XLRSetDBMode(hTarget, SS_SFPDPMODE_NORMAL, SS_DBOPT_SFPDP_CRC_ENABLE) != XLR_SUCCESS)
   {
      printf ("Error:  Set DB Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   
   // Start recording.
   if( XLRRecord(hTarget, FALSE, TRUE) != XLR_SUCCESS)
   {
      printf ("Error:  Record failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Sleep a while, letting some data record.
   Sleep(5000);

   // Stop the recording
   if( XLRStop(hTarget) != XLR_SUCCESS)
   {
      printf ("Error:  Stop failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Get length of recording
   if( XLRGetDirectory(hTarget, &sDir) != XLR_SUCCESS)
   {
      printf ("Error:  Get Directory failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   sprintf(prtBuff,  "Final Data Length of recording for Channel 28 =  %s.\n", 
         LONGLONGFMT);
   printf (prtBuff, sDir.Length);
   
   // Set up to read the data we just recorded.
   if( XLRSetMode(hTarget, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS)
   {
      printf ("Error:  Set Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // The following order of the calls to set up the channel is very important
   if( XLRClearChannels(hTarget) != XLR_SUCCESS)
   {
      printf ("Error:  ClearChannels failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select Channel to read that was written on channel 28. This will be sent out channel 29 because of the Binding below
   if( XLRSelectChannel(hTarget, 28) != XLR_SUCCESS)  // Always call XLRSelectChannel() before
                                                      //   calling XLRBindInputChannel()
   {
      printf ("Error:  Set Select Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Bind the Serial FPDP Port 2 (channel 29), the second from the top SFPDP port, as output channel.
   if( XLRBindOutputChannel(hTarget, 29) != XLR_SUCCESS)
   {
      printf ("Error:  Bind Output Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Enable CRC option on SFPDP Port 2
   if( XLRSetDBMode(hTarget, SS_SFPDPMODE_NORMAL, SS_DBOPT_SFPDP_CRC_ENABLE) != XLR_SUCCESS)
   {
      printf ("Error:  Set DB Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Start the Playback of channel 28 data out port 2 (channel 29).
   if( XLRPlayback(hTarget, 0, 0) != XLR_SUCCESS)
   {
      printf ("Error:  Playback failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Sleep a while, letting some data playback.
   Sleep(5000);

   // Stop the playback
   if( XLRStop(hTarget) != XLR_SUCCESS)
   {
      printf ("Error:  Stop failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   XLRClose(hTarget);

   exit(0);
}
/*
 * Print out the decoded text of the returned StreamStor error code
 *
 */
void PrintXLRError()
{
   char  errorMessage[XLR_ERROR_LENGTH];
   XLRGetErrorMessage(errorMessage, XLRGetLastError());
   printf("\tError Message: %s\n", errorMessage);
}
   

