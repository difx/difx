/* 
 * The following C code shows how to set up the top Serial FPDP (Port 1) 
 * connector as  an input channel to record, then read the data back through 
 * the  second from the top Serial FPDP (Port 2)connector. 
 *
 * XlrStatus should always be checked  to see if a failure 
 * was returned from a StreamStor API function call.
 *
 * This example also utilizes a network connection to communicate with an LTX2
 * system.
 */

//$Id: //streamstor/example/SFPDPNetOpen/SFPDPNetOpenExample.c#5 $

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
#define BUFFER_SIZE     131072
#define LTX2_NET_PORT   10001
#define WAIT_TIME       10000

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

int main(int argc, char *argv[])
{
   S_DIR                sDir;
   SSHANDLE             hTarget;
   UINT64               u64StartAddress=0;
   UINT32               AddrHi,AddrLo;
   char                 prtBuf[256];

   if( argc < 2 )
   {
      printf("Error: Missing network address\n");
      exit(1);
   }

   // Attempt to open LTX2 using IP address supplied by user
   printf("Opening StreamStor device...");
   if( XLRNetOpen(1, argv[1], LTX2_NET_PORT, &hTarget) != XLR_SUCCESS) 
   {
      printf ("\n  Error:  Cannot open StreamStor device 1\n");
      PrintXLRError();
      exit(1);
   }
   printf("OK\n");

   printf("Configuring StreamStor for record...");
   // Make sure system is in single channel record mode
   if( XLRSetMode(hTarget, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Set Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   
   // Clear all the input/output channel setup
   if( XLRClearChannels(hTarget) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Clear Channels failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Bind the Serial FPDP Port 1 (channel 28), the top SFPDP port, as the input channel.
   if( XLRBindInputChannel(hTarget, 28) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Bind Input Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select Channel 28 as channel to record
   if( XLRSelectChannel(hTarget, 28) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Select Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Enable CRC option on SFPDP Port 1
   if( XLRSetDBMode(hTarget, SS_SFPDPMODE_NORMAL, SS_DBOPT_SFPDP_CRC_ENABLE) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Set DB Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   printf("OK\n");

   // Start recording.
   if( XLRAppend(hTarget) != XLR_SUCCESS)
   {
      printf ("Error:  Start Record failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Sleep a while, letting some data record.
   printf( "Recording in progress...\n");
   Sleep(WAIT_TIME);

   printf("Stopping recording...");
   // Stop the recording
   if( XLRStop(hTarget) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Stop failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   printf("OK\n");

   // Get length of recording
   if( XLRGetDirectory(hTarget, &sDir) != XLR_SUCCESS)
   {
      printf ("Error:  Get Directory failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   sprintf (prtBuf, "Final length recorded = %s bytes.\n", LONGLONGFMT);
   printf (prtBuf, sDir.Length);
   
   printf("Configuring StreamStor for playback...");

   // Set up to playback the data we just recorded.
   if( XLRSetMode(hTarget, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Set Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   if( XLRClearChannels(hTarget) != XLR_SUCCESS)
   {
      printf ("\n  Error:  ClearChannels failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Bind the Serial FPDP Port 2 (channel 29), the second from the top SFPDP 
   // port, as output channel.
   if( XLRBindOutputChannel(hTarget, 29) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Bind Output Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select Channel to playback data that was written on channel 28. 
   // This will be sent out channel 29 because of the Binding above.
   if( XLRSelectChannel(hTarget, 28) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Set Select Channel failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Enable CRC option on SFPDP Port 2
   if( XLRSetDBMode(hTarget, SS_SFPDPMODE_NORMAL, SS_DBOPT_SFPDP_CRC_ENABLE) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Set DB Mode failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   printf("OK\n");

   u64StartAddress = 0;

   // Build the read descriptor.
   AddrHi = (UINT32)(u64StartAddress >> 32);
   AddrLo = (UINT32)(u64StartAddress & 0xFFFFFFFF);

   // Start the Playback of data.
   if( XLRPlayback(hTarget, AddrHi, AddrLo) != XLR_SUCCESS)
   {
      printf ("Error:  Playback failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Sleep a while, letting some data playback.
   printf("Playback in progress...");

   // This could query device status to wait for Playback indicator to go FALSE
   // To keep it simple we just playback for a fixed time period
   Sleep(WAIT_TIME);

   // Stop the playback
   if( XLRStop(hTarget) != XLR_SUCCESS)
   {
      printf ("\n  Error:  Stop failed.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   printf("Complete!\n");
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
   
