/*******************************************************************
 *
 *    SFPDPMultiChanRecordExample.cpp
 *
 * This example is for Amazon boards with a serial FPDP daughter card.
 * It shows how to setup the StreamStor system to do a multichannel
 * recording. In the example, data will be recorded from 
 * Port 1 (Channel 28) and Port 2 (channel 29). 
 *
 * The example assumes there is some other device (or devices) that 
 * Port1 and Port2 are connected to at the other end of the optical 
 * fiber that will be transmitting data to be recorded. 
 *
 *******************************************************************/

//$Id: //streamstor/example/SFPDPMultiChanRecordExample.cpp#4 $

/** include files **/
#include <iostream>
using namespace std;

#include <stdio.h>

#ifdef WIN32
#include <conio.h>
#include <windows.h>
#include <winioctl.h>
#endif

#ifndef WIN32
#include <unistd.h>  // for sleep
#define Sleep(x)     (sleep((unsigned int)(x/1000)))
#define LONGLONGFMT "%llu"
#else
#define LONGLONGFMT "%I64u"
#endif

//StreamStor
#include "xlrapi.h"

SSHANDLE hRecCard = INVALID_SSHANDLE;
BOOLEAN  printLastError = TRUE;

void PrintXLRError();
void cleanup();

int main( int argc, char *argv[] )
{
   UINT64      u64ChannelLength=0;

   UINT32      deviceToOpen=0;
   UINT32      channel28 = 28;
   UINT32      channel29 = 29;
   UINT32      channel;

   char        prtBuff[256];
   
   atexit( cleanup );

   printf("This program demonstrates SFPDP recording from two channels.\n");
   printf("Enter StreamStor device number to playback data from, i.e., 1,2,3...: \n");
   cin >> deviceToOpen;

   printf( "Opening StreamStor Recording Device ..." );
   if( XLROpen( deviceToOpen, &hRecCard ) != XLR_SUCCESS )
   {
      printf( "XLROpen failed.\n" );
      exit( 1 );
   }
   printf( "OK.\n" );

   //
   // Set up StreamStor device for Multi-channel mode to record BOTH channels 
   // 28 and 29 over the Serial FPDP bus daughter card.
   //
   if (XLRSetMode(hRecCard, SS_MODE_MULTI_CHANNEL) != XLR_SUCCESS) {
      printf ("XLRSetMode failed - Could not set mode to Multi Channel.\n");
      exit(1);
   }

   //
   // Call XLRClearChannels once before setting up the channels for playback.
   //
   if(XLRClearChannels(hRecCard) != XLR_SUCCESS)
   {
      printf( "Receiver clear channels failed.\n" );
      exit( 1 );
   }
   //
   // Loop to set up Channel 28 (Port 1) and Channel 29 (Port 2)  for recording
   // Always call XLRSelectChannel() before calling XLRBindInputChannel().
   //
   for( channel=channel28; channel<=channel29; channel++)
   {
      printf ("Setting up channel %u for recording.\n", channel );
   
      if(XLRSelectChannel(hRecCard, channel) != XLR_SUCCESS)
      {
         printf( "Receiver select channel failed.\n" );
         exit(1);
      }
      if(XLRBindInputChannel(hRecCard, channel) != XLR_SUCCESS)
      {
         printf( "Receiver bind input channel failed.\n" );
         exit(1);
      }
      if(XLRSetDBMode(hRecCard, SS_SFPDPMODE_NORMAL, 
            SS_DBOPT_SFPDPNRASSERT| SS_DBOPT_SFPDP_CRC_DISABLE) != XLR_SUCCESS)
      {
         printf( "Receiver set DB mode failed.\n" );
         exit(1);
      }
   }

   // Start recording data.
   if(XLRRecord(hRecCard, 0, 1) != XLR_SUCCESS)
   {
      printf( "XLRRecord failed.\n" );
      exit(1);
   }
   else
   {
      printf( "Recording Started.\n" );
   }

   // Sleep for awhile, letting some data record.
   Sleep(5000);

   // Stop the recording.
   if( XLRStop(hRecCard) != XLR_SUCCESS)
   {
      printf ("Error:  XLRStop failed.\n");
      exit(1);
   }

   //
   // Display the amount of data recorded on each channel
   // loop through both of the SFPDP channels.
   //
   for( channel=channel28; channel<=channel29; channel++)
   {
      // You must first Select the channel number that you want to get 
      // the length of.
      if(XLRSelectChannel(hRecCard, channel) != XLR_SUCCESS)
      {
         printf( "Sender select channel failed.\n" );
         exit(1);
      }

      // Get length of recorded data by calling XLRGetLength().
      u64ChannelLength = XLRGetLength( hRecCard );
      sprintf(prtBuff, 
         "Data Length of Channel %u: %s.\n", channel, LONGLONGFMT);
      printf (prtBuff, u64ChannelLength);

   }
   
   printLastError = FALSE;
   exit( 0 );
}
/*
 *  NAME: cleanup
 *
 *  PARAMETERS: none
 *
 *  DESCRIPTION: Called by exit to close handles.
 *
 *  RETURNS: void
 *
 */
void cleanup(void)
{
   if (printLastError == TRUE) {
      PrintXLRError();
   }
   if (hRecCard != INVALID_SSHANDLE) {
      XLRStop (hRecCard);
      printf ("Closing StreamStor card.\n");
      XLRClose(hRecCard);
   }
}

/*
 * PrintXLRError
 *
 *  PARAMETERS: none
 *
 *  DESCRIPTION: Retrieves the last StreamStor error code and then prints
 *    out the corresponding error message.
 *
 *  RETURNS: void
 *
 */
void PrintXLRError() 
{
   char	errorMessage[XLR_ERROR_LENGTH];

   XLRGetErrorMessage(errorMessage, XLRGetLastError());
   printf ("XLR error:  %s\n", errorMessage);
}


