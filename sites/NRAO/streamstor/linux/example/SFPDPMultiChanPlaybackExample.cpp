/*******************************************************************
 *
 * Name;  SFPDPMultiChanPlaybackExample.cpp
 *    
 * This example is for Amazon boards with a serial FPDP daughter card.
 * It shows how to setup the StreamStor system to play data that was
 * recorded in multichannel mode.  In the example, data will be played
 * out Port 1 (Channel 28) and Port 2 (channel 29). 
 *
 * The example assumes there is some other device (or devices) that 
 * Port1 and Port2 are connected to at the other end of the optical 
 * fiber. 
 *
 *******************************************************************/
//$Id: //streamstor/example/SFPDPMultiChanPlaybackExample.cpp#4 $

/** include files **/
#include <iostream>
using namespace std;

#ifdef WIN32
#include <windows.h>
#endif

#include "xlrapi.h"

#ifndef WIN32
#include <unistd.h>  // for sleep
#define Sleep(x)     (sleep((unsigned int)(x/1000)))
#define LONGLONGFMT "%llu"
#else
#define LONGLONGFMT "%I64u"
#endif

#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

void PrintXLRError();

SSHANDLE hSendCard = INVALID_SSHANDLE;
BOOLEAN  printLastError = TRUE;

void cleanup();

#define BUFFER_SIZE     0x100000

int main(int argc, char *argv[])
{
   UINT64            dwLength = 0;
   UINT32            deviceToOpen = 0;
   UINT32            channelNum=0;
   char              prtBuff[256];
   
   atexit(cleanup);

   printf("This program demonstrates playback from two channels.\n");
   printf("Enter Device number to playback data from, i.e., 1,2,3...: \n");
   cin >> deviceToOpen;

   printf("Opening StreamStor Playback Device ...\n");
   if(XLROpen(deviceToOpen, &hSendCard) != XLR_SUCCESS)
   {
      printf("Error: XLROpen failed.\n");
      exit(1);
   }

   //
   // Set up StreamStor device for Multi-channel mode sending BOTH channels 
   // 28 and 29 over the Serial FPDP bus daughter card.
   //
   if (XLRSetMode(hSendCard, SS_MODE_MULTI_CHANNEL) != XLR_SUCCESS) {
      printf ("Error: Could not set mode to Multi Channel.\n");
      exit(1);
   }

   //
   // Call XLRClearChannels once before setting up the channels for playback.
   //
   if (XLRClearChannels(hSendCard) != XLR_SUCCESS) { 
      printf("Error: Clear channels failed.\n");
      exit(1);
   }
  
   //
   // Loop, configuring channel 28 and then channel 29 for playback.
   //
   for (channelNum = 28; channelNum < 30; channelNum++) {
      printf ("Setting up channel %u for output.\n", channelNum);

      // Always call XLRSelectChannel() before binding a channel.
      if (XLRSelectChannel(hSendCard, channelNum) != XLR_SUCCESS) {
         printf("Error: Select channel %u failed.\n", channelNum);
         exit(1);
      }

      if (XLRBindOutputChannel(hSendCard, channelNum) != XLR_SUCCESS) {
         printf("Error: bind output channel %u failed.\n", channelNum);
         exit(1);
      }

      if (XLRSetDBMode(hSendCard, SS_SFPDPMODE_NORMAL, 
        SS_DBOPT_SFPDPNRASSERT| SS_DBOPT_SFPDP_CRC_DISABLE) != XLR_SUCCESS) {
         printf("Error: XLRSetDBMode channel %u failed.\n", channelNum);
         exit(1);
      }

      //
      // Display how much data is recorded on this channel. Note that
      // if data was recorded in multi-channel mode, XLRGetLength will
      // report how much data was recorded on the currently selected 
      // channel.
      //
      dwLength = XLRGetLength(hSendCard);
      sprintf(prtBuff, 
         "Data on StreamStor Channel %u: %s.\n", channelNum, LONGLONGFMT);
      printf (prtBuff, dwLength);

      //
      // This program requires data to have been previously recorded
      // on channels 28 and 29.  Exit if there is no data recorded
      // on the selected channel.
      //
      if(dwLength == 0) {
         printf("Error!  No data recorded on channel %u.\n", channelNum);
         printf
          ("Program requires pre-existing data recorded on 28 and 29.\n");
         exit(1);
      }
   }

   //
   // Channels 28 and 29 are now configured for output and we have 
   // confirmed that data has been previously recorded on both of the
   // channels.  Calling XLRPlayback now will cause data from both 
   // channels to begin playing back.
   //
   printf("Playback starting ...\n");
   if (XLRPlayback(hSendCard, 0, 0) != XLR_SUCCESS) {
      printf("Error:  XLRPlayback failed.\n");
      exit(1);
   }
    
   // Sleep a bit, letting some data play.
   Sleep(5000);

   // Stop the playback.  This call will stop playback on both channels.
   if(XLRStop(hSendCard) != XLR_SUCCESS) {
      printf("Error:  XLRStop failed.\n");
   }

   printLastError = FALSE;
   printf ("Playback finished.\n");
   exit(0);
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
   if (hSendCard != INVALID_SSHANDLE) {
      printf ("Closing StreamStor card.\n");
      XLRClose(hSendCard);
   }
}

 
/*
 * PrintXLRError
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
   char	            errorMessage[XLR_ERROR_LENGTH];
   XLR_ERROR_CODE    code = XLRGetLastError();

   if (code != 3) {
      XLRGetErrorMessage(errorMessage, code);
      printf ("XLR error:  %s\n", errorMessage);
   }
}
 

