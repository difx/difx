/*******************************************************************
 *
 * Name: SFPDPFourChanPlaybackExample.cpp
 *
 * This example is for Amazon boards with a SFPDP daughter board.
 * It shows how to set up the StreamStor card to playback a four channel 
 * recording using SFPDP Ports 1,2,3, and 4.
 *
 * It plays data from a multichannel recording currently on a StreamStor 
 * device sending the data out Port 1 (Channel 28), Port 2 (channel 29), 
 * Port 3 (channel 30), and Port 4 (channel 31). 
 *
 * The example assumes there is some other device (or devices) that 
 * Port1, Port2, Port3, and Port4 are connected to at the other end 
 * of the optical fibers. 
 *
 *******************************************************************/

//$Id: //streamstor/example/SFPDPFourChanPlaybackExample.cpp#5 $

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

#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

#include "xlrapi.h"

void PrintXLRError();

SSHANDLE    hSendCard = INVALID_SSHANDLE;
BOOLEAN     printLastError = TRUE;        

void cleanup(void);
void errorExit(void);

int main(int argc, char *argv[])
{
   UINT64   u64Length=0;

   UINT32   deviceToOpen=0;
   UINT32   channel=0;

   char     prtBuff[256];
   
   atexit(cleanup);

   printf
    ("This program will set up four channels (28,29,30,31) for playback.\n");
   printf("Enter the number of the StreamStor card to playback from: \n");
   cin >> deviceToOpen;

   printf("Opening StreamStor Playback Device ...\n");
   if(XLROpen(deviceToOpen, &hSendCard) != XLR_SUCCESS)
   {
      printf("Error: XLROpen failed.\n");
      exit(1);
   }

   u64Length = XLRGetLength(hSendCard);
   sprintf (prtBuff, "Data on StreamStor = %s bytes\n", LONGLONGFMT);
   printf (prtBuff, u64Length);
   if(u64Length == 0) {
      printf("Error: StreamStor is empty - no data to playback.\n");
      exit(1);
   }

   // The order of the following calls to StreamStor API functions is 
   // very important.

   // Set the StreamStor Mode to Multi-Channel.
   if (XLRSetMode(hSendCard, SS_MODE_MULTI_CHANNEL) != XLR_SUCCESS) 
   {
      printf ("Error: Could not set mode to Multi Channel.\n");
      exit(1);
   }

   // Call XLRClearChannels once before setting up the 4 channels.
   if (XLRClearChannels(hSendCard) != XLR_SUCCESS) 
   {
      printf("Error: Receiver clear channels failed.\n");
      exit(1);
   }

   //Setup all 4 channels for playback.
   for(channel=28; channel<=31; channel++) 
   {
      // Always call XLRSelectChannel() before binding the channel.
      if (XLRSelectChannel(hSendCard, channel) != XLR_SUCCESS) 
      {
         printf("Error: Receiver select channel failed.\n");
         exit(1);
      }

      if (XLRBindOutputChannel(hSendCard, channel) != XLR_SUCCESS) 
      {
         printf("Error: Binding sender to output channel %u failed.\n", 
               channel);
         exit(1);
      }

      // 
      // To configure a channel, call XLRSetDBMode. As shown, you must
      // select and bind the channel before configuring it.
      //
      if (XLRSetDBMode(hSendCard, SS_SFPDPMODE_NORMAL, 
        SS_DBOPT_SFPDPNRASSERT| SS_DBOPT_SFPDP_CRC_DISABLE) != XLR_SUCCESS) 
      {
         printf("Error: Setting DB mode on sender failed.\n");
         exit(1);
      }
   }
 
   printf("Begin data playback ...\n");
   if (XLRPlayback(hSendCard, 0, 0) != XLR_SUCCESS) 
   {
      printf("Error: XLRPlayback error.\n");
      exit(1);
   }
 
   // Sleep for a while, allowing data to playback.
   Sleep(5000);
   
   // Stop the playback.  This call will stop playback on all 4 channels.
   if(XLRStop(hSendCard) != XLR_SUCCESS) 
   {
      printf ("Error: Stop failed.\n");
      exit(1);
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
 *  DESCRIPTION: Called by exit to close handle and print error messages.
 *
 *  RETURNS: void
 *
 */
void cleanup()
{

   if (printLastError == TRUE) 
   {
      PrintXLRError();
   }
   if (hSendCard != INVALID_SSHANDLE) 
   {
      printf ("Closing StreamStor card.\n");
      XLRClose(hSendCard);
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
   char	            errorMessage[XLR_ERROR_LENGTH];
   XLR_ERROR_CODE    code = XLRGetLastError();

   if (code != 3)
   {
      XLRGetErrorMessage(errorMessage, code);
      printf ("Error Message:  %s\n", errorMessage);
   }
}
 

