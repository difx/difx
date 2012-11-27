/************************************************************************
 *
 * Name: SFPDPFourChanRecordExample.cpp
 *
 * This example is for Amazon boards with a serial FPDP daughter card.
 * It shows how to setup the StreamStor system to record on  four 
 * channels using SFPDP Ports 1,2,3, and 4.
 *
 * The example assumes you have data coming from some other device 
 * or devices over the SFPDP buses to the StreamStor device and 
 * are connected to SFPDP Ports 1,2,3,and 4. 
 ************************************************************************/

//$Id: //streamstor/example/SFPDPFourChanRecordExample.cpp#6 $

/** include files **/
#include <iostream>
using namespace std;
#include <stdio.h>

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

SSHANDLE    hRecCard = INVALID_SSHANDLE;
BOOLEAN     printLastError = TRUE;

void cleanup(void);

int main(int argc, char *argv[])
{
   UINT32            deviceToOpen;
   UINT32            channel;
   
   atexit(cleanup);

   printf
     ("This program demonstrates SFPDP recording on four channels (28,29,30,31).\n");
   printf("Enter Device number to Record to 1,2,3...: ");
   cin >> deviceToOpen;

   printf("Opening StreamStor Recording Device ...\n");
   if(XLROpen(deviceToOpen, &hRecCard) != XLR_SUCCESS)
   {
      printf("Open failed.\n");
      exit(1);
   }

   //
   // The order of the following StreamStor API calls is very important.
   //
   
   // Set StreamStor Mode to Multi-Channel.
   if (XLRSetMode(hRecCard, SS_MODE_MULTI_CHANNEL) != XLR_SUCCESS) {
      printf ("Could not set mode to Multi Channel.\n");
      exit(1);
   }

   // Call XLRClearChannels once before setting up the 4 channels.
   if (XLRClearChannels(hRecCard) != XLR_SUCCESS) {
      printf("Receiver clear channels failed.\n");
      exit(1);
   }

   // Set up all 4 channels for recording.
   for(channel=28; channel<=31; channel++)
   {
      // Always call XLRSelectChannel() before binding the channel.
      if (XLRSelectChannel(hRecCard, channel) != XLR_SUCCESS) {
         printf("Receiver select channel failed.\n");
         exit(1);
      }
      if (XLRBindInputChannel(hRecCard, channel) != XLR_SUCCESS) { 
         printf("Receiver bind input channel failed.\n");
         exit(1);
      }
      if (XLRSetDBMode(hRecCard, SS_SFPDPMODE_NORMAL, 
          SS_DBOPT_SFPDPNRASSERT | SS_DBOPT_SFPDP_CRC_DISABLE)!=XLR_SUCCESS) {
         printf("Receiver set DB mode failed.\n");
         exit(1);
      }
   }
   
   //Start recording data.
   if (XLRRecord(hRecCard, 0, 1) != XLR_SUCCESS) {
      printf("Record failed.\n");
      exit(1);
   }
   else
   {
      printf("Recording Started.\n");
   }

   // Record for a while ...
   Sleep(5000);

   // Stop the recording on all channels.
   if(XLRStop(hRecCard) != XLR_SUCCESS) {
      printf ("Error:  Stop failed.\n");
      exit(1);
   }

   printf("Recording finished.\n");
   exit(0);
}


/*
 ** cleanup
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
      printf ("Closing StreamStor card.\n");
      XLRClose(hRecCard);
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
   }
   printf ("XLR error:  %s\n", errorMessage);
}
 
