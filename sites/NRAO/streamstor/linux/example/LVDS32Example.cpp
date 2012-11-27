/* 
 * The following C code shows how to set up LVDS16 and LVDS32 channel 30 as 
 * an input channel to record, then read the data back through the 
 * PCI Bus Channel 0. 
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
#define Sleep(x)		(sleep((UINT)(x/1000)))
#endif

/** local definitions **/
#define BUFFER_SIZE     131072
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define START_ADDRESS         0x100000

void PrintXLRError();

int main(int argc, char *argv[])
{
   SSHANDLE             hTarget;
   S_READDESC           sRead;
   S_DIR                sDir;
   DWORDLONG            startAddress=0;
   DWORDLONG            lengthSnapShot=0;
   PUINT32               pBuf = NULL;
   UINT                 k;
   char                 prtBuf[256];

   if( XLROpen(1, &hTarget) != XLR_SUCCESS) 
   {
      printf ("Cannot open StreamStor card.\n");
      PrintXLRError();
      exit(1);
   } 

   if( XLRSetMode(hTarget, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS) 
   {
      printf ("Cannot set single channel mode\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   if( XLRClearChannels(hTarget) != XLR_SUCCESS) 
   {
      printf ("Cannot clear channels\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select and Bind the (channel 30) as the input channel.
   if( XLRSelectChannel(hTarget, 30) != XLR_SUCCESS)
   {
      printf("Receiver select channel failed.");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   if( XLRBindInputChannel(hTarget, 30) != XLR_SUCCESS)
   {
      printf("Receiver bind input channel failed.");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   if( XLRSetDBMode(hTarget, SS_LVDS16MODE_RECV, SS_DBOPT_LVDS16_FLOWCONTROL) != XLR_SUCCESS)
   {
      printf("Receiver set DB mode failed.");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   //Note: on LVDS16 and LVDS32 board don't set up the clock speed for recording

   // Start recording.
   if (XLRRecord(hTarget, FALSE, TRUE) != XLR_SUCCESS) {
      printf("Cannot start recording.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }
   printf("Recording...\n");

   // Sleep a while, letting some data record.
   for (k=0; k< 5; k++) {
      lengthSnapShot=XLRGetLength(hTarget);
      sprintf (prtBuf, "Length is now = %s\n", LONGLONGFMT);
      printf (prtBuf, lengthSnapShot); 
      Sleep(5000);
   }
   printf("Recording done!\n");

   XLRStop(hTarget);

   if (XLRGetDirectory(hTarget, &sDir) != XLR_SUCCESS) {
      printf("Cannot get directory info.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
  }

   sprintf (prtBuf, "Final length recorded = %s bytes.\n", LONGLONGFMT);
   printf (prtBuf, sDir.Length);

   // Set up to read the data we just recorded.
   if (XLRSetMode(hTarget, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS) {
      printf ("Cannot set single channel mode\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   if (XLRClearChannels(hTarget) != XLR_SUCCESS) {
      printf ("Cannot clear channels\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select Channel to read - channel 0.
   if (XLRSelectChannel(hTarget, 30) != XLR_SUCCESS) {
      printf ("Cannot channel 0.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Bind PCI Bus channel 0 as output channel.
   if (XLRBindOutputChannel(hTarget, 0) != XLR_SUCCESS) {
      printf ("Cannot bind output channel to PCI\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }


   pBuf = (PUINT32)malloc(BUFFER_SIZE);
   startAddress = 4096;

   // Build the read descriptor.
   sRead.AddrHi = (UINT)(startAddress >> 32);
   sRead.AddrLo = (UINT)(startAddress & 0xFFFFFFFF);
   sRead.BufferAddr = pBuf;
   sRead.XferLength = BUFFER_SIZE;

   // Read a buffer.
   if (XLRRead(hTarget, &sRead) != XLR_SUCCESS) {
      printf ("Cannot read.\n");
      PrintXLRError();
      XLRClose(hTarget);
      free(pBuf);
      exit(1);
   }

   printf("Read Complete.\n");

   XLRClose(hTarget);
   free(pBuf);
   pBuf = NULL;
   exit(0);
}

void PrintXLRError()
{
   char  errorMessage[XLR_ERROR_LENGTH];
   XLRGetErrorMessage(errorMessage, XLRGetLastError());
   printf("\tError Message: %s\n", errorMessage);
}


