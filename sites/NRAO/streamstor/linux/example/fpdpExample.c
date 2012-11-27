/* 
 * The following C code shows how to set up the top FPDP connector as 
 * an input channel to record, then read the data back through the 
 * PCI Bus Channel 0. 
 */

//$Id: //streamstor/example/fpdpExample.c#5 $

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
   S_DEVINFO            sDevInfo;
   UINT64               u64StartAddress=0;
   UINT64               u64LengthSnapShot=0;
   PUINT32              pBuf = NULL;
   XLR_RETURN_CODE      xlrStatus;
   UINT32               k=0;
   char                 prtBuf[256];
   int                  inputChar;

   printf("***** WARNING!!! *****\n");
   printf("This example will erase data on your device.\n");
   printf("Do you wish to continue [y/n]: ");
   inputChar = getchar();
   if( ( inputChar != 'y' ) && ( inputChar != 'Y' ) )
   {
      printf("Ok, exiting now.\n");
      exit(0);
   }

   if (XLROpen(1, &hTarget) != XLR_SUCCESS) {
      printf ("Cannot open StreamStor card.\n");
      PrintXLRError();
      exit(1);
   } 

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

   // Bind the top port (channel 30) as the input channel.
   if (XLRBindInputChannel(hTarget, 30) != XLR_SUCCESS) {
      printf ("Cannot bind top port as input channel.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select and set FPDP options on TOP port.
   if (XLRSelectChannel(hTarget, 30) != XLR_SUCCESS) {
      printf("Cannot select channel 30\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   if (XLRGetDeviceInfo(hTarget, &sDevInfo) != XLR_SUCCESS) {
      printf("Cannot get device information.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   if (strncmp(sDevInfo.BoardType, "AMAZON", 6) == 0) {
      xlrStatus = XLRSetDBMode(hTarget, SS_FPDPMODE_RECVM, 0);
   }
   else {
      xlrStatus = XLRSetDBMode(hTarget, SS_FPDP_RECVMASTER, 0);
   }


   if (xlrStatus != XLR_SUCCESS) {
      printf ("Cannot set DB mode.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

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
      u64LengthSnapShot=XLRGetLength(hTarget);
      sprintf (prtBuf, "Length is now = %s\n", LONGLONGFMT);
      printf (prtBuf, u64LengthSnapShot); 
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

   if (sDir.Length == 0) {
      printf("No data was recorded, so nothing to read.\n");
      exit(1);
   }

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

   // Bind PCI Bus channel 0 as output channel.
   if (XLRBindOutputChannel(hTarget, 0) != XLR_SUCCESS) {
      printf ("Cannot bind output channel to PCI\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   // Select Channel to read - channel 0.
   if (XLRSelectChannel(hTarget, 0) != XLR_SUCCESS) {
      printf ("Cannot channel 0.\n");
      PrintXLRError();
      XLRClose(hTarget);
      exit(1);
   }

   pBuf = (PUINT32)malloc((UINT32)BUFFER_SIZE);
   u64StartAddress = 4096;

   // Build the read descriptor.
   sRead.AddrHi = (UINT32)(u64StartAddress >> 32);
   sRead.AddrLo = (UINT32)(u64StartAddress & 0xFFFFFFFF);
   sRead.BufferAddr = pBuf;
   sRead.XferLength = (UINT32)BUFFER_SIZE;

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


