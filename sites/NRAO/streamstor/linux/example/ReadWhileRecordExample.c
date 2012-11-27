/*
 * Name: ReadWhileRecordExample.c
 *
 * Description:  This is an example program that shows how to set up
 * StreamStor to do a Read While Record operation.  
 *     The feature allows you to read a fixed amount of recorded data 
 * while continuing to record incoming data.
 *     This code assumes that there is another card sending the data
 * to this StreamStor card.
 *
 * For details on the feature, see the Conduant Applicaton Note AN501, 
 * "Amazon Read while Record." 
 *
 * Copyright 2007 Conduant Corporation.
 */ 

//$Id: //streamstor/example/ReadWhileRecordExample.c#2 $

#include <stdio.h>
#include <stdlib.h>     // for exit

#include "string.h"   // for memset
#include "xlrapi.h"

#define THRESHOLD       20
#define MEGABYTE        1048576
#define READBUFF_SZ_BYTES    MEGABYTE*2
#define NUM_BUFF_READS  50

#ifndef WIN32
#include <unistd.h>  // for sleep
#define Sleep(x)     (sleep((unsigned int)(x/1000)))
#endif

#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

void cleanup( void );

BOOLEAN  printXLRError = TRUE;
SSHANDLE hTarget;
void *   readBuffer;


int main(int argc, char *argv[])
{
   S_DEVSTATUS          recvDevStatus;
   S_READDESC           readDesc;

   UINT64               u64StartAddr   = 0;
   UINT64               u64BytesRead   = 0;
   UINT64               u64BytesAvailable = 0;
   UINT64               u64NewBytesAvailToRead = 0;

   UINT32               fpdpChannel = 30;
   UINT32               bytesPerXfer= 0;
   UINT32               buffCount   = 0;
   UINT32               k=0;
   char	               errorMessage[XLR_ERROR_LENGTH];

   BOOLEAN              filled      = FALSE;

   atexit(cleanup);     // cleanup() will be called when exit() is called
   hTarget = INVALID_SSHANDLE;
   readBuffer =NULL;

   printf ("ReadWhileRecordExample\n");
   if( XLROpen(1, &hTarget) != XLR_SUCCESS) {
      printf ("Error:  Cannot open StreamStor device 1\n");
      exit(1);
   }
 
   //
   // Allocate a buffer to hold the data that we will read.
   //
   readBuffer = calloc (READBUFF_SZ_BYTES, sizeof(char));
   if (readBuffer == NULL) {
      printXLRError = FALSE;
      printf ("Error: cannot allocate readbuffer.\n");
      exit(1);
   }

   if( XLRSetMode(hTarget, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS) {
      printf("Error: Set mode single channel failed.\n");
      exit(1);
   }

   if(XLRClearChannels(hTarget) != XLR_SUCCESS) {
      printf("Error: Clear channels failed.\n");
      exit(1);
   }

   if(XLRBindInputChannel(hTarget, fpdpChannel) != XLR_SUCCESS) {
      printf("Error: Binding of INPUT channel failed.\n");
      exit(1);
   }
   
   if(XLRBindOutputChannel(hTarget, 0) != XLR_SUCCESS) {
      printf ("Error: Binding of OUTPUT channel to 0 failed.\n"); 
      exit(1);
   }

   if (XLRSelectChannel (hTarget, fpdpChannel) != XLR_SUCCESS) {
      printf("Error: Selecting channel failed.\n");
      exit(1);
   }

   if (XLRSetDBMode(hTarget, SS_FPDPMODE_RECVM, SS_DBOPT_FPDPNRASSERT)
        != XLR_SUCCESS) {
      printf ("Error: Cannot set DBMode.\n" );
      exit(1);
   }
 
   //
   // The SS_OPT_FSMAPPED option turns on the Read While Record feature.
   //
   if (XLRSetOption(hTarget, SS_OPT_FSMAPPED) != XLR_SUCCESS) {
      printf("Error: Cannot set option to SS_OPT_FSMAPPED.\n");
      exit(1);
   }

   printf ("Read while record option - SS_OPT_FSMAPPED - set on Receiver.\n");

   // 
   // Start the Recording.
   //
   if (XLRRecord (hTarget, 0, 1) != XLR_SUCCESS) {
      printf ("Error: XLRRecord failed.\n");
      exit(1);
   }

   //
   // Loop to read the data. First loop waiting for the data to be recorded to
   // StreamStor before trying to read it. The data must be available
   // (recorded on the StreamStor) before you can read it. Each time an amount
   // of data that is larger than buffer size we are going to read has been 
   // recorded, call XLRRead to read it into the buffer. Repeat buffCount # of times 
   //
   bytesPerXfer = READBUFF_SZ_BYTES;
   u64StartAddr = 0;
   do {
      filled = FALSE;
      // 
      // Wait until we have enough data to fill our read buffer.
      // Call XLRGetLength to get the amount of recorded data available.
      //
      for (k = 0; k < THRESHOLD; k++) {
         //
         // Note that if the playback flag has gone off, the number of bytes
         // available to read, as reported by XLRGetLength, will be "stuck."
         //
         printf ("Waiting for read buffer to fill ...\n");
         u64BytesAvailable = XLRGetLength(hTarget); 
         u64NewBytesAvailToRead =  u64BytesAvailable - u64StartAddr;
         if (u64NewBytesAvailToRead < bytesPerXfer) 
         {
            Sleep(2000);
            continue;
         }
         filled = TRUE;
         break;
      }
      if (filled == FALSE) {
         printf ("Error:  Threshold exceeded - read buffer did not fill.\n");
         printXLRError = FALSE;
         exit(1);
      }

      //
      // We can start reading now.
      //
      printf ("Buffer has enough data - read it ...\n");

      readDesc.AddrHi = (UINT32)(u64StartAddr >> 32);
      readDesc.AddrLo = (UINT32)(u64StartAddr & 0xFFFFFFFF);
      readDesc.XferLength = bytesPerXfer;
      readDesc.BufferAddr = (PUINT32)readBuffer;
      memset(readBuffer, (UCHAR)0xFF, READBUFF_SZ_BYTES);
      if (XLRRead(hTarget, &readDesc) != XLR_SUCCESS) {
         printf ("Error - XLRRead failed.\n");
         XLRGetErrorMessage(errorMessage, XLRGetLastError());
         printf ("Error Message from XLRRead:  %s\n", errorMessage);
         if (XLRGetDeviceStatus(hTarget, &recvDevStatus) != XLR_SUCCESS) {
            printf ("Error:  Cannot get device status on receiver.\n");
            exit(1);
         }
         else {
            if (recvDevStatus.DriveFail == TRUE) {
               // A drive has failed.  Print out which one.
               printf 
                  ("Drive %u has failed.\n", recvDevStatus.DriveFailNumber);
            }
         }
         printXLRError = FALSE;
         exit(1);
      }
      u64BytesRead = u64BytesRead + readDesc.XferLength;

      printf ("Read buffer [%u] is ok.\n", buffCount);
      buffCount++;
      u64StartAddr = u64StartAddr + bytesPerXfer;

      if ( buffCount > NUM_BUFF_READS) {
         printf ("Have read %d buffers OK - stopping now.\n", NUM_BUFF_READS);
         break;
      }
      if (XLRGetDeviceStatus(hTarget, &recvDevStatus) != XLR_SUCCESS) {
         printf ("Error:  Cannot get device status on receiver.\n");
         exit(1);
      }
   } while (recvDevStatus.Recording == TRUE);

   printf ("Stopping ...\n");
   if (XLRStop(hTarget) != XLR_SUCCESS) {
      printf ("Error! Stop failed on receiver.\n"); 
      exit(1);
   }

   printXLRError = FALSE;
   if (u64BytesRead == 0) {
      printf ("Error: Did not read any Data.\n");
   }
   else {
      printf ("ReadWhileRecordExample completed successfully.\n");
   }
   exit(0);
}

void dumpBuffer(PUINT32 readBuffer, UINT32 readBuff_SZ_Words, 
      UINT32 wordStart, UINT32 numWords)
{
   PUINT32   ulongBufPtr;
   UINT32    wordCount=0;
   UINT32    wordPrint=0;

   //
   // Print out a window of the buffer, starting at word wordStart.
   // Print out numWords.
   //
   ulongBufPtr = readBuffer;

   for (wordCount = 0; wordCount < readBuff_SZ_Words; wordCount++) {
      if (wordStart == wordCount) {
         for (wordPrint = 0; wordPrint < numWords; wordPrint++) {
            printf ("Word[%u] = %u\n", wordCount, *ulongBufPtr);
            ulongBufPtr++;
         }
         break;
      }
   }
}

/*
 ** cleanup
 *
 *  DESCRIPTION: Called by exit to get error message, close handles, free mem, etc.
 *
 *  RETURNS: void
 *
 */
void cleanup( void )
{
   char	errorMessage[XLR_ERROR_LENGTH];

   if (printXLRError == TRUE) {
      XLRGetErrorMessage(errorMessage, XLRGetLastError());
      printf ("XLR Error Message:  %s\n", errorMessage);
   }

   if (readBuffer != NULL) {
      free(readBuffer);
   }

   if (hTarget != INVALID_SSHANDLE) {
      XLRStop(hTarget);
      XLRClose(hTarget);
   }
}
