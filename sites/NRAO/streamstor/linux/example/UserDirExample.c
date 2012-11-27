//$Id: //streamstor/example/UserDirExample.c#5 $

/*---------------------------------------------------------------------------
 *    IMPORTANT -- This example will erase any existing data!
 * --------------------------------------------------------------------------
 * This example shows how you can create a simple catalog of recordings
 * on the StreamStor by making use of the "user directory" area. In practice, 
 * the catalog you create will likely be more complex (for instance, yours may 
 * include a date stamp, etc.).  The basics are presented here.
 *
 * (An alternative technique is to use partitions, creating a new 
 * partition for every recording.)
 *
 * This example can be built with SDK 7.x or newer.
 * 
 * To simplify the example, error checking is not shown.
 *
 * In the example, four "recordings" are created using FPDP.  For each 
 * recording, an entry is created in the user directory that has this 
 * information:
 *  -- a "record number" 
 *  -- the logical offset into the device where the recording begins   
 *  -- the length of the recording (in bytes) 
 *
 * Once all four records have been written, the user directory will look 
 * like this:
 * 
 *    |--------------------------------------|
 *    |record number                         |
 *    |logical address where record 0 starts |
 *    |length of record 0.                   |
 *    |--------------------------------------|
 *    |record number                         |
 *    |logical address where record 1 starts |
 *    |length of record 1.                   |
 *    |--------------------------------------|
 *    |record number                         |
 *    |logical address where record 2 starts |
 *    |length of record 2.                   |
 *    |--------------------------------------|
 *    |record number                         |
 *    |logical address where record 3 starts |
 *    |length of record 3.                   |
 *    |--------------------------------------|
 * 
 *--------------------------------------------------------------------------*/

#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include "xlrapi.h" 

#ifdef WIN32
#include <dos.h>     // For Windows sleep
#define LONGLONGFMT "%I64u"
#else
#include <unistd.h>  // For Linux sleep
#define Sleep(x)     (sleep((unsigned int)(x/1000)))
#define LONGLONGFMT "%llu"
#endif

#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

#define TOP_PORT     30

//
// This structure will be written to the user directory for each recording 
// that is created.
//
typedef struct _RECORDINGTAG
{
   UINT64   recNumber;      // Unique record number for this record.
   
   UINT64   acqDataStart;   // Offset into a record where acquisition data 
                            // starts for this record. 

   UINT64   length;         // Number of bytes in this recording.
} RECORDINGTAG;

#define RECTAGSZ sizeof(RECORDINGTAG)

void writeRecord (SSHANDLE XlrDevice, UINT32 seconds, UINT64 * u64DevOffset, 
      UINT64 * u64RecLength);
void updateUserDir(SSHANDLE XlrDevice, UINT64 u64DevOffset, 
      UINT64 u64RecLength);
BOOLEAN getRecordInfo (SSHANDLE XlrDevice, UINT64 u64RequestedNumber, 
      RECORDINGTAG *recTagPtr); 
BOOLEAN setupReceiver(SSHANDLE receiver);
void PrintXLRError();

int main(int argc, char *argv[])
{
   SSHANDLE       XlrDevice;
   RECORDINGTAG   recTag = {0,0};
   UINT64         u64DevOffset = 0;
   UINT64         u64RecLength = 0;
   UINT64         count = 0;
   BOOLEAN        found = FALSE;
   BOOLEAN        rc = FALSE;
   BOOLEAN        keepGoing=TRUE;
   char           prtBuf[256];
   char           inputChar;

   printf("***** WARNING!!! *****\n");
   printf("This example will erase data on your device.\n");
   printf("Do you wish to continue [y/n]: ");
   inputChar = '\0';
   inputChar = getchar();
   if( ( inputChar != 'y' ) && ( inputChar != 'Y' ) )
   {
      printf("Ok, exiting now.\n");
      exit(0);
   }

   // Throw away carriage return.
   getchar();

   if (XLROpen(1, &XlrDevice) != XLR_SUCCESS) { 
      printf ("Error:  Cannot open StreamStor device 1.\n");
      PrintXLRError();
      exit(1);
   }

   // Create a zero length user directory.
   if (XLRSetUserDir(XlrDevice, (void *)&recTag, 0) != XLR_SUCCESS) {
      printf ("Error:  Set user dir failed.\n");
      PrintXLRError();
      XLRClose(XlrDevice);
      exit(1);
   }

   // Erase any existing data.
   if (XLRErase(XlrDevice, SS_OVERWRITE_NONE) != XLR_SUCCESS) {
      printf ("Error:  Could not erase device.\n");
      PrintXLRError();
      XLRClose(XlrDevice);
      exit(1);
   }

   // Set up the StreamStor to receive data over FPDP.
   rc = setupReceiver (XlrDevice);
   if (rc == FALSE) {
      printf ("Error: Could not set up receiver\n");
      PrintXLRError();
      XLRClose(XlrDevice);
      exit(1);
   }

   while (keepGoing == TRUE) {
      inputChar = '\0';
      printf ("Ready to start transmitting data?\n");
      printf ("g = go\n");
      printf ("q = quit\n");
      inputChar = getchar();
   
      if (inputChar == 'q') {
         keepGoing = FALSE;
         break;
      }
      else {
         printf ("Start transmitting data on channel %d.\n", TOP_PORT);
         break;
      }
   }

   if (keepGoing == FALSE) {
      XLRClose(XlrDevice);
      exit (1);
   }

   //
   // Write a recording, then update the user directory with information about 
   // each recording.  We will write four records.
   //
   writeRecord(XlrDevice, 4, &u64DevOffset, &u64RecLength);
   updateUserDir(XlrDevice, u64DevOffset, u64RecLength);

   writeRecord(XlrDevice, 2, &u64DevOffset, &u64RecLength);
   updateUserDir(XlrDevice, u64DevOffset, u64RecLength);

   writeRecord(XlrDevice, 5, &u64DevOffset, &u64RecLength);
   updateUserDir(XlrDevice, u64DevOffset, u64RecLength);

   writeRecord(XlrDevice, 3, &u64DevOffset, &u64RecLength);
   updateUserDir(XlrDevice, u64DevOffset, u64RecLength);

   // 
   // Call getRecordInfo for each record, retrieving its information and
   // printing it out.
   //
   for (count=0; count < 4; count++) {
      found = getRecordInfo (XlrDevice, count, &recTag);
      if (found == TRUE) {
         sprintf (prtBuf, "Record %s starts at offset %s", LONGLONGFMT, 
               LONGLONGFMT);
         printf (prtBuf, recTag.recNumber, recTag.acqDataStart);
         sprintf (prtBuf, "\tand is %s bytes long.\n", LONGLONGFMT);
         printf (prtBuf, recTag.length);
      }
      else {
         sprintf (prtBuf, "Could not find record number %s.\n", LONGLONGFMT);
         printf (prtBuf, count);
      }
   }

   XLRClose (XlrDevice);
}

/*
 * updateUserDir will update the user directory with information about a
 * new recording.
 */
void updateUserDir(SSHANDLE XlrDevice, UINT64 u64DevOffset, 
      UINT64 u64RecLength)
{
   UINT32            uDirLength = 0;
   UINT32            totalNumRecs = 0;
   UINT32            recCount = 0;
   UINT32            newUdirBuffSize = 0;
   RECORDINGTAG *    udirBuffPtr;
   RECORDINGTAG *    udirOrigBuffPtr;
   RECORDINGTAG      newRecTag={0,0};

   //
   // The structure newRecTag will contain the information that is to be 
   // written to the user directory.
   //
   newRecTag.acqDataStart = u64DevOffset;
   newRecTag.length = u64RecLength;

   // Calculate this record's number.
   uDirLength = XLRGetUserDirLength(XlrDevice);
   totalNumRecs = uDirLength/RECTAGSZ;
   newRecTag.recNumber = totalNumRecs;

   if (uDirLength == 0) {
      // Update the user directory with the information about the first record.
      if (XLRSetUserDir(XlrDevice, (void *)&newRecTag,RECTAGSZ)!= XLR_SUCCESS) {
         printf ("Error: Could not create first record.\n");
         PrintXLRError();
      }
   }
   else {
      //
      // Need to append this record's information to the existing user 
      // directory.  Read the entire user directory into memory, append the 
      // information about the latest recording to it, then write the 
      // user directory back out.
      //
      newUdirBuffSize = RECTAGSZ * (totalNumRecs+1);
      udirBuffPtr = (RECORDINGTAG *) calloc(totalNumRecs+1, RECTAGSZ);
      udirOrigBuffPtr = udirBuffPtr;

      // Get the existing user directory.
      if (XLRGetUserDir (XlrDevice, uDirLength, 0,(void *)udirBuffPtr) 
            != XLR_SUCCESS) {
         printf ("Error:  could not get user directory.\n");
         PrintXLRError();
      }
      else {
         // Position ourselves to the end of the user directory buffer.
         for (recCount = 0; recCount < totalNumRecs; recCount++) {
            udirBuffPtr++;   
         }
      
         //
         // Append the information about the new recording, then write  
         // the user directory out.
         //
         memcpy ((void *)udirBuffPtr, &newRecTag, RECTAGSZ);
         if (XLRSetUserDir (XlrDevice, (void *) udirOrigBuffPtr, 
                  newUdirBuffSize) != XLR_SUCCESS) {
            printf ("Error:  Could not update user directory.\n");
            PrintXLRError();
         }
      }
      free (udirOrigBuffPtr);
   }
}

/*
 * getRecordInfo will find look up the information about a specific 
 * record number and return that information to the caller.
 */
BOOLEAN getRecordInfo (SSHANDLE XlrDevice, UINT64 u64RequestedNumber, 
      RECORDINGTAG *recTagPtr) 
{
   RECORDINGTAG   sampleInfo = {0,0};
   UINT64         totalNumRecs=0;
   UINT64         count=0;
   UINT32         uDirLength=0;
   UINT32         uDirOffset=0;
   BOOLEAN        found=FALSE;

   uDirLength = XLRGetUserDirLength(XlrDevice);
   if (uDirLength == 0) {
      printf ("Cannot get record info - no user directory.\n");
      return (FALSE);
   }

   totalNumRecs = uDirLength/RECTAGSZ;
   if (totalNumRecs == 0) {
      printf ("User Directory is empty.\n");
      return (FALSE);
   }
   else {
      if (u64RequestedNumber > (totalNumRecs-1)) {
         return (FALSE);
      }
   }

   uDirOffset = 0;  
   for (count = 0; count < totalNumRecs; count++) {
      if (XLRGetUserDir (XlrDevice, RECTAGSZ, uDirOffset, 
               (void *)(&sampleInfo)) != XLR_SUCCESS) {
         printf ("Error:  Could not get record info.  GetUserDir failed.\n");
         PrintXLRError();
         break;
      }
      else {
         if (sampleInfo.recNumber == u64RequestedNumber) {
            recTagPtr->recNumber = sampleInfo.recNumber;
            recTagPtr->acqDataStart = sampleInfo.acqDataStart;
            recTagPtr->length = sampleInfo.length;
            found = TRUE;
            break;
         }
         else {
            uDirOffset = uDirOffset + RECTAGSZ;
         }
      }
   }
   return (found);
}

/*
 * writeRecord will put the StreamStor in append mode and record some data.
 * It will return:
 *   -- u64DevOffset - the offset into the device where the recording started.
 *   -- u64RecLength - the lenght of data, in bytes, of the recording.
 */

void writeRecord (SSHANDLE XlrDevice, UINT32 seconds, UINT64 * u64DevOffset, 
      UINT64 * u64RecLength)
{
   S_DIR dir;

   //
   // Determine the logical offset on the device where the new recording 
   // will begin.
   //
   if (XLRGetDirectory(XlrDevice, &dir) != XLR_SUCCESS) {
      printf ("Error:  could not get directory length.\n");
      PrintXLRError();
   }
   else {
      *u64DevOffset = dir.Length;

      //
      // Put the StreamStor in append mode, so new data will be appended to
      // existing data. It is assumed here that data is streaming in from
      // FPDP from, for example, a data acquisition card or another StreamStor.
      //

      if (XLRAppend(XlrDevice) != XLR_SUCCESS) {
         printf ("Error:  Append failed.\n");
         PrintXLRError();
      }
      else {
         //
         // For purposes of this example, we will sleep to give the data some
         // time to record.  
         //
         Sleep (10000);
         XLRStop(XlrDevice);
   
         // Determine how much data was recorded and return that length.
         if (XLRGetDirectory(XlrDevice, &dir) != XLR_SUCCESS) {
            printf ("Error:  Cannot determine data length.\n");
            PrintXLRError();
         }
         else {
            *u64RecLength = dir.AppendLength;
         }
      }
   }
   return;
}

/*
 * setupReceiver will set up the channel for recording.
 */
BOOLEAN setupReceiver(SSHANDLE receiver) 
{
   //
   // Single channel is the default mode, so the call to XLRSetMode is not 
   // necessary here.  However, it is shown here to emphasize that the 
   // StreamStor will be in single channel mode.
   //
   if (XLRSetMode(receiver, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS) {
      printf ("Error:  Cannot set to single channel mode.\n");
      PrintXLRError();
      return (FALSE);
   }
   if (XLRClearChannels(receiver) != XLR_SUCCESS) {
      printf ("Error:  Cannot clear channels.\n");
      PrintXLRError();
      return (FALSE);
   }
   
   //
   // Prior to setting an FPDP mode, you must select the channel to which
   // the FPDP settings will apply.
   //
   if (XLRSelectChannel (receiver, TOP_PORT) != XLR_SUCCESS) {
      printf ("Error:  Cannot Select TOP_PORT.\n");
      PrintXLRError();
      return (FALSE);
   }

   if (XLRBindInputChannel(receiver, TOP_PORT) != XLR_SUCCESS) {
      printf ("Error:  Cannot bind channel TOP_PORT to input.\n");
      PrintXLRError();
      return (FALSE);
   }

   if (XLRSetFPDPMode(receiver, SS_FPDP_RECVMASTER, SS_OPT_FPDPNRASSERT) 
         != XLR_SUCCESS) {
      printf ("Error:  Cannot set FPDP mode on the receiver.\n");
      PrintXLRError();
      return (FALSE);
   }

   return (TRUE);
}

void PrintXLRError()
{
   char  errorMessage[XLR_ERROR_LENGTH];
   XLRGetErrorMessage(errorMessage, XLRGetLastError());
   printf("\tError Message: %s\n", errorMessage);
}
                                                                                

