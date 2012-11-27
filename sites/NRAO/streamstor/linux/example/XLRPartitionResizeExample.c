 /*  
 *  Name:  XLRPartitionResize.c
 * 
 *  Purpose:  Demonstrates resizing a partition.  Note that XLRPartitionResize
 *  is supported only on the Amazon boardtype.
 *
 *  Note that this code will erase all partitions and data on the StreamStor.
 */

//$Id: //streamstor/example/XLRPartitionResizeExample.c#4 $

#include <stdio.h>
#include "xlrapi.h"

#ifdef WIN32
#define LONGLONGFMT "%I64u"
#else
#define LONGLONGFMT "%llu"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#endif

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define LENGTH_TO_TRANSFER  0x20000000
#define READ_BUFFER_SIZE    0x10000


SSHANDLE    gl_XlrDevice = INVALID_SSHANDLE;
int         gl_ExitStatus = EXIT_FAILURE;

void PrintXLRError();
void PrintPartitionInfo(S_PARTITIONINFO sPartition);
void cleanup(void);
BOOLEAN CreateTestData();
BOOLEAN BindInputForPCI();

int main(int argc, char *argv[])
{
   S_PARTITIONINFO     sPartition;
   S_DEVINFO           devInfo;

   UINT64              dwPartSizeBytes=0;
   UINT64              dwOverheadBytes=0;

   UINT32              numCards=0;
   UINT32              partCount=0;
   UINT32              lastPartitionNumber=0;
   UINT32              numPartitionsToMake=2;

   int                 inputChar;
   char                prtBuf[256];

   atexit(cleanup);
   printf("Partition Resize Example\n");

   //
   // Erase any existing partitions and data.
   //
   printf("***** WARNING!!! *****\n");
   printf("This example will erase all data and partitions for StreamStor card 1.\n");
   printf("Do you wish to continue [y/n]: ");
   inputChar = getchar();

   if((inputChar != 'y') && (inputChar != 'Y')) 
   {
      printf("Ok, no erase done, exiting now.\n");
      gl_XlrDevice = INVALID_SSHANDLE;
      gl_ExitStatus = EXIT_SUCCESS;
      exit(gl_ExitStatus);
   }

   numCards = XLRDeviceFind();
   if (numCards == 0) 
   {
      printf ("Error:  No StreamStor cards detected.\n");
      exit(EXIT_FAILURE);
   }

   if (XLROpen(1, &gl_XlrDevice) != XLR_SUCCESS) 
   {
      printf ("\t\tERROR:  Cannot open StreamStor card 1.\n");
      exit(EXIT_FAILURE);
   }


   printf("Erasing partitions ...\n");
   if(XLRErase(gl_XlrDevice, SS_OVERWRITE_PARTITION) != XLR_SUCCESS)
   {
      printf("\t\tERROR: Erase of partitions failed.\n");
      exit(EXIT_FAILURE);
   }

   if (XLRGetDeviceInfo (gl_XlrDevice, &devInfo) != XLR_SUCCESS) 
   {
      printf("\t\tERROR: getDeviceInfo failed.\n");
      exit(EXIT_FAILURE);
   }

   if (devInfo.NumDrives  < 1) 
   {
      printf("\t\tERROR:  No StreamStor drives detected.\n");
      exit(EXIT_FAILURE);
   }
   
   // 
   // Calculate the overhead associated with creating a partition.
   //
   dwOverheadBytes = 
      (UINT64)(XLR_PARTOVERHEAD_BYTES * (UINT64)devInfo.NumDrives);

   //
   // Calculate how big to make each partition.
   //
   dwPartSizeBytes = 
     (devInfo.TotalCapacityBytes/(UINT64)numPartitionsToMake) - dwOverheadBytes;
   
   printf ("Creating %u partitions of equal size ...\n", numPartitionsToMake);

   for (partCount=0; partCount < numPartitionsToMake; partCount++) 
   {
      printf("Creating partition %u\n", partCount);
      if(XLRPartitionCreate(gl_XlrDevice,dwPartSizeBytes) != XLR_SUCCESS)
      {
         printf("\tError: Creation of partition %u failed.\n", partCount);
         exit(EXIT_FAILURE);
      }
      if(XLRPartitionSelect(gl_XlrDevice, partCount) != XLR_SUCCESS) 
      {
         printf("\t\tError: Selection of partition %u failed.\n", partCount);
         exit(EXIT_FAILURE);
      }
      printf("Writing test data into the partition ...\n");
      if (CreateTestData() != TRUE) 
      {
         printf ("\tError:  Could not write test data.\n");
         exit(EXIT_FAILURE);
      }
   } 

   // 
   // Determine the number of the last partition on the device.
   // Note that partition numbering starts at 0.
   //
   if(XLRGetPartitionInfo(gl_XlrDevice, &sPartition) != XLR_SUCCESS) 
   {
      printf ("\t\tError: Cannot get partition info for %u.\n", partCount);
      exit(EXIT_FAILURE);
   }

   lastPartitionNumber = sPartition.NumPartitions - 1;

   printf ("Selecting the last partition ...\n");
   if(XLRPartitionSelect(gl_XlrDevice, lastPartitionNumber) != XLR_SUCCESS) 
   {
      printf("\t\tError: Selection of partition %u failed.\n", 
            lastPartitionNumber);
      exit(EXIT_FAILURE);
   }

   if(XLRGetPartitionInfo(gl_XlrDevice, &sPartition) != XLR_SUCCESS) 
   {
      printf ("\t\tError: Cannot get partition info for %u.\n", 
            lastPartitionNumber);
      exit(EXIT_FAILURE);
   }
   printf("Before resize:\n");
   PrintPartitionInfo(sPartition);
   sprintf (prtBuf, "\t\tBytes recorded:  %s\n", LONGLONGFMT);
   printf (prtBuf, XLRGetLength(gl_XlrDevice));

   //
   // To resize the partition to the smallest possible size that will
   // accommodate the existing data, we pass a size of zero. 
   //
   printf("Calling XLRPartitionResize with newsize = 0 ...\n");
   if(XLRPartitionResize(gl_XlrDevice, 0) != XLR_SUCCESS) 
   {
      printf("\t\tError: Resize of partition failed.\n");
      exit(EXIT_FAILURE);
   }
   
   if(XLRGetPartitionInfo(gl_XlrDevice, &sPartition) != XLR_SUCCESS) 
   {
      printf ("\t\tError: Cannot get partition info for %u.\n", 
            lastPartitionNumber);
      exit(EXIT_FAILURE);
   }
   printf("After resize:\n");
   PrintPartitionInfo(sPartition);
   sprintf (prtBuf, "\t\tBytes recorded:  %s\n", LONGLONGFMT);
   printf (prtBuf, XLRGetLength(gl_XlrDevice));

   //
   // Now try to resize partition zero.  It will fail because you are 
   // allowed to resize only the last partition on the device.
   //
   printf ("Selecting partition 0.\n");
   if(XLRPartitionSelect(gl_XlrDevice, 0) != XLR_SUCCESS) 
   {
      printf("\t\tError: Selection of partition %u failed.\n", 
            lastPartitionNumber);
      exit(EXIT_FAILURE);
   }

   printf("Try to resize partition 0.\n");
   printf("It will fail since it is not the last partition on the device.\n");
   if(XLRPartitionResize(gl_XlrDevice, 0) != XLR_SUCCESS) 
   {
      printf("Resize of partition failed, as expected.\n");
   }

   gl_ExitStatus = EXIT_SUCCESS;
}

void cleanup(void)
{
   PrintXLRError();

   if (gl_XlrDevice != INVALID_SSHANDLE) 
   {
      XLRStop(gl_XlrDevice);

      // Get rid of all of the partitions.
      printf("Erasing partitions ...\n");
      if (XLRErase(gl_XlrDevice, SS_OVERWRITE_PARTITION) != XLR_SUCCESS) 
      {
         printf ("\tError - Cleanup failed - could not erase partitions.\n");
         PrintXLRError();
      } 
      XLRClose(gl_XlrDevice);
   }

   if (gl_ExitStatus == EXIT_FAILURE) 
   {
      printf("PartitionResizeExample exited with an error.\n");
   }
   else 
   {
      printf("PartitionResizeExample completed.\n");
   }
} // end cleanup()

//
// Name:  PrintPartitionInfo
//
// Purpose:  Prints out information in the partition structure.
//
// Inputs: a partition structure that was returned by XLRGetPartitionInfo.
//
//
void PrintPartitionInfo (S_PARTITIONINFO sPartition)
{
   char  prtBuf[256];

   printf("\tInformation on Partition %u\n", sPartition.SelectedPartition);
   sprintf(prtBuf, "\t\tAllocated on this system:      %s bytes\n", 
         LONGLONGFMT);
   printf(prtBuf, sPartition.SpaceAllocatedBytes);

   sprintf(prtBuf, "\t\tUnpartitioned on this system:  %s bytes\n", 
         LONGLONGFMT);
   printf(prtBuf, sPartition.SpaceAvailableBytes);

   sprintf(prtBuf, "\t\tCapacity of this partition:    %s bytes\n", 
         LONGLONGFMT);
   printf(prtBuf, sPartition.PartitionCapacityBytes);
}


void PrintXLRError()
{
   char              errorMessage[XLR_ERROR_LENGTH];
   XLR_ERROR_CODE    code = XLRGetLastError();
   if (code != 3) 
   {
      XLRGetErrorMessage(errorMessage, code);
      printf("\tError Message: %s\n", errorMessage);
   }
}


//
// Name: CreateTestData
// Purpose: writes some test data to the selected partition.
// 
BOOLEAN CreateTestData ()
{
   S_READDESC     xferDesc;
   UINT64         u64DataToSend = 0;
   PUINT32        testBuffer = NULL;
   BOOLEAN        firstTime = TRUE;
   BOOLEAN        status = FALSE;

   // Make sure length lands on 8 bytes boundary
   u64DataToSend = (UINT64)((LENGTH_TO_TRANSFER / 8) * 8);
   testBuffer = (PUINT32)malloc(READ_BUFFER_SIZE);
   if(testBuffer == NULL)
   {
     printf("malloc failed for test Buffer.\n");
     return (FALSE);
   }
   else 
   {
      status = BindInputForPCI();
      if (status == TRUE) 
      {
         // Write out some test data to gl_XlrDevice.
         if(XLRRecord(gl_XlrDevice, 0, 1) != XLR_SUCCESS)
         {
            printf("\nError: Record of test data failed.\n");
            PrintXLRError();
            free(testBuffer);
            return (FALSE);
         }

         // Init write descriptor
         xferDesc.AddrHi = 0;
         xferDesc.AddrLo = 0;
         xferDesc.BufferAddr = testBuffer;
      
         while(u64DataToSend > 0)
         {
            if(u64DataToSend < (UINT64)READ_BUFFER_SIZE)
            {
               xferDesc.XferLength = (UINT32)u64DataToSend;
            }
            else
            {
               xferDesc.XferLength = READ_BUFFER_SIZE;
            }
   
            if(! firstTime)
            {
               memset(testBuffer, 0xFF, READ_BUFFER_SIZE);
               firstTime = TRUE;
            }
            else
            {
               memset(testBuffer, 0, READ_BUFFER_SIZE);
            }

            if(XLRWrite(gl_XlrDevice, &xferDesc) != XLR_SUCCESS)
            {
               printf("\nError: Write failed.\n");
               PrintXLRError();
               free(testBuffer);
               return(FALSE);
            }
            // Update counter
            u64DataToSend = u64DataToSend - (UINT64)xferDesc.XferLength;
         }

         status = TRUE;
         XLRStop(gl_XlrDevice);
      }
      else 
      {
         status = FALSE;
      }

      free(testBuffer);
   }
   return (status);
}

//
// Name:  BindInputForPCI
// Purpose:  Setup channels for recording over the PCI bus.
//
BOOLEAN BindInputForPCI()
{

   if(XLRSetMode(gl_XlrDevice, SS_MODE_SINGLE_CHANNEL) != XLR_SUCCESS)
   {
      printf("\nError: Set mode single channel failed.\n");
      PrintXLRError();
      return (FALSE);
   }

   if(XLRClearChannels(gl_XlrDevice) != XLR_SUCCESS)
   {
      printf("\nError: Clear channels failed.\n");
      PrintXLRError();
      return (FALSE);
   }

   if(XLRBindInputChannel(gl_XlrDevice, 0) != XLR_SUCCESS)
   {
      printf("\nError: Binding of Input channel failed.\n");
      PrintXLRError();
      return (FALSE);
   }

   if (XLRSelectChannel (gl_XlrDevice, 0) != XLR_SUCCESS) {
      printf("\nError: Select channel failed.\n");
      PrintXLRError();
      return(FALSE);
   }

   return (TRUE);
}

