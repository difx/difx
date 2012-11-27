/*  
 *  Name:  XLRPartitionExample.c
 * 
 *  Purpose:  Demonstrates some partition concepts, such as creating 
 *  partitions, erasing partitions, and dividing up available space 
 *  into equally sized partitions.
 *
 *  Note that this code will erase all partitions and data on the StreamStor.
 */

//$Id: //streamstor/example/XLRPartitionExample.c#10 $

#include <stdio.h>
#include "xlrapi.h"

#ifdef WIN32
#define LONGLONGFMT "%I64u"
#else
#define LONGLONGFMT "%llu"
#include <stdlib.h>
#include <unistd.h>
#endif

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define MEGABYTE  1048576           // 1 megabyte

SSHANDLE    gl_XlrDevice = INVALID_SSHANDLE;
int         gl_ExitStatus = EXIT_FAILURE;

void PrintXLRError();
void PrintPartitionInfo (S_PARTITIONINFO sPartition);
void cleanup(void);

int main(int argc, char *argv[])
{
   S_PARTITIONINFO      sPartition;
   S_DEVINFO            devInfo;

   UINT64               dwPartSizeBytes=0;
   UINT64               dwOverheadBytes=0;

   UINT32               numCards=0;
   UINT32               partitionsToMake=0;
   UINT32               partCount=0;
   int                  inputChar;
   char                 prtBuf[256];

   atexit(cleanup);
   printf("PartitionExample\n");

   printf("***** WARNING!!! *****\n");
   printf("This example will erase all data and partitions on your device.\n");
   printf("Do you wish to continue [y/n]: ");
   inputChar = getchar();
   if( ( inputChar != 'y' ) && ( inputChar != 'Y' ) )
   {
      printf("Ok, no erase done, exiting now.\n");
      gl_ExitStatus = EXIT_SUCCESS;
      gl_XlrDevice = INVALID_SSHANDLE;
      exit( gl_ExitStatus );
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

   //
   // Erase any existing partitions and data.
   //

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
   
   dwOverheadBytes = (UINT64)(XLR_PARTOVERHEAD_BYTES * 
                     (UINT64)devInfo.NumDrives);

   sprintf (prtBuf, "Total Capacity = %s bytes\n", LONGLONGFMT);
   printf(prtBuf, devInfo.TotalCapacityBytes);

   //
   // This demonstrates how to create 5 partitions of equal size,
   // given the available capacity.  The calculation is:
   //    partitionSize = (capacity/numPartitions) - Overhead
   // 
   dwPartSizeBytes = (devInfo.TotalCapacityBytes/5) - dwOverheadBytes;
   
   printf ("Creating 5 partitions of equal size ...\n");
   for (partCount=0; partCount < 5; partCount++) 
   {
      printf ("----------  Creating Partition [%u] ---------------\n", 
            partCount);

      if(XLRPartitionCreate(gl_XlrDevice,dwPartSizeBytes) != XLR_SUCCESS)
      {
         printf("\t\tError: Creation of partition %u failed.\n", partCount);
         exit(EXIT_FAILURE);
      }

      if(XLRPartitionSelect(gl_XlrDevice, partCount) != XLR_SUCCESS) 
      {
         printf("\t\tError: Selection of partition %u failed.\n", partCount);
         exit(EXIT_FAILURE);
      }

      if(XLRGetPartitionInfo(gl_XlrDevice, &sPartition) != XLR_SUCCESS) 
      {
         printf ("\t\tError: Cannot get partition info for %u.\n", partCount);
         exit(EXIT_FAILURE);
      }

      PrintPartitionInfo(sPartition);
   } 
   printf ("--------------------------------------------------\n");

   //
   // Erase the partitions we just created.
   //
   printf("Erasing partitions ...\n");
   if(XLRErase(gl_XlrDevice, SS_OVERWRITE_PARTITION) != XLR_SUCCESS)
   {
      printf("\t\tERROR: Erase of partitions failed.\n");
      exit(EXIT_FAILURE);
   }

   //
   // This demonstrates how to calculate the maximum number of partitions 
   // of size X that can be created. The formula is:
   //    numPartitions = capacity/(X + overhead)
   //
   // Assume we want to divide the available space into 400 megabyte
   // partitions.
   //
   printf 
      ("Calculating maximum number of 1000 Megabyte partitions that can be created...\n");
   dwPartSizeBytes = (UINT64)(1000*MEGABYTE);
   partitionsToMake =
      (UINT32)(devInfo.TotalCapacityBytes/(dwPartSizeBytes + dwOverheadBytes));

   //
   // Note that there is a maximum number of partitions that can be 
   // created, as defined by XLR_MAX_PARTITIONS.
   //
   printf("You can create %u partitions of length ",
     (partitionsToMake > XLR_MAX_PARTITIONS) ?  
     XLR_MAX_PARTITIONS : partitionsToMake);
   sprintf(prtBuf, "%s bytes.\n", LONGLONGFMT);
   printf(prtBuf, dwPartSizeBytes);

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
      printf("PartitionExample exited with an error.\n");
   }
   else 
   {
      printf("PartitionExample completed.\n");
   }
} // end cleanup()

//
// Name:  PrintPartitionInfo
//
// Purpose:  Prints out information in the partition structure.
//
// Inputs: a partition structure that was returned by XLRGetPartitionInfo.
//
// Outputs: each structure member is printed.
//
void PrintPartitionInfo (S_PARTITIONINFO sPartition)
{
   char     prtBuf[256];

   printf("  Partition Info:\n");
   printf("    Total Number of Partitions:         %u\n", 
         sPartition.NumPartitions);
   printf("    Currently Selected Partition:       %u\n", 
         sPartition.SelectedPartition);

   sprintf(prtBuf, "    Bytes Allocated:    %s\n", LONGLONGFMT);
   printf(prtBuf, sPartition.SpaceAllocatedBytes);

   sprintf(prtBuf, "    Bytes Available:    %s\n", LONGLONGFMT);
   printf(prtBuf, sPartition.SpaceAvailableBytes);

   sprintf(prtBuf, "    This partition's Capacity: %s bytes\n", LONGLONGFMT);
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

