/*

   This is a simple example program to control the StreamStor
   recorder.

*/

//$Id: //streamstor/example/example.c#3 $

#include <stdio.h>
#include <stdlib.h>
#include "xlrapi.h"

#define  BUF_SIZE    0x8000
SSHANDLE   XlrDevice;

int main( int argc, char *argv[] )
{
   XLR_RETURN_CODE   xlrStatus;
   UINT32            phyAddress;
   UINT64            readAddress;
   S_READDESC        readDesc;
   S_DIR             xlrDir;
   char              userbuf[BUF_SIZE];
   
   /*------------------------------------------------------------------------*/
   /* Open the StreamStor device and get a handle to it                      */
   /*                                                                        */
   /* XLR_RETURN_CODE XLROpen( UINT32 devNumber, HANDLE *XlrDevice           */
   /*   devNumber - This parameter is currently ignored but may be utilized  */
   /*             in future releases to support multiple StreamStor devices. */
   /*               Initialize to 1 to maintain future compatability.        */
   /*   devHandle - Handle to XLR device, required for subsequent API calls  */
   /*------------------------------------------------------------------------*/
   xlrStatus = XLROpen( 1, &XlrDevice );
   if( xlrStatus != XLR_SUCCESS )
   {
      char temp[XLR_ERROR_LENGTH];
      XLRGetErrorMessage( temp, XLRGetLastError() );
      printf( "ERROR: %s\n", temp );
      return(1);
   }
   printf("StreamStor opened successfully.\n");
   
   /*------------------------------------------------------------------------*/
   /* Get PCI physical address of the StreamStor device for data recording.  */
   /*                                                                        */
   /* UINT32 XLRGetBaseAddr( HANDLE XlrDevice )                               */
   /*     This function returns the PCI physical address of the StreamStor   */
   /*     card. Applications will use this or the virtual address returned   */
   /*     by XLRGetWindowAddr to send data for recording.                    */
   /*------------------------------------------------------------------------*/
   phyAddress = XLRGetBaseAddr( XlrDevice );

   
   /*------------------------------------------------------------------------*/
   /* Put StreamStor device into record mode.                                */
   /*                                                                        */
   /* XLR_RETURN_CODE XLRRecord( HANDLE XlrDevice, BOOL WrapEnable,          */
   /*     USHORT ZoneRange )                                                 */
   /*     XlrDevice - HANDLE returned from XLROpen call.                     */
   /*     WrapEnable - Flag that enable overwrite of oldest data after       */
   /*                  disk space is exhausted.                              */
   /*     ZoneRange - Not implemented, should be set to 1                    */
   /*                                                                        */
   /* NOTE: After calling this function, no other StreamStor functions       */
   /*       should be called except XLRGetDeviceStatus until the data source */
   /*       is no longer sending data.                                       */
   /*------------------------------------------------------------------------*/
   xlrStatus = XLRRecord( XlrDevice, 0, 1 );
   if( xlrStatus != XLR_SUCCESS )
   {
      return(1);
   }
   printf("StreamStor is in record mode.\n");


   /***********************************************/
   /* DATA SOURCE can now send data to StreamStor */
   /***********************************************/

   
   
   /*------------------------------------------------------------------------*/
   /* STOP the recorder.                                                     */
   /*                                                                        */
   /* XLR_RETURN_CODE XLRStop( HANDLE XlrDevice )                            */
   /*------------------------------------------------------------------------*/
   xlrStatus = XLRStop( XlrDevice );
   if( xlrStatus != XLR_SUCCESS )
   {
      return(1);
   }
   printf("StreamStor stopped.\n");

   /*------------------------------------------------------------------------*/
   /* Read the directory for data just recorded.                             */
   /*                                                                        */
   /* XLR_RETURN_CODE XLRGetDirectory( HANDLE XlrDevice, S_DIR *xlrDir )     */
   /*     XlrDevice - HANDLE returned from XLROpen call.                     */
   /*     xlrDir - Structure to hold directory information returned.         */
   /*------------------------------------------------------------------------*/
   xlrStatus = XLRGetDirectory( XlrDevice, &xlrDir );
   if( xlrStatus != XLR_SUCCESS )
   {
      return(1);
   }
   printf("StreamStor recorded 0x%02X%08X bytes.\n", (UINT32)(xlrDir.Length>>32),(UINT32)(xlrDir.Length) );

   /*------------------------------------------------------------------------*/
   /* Read some data back from StreamStor                                    */
   /*                                                                        */
   /* XLR_RETURN_CODE XLRRead( HANDLE XlrDevice, S_READDESC *readDesc )      */
   /*     XlrDevice - HANDLE returned from XLROpen call.                     */
   /*     readDesc - Structure with description of requested read            */
   /*------------------------------------------------------------------------*/
   readAddress = 0;   // Byte address to start retrieval

   readDesc.BufferAddr = (PUINT32)userbuf;        /* Pointer to buffer for data    */
   readDesc.XferLength = BUF_SIZE;                /* Number of bytes to transfer   */
   readDesc.AddrHi = (UINT32)(readAddress >> 32);  /* Upper 32 bits of data address */
   readDesc.AddrLo = (UINT32)readAddress;          /* Lower 32 bits of data address */
   /* NOTE: readAddress + XferLength should never be greater than xlrDir.Length */

   xlrStatus = XLRRead( XlrDevice, &readDesc );
   if( xlrStatus != XLR_SUCCESS )
   {
      return(1);
   }
   printf("StreamStor data read successfully.\n");

   /*------------------------------------------------------------------------*/
   /* Close the StreamStor device                                            */
   /*                                                                        */
   /* XLR_RETURN_CODE XLRStop( HANDLE XlrDevice )                            */
   /*     XlrDevice - HANDLE returned from XLROpen call.                     */
   /*------------------------------------------------------------------------*/
   XLRClose( XlrDevice );
   return(0);
}

