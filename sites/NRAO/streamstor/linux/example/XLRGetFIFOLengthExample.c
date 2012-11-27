/*
 *  This example shows how you can use channels to record data over FPDP 
 *  and then use SS_MODE_FORK for playback so data can be read into a fifo.
 *  
 *  The example requires two StreamStor cards.  Some test data is needed 
 *  for this example, so if the sending device does not already have 
 *  some test data on it, some test data is generated.
 */

//$Id: //streamstor/example/XLRGetFIFOLengthExample.c#4 $

#include <stdio.h>
#include <string.h>
#include <stdlib.h>   // for memory operations

#include "xlrapi.h"

#define LENGTH_TO_TRANSFER  1047451680
#define WAIT_LIMIT          10 
#define NAPLENGTH           1000
#define READ_BUFFER_SIZE    0x100000
#define THRESHOLD           131064

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

BOOLEAN CreateTestData (SSHANDLE sender);
BOOLEAN SetupReceiver(SSHANDLE receiver);
BOOLEAN SetupSender(SSHANDLE sender);
BOOLEAN BindInputForPCI(SSHANDLE xlrDevice);
void help();
void PrintXLRError();


int main(int argc, char * argv[]) 
{ 
   S_DEVSTATUS devStat;
   S_DIR       dir;
   SSHANDLE    receiver = INVALID_SSHANDLE;
   SSHANDLE    sender = INVALID_SSHANDLE;

   UINT64      u64BytesRead = 0;
   UINT64      u64DataToSend = 0;
   UINT64      u64FifoLength = 0;
   UINT64      u64SenderLength = 0;
   UINT64      u64PlayLength = 0;
   UINT64      u64Diff = 0;

   UINT32      u32LengthToRead = 0;
   PUINT32     u32ReadBuffPtr = NULL;
   UINT32      XlrErrorCode; 
   UINT32      numCards=0;
   UINT32      waitCount=0;
   UINT32      sendCardNum = 1;
   UINT32      recvCardNum = 2;
   UINT32      errorCount = 0;

   char        prtBuf[256];
   char        notReadyMsg[256];
   char        progressMsg[256];
   BOOLEAN     status=FALSE;
   char        errorMessage[XLR_ERROR_LENGTH];
   int         inputChar;

   printf ("%s\n", argv[0]);

   numCards = XLRDeviceFind();
   if( numCards < 2 )
   {
      printf
       ("Error:  Test requires at least 2 StreamStors connected by FPDP front ports.\n");
      exit (1);
   }

   if (argc == 1)
   {
      // No command line arguments, so use the default card numbers.
      sendCardNum = 1;
      recvCardNum = 2;
   }
   else if (argc == 2)
   {
      if ( (strncmp (argv[1], "-?", 2) == 0) ||
           (strncmp (argv[1], "-h", 2) == 0))
      {
         help();
         exit(0);
      }
      else
      {
         printf ("Error:  Invalid command line arguments.\n");
         help();
         exit(1);
      }
   }
   else if (argc == 3)
   {
      sendCardNum = strtol(argv[1], (char **)NULL, 10);
      recvCardNum = strtol(argv[2], (char **)NULL, 10);
      if (recvCardNum < 1 || recvCardNum > numCards ||
          sendCardNum < 1 || sendCardNum > numCards)
      {
         printf 
            ("Error: Invalid card index entered.  Must be between 1 and %u.\n",
            numCards);
         help();
         exit(1);
      }
      if (sendCardNum == recvCardNum)
      {
         printf ("Error:  receiver and sender cannot be the same card.\n");
         help();
         exit(1);
      } 
   }
   else
   {
      printf ("Error:  Invalid command line arguments.\n");
      help();
      exit(1);
   }

   printf ("Sender   = card %u\n", sendCardNum);
   printf ("Receiver = card %u\n", recvCardNum);

   printf("***** WARNING!!! *****\n");
   printf("This example will erase all data on StreamStor card %u.\n",
         recvCardNum);
   printf("Do you wish to continue [y/n]: ");
   inputChar = getchar();
   if( ( inputChar != 'y' ) && ( inputChar != 'Y' ) )
   {
      printf( "Ok, Exiting now.\n" );
      exit( 0 );
   }

   printf ("Opening sending card %u\n", sendCardNum);
   if( XLROpen( sendCardNum, &sender ) != XLR_SUCCESS )
   {
     printf("\nError: XLROpen() of sender return error.\n");
     PrintXLRError();
     goto CLOSE_ALL;
   }

   //
   // Create some test data to send, if we need to.
   //
   status = CreateTestData (sender);
   if (status == FALSE) 
   {
      printf ("\nError: Cannot create test data.\n");
      goto CLOSE_ALL;
   }

   //
   // Now we have some test data.  We will send the test data from 
   // the top port of the sender card to the top port of the receiver card.
   // The receiver card will be the one that ultimately forks the data.
   //

   printf("Opening Receiver card %u ...", recvCardNum);
   if( XLROpen( recvCardNum, &receiver ) != XLR_SUCCESS )
   {
      printf("\nError: XLROpen() receiver returned error.\n");
      PrintXLRError();
      goto CLOSE_ALL;
   }
   printf("OK.\n");

   status = SetupReceiver( receiver );
   if (status == FALSE) 
   {
      printf ("Error: could not set up receiver.\n");
      goto CLOSE_ALL;
   }

   //
   // The sending card is already open.  Just need to set it up.
   // 
   status = SetupSender( sender );
   if (status == FALSE) 
   {
      printf ("Error: could not set up sender.\n");
      goto CLOSE_ALL;
   }

   //
   // Get length of sender before playback begins.
   //
   u64DataToSend = XLRGetLength( sender );
   
   //
   // Put the receiver card in record mode.  
   //
   printf ("Start record on receiver.\n");
   if( XLRRecord( receiver, 0, 1 ) != XLR_SUCCESS )
   { 
      printf("\nError: XLRRecord() returned error for receiver.\n");
      PrintXLRError();
      goto CLOSE_ALL;
   }

   // 
   // Put the sender in playback mode.  Data will now flow from the
   // top FPDP port of the sender card to the top FPDP port of the
   // receiver card.  As the data flows, it will be recorded to StreamStor
   // disk on the receiving device.
   //
   printf("Start playback...");
   if( XLRPlayback(sender, 0, 0) != XLR_SUCCESS )
   {
      printf("\nError: XLRPlayback() returned error \n");
      PrintXLRError();
      goto CLOSE_ALL;
   }
   printf("OK.\n");

   u64BytesRead = 0;
   u32LengthToRead = READ_BUFFER_SIZE;

   sprintf (progressMsg, "read: %s  remain: %s  fifo: %s\n", LONGLONGFMT, 
         LONGLONGFMT, LONGLONGFMT);

   u32ReadBuffPtr = (PUINT32)malloc(READ_BUFFER_SIZE);
   if( u32ReadBuffPtr == NULL )
   {
     printf("malloc failed\n");
     goto CLOSE_ALL;
   }

   waitCount = 0;
   sprintf (notReadyMsg, "\t Waiting to fill - fifo = %s\n", LONGLONGFMT);
   while( TRUE )
   {
      memset( u32ReadBuffPtr, 0, READ_BUFFER_SIZE );
      u64FifoLength = XLRGetFIFOLength( receiver );

      //
      // Loop, checking the fifo until it fills.  Once it fills, we are
      // ready to start reading from it.
      //
      if (u64FifoLength < (UINT64)u32LengthToRead) 
      {
         printf (notReadyMsg, u64FifoLength);
         if (waitCount > WAIT_LIMIT) 
         {
            printf ("\tTimed out waiting for fifo to fill.\n");
            break;
         }
         waitCount++;

         //
         // If you sleep too long, the FIFO may overflow. So, we only sleep
         // a second here.
         //
         Sleep(1000);
         continue;
      }

      if( XLRReadFifo( receiver, u32ReadBuffPtr, u32LengthToRead, FALSE ) 
          != XLR_SUCCESS )
      {
          //
          // Check the error code returned from XLRReadFifo.  XLR_ERR_EMPTY
          // (code 144) is a special case - it does not necessarily indicate 
          // an error.  It is also used to indicate that there are fewer 
          // bytes in the FIFO than requested.
          //
          XlrErrorCode = XLRGetLastError();
          if( XlrErrorCode == 144)
          {
              // See if there is any residual data left in the fifo.
              u64FifoLength = XLRGetFIFOLength( receiver );
              sprintf (prtBuf, "BEFORE STOP u64FifoLength  : %s\n", 
                    LONGLONGFMT);
              printf (prtBuf, u64FifoLength);
              break;
          }
          else {
            // Didn't get XLR_ERR_EMPTY, so a "real" error has occurred.
            printf("\nError: Read FIFO failed.\n");
            XLRGetErrorMessage(errorMessage, XlrErrorCode);
            printf("\tError Message: %s\n", errorMessage );
            goto CLOSE_ALL;
          }
      }
      else
      {
          u64DataToSend = u64DataToSend  - (UINT64)u32LengthToRead;
          u64BytesRead  = u64BytesRead + (UINT64)u32LengthToRead;

          if( XLRGetDeviceStatus(receiver, &devStat) != XLR_SUCCESS )
          {
            printf("\nError: XLRGetDeviceStatus() returned error.\n");
            PrintXLRError();
            goto CLOSE_ALL;
          }

          //
          // If the fifo has overflowed, data will still be sent 
          // correctly to disk, so continue.
          //
          if (devStat.FifoFull == TRUE) {
             printf ("Fifo has overflowed.\n");
          }

          waitCount = 0;
      }

      // output progress  
      printf (progressMsg, u64BytesRead, u64DataToSend, XLRGetFIFOLength(receiver) );
   }

   //
   // Stop to shut down the receiving interface.
   //
   if( XLRStop(receiver) != XLR_SUCCESS )
   {
      printf("\nError: Receiver XLRStop() returned error \n");
      PrintXLRError();
      goto CLOSE_ALL;
   }
   if( XLRGetDeviceStatus(receiver, &devStat) != XLR_SUCCESS )
   {
      printf("\nError: XLRGetDeviceStatus() returned error.\n");
      PrintXLRError();
      goto CLOSE_ALL;
   }
   if (devStat.FifoFull == TRUE) {
      printf ("Fifo has overflowed.\n");
   }
   

   //
   // While data is getting transferred, a call to XLRGetFIFOLength is just
   // a snapshot of how much data is in the FIFO at that moment.  Once XLRStop
   // is called, you can get the precise number of bytes in the FIFO.
   //
   u64FifoLength = XLRGetFIFOLength( receiver );
   if( ( u64FifoLength % 8 ) != 0 )
   {
       u64FifoLength = ( u64FifoLength / 8 ) * 8;
   }
   sprintf (prtBuf, "AFTER STOP u64FifoLength   : %s\n", LONGLONGFMT);
   printf (prtBuf, u64FifoLength);

   //
   //  Note that after XLRStop is called, you can call XLRReadFifo only
   //  one time to get any data that may be remaining in the FIFO.  So,
   //  you call XLRReadFifo with u32LengthToRead set to the FIFO length, 
   //  not the buffer length.  (Make sure that the buffer is at least 
   //  u32LengthToRead bytes long.)
   //
   u32LengthToRead = (UINT32)u64FifoLength;
   if (u32LengthToRead != 0) 
   {
      printf ( "Read last buffer ...\n" );
      if ( u32LengthToRead > READ_BUFFER_SIZE ) {
         printf ("\tCreating a bigger buffer of %u.\n", u32LengthToRead);
         free(u32ReadBuffPtr);
         u32ReadBuffPtr = NULL;
         u32ReadBuffPtr = (PUINT32)malloc( u32LengthToRead );
         memset( u32ReadBuffPtr, 0, u32LengthToRead );
         if( u32ReadBuffPtr == NULL )
         {
            printf("malloc of new buffer failed.\n");
            goto CLOSE_ALL;
         }
      }
      if( XLRReadFifo( receiver, u32ReadBuffPtr, u32LengthToRead, FALSE ) 
            != XLR_SUCCESS )
      {
         printf("\nError: Reading last fifo failed.\n");
         PrintXLRError();
         goto CLOSE_ALL;
      }
      u64BytesRead = u64BytesRead + (UINT64)u32LengthToRead;
   }

   if( XLRStop(sender) != XLR_SUCCESS )
   {
      printf("\nError: Sender XLRStop() returned error \n");
      PrintXLRError();
      goto CLOSE_ALL;
   }

   // Take receiver out of record mode.
   if( XLRStop(receiver) != XLR_SUCCESS )
   {
      printf("\nError: receiver second XLRStop() returned error \n");
      PrintXLRError();
   }

   printf("\nTransfer Complete.\n\n");

   if( XLRGetDirectory( receiver, &dir ) != XLR_SUCCESS )
   {
     printf("\nError: XLRGetDirectory() returned error\n");
     PrintXLRError();
     goto CLOSE_ALL;
   }

   u64SenderLength = XLRGetLength(sender);

   sprintf (prtBuf, "Sender   transferred %s bytes from DISK\n", LONGLONGFMT);
   printf (prtBuf, u64SenderLength);

   u64PlayLength = XLRGetPlayLength(sender);
   sprintf (prtBuf, "Sender   transferred %s bytes from FPDP\n", LONGLONGFMT);
   printf (prtBuf, u64PlayLength);

   sprintf (prtBuf, "Receiver         has %s bytes on   DISK\n", LONGLONGFMT);
   printf (prtBuf, dir.Length);

   sprintf (prtBuf, "Receiver transferred %s bytes to   PCI\n", LONGLONGFMT);
   printf (prtBuf, u64BytesRead);

   if( errorCount > 0 )
   {
      printf( "***ERROR COUNT: %u***\n", (errorCount * 4) ); //bytes
   }

   if (u64SenderLength == 0) {
      printf ("Error!  No data was sent!\n");
   }
   else {
      if (u64SenderLength != u64PlayLength || u64SenderLength != dir.Length) 
      {
            printf ("Error!  Number of bytes played should match\n");
            printf ("the number of bytes recorded to disk.\n");
      }

      if (u64SenderLength != u64BytesRead) {
         u64Diff = (u64SenderLength > u64BytesRead) ? 
            (u64SenderLength - u64BytesRead) : (u64BytesRead - u64SenderLength);
         if (u64Diff > THRESHOLD) {
            printf ("Error! Difference between number of bytes sent and number of\n");
            printf ("bytes transferred to PCI should be no more than %d\n", 
               THRESHOLD);
            sprintf (prtBuf, "Difference is %s\n", LONGLONGFMT);
            printf (prtBuf, u64Diff);
         }
      }
   }
CLOSE_ALL:
   if( receiver != INVALID_SSHANDLE )
   {
      XLRClose(receiver);
   }
   if( sender != INVALID_SSHANDLE )
   {
      XLRClose(sender);
   }
   if( u32ReadBuffPtr )
   {
     free( u32ReadBuffPtr );
   }
   
   printf ("%s Completed.\n", argv[0]);
   return 0;
}

BOOLEAN CreateTestData (SSHANDLE sender) 
{
   S_READDESC  xferDesc;
   UINT64      u64DataToSend = 0;
   PUINT32     u32TestBuffPtr = NULL;
   BOOLEAN     status = FALSE;
   BOOLEAN     firstTime = FALSE;
   char        prtBuf[256];

   // Make sure length lands on 8 bytes boundary
   u64DataToSend = (UINT64  )( ( LENGTH_TO_TRANSFER / 8 ) * 8 );
   
   //
   // If there is already enough test data on this device, we don't 
   // need to create any more. 
   //
   if( XLRGetLength( sender ) == u64DataToSend )
   {
      printf("Send data already present.\n" );
      return (TRUE);
   }

   u32TestBuffPtr = (PUINT32)malloc(READ_BUFFER_SIZE);
   if( u32TestBuffPtr == NULL )
   {
     printf("malloc failed for test Buffer.\n");
     return (FALSE);
   }
   else {
      status = BindInputForPCI (sender );
      if (status == TRUE) 
      {
         // Write out some test data to sender.
         printf("Write test data...\n");
         if( XLRRecord( sender, 0, 1 ) != XLR_SUCCESS )
         {
            printf("\nError: Record of test data failed.\n");
            PrintXLRError();
            free(u32TestBuffPtr);
            return (FALSE);
         }

         // Init write descriptor
         xferDesc.AddrHi = 0;
         xferDesc.AddrLo = 0;
         xferDesc.BufferAddr = u32TestBuffPtr;
      
         while( u64DataToSend > 0 )
         {
            if( u64DataToSend < READ_BUFFER_SIZE )
            {
               xferDesc.XferLength = (UINT32)u64DataToSend;
            }
            else
            {
               xferDesc.XferLength = READ_BUFFER_SIZE;
            }
   
            if( ! firstTime )
            {
               memset( u32TestBuffPtr, 0xFF, READ_BUFFER_SIZE );
               firstTime = TRUE;
            }
            else
            {
               memset( u32TestBuffPtr, 0, READ_BUFFER_SIZE );
            }

            if( XLRWrite( sender, &xferDesc ) != XLR_SUCCESS )
            {
               printf("\nError: Write failed.\n");
               PrintXLRError();
               free(u32TestBuffPtr);
               return(FALSE);
            }
            // Update counter
            u64DataToSend -= xferDesc.XferLength;
         }

         status = TRUE;
         XLRStop( sender );
         sprintf (prtBuf, "Data on sender: %s\n", LONGLONGFMT);
         printf (prtBuf, XLRGetLength( sender ));
      }
      else {
         status = FALSE;
      }

      free(u32TestBuffPtr);
   }
   return (status);
}

BOOLEAN SetupReceiver(SSHANDLE receiver) 
{
   printf("Setup receiver for forking ...\n");

   //
   // Set SS mode for input board to FORK.  Data will go to StreamStor
   // disk and will be available to read from the FIFO.
   //
   if (XLRSetMode( receiver, SS_MODE_FORK ) != XLR_SUCCESS)
   { 
      printf("\nError: XLRSetMode() returned error for SS_MODE_FORK.\n");
      PrintXLRError();
      return (FALSE);
   }

   if( XLRClearChannels( receiver ) != XLR_SUCCESS )
   {
      printf( "\nError: Receiver Clear channels failed.\n" );
      PrintXLRError();
      return (FALSE);
   }
   
   //
   // Data will be getting played back over the TOP fpdp port 
   // and recorded to this StreamStor.
   //
   printf ("Binding channel 30 on receiver as INPUT channel.\n");
   if( XLRBindInputChannel( receiver, 30 ) != XLR_SUCCESS )
   {
      printf( "\nError: Receiver Binding of Input channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   printf ("Binding channel 0 on receiver as OUTPUT channel.\n");
   if( XLRBindOutputChannel( receiver, 0 ) != XLR_SUCCESS )
   {
      printf( "\nError: Receiver Binding of Output channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   if ( XLRSelectChannel ( receiver, 30 ) != XLR_SUCCESS ) {
      printf( "\nError: Receiver Select channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   //
   // Set FPDP mode.  The fpdp mode will be applied to the currently
   // selected channel, i.e, 30.
   //
   if (XLRSetFPDPMode( receiver, SS_FPDP_RECVMASTER, SS_OPT_FPDPNRASSERT ) 
         != XLR_SUCCESS)
   { 
      printf
       ("\nError: Receiver XLRSetFPDPMode() returned error for RECV MASTER.\n");
      PrintXLRError();
      return (FALSE);
   }

   return (TRUE);
}

BOOLEAN BindInputForPCI (SSHANDLE xlrDevice)
{

   if( XLRSetMode( xlrDevice, SS_MODE_SINGLE_CHANNEL ) != XLR_SUCCESS )
   {
      printf( "\nError: Set mode single channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   if( XLRClearChannels( xlrDevice) != XLR_SUCCESS )
   {
      printf( "\nError: Clear channels failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   if( XLRBindInputChannel( xlrDevice, 0 ) != XLR_SUCCESS )
   {
      printf( "\nError: Binding of Input channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   if ( XLRSelectChannel ( xlrDevice, 0 ) != XLR_SUCCESS ) {
      printf( "\nError: Select channel failed.\n" );
      PrintXLRError();
      return(FALSE);
   }

   return (TRUE);
}

BOOLEAN SetupSender ( SSHANDLE sender ) 
{

   if( XLRSetMode( sender, SS_MODE_SINGLE_CHANNEL ) != XLR_SUCCESS )
   {
      printf( "\nError: Sender Set mode single channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   if( XLRClearChannels( sender ) != XLR_SUCCESS )
   {
      printf( "\nError: Sender Clear channels failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   printf ("Binding channel 30 on sender as OUTPUT channel.\n");
   if( XLRBindOutputChannel( sender, 30 ) != XLR_SUCCESS )
   {
      printf( "\nError: Sender Binding of OUTPUT channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   if ( XLRSelectChannel ( sender, 30 ) != XLR_SUCCESS ) {
      printf( "\nError: Selecting sender channel failed.\n" );
      PrintXLRError();
      return (FALSE);
   }

   // 
   //  Since this is the card that is sending data, we use transmit-master.
   //
   if (XLRSetFPDPMode(sender, SS_FPDP_XMITMASTER, 0) != XLR_SUCCESS)
   { 
      printf("\nError: Sender XLRSetFPDPMode() returned error for TRANSMIT.\n");
      PrintXLRError();
      return (FALSE);
   }

   // 
   // Set Clock for transmitter.
   //
   if (XLRSetPortClock(sender, SS_PORTCLOCK_10MHZ) != XLR_SUCCESS)
   { 
      printf("\nError: Sender XLRSetPortClock() returned error.\n"); 
      PrintXLRError();
      return (FALSE);
   }

   return (TRUE);
}
      
void help()
{
   printf(
"Usage: XLRGetFIFOLengthExample [senderCardIndex receiverCardIndex]\n"
"Requires two StreamStor cards connected by FPDP top ports.\n"
"By default the sender = 1 and receiver = 2.\n"
   );
}

void PrintXLRError()
{
   char  errorMessage[XLR_ERROR_LENGTH];
   XLRGetErrorMessage(errorMessage, XLRGetLastError());
   printf("\tError Message: %s\n", errorMessage );
}
