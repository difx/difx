#ifndef _SSTYPES_
#define _SSTYPES_
/*******************************************************************
 *
 *    DESCRIPTION:
 *
 *    AUTHOR:
 *
 *    HISTORY:    
 *
 * Copyright 2008 Conduant Corporation
 *******************************************************************/
// $Id: //streamstor/include/xlrtypes.h#61 $
//
#ifndef WIN32
#include <stddef.h>     // for size_t
#endif

#ifndef _WINDRVR_H_
#ifndef I2O
#ifndef __VOID
#ifdef TARGET
typedef void                  VOID;
#define size_t	UINT32
#endif
typedef void *                PVOID;
#endif	/* __VOID */
#endif	/* I2O */

typedef char                  CHAR;
typedef char *                PCHAR;
typedef unsigned char         UCHAR;

typedef short                 SHORT;
typedef short *               PSHORT;
typedef unsigned short        USHORT;
typedef unsigned short *      PUSHORT;

typedef int                   INT32;
typedef int *                 PINT32;
typedef unsigned int          UINT32;
typedef unsigned int *        PUINT32;


#ifndef WIN32
#ifndef DWORD
typedef unsigned long         DWORD;
#endif
#endif

// longs may be either 32 or 64 bit depending on OS
typedef long                  LONG;
typedef long *                PLONG;
typedef unsigned long         ULONG;
typedef unsigned long *       PULONG;

#ifndef WIN32
typedef unsigned long long    UINT64;
typedef unsigned long long *  PUINT64;
#endif

#ifdef TARGET
typedef unsigned long long    ULONGLONG;
#endif

typedef UCHAR                 BYTE;

#endif  // _WINDRVR_H_

typedef unsigned char *       PUCHAR;
typedef unsigned short *      PUSHORT;
typedef unsigned long *       PULONG;
typedef unsigned int *        PUINT32;
typedef short                 SHORT;
typedef short *               PSHORT;

#ifndef WIN32
typedef unsigned long long    DWORDLONG;
typedef UCHAR                 BOOLEAN;
typedef UCHAR *               PBOOLEAN;
#endif
#ifdef TARGET
typedef UINT32                NTSTATUS;
#endif

typedef UINT32                XLRHEADER;
typedef PUINT32               PXLRHEADER;
typedef UINT32                SSHANDLE;
typedef SSHANDLE *            PSSHANDLE;
#define INVALID_SSHANDLE	   ((SSHANDLE)0xFFFFFFFF)

#ifndef SSPORTDRIVER
typedef char *                PSTRING; /* Pointer to string	*/
#endif
typedef UINT32                XLR_RETURN_CODE;
typedef UINT32                XLR_ERROR_CODE;
typedef UINT32                XLR_READ_STATUS;

#ifdef TARGET
typedef volatile PUCHAR    PREG8;
typedef volatile PUSHORT   PREG16;
typedef volatile PUINT32   PREG32;
typedef volatile UINT32    REG32;
typedef UINT64             BLOCKHEADER;
typedef BLOCKHEADER *      PBLOCKHEADER;
typedef UINT64             BLOCKNUM;      // Seperate type to allow partitioning
typedef BLOCKNUM *         PBLOCKNUM;

#define PASS   0
#define FAIL   1
typedef BOOLEAN FLAG;
#endif
  
#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

#define MAX_VIRTUAL_CHANNELS   16

//
// XLR typedefs
//
typedef UINT32         CMDSTATUS;

//For XLRGetMode and XLRSetMode.
typedef UINT32         SSMODE;
typedef UINT32 *       PS_SSMODE;

//For XLRSetFPDPMode().
typedef UINT32         FPDPMODE;
typedef UINT32         FPDPOP;

typedef struct _VERSION
{
   UCHAR    Major;
   UCHAR    Minor;
}S_VERSION, *PS_VERSION;

typedef struct _XLRVERSION
{
   S_VERSION   Firmware;
   S_VERSION   Monitor;
   S_VERSION   Xbar;
   S_VERSION   Ata;

   S_VERSION   Uata;
   char     FirmBuildDate[14];
}S_XLRVERSION, *PS_XLRVERSION;

typedef struct _XLRDBVERSION
{
   S_VERSION   PCB;
   S_VERSION   FPGA;
}S_XLRDBVERSION, *PS_XLRDBVERSION;

// MONVERSION is old structure, kept for monitor for backward compatibility
typedef struct _MONVERSION
{
   UINT32  FirmMajor;
   UINT32  FirmMinor;
   char     FirmBuildDate[12];
   UINT32  XbarMajor;
   UINT32  XbarMinor;
}S_MONVERSION, *PS_MONVERSION;

typedef struct _VIRTUAL_CHANNEL
{
   UINT32   u32NumChannelsBound;
   UINT32   u32NumAddrChunks;
   UINT32   u32BaseRange;
   size_t   BaseAddr[MAX_VIRTUAL_CHANNELS];
}S_VIRTUAL_CHANNEL, *PS_VIRTUAL_CHANNEL;

//For Bank Mode
typedef enum _BANK
{
   BANK_A,
   BANK_B,
   BANK_INVALID
} E_BANK;


//
// For the XLRErase command.
//
enum _SS_OWMODE
{
   SS_OVERWRITE_NONE = 0,
   SS_OVERWRITE_RANDOM_PATTERN = 1,
   SS_OVERWRITE_RW_PATTERN = 2,
   SS_OVERWRITE_DIRECTORY = 3,
   SS_OVERWRITE_PARTITION = 4
};

typedef enum _SS_OWMODE SS_OWMODE;

//
// Self-test level definition list
//
enum _SS_SELFTEST
{ 
   XLR_BIST_PCI,                   // test communications with PLX
   XLR_BIST_BUFFER,                // memory test on StreamStor buffer
   XLR_BIST_DISK0,                 // read/write verify test on disk drive
   XLR_BIST_DISK1,                 // read/write verify test on disk drive
   XLR_BIST_DISK2,                 // read/write verify test on disk drive
   XLR_BIST_DISK3,                 // read/write verify test on disk drive
   XLR_BIST_DISK4,                 // read/write verify test on disk drive
   XLR_BIST_DISK5,                 // read/write verify test on disk drive
   XLR_BIST_DISK6,                 // read/write verify test on disk drive
   XLR_BIST_DISK7,                 // read/write verify test on disk drive
   XLR_BIST_ALL,                   // all self tests except the loopback test
   XLR_NUM_BISTLEVELS              // number of self-test levels
}; 

typedef enum _SS_SELFTEST SS_SELFTEST;

//
// The B_SS_OPTIONS enumeration defines bit positions of various recorder options.
// These enum values are referenced in xlrapi.h and xlrpci.h and are designed to 
// keep coherency between API and firmware.  
//
// NOTE:  For legacy reasons, when adding a new enum value, append it rather than 
//        insert it.  Also note that this enum should not exceed 31 since options 
//        are currently stored as bits in one 32-bit integer.
//
enum  B_SS_OPTIONS
{
   B_OPT_DRIVESTATS,               // Enable drive statistics gathering
   B_OPT_SKIPCHECKDIR,             // Skip the directory check.
   B_OPT_RESERVE_END_BLOCK,        // Reserve space at end of recording.
   B_OPT_RECORDWAIT,               // Set wait on the waiting recorder.
   B_OPT_PLAYARM,                  // Enables "play arm" feature so that a play request will not
                                   // start sending data until a PLAY_TRIGGER command.
   B_OPT_REALTIMEPLAYBACK,         // Replace missing blocks with pattern
   B_OPT_FSMAPPED,                 // Use fixed map FS when recording 
   B_OPT_FPDPSYNCARM,              // Used for Start on Sync 
   B_OPT_COMPATDIR                 // Directory compatiblity mode
};
#if( GEN2 | GEN3 | GEN4 )
typedef PUCHAR P_ATATYPE;
typedef UCHAR ATATYPE;
#else
// GEN5 ATA registers are now 32 bits
typedef PUINT32  P_ATATYPE;
typedef UINT32   ATATYPE;
#endif

#define BITISSET(x,y)   ((x)&(1<<(y)))
#define BITISCLR(x,y)   ((~(x))&(1<<(y)))
#define SETBIT(x,y)     ((x) |= (1<<(y)))
#define CLRBIT(x,y)     ((x) &= (~(1<<(y))))
#define FLIPBIT(x,y)    ((x) ^= (1<<(y)))

#ifndef WIN32
#ifndef TARGET
# define IN
# define OUT
# ifndef _WINDRVR_H_
//typedef void *HANDLE;
# endif
//typedef HANDLE *PHANDLE;
#endif
#endif

typedef struct _FLASHSECT_STRUCT
{
   USHORT   Id;
   PUSHORT  Addr;
   UINT32     Size;
} FLASHSECT_STRUCT;

#endif
