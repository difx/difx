#ifndef XLRAPI_H
#define XLRAPI_H
//
// Header file for StreamStor API functions
//
// Copyright 2008 Conduant Corporation
//
//***********************************************************
// $Id: //streamstor/include/xlrapi.h#178 $
//
#ifdef WIN32
#include <windows.h>
#endif
#include "xlrtypes.h"
#include "xlrdbcommon.h"

#ifdef WIN32
#  ifdef SSAPI_EXPORTS
#    define XLREXPORT extern "C"
#  else
#    ifdef __cplusplus
#      define XLREXPORT extern "C" __declspec(dllimport)
#    else
#      define XLREXPORT __declspec(dllimport)
#    endif
#  endif
#else
#  define XLREXPORT   extern
#endif

#if defined( WIN32 )
# if defined( NOSTDCALL )
#  define XLRCALLTYPE
# else
#  define XLRCALLTYPE  __stdcall
# endif
#else
# define XLRCALLTYPE
#endif


#define XLR_SUCCESS           0     // API Function succeeded
#define XLR_FAIL              1     // API Function failed
#define XLR_DATECODE_LENGTH   12    // Character buffer length required for version datecode
#define XLR_VERSION_LENGTH    8     // Character buffer length required for version number
#define XLR_ERROR_LENGTH      81    // Character buffer length required for error string
#define XLR_WRAP_DISABLE      0     // Switch value to disable data wrapping in XlrRecord()
#define XLR_WRAP_ENABLE       1     // Switch value to enable data wrapping in XlrRecord()
#define XLR_MAX_NAME          24    // Length of maximum name string
#define XLR_MAX_VRS           4     // Maximum number of configurable virtual recorders
#define XLR_MAX_DRIVENAME     44
#define XLR_MAX_DRIVESERIAL   24
#define XLR_MAX_DRIVEREV      12
#define XLR_MAX_UDIR_LENGTH   10485760  // Maximum size of a user directory is 
                                        //10 megabyte.
#define XLR_MAX_PARTITIONS    2048
#define XLR_PARTOVERHEAD_BYTES 12320768   // Partition overhead

#define XLR_MAX_SMARTVALUES   30
#define XLR_MAX_IP_ADDR       24    // Character buffer length for IP Address.

#define XLR_MASTER_DRIVE      0
#define XLR_SLAVE_DRIVE       1

#define MAX_NUM_CHANNELS      32
// FIFO mode definitions
#define XLR_MODE_DEFAULT      0     // Default recorder mode
#define XLR_MODE_FIFO         1     // Normal FIFO mode (destructive reads)

// Read status return values
#define XLR_READ_COMPLETE  0
#define XLR_READ_ERROR     1
#define XLR_READ_WAITING   2
#define XLR_READ_RUNNING   3

// Data Recovery modes
#define SS_RECOVER_POWERFAIL  0
#define SS_RECOVER_OVERWRITE  1
#define SS_RECOVER_UNERASE    2

// XLRSetMode modes
#define SS_MODE_SINGLE_CHANNEL      30
#define SS_MODE_PASSTHRU            31
#define SS_MODE_FORK                32
#define SS_MODE_MULTI_CHANNEL       33
#define SS_MODE_REC_AND_PLAY        7
#define SS_MODE_CHANCOPY            34
#define SS_MODE_BLOCKDUMP           35

// Obsolete modes
#define SS_MODE_PCI             0  // default mode
#define SS_MODE_EXT             1  // Data received and played over external port
#define SS_MODE_READ_EXT        2  // Data passed from external to PCI (no disk)
#define SS_MODE_WRITE_EXT       3  // Data passed from PCI to external (no disk)
#define SS_MODE_PCI_FORK        4  // Data forked from PCI to disk and ext port
#define SS_MODE_EXT_FORK        5  // Data forked from ext port to disk and PCI
#define SS_MODE_EXT_TO_PCI_FIFO 7  // FIFO data through disk from external to PCI
#define SS_MODE_EXT_FORK_TOP    8  // Data forked from front port to disk and top port
#define SS_MODE_EXT_FORK_FRT    9  // Data forked from top port to disk and front port
#define SS_MODE_EXT_PASS_TOP    10 // Pass through from front port to top (no disk)
#define SS_MODE_EXT_PASS_FRT    11 // Pass through from top port to front port (no disk)

//
// Options for the XLRSetOption() and XLRGetOption commands.
// NOTE:  These SS_OPT_ names must reference an enumerated B_ name defined 
// in xlrtypes.h so that the SS_OPT_ names will always match the bits 
// defined for firmware.
//
//

#define SS_OPT_DRVSTATS          (1<<B_OPT_DRIVESTATS)   
#define SS_OPT_SKIPCHECKDIR      (1<<B_OPT_SKIPCHECKDIR) 
#define SS_OPT_RESERVE_END_BLOCK (1<<B_OPT_RESERVE_END_BLOCK) 
#define SS_OPT_RECORDWAIT        (1<<B_OPT_RECORDWAIT)  
#define SS_OPT_PLAYARM           (1<<B_OPT_PLAYARM)
#define SS_OPT_REALTIMEPLAYBACK  (1<<B_OPT_REALTIMEPLAYBACK)
#define SS_OPT_FSMAPPED          (1<<B_OPT_FSMAPPED)
#define SS_OPT_COMPATDIR         (1<<B_OPT_COMPATDIR)
#define SS_ALL_OPTIONS           0xFFFFFFFF     

// Modes for the XLRSetFPDPMode() command.
#define SS_FPDP_RECVMASTER          0     // Terminated.  This is the default.
#define SS_FPDP_RECV                1     // Non-terminated.
#define SS_FPDP_XMIT                2     // does not drive clock
#define SS_FPDP_XMITMASTER          3     // normal transmit
#define SS_FPDP_RECVMASTER_CLOCKS   4     // Like RECVMASTER but drives clocks.

//
// Options for the XLRSetFPDPMode() command.  These must have corresponding
// values in xlrpci.h  Option values must equate to a single set bit so that
// they can be OR'ed together to set multiple options.
//

#define SS_OPT_FPDPNRASSERT     0x00001   // Causes FPDP port to assert
                                         // "Not Ready" when in external port
                                         // mode.
#define SS_OPT_FPDPEXTCONN      0x00002   // Use the connector on the card 
                                         // bracket  (external access)
#define SS_OPT_FPDPSYNC         0x00004   // Wait for synch pulse before 
                                         // recording
#define SS_OPT_FPDPSTROB        0x00008   // Enable data strobe clock (TTL stobe
                                         // signals).  Default is pstrobe clock
                                         // (PECL strobe signals).
#define SS_OPT_FPDPNOPLL        0x00010   // No Phase Lock Loop - allows 0 - 
                                         // 40 MHz operation without this 
                                         // option permitted speeds are 
                                         // 10 - 50 MHz.
#define SS_OPT_FPDPSINGLEFRAME  0x00020   // Single frame data format while
                                         // recording and playing back.
#define SS_OPT_FPDPEVENTLOG     0x00040   // Event Logging - sync pulse triggers
                                         // the recording of the position w/in
                                         // the data where pulse occured
#define SS_OPT_FPDPSYNCARM      0x00080   // Preps Recording but waits for FPDP sync
                                         // Recording of data will not occur until
                                         // XLRArmFPDP call is made and then sync is
                                         // received
//
// Options for XLRArmPortForSync() command to control the Start on Sync functionality
//
#define SS_OPT_CLEAR_START_ON_SYNC     0x0000   // turns off waiting for start on sync pulse
#define SS_OPT_ARM_START_ON_SYNC       0x0001   // sets port to wait for Start on Sync pulse before recording data
#define SS_OPT_GENERATE_START_ON_SYNC  0x0002   // Generates an outgoing Start on sync pulse on the specified port


#define SS_OPT_EVENT_SYNC_RISE  0x00000100  // Capture events on rising edge of SYNC* signal
#define SS_OPT_EVENT_SYNC_FALL  0x00000200  // Capture event on falling edge of SYNC* signal
#define SS_OPT_EVENT_PIO1_RISE  0x00000400  // Capture event on rising edge of PIO1 signal
#define SS_OPT_EVENT_PIO1_FALL  0x00000800  // Capture event on falling edge of PIO1 signal
#define SS_OPT_EVENT_PIO2_RISE  0x00001000  // Capture event on rising edge of PIO2 signal
#define SS_OPT_EVENT_PIO2_FALL  0x00002000  // Capture event on falling edge of PIO2 signal
#define SS_OPT_EVENT_UNDEFINED  0x00004000
#define SS_OPT_EVENT_FULLBLOCK  0x00008000  // Block event capture if FIFO goes full

#define SS_OPT_FPDPALL (SS_OPT_FPDPNRASSERT|SS_OPT_FPDPEXTCONN|SS_OPT_FPDPSYNC \
                       |SS_OPT_FPDPSTROB|SS_OPT_FPDPNOPLL|SS_OPT_FPDPSINGLEFRAME \
                       |SS_OPT_FPDPEVENTLOG|SS_OPT_FPDPSYNCARM \
                       |SS_OPT_EVENT_SYNC_RISE|SS_OPT_EVENT_SYNC_FALL \
                       |SS_OPT_EVENT_PIO1_RISE|SS_OPT_EVENT_PIO1_FALL \
                       |SS_OPT_EVENT_PIO2_RISE|SS_OPT_EVENT_PIO2_FALL \
                       |SS_OPT_EVENT_FULLBLOCK)

// Bank Mode definitions
enum _SSBANKMODE {
   SS_BANKMODE_NORMAL   = 0,        // Puts the system into bank mode
   SS_BANKMODE_DISABLED = 1,        // Puts the system into non-bank mode
   SS_BANKMODE_AUTO_ON_FULL = 2,    // Puts the system into bank mode with
                                    // auto-continue when bank is full.
   SS_BANKMODE_AUTO_ON_SYNC = 3     // Puts the system into bank mode
                                    // switches bank when recording bank is
                                    // near full and sync pulse is received
};
typedef enum _SSBANKMODE S_BANKMODE;

// Sample Mode definitions
enum _SS_SAMPLEMODE {
   SS_SAMPLEMODE_DISABLED = 0,      // Disables sampling
   SS_SAMPLEMODE_NORMAL   = 1,      // Enables normal sample mode 
   SS_SAMPLEMODE_PASSTHRU = 2       // Enables pass-thru sample mode
};
typedef enum _SS_SAMPLEMODE S_SAMPLEMODE;


#define XLR_LABEL_LENGTH   256   // Maximum length of label that can be
                                 // written using XLRSetLabel command.

#define MAX_EVENTS         512

// Chassis Types
#define UNKNOWN_CHASSIS_TYPE  0  // For chassis types prior to the bank
                                 // enabled type.  Also for unknown chassis
                                 // types

#define TK200                 1  // TK200 chassis (supports bank switching)

#define LTX                   2  // LTX chassis


// For collecting drive statistics.
#define XLR_MAXBINS        8     // Number of bins to use when collecting
                                 // frequency data for GetDriveStats.

// To indicate that XLRGetDiskLength could not return a valid value.
#define SS_UNDEFINED_LENGTH   1

// Applications that can be loaded.
#define SS_DEFAULT_APP        0
#define SS_APP_1              1

// Playback buffer status values.
#define SS_PBS_IDLE     0
#define SS_PBS_FILLING  1
#define SS_PBS_FULL     2
#define SS_PBS_PLAYING  3
#define SS_PBS_UNKNOWN  10

// XLRNetOpen device type values.
#define SS_NET_TYPE_LTX2   1
#define SS_NET_TYPE_REMOTE 2

// DEVSTATUS replaced by STREAMSTORSTATUS and VRSTATUS
//
typedef struct _DEVSTATUS
{
   BOOLEAN SystemReady;             // The system is ready for read/write
   BOOLEAN BootmonReady;            // The flash monitor is ready
   BOOLEAN Recording;               // At least one VR is recording
   BOOLEAN Playing;                 // System is playing data.
   BOOLEAN Reserved1;               // unused 
   BOOLEAN Reserved2;               // unused
   BOOLEAN Reserved3;               // unused
   BOOLEAN Reserved4;               // unused
   BOOLEAN RecordActive[4];         // VR is recording
   BOOLEAN ReadActive[4];           // VR is currently reading
   BOOLEAN FifoActive;              // Recorder is in FIFO mode
   BOOLEAN DriveFail;               // Drive failure
   UINT32 DriveFailNumber;          // Drive number of failed drive (valid only if DriveFail set)
   BOOLEAN SysError;                // A system error occurred
   UINT32 SysErrorCode;             // System error code
   BOOLEAN Booting;                 // The system is booting 
   BOOLEAN FifoFull;                // The fifo overflowed
   BOOLEAN Overflow[4];             // True if recorder has reach capacity during record.
   BOOLEAN PrefetchComplete;        // Initial prefetch of data from Read Command has completed
} S_DEVSTATUS, *PS_DEVSTATUS;

typedef struct _STREAMSTORSTATUS
{
   BOOLEAN SystemReady;
   BOOLEAN BootmonReady;
   BOOLEAN Booting;
   BOOLEAN DriveFailed;
   BOOLEAN SysErrorOccurred;
   UINT32  DriveFailNumber;
   UINT32  SysErrorCode;
} S_STREAMSTORSTATUS, *PS_STREAMSTORSTATUS;


typedef struct _VRSTATUS
{
   BOOLEAN Ready;
   BOOLEAN Recording;               // VR is recording
   BOOLEAN Playing;                 // VR is playing data
   BOOLEAN Reading;                 // VR is in read mode (may also be playing)
   BOOLEAN FifoActive;              // VR is in FIFO mode
   BOOLEAN RecordFifoFull;          // VR record buffer is full, possible loss of recording data
   BOOLEAN StorageOverflow;         // True if recorder has reached capacity during record.
   BOOLEAN PrefetchComplete;        // Initial prefetch of data from Read Command has completed
} S_VRSTATUS, *PS_VRSTATUS;


typedef struct _DEVINFO
{
   char       BoardType[XLR_MAX_NAME];
   UINT32     SerialNum;           // Board serial number
   UINT32     NumDrives;           // Number of drives attached
   UINT32     NumBuses;            // Number of ATA buses in use
   UINT32     MaxBandwidth;        // Reserved
   UINT32     PciBus;              // PCI bus number
   UINT32     PciSlot;             // PCI slot number
   UINT32     NumExtPorts;         // Number of external ports
   UINT64     TotalCapacity;       // Total capacity expressed in system pages
   UINT64     TotalCapacityBytes;  // Total capacity expressed in bytes.
}S_DEVINFO, *PS_DEVINFO;

typedef struct _DRIVEINFO
{
   char     Model[XLR_MAX_DRIVENAME];      // Drive Model
   char     Serial[XLR_MAX_DRIVESERIAL];   // Drive serial number
   char     Revision[XLR_MAX_DRIVEREV];    // Drive firmware revision
   BOOLEAN  SMARTCapable;
   BOOLEAN  SMARTState;                    // Drive SMART state
   UINT64   Capacity;                      // Capacity in blocks (512 bytes)
}S_DRIVEINFO, *PS_DRIVEINFO;   

//
// For collection of drive statistics.  Need to declare one of thes
// for each bin.  See XLR_MAXBINS.
//
typedef struct _S_DRIVESTATS {
   UINT32 range;                           // end range of this bin (nanosecs)
   UINT32 count;                           // number of observations within 
                                          // the range.
} S_DRIVESTATS, *PS_DRIVESTATS; 
    
typedef struct _DIR
{
   BOOLEAN  Full;             // If FALSE (i.e. 0), system is not full 
                              // If TRUE  (i.e. 1), system is full
   BOOLEAN  WriteProtected;   // If FALSE (i.e. 0), system is not 
                              // write protected.
                              // If TRUE  (i.e. 1), system is write 
                              // protected.
   UINT64   Length;           // Total length recorded
   UINT64   AppendLength;     // Length of last data set
}S_DIR, *PS_DIR;

typedef struct _READDESC
{
   PUINT32   BufferAddr;
   UINT32    AddrHi;
   UINT32    AddrLo;
   UINT32    XferLength;
}S_READDESC, *PS_READDESC;


typedef struct _XLRSWREV
{
   char     ApiVersion[XLR_VERSION_LENGTH];
   char     ApiDateCode[XLR_DATECODE_LENGTH];
   char     FirmwareVersion[XLR_VERSION_LENGTH];
   char     FirmDateCode[XLR_DATECODE_LENGTH];
   char     MonitorVersion[XLR_VERSION_LENGTH];
   char     XbarVersion[XLR_VERSION_LENGTH];
   char     AtaVersion[XLR_VERSION_LENGTH];
   char     UAtaVersion[XLR_VERSION_LENGTH];
   char     DriverVersion[XLR_VERSION_LENGTH];
}S_XLRSWREV, *PS_XLRSWREV;

//
// Structure of items in the array of error messages and their
// corresponding error codes.
struct XLR_ERRMSGS
{
   char msg_string[XLR_ERROR_LENGTH];
      UINT32 msg_code;
};

//
// Structure that is populated by XLRGetPartitionInfo
//
typedef struct _PARTITIONINFO
{
   BOOLEAN     Partitioned;         // True if system is partitioned
   UINT32      NumPartitions;
   UINT32      SelectedPartition;    
   UINT64      SpaceAllocatedBytes; 
   UINT64      SpaceAvailableBytes; 
   UINT64      PartitionCapacityBytes;
} S_PARTITIONINFO, *PS_PARTITIONINFO;

//
// Structure that is populated by XLRGetDBInfo (daughter board information).
//
typedef struct _DBINFO
{
   UINT32     SerialNum;                       // Board serial number.
   char     PCBVersion[XLR_VERSION_LENGTH];  // Board version.
   char     PCBType[XLR_MAX_NAME];           // Type of board.
   char     PCBSubType[XLR_MAX_NAME];        // Board subtype
   char     FPGAConfig[XLR_MAX_NAME];        // Function type of the code
                                             // loaded in the FPGA
   char     FPGAConfigVersion[XLR_VERSION_LENGTH]; // Version of the code
                                                   // loaded in the FPGA
   UINT32     NumChannels;                     // number of channels on board.
   UINT32     Param[8];                        // Reserved.
} S_DBINFO, *PS_DBINFO;

//
// Structure used by XLRGetFrameLog
//
typedef struct _FRAMELOG
{
   UINT32   Type;
   UINT32   FrameNumber;
   UINT32   FrameLength;
}S_FRAMELOG, *PS_FRAMELOG;
// These need to match "SS_" versions in xlrapi.h
#define SS_FRAMELOG_TYPE_SHORTFRAME    (0)
#define SS_FRAMELOG_TYPE_LONGFRAME     (1)
#define SS_FRAMELOG_TYPE_LOGOVERFLOW   (2)
#define SS_FRAMELOG_TYPE_LOGEMPTY      (3)

// Structure that is populated by XLRReadSmartValues
typedef struct _SMARTVALUES
{
   UCHAR       ID;
   USHORT      Status;
   UCHAR       Current;
   UCHAR       Worst;
   UCHAR       raw[6];
   UCHAR       Reserved;
}S_SMARTVALUES, *PS_SMARTVALUES;

// Structure that is populated by XLRReadSmartThresholds
typedef struct _SMARTTHRESHOLDS
{
   UCHAR       ID;
   UCHAR       Threshold;
   UCHAR       Reserved[10];
}S_SMARTTHRESHOLDS, *PS_SMARTTHRESHOLDS;

//
// Structure that is populated by XLRGetEvents
//
typedef struct _EVENTS
{
   UINT32        Source;           // Source is bit significant - see bit defines below
   UINT32        AddressHigh;      // Upper 32 bits of location in bit stream where event occurred
   UINT32        AddressLow;       // Lower 32 bits of location in bit stream where event occurred
   UCHAR       Reserved[4];      // Reserved.
} S_EVENTS, *PS_EVENTS;

#define SS_B_FPDP_EVENT_SOURCE_PIO2    0
#define SS_M_FPDP_EVENT_SOURCE_PIO2    0x00000001
#define SS_B_FPDP_EVENT_SOURCE_PIO1    1
#define SS_M_FPDP_EVENT_SOURCE_PIO1    0x00000002
#define SS_B_FPDP_EVENT_SOURCE_SYNC    2
#define SS_M_FPDP_EVENT_SOURCE_SYNC    0x00000004

//
// Number and which channels recorded stucture
//
typedef struct _RECDCHANNELINFO
{
   UINT32  NumChannelsRecorded;         // Number of channels recorded on this StreamStor device
   UINT32  RecordedChannelNumber[MAX_NUM_CHANNELS];     //The channel numbers that are recored on this StreamStor device                                     
} S_RECDCHANNELINFO, *PS_RECDCHANNELINFO;

//
// Stucture that contains data about the SFPDP Link 
//
typedef struct _SFPDPSTATUS
{
      BOOLEAN PortOpticalEngPrsnt;        // optical energy is detected on this port by the SFPDP Daughter Board.
      BOOLEAN PortRXDataActive;           // data is currently passing through the RX side of this port.
      BOOLEAN PortTXDataActive;           // data is currently passing through the TX side of this port.
      BOOLEAN PortRcvProtocolOK;          // Link up and receiving SFPDP protocol correctly on this port.
      BOOLEAN PortNearEndFlowCtrlActive;  // near end flow control active on this port.
      BOOLEAN PortFarEndFlowCtrlActive;   // far end flow control active on this port.
      BOOLEAN PortCRCErrorDetected;       // a CRC Error has been detected on this SFPDP port at some time in the past.
      BOOLEAN PortCRCErrPrevFrame;        // a CRC Error was detected in the previous frame received on this SFPDP port.
} S_SFPDPSTATUS, *PS_SFPDPSTATUS;

//
// A structure of type S_BANKSTATUS is populated and returned to the
// caller where XLRGetBankStatus is called.  Note that all char type
// declarations must be at the beginning of the structure.
//

typedef struct _BANKSTATUS
{

   char Label[XLR_LABEL_LENGTH]; // The label on the drive module.

   UINT32  State;          // If ready, this drive module is ready (powered 
                           // up, etc.) and can be used immediately for 
                           // recording or playback.  Otherwise, this
                           // drive module is not ready.

   UINT32  Selected;       // If TRUE, the specified bank in 
                           // XLRGetBankStatus is the selected bank.

   UINT32  PowerRequested; // If TRUE, a request for power up has been
                           // received on this drive module.  Otherwise, a
                           // request has not been made.

   UINT32  PowerEnabled;   // If TRUE, this, this drive module has power.
                           // Otherwise, it has not been powered up.

   UINT32  MediaStatus;    // See below for valid values

   UINT32  WriteProtected; // If TRUE, this drive module is write 
                           // protected. Otherwise, this drive module 
                           // is not write protected.

   UINT32  ErrorCode;      // If 0, there is no error, otherwise 
                           // this value is an error code.
                              
   UINT32  ErrorData;      // For additional data (if required) related 
                           // to ErrorCode.

   UINT64   Length;        // Amount of data recorded on the drive set or
                           // (if bank mode) the currently selected drive
                           // module.

   UINT64  TotalCapacity;  // Total capacity expressed in system pages.

   UINT64  TotalCapacityBytes; // Total capacity expressed in bytes.
                             
} S_BANKSTATUS, *PS_BANKSTATUS;

typedef UINT32  S_LVDS_CONFIG[6];


// ** BANKSTATUS **
// MediaStatus
#define MEDIASTATUS_EMPTY           0     // Module is empty
#define MEDIASTATUS_NOT_EMPTY       1     // Module is not empty - but not full
#define MEDIASTATUS_FULL            2     // Module is filled to capacity
#define MEDIASTATUS_FAULTED         3     // Module is not empty, but faulted
                                          // must be erased before it can be used

// State
#define STATE_NOT_READY             0     // Module is not ready
#define STATE_TRANSITION            1     // Module is transitioning between ready and not ready
#define STATE_READY                 2     // Module is ready to be used


// Function prototypes
//------------------------------------------------------
//

XLREXPORT void XLRCALLTYPE 
XLRApiVersion( char * string );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRAppend( IN SSHANDLE xlrHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRCardReset ( UINT32 devIndex );

XLREXPORT void XLRCALLTYPE
XLRClose ( IN SSHANDLE xlrHandle );

XLREXPORT UINT32 XLRCALLTYPE
XLRDeviceFind ( void );

XLREXPORT size_t XLRCALLTYPE
XLRGetBaseAddr( IN SSHANDLE xlrHandle );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetBaseRange( IN SSHANDLE xlrHandle );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetBlockSize( IN SSHANDLE xlrHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDeviceInfo ( IN SSHANDLE xlrHandle, OUT PS_DEVINFO devInfo);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDeviceStatus( IN SSHANDLE xlrHandle, OUT PS_DEVSTATUS devStatus );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDirectory (IN SSHANDLE xlrHandle, OUT PS_DIR Dir );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDriveInfo (IN SSHANDLE device, IN UINT32 Bus, IN UINT32 MasterSlave,
                 OUT PS_DRIVEINFO driveInfo );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetErrorMessage( char *errstr, XLR_ERROR_CODE error );

XLREXPORT XLR_ERROR_CODE XLRCALLTYPE
XLRGetLastError( void );

XLREXPORT UINT64 XLRCALLTYPE
XLRGetLength( IN SSHANDLE xlrHandle );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetLengthHigh (IN SSHANDLE xlrHandle );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetLengthLow (IN SSHANDLE xlrHandle );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetLengthPages (IN SSHANDLE device );

XLREXPORT void XLRCALLTYPE
XLRGetLengthLowHigh (IN SSHANDLE device, PUINT32 low, PUINT32 high );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetNextBlock( IN SSHANDLE device, IN PVOID Buffer, PUINT32 Channel, PUINT32 Length, PVOID *Data );

XLREXPORT void XLRCALLTYPE 
XLRSdkVersion( char * string );

XLREXPORT size_t XLRCALLTYPE
XLRGetSystemAddr( IN SSHANDLE xlrHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetVersion( IN SSHANDLE xlrHandle, OUT PS_XLRSWREV swRev );

XLREXPORT PUINT32 XLRCALLTYPE
XLRGetWindowAddr( IN SSHANDLE xlrHandle );

XLREXPORT UINT32 XLRCALLTYPE
_XLRGetWindowAddr( IN SSHANDLE xlrHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLROpen ( IN UINT32 devIndex, OUT PSSHANDLE newHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRPlay ( IN SSHANDLE xlrHandle, IN PS_READDESC readData );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRPlayback( IN SSHANDLE device, IN UINT32 AddrHi, IN UINT32 AddrLo );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRPlaybackLoop( IN SSHANDLE device, IN UINT32 startHi, IN UINT32 startLo, 
   IN UINT32 lengthHi, IN UINT32 lengthLo);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRRead ( IN SSHANDLE xlrHandle, IN PS_READDESC readData );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadFifo ( IN SSHANDLE xlrHandle, IN PUINT32 Buffer, IN UINT32 Length, IN BOOLEAN Direct );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
_XLRRead ( IN SSHANDLE xlrHandle, IN PUINT32 BufferAddr, IN UINT32 AddrHi,
          IN UINT32 AddrLo, IN UINT32 XferLength );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadData ( IN SSHANDLE xlrHandle, IN PUINT32 BufferAddr, IN UINT32 AddrHi,
          IN UINT32 AddrLo, IN UINT32 XferLength );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadImmed ( IN SSHANDLE device, IN PS_READDESC readDesc );

XLREXPORT XLR_READ_STATUS XLRCALLTYPE
XLRReadStatus ( IN BOOLEAN Wait );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRRecord ( IN SSHANDLE xlrHandle, IN BOOLEAN WrapEnable, IN SHORT ZoneRange );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReset ( IN SSHANDLE xlrHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetFifoMode( IN SSHANDLE xlrHandle, IN UINT32 Mode );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetReadLimit( SSHANDLE xlrHandle, IN UINT32 ReadLimit );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRStop ( IN SSHANDLE xlrHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRWrite ( IN SSHANDLE xlrHandle, IN PS_READDESC writeData );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRWriteData ( IN SSHANDLE xlrHandle, IN PVOID BufferAddr, IN UINT32 XferLength );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetMode ( IN SSHANDLE device, IN UINT32 mode);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRGetMode ( IN SSHANDLE device, OUT PS_SSMODE mode_ptr);

XLREXPORT UINT32 XLRCALLTYPE 
XLRSkip ( IN SSHANDLE device, IN UINT32 skip, IN BOOLEAN Forward);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetOption ( IN SSHANDLE device, IN UINT32 options_to_set);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRClearOption ( IN SSHANDLE device, IN UINT32 options_to_clear);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRGetOption ( IN SSHANDLE device, IN UINT32 options_to_get, 
OUT PBOOLEAN options_on);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetDriveStats ( IN SSHANDLE device, IN S_DRIVESTATS driveStats[] );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRGetDriveStats ( IN SSHANDLE device, IN UINT32 Bus, IN UINT32 MasterSlave, 
  OUT PS_DRIVESTATS driveStats );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetFPDPMode ( IN SSHANDLE device, IN FPDPMODE mode, IN FPDPOP options );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetPortClock ( IN SSHANDLE device, IN UINT32 value );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetPortBaseClock ( IN SSHANDLE device, IN UINT32 Rdw, IN UINT32 Vdw, 
  IN UINT32 outputDivider);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRDeleteAppend( IN SSHANDLE device, IN UINT32 AddrHi, IN UINT32 AddrLo );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRTruncate( IN SSHANDLE device, IN UINT32 AddrHi, IN UINT32 AddrLo );

XLREXPORT UINT64 XLRCALLTYPE 
XLRGetPlayLength (IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetFillData ( IN SSHANDLE device, IN UINT32 pattern );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetBankMode( IN SSHANDLE device, IN S_BANKMODE mode );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSelectBank( IN SSHANDLE device, IN UINT32 bankID );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSelectVR( IN SSHANDLE device, IN UINT32 VRID );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetLabel( IN SSHANDLE device, IN char *label, IN UINT32 labelsize );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetLabel( IN SSHANDLE device, OUT char *label );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetWriteProtect ( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRClearWriteProtect ( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetBankStatus( IN SSHANDLE device, IN UINT32 bankID, 
      OUT PS_BANKSTATUS bankStatus );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSyncArm( IN SSHANDLE xlrDevice, IN BOOLEAN armed );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRRecoverData( IN SSHANDLE device, IN UINT32 Mode );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetPlaybackLength( IN SSHANDLE xlrDevice, IN UINT32 AddrHi, 
   IN UINT32 AddrLo );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRErase( IN SSHANDLE xlrDevice, IN SS_OWMODE mode );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetUserDir( IN SSHANDLE xlrDevice, IN PVOID udirPtr, UINT32 udirSize );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetUserDir( IN SSHANDLE xlrDevice, IN UINT32 XferLength, 
   IN UINT32 Offset, OUT PVOID udirPtr );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetUserDirAll( IN SSHANDLE xlrDevice, IN UINT32 udirTotalSize, 
   OUT PVOID udirPtr );

XLREXPORT UINT32 XLRCALLTYPE 
XLRGetUserDirLength( IN SSHANDLE xlrDevice );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetChassisType( IN SSHANDLE xlrDevice );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLREdit( IN SSHANDLE device, IN PS_READDESC readDesc );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRMountBank( IN SSHANDLE device, IN UINT32 bankId );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRDismountBank( IN SSHANDLE device, IN UINT32 bankId );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSelfTest( IN SSHANDLE device, IN SS_SELFTEST test );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetSwitchOver( IN SSHANDLE device ); 

XLREXPORT UINT64 XLRCALLTYPE 
XLRGetDiskLength( IN SSHANDLE device, IN UINT32 Bus, IN UINT32 MasterSlave );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRPlayTrigger( IN SSHANDLE device );

XLREXPORT DWORD XLRCALLTYPE 
XLRDiskRepBlkCount( IN SSHANDLE device, IN UINT32 Bus, IN UINT32 MasterSlave );

XLREXPORT DWORD XLRCALLTYPE 
XLRTotalRepBlkCount( IN SSHANDLE device);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadToPhy ( IN SSHANDLE xlrHandle, IN PS_READDESC readData );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetAppID( IN UINT32 Index, IN UINT32 appID );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRGetAppID ( IN UINT32 Index, OUT PUINT32 appID );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRGetPlayBufferStatus ( IN SSHANDLE device, OUT PUINT32 statPtr );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRDumpMemory ( IN SSHANDLE device, IN PUINT32 BufferAddr, 
   IN UINT32 Offset, IN UINT32 XferLength );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLREditData ( IN SSHANDLE device,
   IN PUINT32 BufferAddr, IN UINT32 AddrHi, IN UINT32 AddrLo,
   IN UINT32 XferLength );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRRetrieveEvents ( IN SSHANDLE device,
   IN UINT64 EventLog[] );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRArmFPDP ( IN SSHANDLE device );

XLREXPORT UINT64 XLRCALLTYPE XLRGetFIFOLength( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRNetOpen ( IN UINT32 devIndex,
   const char *address, IN USHORT port, OUT PSSHANDLE newHandle );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRNetCardReset( UINT32 NetType, 
   const char *address, IN USHORT port);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRClearChannels ( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRSelectChannel ( IN SSHANDLE device, 
   IN UINT32 channel );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRBindInputChannel ( IN SSHANDLE device,
   IN UINT32 channel );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRBindOutputChannel ( IN SSHANDLE device,
   IN UINT32 channel );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRGetRecordedChannelInfo ( IN SSHANDLE device, 
   OUT PS_RECDCHANNELINFO recordedChannelInfo );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRGetSFPDPInterfaceStatus ( IN SSHANDLE device, 
   IN UINT32 portNum, OUT PS_SFPDPSTATUS sfpdpStatus );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRPartitionCreate ( IN SSHANDLE device,
   IN UINT64 length );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRPartitionSelect ( IN SSHANDLE device,
   IN UINT32 partition );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRGetPartitionInfo ( IN SSHANDLE device,
   OUT PS_PARTITIONINFO partInfo );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRPartitionDelete ( IN SSHANDLE device, 
   IN UINT32 partition );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRSetDriveStandbyMode ( IN SSHANDLE
   device, IN BOOLEAN StandbyEnable );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDBInfo ( IN SSHANDLE device, OUT PS_DBINFO dbInfo );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRSetDBMode ( IN SSHANDLE device, IN FPDPMODE mode, IN FPDPOP options );

XLREXPORT UINT32 XLRCALLTYPE 
XLRGetEventsLength ( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetEvents ( IN SSHANDLE device, IN UINT32 bufSize, OUT PS_EVENTS events );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetDirectoryVersion( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetSampleMode( IN SSHANDLE device, IN UINT32 bufSize, IN UINT32 operation );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetSample( IN SSHANDLE device, IN UINT32 bufSize, IN PUINT32 pBuffer );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetDBBitPhase( IN SSHANDLE device, IN USHORT adjust, IN USHORT direction );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetDBEdge( IN SSHANDLE device, IN UINT32 edgeSelect );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetLVDSClock( IN SSHANDLE device, IN USHORT pll, IN S_LVDS_CONFIG sLVDS[] );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDriveTemp( IN SSHANDLE device, IN UCHAR bus, IN UCHAR dev, OUT PUINT32 temp );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadSmartValues( IN SSHANDLE device, OUT PUSHORT version, 
      OUT S_SMARTVALUES values[], IN UINT32 bus, IN UINT32 dev );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadSmartThresholds( IN SSHANDLE device, OUT S_SMARTTHRESHOLDS values[], 
      IN UINT32 bus, IN UINT32 dev );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRWriteDBSerial( IN SSHANDLE device, IN PUCHAR pBuffer, IN UINT32 Characters );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadDBSerial( IN SSHANDLE device, OUT PUCHAR pBuffer, IN UINT32 Characters );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDBSerialAvail( IN SSHANDLE device, OUT PUINT32 CharsAvail );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRFlushDBSerial( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetTime( IN SSHANDLE device, OUT PUINT32 Time );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetTime( IN SSHANDLE device, IN UINT32 Time );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRWriteDBReg32( IN SSHANDLE device, IN int Register, IN UINT32 Value );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRReadDBReg32( IN SSHANDLE device, IN int Register, OUT PUINT32 Value );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRDisableDrive( IN SSHANDLE device, IN UINT32 driveNumber );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLREnableDBAgc( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRDisableDBAgc( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetDBAgcParam( IN SSHANDLE device, IN UINT32 ParamID, IN UINT32 ParamValue );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetDBAgcParam( IN SSHANDLE device, IN UINT32 ParamID, OUT PUINT32 ParamValue );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRArmChannelForSync( IN SSHANDLE device, IN UINT32 Channel, IN UINT32 SyncOption );

XLREXPORT UINT64 XLRCALLTYPE
XLRGetWrapLength (IN SSHANDLE device);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRPartitionResize( IN SSHANDLE device, IN UINT64 dwRequestedBytes);

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetFrameLog( IN SSHANDLE device, OUT PS_FRAMELOG );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRSetFrameSize( IN SSHANDLE device, IN UINT32 FrameSizeBytes );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetFrameSize( IN SSHANDLE device );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetNumVirtualRecorders( IN SSHANDLE device );

XLREXPORT UINT32 XLRCALLTYPE
XLRGetNumSystemDrives (IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE 
XLRGetVRConfigXML ( IN SSHANDLE device, IN PVOID pBuffer, IN UINT32 size );

XLREXPORT UINT32 XLRCALLTYPE 
XLRGetVRConfigXMLSize ( IN SSHANDLE device );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetSelectedVRStatus( IN SSHANDLE device, OUT PS_VRSTATUS VRStatus );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetVRStatus( IN SSHANDLE device, IN UINT32 Vrid, OUT PS_VRSTATUS VRStatus );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE
XLRGetStreamStorStatus( IN SSHANDLE device, OUT PS_STREAMSTORSTATUS StreamStorStatus );

XLREXPORT XLR_RETURN_CODE XLRCALLTYPE XLRSelectCopyChannel ( IN SSHANDLE device, 
   IN UINT32 channel );
#endif      //XLRAPI_H

