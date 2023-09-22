/*
 * x3cTaskDefs.h
 *
 *  Created on: Feb 17, 2012
 *      Author: jtrier
 */

#ifndef X3CTASKDEFS_H_
#define X3CTASKDEFS_H_

// TASK TYPE NAME STRINGS
#define TASK_NAME_UNKNOWN   "UNKNOWN_TASK"
#define TASK_NAME_LGRMGR    "LOGGER_MGR"
#define TASK_NAME_MSGMGR    "MSG_MGR"
#define TASK_NAME_STATSMGR  "STATS_MGR"
#define TASK_NAME_USERMGR   "USER_MGR"
#define TASK_NAME_BUFMGR    "BUF_MGR"
#define TASK_NAME_ETHERNET  "ENET_MGR"		//Ethernet, RAW Packets
#define TASK_NAME_1394A     "1394A_MGR"
#define TASK_NAME_1394B     "1394B_MGR"
#define TASK_NAME_CANK      "CANK_MGR"
#define TASK_NAME_USB       "USB_MGR"
#define TASK_NAME_DGEN      "DGEN_MGR"
#define TASK_NAME_AVTCAMERA "AVTCAMERA_MGR"
#define TASK_NAME_CANP      "CANP_MGR"
#define TASK_NAME_HRFT      "HRFT_MGR"
#define TASK_NAME_GPU       "GPU_MGR"
#define TASK_NAME_MRR       "MRR_MGR"  		//Medium Range Radar
#define TASK_NAME_SOCKET    "SOCK_MGR" 		//Socket Mgr
#define TASK_NAME_SERIAL    "SERIAL_MGR" 	//Serial Port Mgr
#define TASK_NAME_PTGCAMERA "PTGCAMERA_MGR" //Point Grey Camera Mgr
#define TASK_NAME_GPS       "GPS_MGR"		//GPS
#define TASK_NAME_IDC       "IDC_MGR"		//IBEO
#define TASK_NAME_IPCAM     "IPCAM_MGR" 	//IP Camera, Point Grey Cricket.
#define TASK_NAME_PCAP      "PCAP_MGR"      //Ethernet, PCAP formatted
#define TASK_NAME_OTHER     "OTHER_TASK"

#define TASK_NAME_SIMXMGR   "SIMEXEC_MGR"

//Identifies the TYPE of task,
typedef enum _TASK_TYPE_
{
    TASK_TYPE_UNKNOWN=0,
    TASK_TYPE_LGRMGR,
    TASK_TYPE_MSGMGR,
    TASK_TYPE_STATSMGR,
    TASK_TYPE_USERMGR,
    TASK_TYPE_BUFMGR,
    TASK_TYPE_SIMXMGR,          // RESIM_BUILD

    // DSSC Interfaces
    TASK_TYPE_DSSC_INTERFACES = 30,
    TASK_TYPE_GENERIC_PLAYER,
    TASK_TYPE_ASC_PLAYER,
    TASK_TYPE_CAN_PLAYER,
    TASK_TYPE_AVI_PLAYER,
    TASK_TYPE_CAM_PLAYER,
    TASK_TYPE_BAN_PLAYER,
    TASK_TYPE_GEN5PGM_PLAYER,
    TASK_TYPE_IDC_PLAYER,

    TASK_TYPE_INTERFACES=50,    // 50 Not really a task - it separates IFs from others
    TASK_TYPE_IF_ETHERNET,      // 51 Ethernet, RAW Packets
    TASK_TYPE_IF_CANK,          // 52 CAN Kvaser
    TASK_TYPE_IF_USB,			// 53
    TASK_TYPE_IF_1394A,         // 54
    TASK_TYPE_IF_1394B,         // 55
    TASK_TYPE_IF_DGEN,          // 56 Data Generator
    TASK_TYPE_IF_AVTCAMERA,     // 57 AVT Camera
    TASK_TYPE_IF_CANP,          // 58 CAN PEAK/Grid Connect
    TASK_TYPE_IF_HRFT,          // 59 FOI HRFT Bus Master DMA Hardware
    TASK_TYPE_IF_GPU,           // 60
    TASK_TYPE_IF_MRR,           // 61 Denso Medium Range Radar
    TASK_TYPE_IF_XML,           // 62 XML
    TASK_TYPE_IF_SOCKET,		// 63 Socket Listener
    TASK_TYPE_IF_SERIAL,		// 64 Serial Port
    TASK_TYPE_IF_PTGCAMERA,     // 65 Point Grey Camera
    TASK_TYPE_IF_GPS,           // 66 GPD
    TASK_TYPE_IF_IDC,           // 67 Ibeo IDC
    TASK_TYPE_IF_IPCAM,         // 68 IP Camera, Point Grey Cricket.
    TASK_TYPE_IF_PCAP,          // 69 Ethernet, PCAP formatted
    TASK_TYPE_OTHER,            // 70
    TASK_TYPE_IF_USER=256
} LGR_TASK_TYPE;

//What is the Task currently doing?
typedef enum _TASK_STATE_
{
    TASK_STATE_UNKNOWN   = 0,
    TASK_STATE_INIT,
    TASK_STATE_EXIT,
    TASK_STATE_IDLE,
    TASK_STATE_RUNNING,
    TASK_STATE_PAUSED,
    TASK_STATE_ERROR,
    TASK_STATE_FINISHED,
    TASK_STATE_LAST
} LGR_TASK_STATE;


#endif /* X3CTASKDEFS_H_ */
