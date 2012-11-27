#ifndef XLRDBCOMMON_H
#define XLRDBCOMMON_H
/*******************************************************************
 *
 *    DESCRIPTION:  Common API constants that are unique to AMAZON 
 *    daughter boards.
 *                  
 *    IMPORTANT:  This file should contain ONLY constants.
 *       Please adhere to these ranges when selecting values:
 *           1 to 999:  Use for FPDP constants. 
 *       1000 to 1999:  Use for LVDS16 constants.
 *       2000 to 2999:  Use for SFPDP constants.
 *******************************************************************/
// $Id: //streamstor/include/xlrdbcommon.h#34 $
//
//--------------------------------------------------------------
// Generic daughter board temperature sensor information
#define SS_DBPARAM_TEMPLOCAL        0
#define SS_DBPARAM_TEMPREMOTE       1
#define SS_DBPARAM_TEMPSTATUS       2
#define SS_DBPARAM_TEMPCONFIG       3
#define SS_DBPARAM_TEMPUNITS        4
#define SS_DBPARAM_TEMPLOCALMAX     5
#define SS_DBPARAM_TEMPLOCALMIN     6
#define SS_DBPARAM_TEMPREMOTEMAX    7
#define SS_DBPARAM_TEMPSPARE        8
#define SS_DBPARAM_TEMPREMOTEMIN    9
#define SS_DBPARAM_TEMPDEVID        10
#define SS_DBPARAM_TEMPDIEREV       11

// FPDP settings for the XLRSetPortClock() command.
#define SS_PORTCLOCK_6MHZ           0
#define SS_PORTCLOCK_8MHZ           1
#define SS_PORTCLOCK_10MHZ          2
#define SS_PORTCLOCK_11MHZ          3  // 11.4 MHz
#define SS_PORTCLOCK_13MHZ          4  // 13.33 MHz
#define SS_PORTCLOCK_16MHZ          5
#define SS_PORTCLOCK_20MHZ          6
#define SS_PORTCLOCK_25MHZ          7  // 26.56 MHz
#define SS_PORTCLOCK_26MHZ          8  // 26.66 MHz
#define SS_PORTCLOCK_32MHZ          9
#define SS_PORTCLOCK_40MHZ          10
#define SS_PORTCLOCK_50MHZ          11
#define SS_PORTCLOCK_51MHZ          12 // 50.04 MHz
#ifdef TARGET
  // see xlrdiag.h for Hidden port clock settings 13-15
  #define SS_PORTCLOCK_55MHZ        13
  #define SS_PORTCLOCK_60MHZ        14
  #define SS_PORTCLOCK_65MHZ        15
  #define SS_PORTCLOCK_62_5MHZ      16  
  #define SS_PORTCLOCK_TOTAL        17
#endif //TARGET

// LVDS16 settings for XLRSetPortClock() command.
#define SS_LVDSCLOCK_20MHZ          100
#define SS_LVDSCLOCK_31_25MHZ       101
#define SS_LVDSCLOCK_62_5MHZ        102
#define SS_LVDSCLOCK_95MHZ          103
#define SS_LVDSCLOCK_100MHZ         104
#define SS_LVDSCLOCK_125MHZ         105
#define SS_LVDSCLOCK_150MHZ         106
#define SS_LVDSCLOCK_160MHZ         107
#define SS_LVDSCLOCK_190MHZ         108
#define SS_LVDSCLOCK_200MHZ         109
#define SS_LVDSCLOCK_TOTAL          10

//--------------------------------------------------------------
// FPDP operation modes
#define SS_FPDPMODE_DEFAULT         (20)
#define SS_FPDPMODE_FIRST           (SS_FPDPMODE_DEFAULT)       // Used to check for valid value
#define SS_FPDPMODE_RECVM           (21)   // Terminated
#define SS_FPDPMODE_RECV            (22)   // Non terminated
#define SS_FPDPMODE_XMIT            (23)   // Does not drive clock
#define SS_FPDPMODE_XMITM           (24)   // Normal xmit
#define SS_FPDPMODE_RECVM_CLOCKS    (25)   // RECVM but drives clocks.
#define SS_FPDPMODE_LAST            (SS_FPDPMODE_RECVM_CLOCKS)  // Used to check for valid value
 
//--------------------------------------------------------------
// FPDPII operation options
#define SS_DBOPT_FPDPSTROB          (0x100)// Enable data strobe clock 
                                           // (TTL strobe signals).  Default
                                           // is pstrobe clock (PECL strobe 
                                           // signals).
#define SS_DBOPT_FPDP2DISABLE       (0x200)// Disables FPDP2 mode
#define SS_DBOPT_FPDPNRASSERT       (0x400)// Rcvr asserts NRDY to hold bus
                                           // at startup.
#define SS_DBOPT_FPDPALL            (SS_DBOPT_FPDPSTROB|SS_DBOPT_FPDP2DISABLE| \
                                     SS_DBOPT_FPDPNRASSERT)

//--------------------------------------------------------------
// LVDS16 operation modes
#define SS_LVDS16MODE_DEFAULT       (1000)
#define SS_LVDS16MODE_FIRST         (SS_LVDS16MODE_DEFAULT)   // Used to check for valid value
#define SS_LVDS16MODE_RECV          (1001)  // Non terminated
#define SS_LVDS16MODE_XMIT          (1002)  // Does not drive clock
#define SS_LVDS16MODE_LAST          (SS_LVDS16MODE_XMIT)      // Used to check for valid value

//--------------------------------------------------------------
// LVDS16 operation options
#define B_OPT_LVDS_DATAVALID_GLOBAL    0
#define B_OPT_LVDS_DATAVALID_RS422     1
#define B_OPT_LVDS_FLOWCONTROL         31

#define SS_DBOPT_LVDS16_DATAVALID_GLOBAL  0x1
#define SS_DBOPT_LVDS16_DATAVALID_RS422   0x2
#define SS_DBOPT_LVDS16_FLOWCONTROL       0x80000000

#define SS_DBOPT_LVDS16ALL          (SS_DBOPT_LVDS16_DATAVALID_GLOBAL|SS_DBOPT_LVDS16_DATAVALID_RS422| \
                                     SS_DBOPT_LVDS16_FLOWCONTROL)

//--------------------------------------------------------------
// LVDS16 PLL values
#define SS_LVDS_RECORD_CLOCK_0      0
#define SS_LVDS_RECORD_CLOCK_1      1
#define SS_LVDS_RECORD_CLOCK_2      2
#define SS_LVDS_RECORD_CLOCK_3      3

//--------------------------------------------------------------
// Serial FPDP (SFSDP) operation modes
#define SS_SFPDPMODE_DEFAULT        (2000)
#define SS_SFPDPMODE_FIRST          (SS_SFPDPMODE_DEFAULT) // Used to check for valid value
#define SS_SFPDPMODE_NORMAL         (2001)   
#define SS_SFPDPMODE_SYNCFRAME      (2002)
#define SS_SFPDPMODE_LAST           (SS_SFPDPMODE_SYNCFRAME)   // Used to check for valid value
//--------------------------------------------------------------
// Serial FPDP (SFPDP) operation options - these should be defined with unique bits
#define SS_DBOPT_SFPDPNRASSERT      (0x1) // Rcvr asserts NRDY to hold bus at startup.
#define SS_DBOPT_SFPDP_NRASSERT     (0x1) // Added to match style
#define SS_DBOPT_SFPDP_CRC_ENABLE   (0x2) // CRC enable for all SFPDP channels
#define SS_DBOPT_SFPDP_CRC_DISABLE  (0x4)
#define SS_DBOPT_SFPDP_FLOWCTL_ENABLE  (0x8)    // Flow Control enable for all SFPDP channels -default
#define SS_DBOPT_SFPDP_FLOWCTL_DISABLE (0x10)
#define SS_DBOPT_SFPDP_FRAMELOG_ENABLE (0x20)   // Enable frame error logging
#define SS_DBOPT_SFPDP_FRAME_ENABLE    (0x40)   // Enable frame size checking and truncation based on frame size register and SYNC
#define SS_DBOPT_SFPDP_TRUNCATE_ENABLE (0x80)   // Enable frame truncation
#define SS_DBOPT_SFPDP_NRINVERT        (0x100)  // Invert the default polarity of NRASSERT
#define SS_DBOPT_SFPDPALL              (SS_DBOPT_SFPDP_NRASSERT\
                                       |SS_DBOPT_SFPDP_CRC_ENABLE\
                                       |SS_DBOPT_SFPDP_CRC_DISABLE\
                                       |SS_DBOPT_SFPDP_FLOWCTL_ENABLE\
                                       |SS_DBOPT_SFPDP_FLOWCTL_DISABLE\
                                       |SS_DBOPT_SFPDP_FRAMELOG_ENABLE\
                                       |SS_DBOPT_SFPDP_FRAME_ENABLE\
                                       |SS_DBOPT_SFPDP_TRUNCATE_ENABLE\
                                       |SS_DBOPT_SFPDP_NRINVERT\
                                       )
//--------------------------------------------------------------
// CameraLink operation modes
#define SS_CAMLINKMODE_DEFAULT        (2000)
#define SS_CAMLINKMODE_FIRST          (SS_CAMLINKMODE_DEFAULT) // Used to check for valid value
#define SS_CAMLINKMODE_NORMAL         (2001)   // Only valid mode for CameraLink from API
#define SS_CAMLINKMODE_LAST           (SS_CAMLINKMODE_NORMAL)   // Used to check for valid value
//--------------------------------------------------------------
// Serial FPDP (CAMLINK) operation options - these should be defined with unique bits
#define SS_DBOPT_CAMLINKNRASSERT      (0x1) // Rcvr asserts NRDY to hold bus at startup.
#define SS_DBOPT_CAMLINK_CRC_ENABLE   (0x2) // CRC enable for all CAMLINK channels
#define SS_DBOPT_CAMLINK_CRC_DISABLE  (0x4) // CRC disable for all CAMLINK channels
#define SS_DBOPT_CAMLINK_FLOWCTL_ENABLE  (0x8) // Flow Control enable for all CAMLINK channels -default
#define SS_DBOPT_CAMLINK_FLOWCTL_DISABLE (0x10) // Flow Control disable for all CAMLINK channels- Could be dangerous to disable flow control 

#define SS_DBOPT_CAMLINKALL           (SS_DBOPT_CAMLINKNRASSERT|SS_DBOPT_CAMLINK_CRC_ENABLE| \
                                     SS_DBOPT_CAMLINK_CRC_DISABLE|SS_DBOPT_CAMLINK_FLOWCTL_ENABLE| \
                                     SS_DBOPT_CAMLINK_FLOWCTL_DISABLE)   

#define SS_DBOPT_ALL                (SS_DBOPT_FPDPALL|SS_DBOPT_LVDS16ALL|SS_DBOPT_SFPDPALL|SS_DBOPT_CAMLINKALL)

//-------------------------------------------------------------
// Camera Link Daughter Board Register Indexes
//   -- Use with XLRWriteDBReg32 and XLRReadDBReg32
//
#define  SSREG_DB_CAMLINK_CTL                0
#define  SSREG_DB_CAMLINK_RCRDSTATUS         1
#define  SSREG_DB_CAMLINK_WRAPREG            2
#define  SSREG_DB_CAMLINK_SDRAMWRAPREG       3
#define  SSREG_DB_CAMLINK_UART_CNTL_A        4
#define  SSREG_DB_CAMLINK_UART_STATUS_DATA_A 5
#define  SSREG_DB_CAMLINK_UART_CNTL_B        6
#define  SSREG_DB_CAMLINK_UART_STATUS_DATA_B 7
#define  SSREG_DB_CAMLINK_X_OFFSET           8
#define  SSREG_DB_CAMLINK_X_ACTIVE           9
#define  SSREG_DB_CAMLINK_Y_OFFSET           10
#define  SSREG_DB_CAMLINK_Y_ACTIVE           11
#define  SSREG_DB_CAMLINK_ULMARKER           12
#define  SSREG_DB_CAMLINK_ULSIZE             13
#define  SSREG_DB_CAMLINK_YEARMONTH_RD       14
#define  SSREG_DB_CAMLINK_DAYHOUR_RD         15
#define  SSREG_DB_CAMLINK_MINSECD_RD         16
#define  SSREG_DB_CAMLINK_MILLISECD_RD       17
#define  SSREG_DB_CAMLINK_YEARMONTH_WRT      18
#define  SSREG_DB_CAMLINK_DAYHOUR_WRT        19
#define  SSREG_DB_CAMLINK_MINSECD_WRT        20
#define  SSREG_DB_CAMLINK_MILLMICROSECD_WRT  21
#define  SSREG_DB_CAMLINK_MAX                22 // For bounds checking

//-------------------------------------------------------------
// SFPDP Daughter Board Register Indexes
//   -- Use with XLRWriteDBReg32 and XLRReadDBReg32
//
#define  SSREG_DB_SFPDP_SERIALCTL            0
#define  SSREG_DB_SFPDP_RCRDCTRL_A           1
#define  SSREG_DB_SFPDP_RCRDCTRL_B           2
#define  SSREG_DB_SFPDP_RCRDCTRL_C           3
#define  SSREG_DB_SFPDP_RCRDCTRL_D           4
#define  SSREG_DB_SFPDP_PLAYCTRL_A           5
#define  SSREG_DB_SFPDP_PLAYCTRL_B           6
#define  SSREG_DB_SFPDP_PLAYCTRL_C           7
#define  SSREG_DB_SFPDP_PLAYCTRL_D           8
#define  SSREG_DB_SFPDP_OPTICALSTS_A         9
#define  SSREG_DB_SFPDP_OPTICALSTS_B         10
#define  SSREG_DB_SFPDP_OPTICALSTS_C         11
#define  SSREG_DB_SFPDP_OPTICALSTS_D         12
#define  SSREG_DB_SFPDP_SYNC_CTRL            13
#define  SSREG_DB_SFPDP_SERIALSTS            14
#define  SSREG_DB_SFPDP_WRAP                 15
#define  SSREG_DB_SFPDP_BUCKETS              16
#define  SSREG_DB_SFPDP_FRAMESIZEA           17
#define  SSREG_DB_SFPDP_FRAMESIZEB           18     
#define  SSREG_DB_SFPDP_FRAMESIZEC           19
#define  SSREG_DB_SFPDP_FRAMESIZED           20
#define  SSREG_DB_SFPDP_FRAMELOGA            21
#define  SSREG_DB_SFPDP_FRAMELOGB            22
#define  SSREG_DB_SFPDP_FRAMELOGC            23
#define  SSREG_DB_SFPDP_FRAMELOGD            24
#define  SSREG_DB_SFPDP_LOGCTRL              25
#define  SSREG_DB_SFPDP_THROTTLEA            26
#define  SSREG_DB_SFPDP_THROTTLEB            27
#define  SSREG_DB_SFPDP_THROTTLEC            28
#define  SSREG_DB_SFPDP_THROTTLED            29
#define  SSREG_DB_SFPDP_RECVBUCKET           30 
#define  SSREG_DB_SFPDP_INTEGRITY_STS        31
// Old definitions 
#define SSREG_DB_SFPDP_START_ON_SYNC  13
#define SS_SYNC_CLEAR      0  
#define SS_SYNCSET_PORT_1  1
#define SS_SYNCSET_PORT_2  2 
#define SS_SYNCSET_PORT_3  4 
#define SS_SYNCSET_PORT_4  8


// PARAMETERS FOR CAMLINK AGC
//   --Use with XLRSetDBAgcParam
//
#define SS_CL_AGC_ADJUSTMENTTYPE    0
#define SS_CL_AGC_EXPOSURESTEP      1
#define SS_CL_AGC_INTEGRATIONMIN    2
#define SS_CL_AGC_INTEGRATIONMAX    3
#define SS_CL_AGC_GAINSTEP          4
#define SS_CL_AGC_FRAMESKIP         5
#define SS_CL_AGC_FRAMEWAIT         6
#define SS_CL_AGC_TARGETRANGELOW    7
#define SS_CL_AGC_TARGETRANGEHIGH   8
#define SS_CL_AGC_NUMROIS           9
#define SS_CL_AGC_ROI0_OFFSETX      10
#define SS_CL_AGC_ROI0_OFFSETY      11
#define SS_CL_AGC_ROI0_WIDTH        12
#define SS_CL_AGC_ROI0_HEIGHT       13
#define SS_CL_AGC_ROI0_WEIGHT       14
#define SS_CL_AGC_ROI1_OFFSETX      15
#define SS_CL_AGC_ROI1_OFFSETY      16
#define SS_CL_AGC_ROI1_WIDTH        17
#define SS_CL_AGC_ROI1_HEIGHT       18
#define SS_CL_AGC_ROI1_WEIGHT       19
#define SS_CL_AGC_ROI2_OFFSETX      20
#define SS_CL_AGC_ROI2_OFFSETY      21
#define SS_CL_AGC_ROI2_WIDTH        22
#define SS_CL_AGC_ROI2_HEIGHT       23
#define SS_CL_AGC_ROI2_WEIGHT       24
#define SS_CL_AGC_INITIALGAIN       25

// CAMLINK GAIN/EXPOSURE adjustment selections for CL_AGC_ADJUSTMENTTYPE
#define SS_CL_AGC_TYPE_GAIN            0
#define SS_CL_AGC_TYPE_GAINEXPOSURE    1
#define SS_CL_AGC_TYPE_EXPOSURE        2


//--------------------------------------------------------------
// 10 GigE Daughter Board Register Indexes
//--------------------------------------------------------------
#define  SS_10GIGE_REG_WRAP               0x01
#define  SS_10GIGE_REG_MAC_FLTR_CTRL      0x02
#define  SS_10GIGE_REG_DATA_PAYLD_OFFSET  0x03
#define  SS_10GIGE_REG_DATA_FRAME_OFFSET  0x04
#define  SS_10GIGE_REG_PSN_OFFSET         0x05
#define  SS_10GIGE_REG_BYTE_LENGTH        0x06
#define  SS_10GIGE_REG_FILL_PATTERN       0x07
#define  SS_10GIGE_REG_TOTAL_PKTS         0x08
#define  SS_10GIGE_REG_NUM_ERR_PKTS       0x09
#define  SS_10GIGE_REG_MON_MODE1_ERR_PKTS 0x0A
#define  SS_10GIGE_REG_MON_MODE2_ERR_PKTS 0x0B
#define  SS_10GIGE_REG_ETHR_FILTER_CTRL   0x0C
#define  SS_10GIGE_REG_ETHR_PKT_LENGTH    0x0D
#define  SS_10GIGE_REG_ETHR_TOTAL_PKTS    0x0E
#define  SS_10GIGE_REG_ETHR_FSC_ERR_PKTS  0x0F
#define  SS_10GIGE_REG_ETHR_LGTH_ERR_PKTS 0x10
#define  SS_10GIGE_REG_ETHR_ADDR_RJT_PKTS 0x11
#define  SS_10GIGE_REG_SRC_ADDR_0_LSB     0x12
#define  SS_10GIGE_REG_SRC_ADDR_0_MSB     0x13
#define  SS_10GIGE_REG_SRC_ADDR_1_LSB     0x14
#define  SS_10GIGE_REG_SRC_ADDR_1_MSB     0x15
#define  SS_10GIGE_REG_SRC_ADDR_2_LSB     0x16
#define  SS_10GIGE_REG_SRC_ADDR_2_MSB     0x17
#define  SS_10GIGE_REG_SRC_ADDR_3_LSB     0x18
#define  SS_10GIGE_REG_SRC_ADDR_3_MSB     0x19
#define  SS_10GIGE_REG_SRC_ADDR_4_LSB     0x1A
#define  SS_10GIGE_REG_SRC_ADDR_4_MSB     0x1B
#define  SS_10GIGE_REG_SRC_ADDR_5_LSB     0x1C
#define  SS_10GIGE_REG_SRC_ADDR_5_MSB     0x1D
#define  SS_10GIGE_REG_SRC_ADDR_6_LSB     0x1E
#define  SS_10GIGE_REG_SRC_ADDR_6_MSB     0x1F
#define  SS_10GIGE_REG_SRC_ADDR_7_LSB     0x20
#define  SS_10GIGE_REG_SRC_ADDR_7_MSB     0x21
#define  SS_10GIGE_REG_SRC_ADDR_8_LSB     0x22
#define  SS_10GIGE_REG_SRC_ADDR_8_MSB     0x23
#define  SS_10GIGE_REG_SRC_ADDR_9_LSB     0x24
#define  SS_10GIGE_REG_SRC_ADDR_9_MSB     0x25
#define  SS_10GIGE_REG_SRC_ADDR_A_LSB     0x26
#define  SS_10GIGE_REG_SRC_ADDR_A_MSB     0x27
#define  SS_10GIGE_REG_SRC_ADDR_B_LSB     0x28
#define  SS_10GIGE_REG_SRC_ADDR_B_MSB     0x29
#define  SS_10GIGE_REG_SRC_ADDR_C_LSB     0x2A
#define  SS_10GIGE_REG_SRC_ADDR_C_MSB     0x2B
#define  SS_10GIGE_REG_SRC_ADDR_D_LSB     0x2C
#define  SS_10GIGE_REG_SRC_ADDR_D_MSB     0x2D
#define  SS_10GIGE_REG_SRC_ADDR_E_LSB     0x2E
#define  SS_10GIGE_REG_SRC_ADDR_E_MSB     0x2F
#define  SS_10GIGE_REG_SRC_ADDR_F_LSB     0x30
#define  SS_10GIGE_REG_SRC_ADDR_F_MSB     0x31

#define  M_10GIGE_REG_ENABLE_MAC_ADDR      (0x80000000) 


#endif //XLRDBCOMMON_H
