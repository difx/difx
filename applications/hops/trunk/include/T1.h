/*- */
/* Diddled 2001 June 11, JAB */ 

#ifndef T1_H_INCLUDED 
#define T1_H_INCLUDED

#if defined(linux)
   #include <stdint.h>
#elif defined(hpux)
   #include <sys/types.h>
#elif defined(HP_RT)
   #include <inttypes.h>
#else
   #include <stdint.h>
#endif

/*0 Defines */
#define T1_UNDEF    0;
#define T1_REF_CHN  1
#define T1_REM_CHN  2
#define T1_HEAD     1
#define T1_TAIL     2
/*0 */

/*0 Typedefs */

/*1 T1 Record Header */
typedef struct T1_RecHdr {
   char       recId[3];
   char       recVer[2];
   uint8_t    recFmt;
   uint16_t   cFOffset;
   char       baseline[2];
   char       rootcode[6];
   uint32_t   index;
   uint32_t   ap;
} T1_RecHdr;
/*1 */

/*1 AP Err Cnts */
typedef struct T1_R130 {
   T1_RecHdr  recHdr;
   uint32_t   enabled;
   uint32_t   occurred;
   int32_t    xSuId;
   int32_t    xChnId;
   int32_t    xCFNum;
   int32_t    xChkSum;
   int32_t    xInBdLink;
   int32_t    xInBdSync;
   int32_t    ySuId;
   int32_t    yChnId;
   int32_t    yCFNum;
   int32_t    yChkSum;
   int32_t    yInBdLink;
   int32_t    yInBdSync;
   int32_t    headTapPastEnd;
   int32_t    headTap3Carry;
   int32_t    tailTapPastEnd;
   int32_t    tailTap3Carry;
} T1_R130;
/*1 */

/*1 Link Status */
typedef struct T1_R131 {
   T1_RecHdr  recHdr;
   uint32_t   linkStatus[64];
} T1_R131;
/*1 */

/*1 Correlator Frame Header */
typedef struct T1_R141 {
   T1_RecHdr  recHdr;    /*- recFmt = {T1_REF_CHN, T1_REM_CHN} */
   uint32_t   suId;
   uint32_t   chnId;
   uint32_t   cFNum;
   int32_t    dlyErr;
   int32_t    dlyErrRate;
   int32_t    phase;
   int32_t    phaseRate;
   int32_t    phaseAcc;
   int32_t    phaseLogIncPeriod;
   int32_t    phaseKAccSegLen;
   int32_t    sideband;
   int32_t    oversamplingFactor;
   uint32_t   chkSum; 
   uint32_t   flags; /* see type_120.h for bit definitions */
} T1_R141;
/*1 */

/*1 Correlator Frame Dynamic Parameters */
typedef struct T1_R142 {
   T1_RecHdr  recHdr;    /*- recFmt = {T1_HEAD, T1_TAIL} */
   uint32_t   phaseAdj;
   uint32_t   phaseIncClkDiv;
   uint32_t   phaseRateIncCnt;
   int32_t    phase;
   int32_t    phaseRate;
   int32_t    phaseAcc;
   int32_t    xDly;
   int32_t    xDlyRate;
   int32_t    yDly;
   int32_t    yDlyRate;
   int32_t    bDly;
   int32_t    bDlyRate;
   uint32_t   tapPos;
   uint32_t   xDlyRateSign;
   uint32_t   yDlyRateSign;
   uint32_t   bDlyRateSign;
   uint32_t   udr;
   uint8_t    unused0[4];
} T1_R142;
/*1 */

/*1 Correlator Frame Dynamic Parameter Residues */
typedef struct T1_R143 {
   T1_RecHdr  recHdr;    /*- recFmt = {T1_HEAD, T1_TAIL} */
   uint32_t   phaseAdj;
   uint32_t   phaseIncClkDiv;
   uint32_t   phaseRateIncCntFinal;
   int32_t    phaseRateFinal;
   int32_t    phaseFinal;
   int32_t    phaseInitial;
   int32_t    xDlyFinal;
   int32_t    xDlyInitial;
   int32_t    yDlyFinal;
   int32_t    yDlyInitial;
   int32_t    bDlyFinal;
   int32_t    bDlyInitial;
   uint32_t   tapPosFinal;
   uint32_t   tapErr;
   uint32_t   udr;
   uint8_t    unused0[4];
} T1_R143;
/*1 */

/*1 Correlator Frame Chn Errors */
typedef struct T1_R144 {
   T1_RecHdr  recHdr;    /*- recFmt = {T1_REF_CHN, T1_REM_CHN} */
   uint32_t   suIdEx;
   uint32_t   suIdRx;
   uint32_t   chnIdEx;
   uint32_t   chnIdRx;
   uint32_t   cFNumEx;
   uint32_t   cFNumRx;
   uint32_t   chkSumEx;
   uint32_t   chkSumRx;
} T1_R144;
/*1 */

/*1 Correlator Task Finished */
typedef struct T1_R150 {
   T1_RecHdr recHdr;
   char      qcode[2];
   uint8_t   unused0[6];
} T1_R150;
/*1 */

/*0 */
#endif
