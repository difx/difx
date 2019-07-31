#ifndef T120_VERSION
#define T120_VERSION 0

#include "mk4_typedefs.h"
                                    /* Set this to current version,            */
                                    /* defined to be same as app struct        */
#define type_120_v0 type_120


#define COUNTS_PER_LAG 1
#define COUNTS_GLOBAL  2
#define AUTO_GLOBAL    3
#define AUTO_PER_LAG   4
#define SPECTRAL       5

                                    /* WARNING: Don't alter the definitions of */
                                    /* the next four structs, or the union,    */
                                    /* without first closely examining the     */
                                    /* structure size calculations done in     */
                                    /* the *120*.c routines in $DFIO.  Also    */
                                    /* be aware that type_120 records do not   */
                                    /* take readily to normal DFIO library     */
                                    /* version control.                        */
struct counts_per_lag
    {
    int coscor;
    int cosbits;
    int sincor;
    int sinbits;
    };

struct counts_global
    {
    int cosbits;
    int sinbits;
    struct lag_tag
    {
        int coscor;
        int sincor;
    } lags[1];
    };

struct auto_per_lag
    {
    int coscor;
    int cosbits;
    };

struct auto_global
    {
    int cosbits;
    char unused[4];
    int coscor[1];
    };

struct spectral
    {
    float re;
    float im;
    };

union lag_data
    {
    struct counts_per_lag cpl[1];
    struct counts_global  cg;
    struct auto_per_lag  apl[1];
    struct auto_global  ag;
    struct spectral spec[1];
    };

union flag_wgt
    {
    int             flag;           /* Up to 32 correlation flags             */
    float           weight;         // in spectral mode: ap weight (0.0-1.0)
    };

struct type_120 
    {
    char            record_id[3];   /* Standard 3-digit id                    */
    char            version_no[2];  /* Standard 2-digit version #             */
    char            type;           /* Data type (defines above)              */
    short           nlags;          /* Needed by IO library                   */
    char            baseline[2];    /* Standard baseline id                   */
    char            rootcode[6];    /* Root suffix                            */
    int             index;          /* Index number for type 101 rec.         */
    int             ap;             /* Acc period number                      */
    union flag_wgt  fw;             // either flag or weight for lag or spectral
    int             status;         /* Up to 32 status bits                   */
    int             fr_delay;       /* Mid-AP fractional delay (bits * 2^32)  */
    int             delay_rate;     /* Mid-AP delay rate (bits/sysclk * 2^32) */
    union lag_data  ld;             /* Correlation counts                     */
    };

/* Bit Defs for Status and Flag
 *   Each cf has a status word which indicates errors for that frame and a
 *   parallel flag word which indicates whether the corresponding error in the
 *   status word resulted in cf lag data being dropped from the runnning
 *   accumulation.  The cf status words for each cf in the AP are ORed
 *   together to get the 120 status member.  The cf flag words for each cf in
 *   the AP are ORed together to get the 120 flag member.
 */
#define E_MISSED_CF_SERV  0x00000001   /* Missed a cf service                 */
#define E_MISSED_AP_MIDS  0x00000002   /* Failed to capture the mid-ap parms  */
#define E_TAP_OVERRUN     0x00000004   /* Hw shifted dly tap beyond end       */
#define E_DLY_3_CARRY     0x00000008   /* Hw attempted a simultaneous carry out
                                        * of all 3 dly regs (ref, rem, baseline)
                                        */

#define E_REF_SU_ID       0x00000100   /* Ref su id error                     */
#define E_REF_CHN_ID      0x00000200   /* Ref chn id error                    */
#define E_REF_CHKSUM      0x00000400   /* Ref checksum error                  */
#define E_REF_CFNUM       0x00000800   /* Ref cf number error                 */
#define E_REF_BADD        0x00001000   /* Ref cf number marked as 0xbadd      */
#define E_REF_ZERO        0x00002000   /* Ref checksum = 0                    */
#define E_REF_LINK        0x00004000   /* Ref link error (dav=0,err=1,rdy=0)  */
#define E_REF_SYNC        0x00008000   /* Ref link not synced                 */

#define E_REM_SU_ID       0x00100000   /* Rem su id error                     */
#define E_REM_CHN_ID      0x00200000   /* Rem chn id error                    */
#define E_REM_CHKSUM      0x00400000   /* Rem checksum error                  */
#define E_REM_CFNUM       0x00800000   /* Rem cf number error                 */
#define E_REM_BADD        0x01000000   /* Rem cf number marked as 0xbadd      */
#define E_REM_ZERO        0x02000000   /* Rem checkksum = 0                   */
#define E_REM_LINK        0x04000000   /* Rem link error (dav=0,err=1,rdy=0)  */
#define E_REM_SYNC        0x08000000   /* Rem link not synced                 */

#endif
