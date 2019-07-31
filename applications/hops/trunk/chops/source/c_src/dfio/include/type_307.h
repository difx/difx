
#ifndef T307_VERSION
#define T307_VERSION 0               /* Set this to current version, */
                                     /* defined to be same as app struct */ 

/* Revised:  2000 August 18, JAB */ 
/* Revised:  2000 November 15, CJL */ 
#include "mk4_typedefs.h"

#define type_307_v0 type_307 

/*    *** Phase Calibration Raw Data *** 
 * This is intended to be an equivalent of EVN message 0x20000. 
 * Reference:  "EVN Correlator Message Formats," Appendix III, 
 * Section III.4.16, (pages 39--40). 
 * See also Mark-4 Memos 133 and 178. */ 

typedef struct ChanCount { 
  U32         count[8];              /* LSB first of table RAM */ 
  U32         val_count;             /* Valid-sample counts */ 
} ChanCount; 

struct type_307 { 
  char        record_id[3];           /* Standard 3-digit id */
  char        version_no[2];          /* Standard 2-digit version # */
  char        unused1[3]; 
  int         su;                     /* SU */ 
  char        unused2[4];
  double      tot;                    /* TOT */ 
  double      rot;                    /* ROT */ 
  double      accum_period;           /* In SYSCLKs */ 
  U32         frame_count; 
  ChanCount   counts[16];             /* Index is channel number */ 
  char        unused3[4];
};

#endif 

