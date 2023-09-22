
#ifndef T309_VERSION
#define T309_VERSION 1               // Set this to current version,
                                     // defined to be same as app struct 

// First created 2001.1.26  rjc
// created v.1 with 64 channels & tones  2010.10.1   rjc

#include "mk4_typedefs.h"

#define type_309_v1 type_309 

//    *** DOM-generated Phase Calibration Data *** 

struct type_309_v0
  { 
  char        record_id[3];           // Standard 3-digit id
  char        version_no[2];          // Standard 2-digit version #
  char        unused1[3]; 
  int         su;                     // SU
  int         ntones;                 // number of tones [0..16]
  double      rot;                    // ROT at start of AP
  double      acc_period;             // in secs
  struct ch_tag
    {
    char      chan_name[8];
    double    freq;                   // tone frequency in Hz
    U32       acc[16][2];             // accumulators for 16 freqs x 2 quads (C..S)
    } chan[16];
  };

struct type_309
  { 
  char        record_id[3];           // Standard 3-digit id
  char        version_no[2];          // Standard 2-digit version #
  char        unused1[3]; 
  int         su;                     // SU
  int         ntones;                 // number of tones [0..64]
  double      rot;                    // ROT at start of AP
  double      acc_period;             // in secs
  struct ch1_tag
    {
    char      chan_name[8];
    double    freq;                   // tone frequency in Hz
    U32       acc[64][2];             // accumulators for 64 freqs x 2 quads (C..S)
    } chan[64];
  };
void write_t309 (struct type_309 *, FILE *);

#endif 

