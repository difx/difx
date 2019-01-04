/***************************************************************************
 *   Copyright (C) 2010 by Roger Cappallo                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef __DIFX2MARK4_H__
#define __DIFX2MARK4_H__

#include <difxio/difx_input.h>
#include <limits.h>
#include <time.h>

#define MAX_INPUT_FILES 4096
#define MAX_VIS 8192
#define MAX_STN 50
#define MAX_FBANDS 20 
#define EXP_CODE_LEN 4
#define NUMFILS 500                 // max number of type 1 output files
#define MAGLIM 10000.0              // threshold magnitude for vis. rejection
#define MAX_FPPAIRS 5000            // dimensioned for b-lines x chans x pol_prods
#define MAX_DFRQ 100                // allowed max number of *DiFX* frequencies
#define NVRMAX 1000000              // max # of vis records

enum booleans {FALSE, TRUE};

struct CommandLineOptions
    {
    char exp_no[EXP_CODE_LEN+1];
    char *baseFile[MAX_INPUT_FILES];
    char scodeFile[PATH_MAX];
    FILE **in;
    char *scan;
    int nBaseFile;
    int writemodel;
    int pretend;
    double scale;
    int verbose;
    /* some overrides */
    int specAvg;
    int doalldifx;
    float nOutChan;
    float startChan;
    int keepOrder;
    int dontCombine;
    int overrideVersion;
    double sniffTime;
    int pulsarBin;
    int phaseCentre;
    double jobMatrixDeltaT; /* seconds */
    int raw;
    char fgroups[16];
    char bandwidth[8];
    };

typedef struct 
    {
    int sync;
    int version;
    int baseline;
    int nvis;
    int mjd;
    double iat;
    int config_index;
    int source_index;
    int freq_index;
    char pols[2];
    int pulsar_bin;
    double weight;
    double uvw[3];
    struct components
        {
        float real;
        float imag;
        } comp[MAX_VIS];
    } vis_record;

struct stations
    {
    int inscan;                     // in scan according to vex SCHED block
    int invis;                      // visibilities found in .difx/ data for this antenna
    char mk4_id;                    // single char mk4 station code
    char intl_name[2];              // two letter international stn name
    char difx_name[2];              // two letter code used by difx
    };

struct fbands
    {
    char code;
    double flo;
    double fhi;
    };

struct fblock_tag
    {
    struct
        {
        char chan_id[8];            // channel id for identifying channels
        char sideband;              // U or L
        char pol;                   // R or L
        int ant;                    // antenna table index
        int find;                   // frequency table index
        int bs;                     // quantization bits/sample
        int first_time;             // true iff first entry in table of chan_id for ant 
        int zoom;                   // true iff this channel is zoom mode
        int n_spec_chan;            // # of spectral channels output from difx
        double pcal_int;            // pcal interval (MHz)
        double freq;                // LO frequency (MHz); negative for LSB
        double bw;                  // bandwidth (MHz)
        } stn[2];                   // reference | remote
    };

#include "type_000.h"
#include "type_100.h"
#include "type_101.h"
#include "type_120.h"
#include "type_300.h"
#include "type_301.h"
#include "type_302.h"
#include "type_303.h"
#include "type_309.h"


                                    // function prototypes 
                                    // conv2date.c
void conv2date (double, struct date *);
                                    // createRoot.c
int createRoot (DifxInput *, struct fblock_tag *, int, int, char *, char *, 
                struct stations *, struct CommandLineOptions *, char *);
char getband (double);
                                    // createType1s.c
int createType1s (DifxInput *, struct fblock_tag *, int *, int, char *, char *, 
                  struct stations *, struct CommandLineOptions *, char *);
                                    // createType3s.c
int createType3s (DifxInput *, struct fblock_tag *, int, int, int, char *, char *, 
                  struct stations *, struct CommandLineOptions *);
                                    // get_vis.c
int get_vis (DifxInput *, char *, struct CommandLineOptions *, int *, int *, int *, 
                 vis_record **, char *, struct fblock_tag *);
                                    // new_type1.c
int new_type1 (DifxInput *, struct fblock_tag *, int, int, int, int, int *, double *,
               struct stations *, char *, struct CommandLineOptions *, FILE **, 
               int, char *, char *, char *, char *, int, int);
                                    // write_t120.c
void write_t120 (struct type_120 *, FILE *);
                                    // normalize.c
void normalize (struct CommandLineOptions *, vis_record *, int, int *, int *, 
                struct fblock_tag *);
                                    // root_id.c
char *root_id(int, int, int, int, int);
char *root_id_break(time_t, int, int, int, int, int);
int root_id_delta(time_t);
                                    // single_code.c
char single_code (char *, char *);
                                    // swabr.c
short short_reverse (short);
unsigned short unsig_reverse (unsigned short);
int int_reverse (int);
long long_reverse (long);
float float_reverse (float);
double double_reverse (double);

#endif
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
