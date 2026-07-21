#ifndef AVERAGE_H
#define AVERAGE_H

#include <stdio.h>

#define NO_MODE            0
#define AVERAGE            1
#define SUMMARY            2
#define TEST               3

#define ENDOFSCAN          1

#define FALSE 0
#define TRUE  1

#define PI 3.1415926536

#include "adata.h"
#include "msg.h"

#define MAXID 560
#define MAXSRC 20000

typedef struct
    {
    int         order;
    int         lastorder;
    int         keyval;
    union
        {
        fringesum       fdata;
        trianglesum     tdata;
        } u;
    } seg_data;

typedef struct
    {
    char        id[5];
    int         start_index;
    int         end_index;
    int         seglen;
    int         valid;
    int         start_time;
    } tbsumm;

typedef struct
    {
    int         start_index;
    int         end_index;
    int         seglen;
    int         valid;
    int         nid;
    tbsumm      tribase[MAXID];
    } summary;

struct config
    {
    int         int_time;
    int         coherent;
    int         cofit_output;
    int         binary_input;
    float       snrfact;
    int         multiscan;
    int         account;
    int         header;
    };

extern int  assess_data (seg_data *, int, struct config, summary []);
extern int  baseline_avg (seg_data *, tbsumm *, struct config, FILE *);
extern void check_source (seg_data *, summary *);
extern void check_times (seg_data *, summary *, int);
extern void check_tribase (seg_data *, tbsumm *);
extern int  get_int_time (char *);
extern int  index_data (seg_data *, int, summary []);
extern int  parse_cmdline (int, char **, FILE **, struct config *);
extern int  read_data (seg_data **, FILE *, struct config, int *);
extern int  sort_data (seg_data *, int);
extern void sorter (seg_data *, int);
extern int  triangle_avg (seg_data *, tbsumm *, struct config, FILE *);
#endif
