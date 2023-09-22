#ifndef FFSEARCH_H__
#define FFSEARCH_H__

#include "vex.h"
#include "mk4_util.h"
#include "refringe.h"


extern void pcalibrate (struct type_pass *, int);
extern void rotate_pcal(struct type_pass *pass);
extern void sampler_delays (struct type_pass *pass);
extern void norm_fx (struct type_pass*, struct type_param*, struct type_status*, int, int);
extern void norm_xf (struct type_pass*, struct type_param*, struct type_status*, int, int);
extern void freq_spacing (struct type_pass*);
extern void search_windows(struct type_pass*);
extern void delay_rate (struct type_pass*, int, hops_complex rate_spectrum[]);
extern void update (struct type_pass*, int, double, int, int, int);
extern int apply_filter (struct type_pass*);
extern int ion_search (struct type_pass*);
extern int ion_search_smooth (struct type_pass*);
extern int parse_cmdline(int argc,char **argv,fstruct **files, bsgstruct **base_sgrp,   struct type_param *param);
extern int precorrect (struct scan_struct*, struct type_pass*);
extern char* check_rflist ( char* baseline, int files_index, bsgstruct* base_sgrp);
extern int fringe_search ( struct vex* root, struct type_pass* pass);
extern int organize_data(struct mk4_corel *cdata, struct scan_struct *ovex, struct ivex_struct *ivex, 
    struct mk4_sdata *sdata, struct freq_corel *corel, struct type_param *param, struct type_status *status, struct c_block *cb_head);
// extern int output (struct vex*, struct type_pass*);

#endif