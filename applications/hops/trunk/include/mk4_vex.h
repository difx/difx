#ifndef MK4_VEX_H
#define MK4_VEX_H

/*----------------------------------------------------------------------------*/
#include <stdio.h>
#include "vex.h"

/*----------------------------------------------------------------------------*/
extern int  block_params (void);
extern int  check_intrange (int, char *);
extern int  check_realrange (double, char *);
extern void prt_date (char *, struct date *);
extern int  check_stloc (void);
extern int  check_strrange (char *, char *);
extern struct Cvex_Config *cvex_info (char *);
extern int  decode_pval (struct param_val *, struct pval_format *);
extern int  do_antenna (struct def_list *, struct station_struct *);
extern int  do_bbc (struct def_list *, int, struct station_struct *);
extern int  do_clock (struct def_list *,struct date *,struct station_struct *);
extern int  do_das (struct def_list *, struct station_struct *);
extern int  do_eop (struct def *, struct scan_struct *);
extern int  do_exper (struct def *, struct scan_struct *);
extern int  do_freq (struct def_list *, struct station_struct *, int *);
extern int  do_head_pos (struct def_list *, struct station_struct *);
extern int  do_if (struct def_list *, int, struct station_struct *);
extern int  do_pass_order (struct def_list *, struct station_struct *);
extern int  do_phase_cal_detect (struct def_list *,int,struct station_struct *);
extern int  do_roll (struct def_list *, struct station_struct *);
extern int  do_site (struct def_list *, struct station_struct *);
extern int  do_source (struct def *, struct source_struct *);
extern int  do_track (struct def_list *, int, struct station_struct *);
extern struct evex_struct *evex_info (char *);
extern int  fill_deflists (void);
extern int  fill_scanlist (void);
extern int  fill_station_parms (struct def_list *, int, struct date *,
                                struct station_struct *);
extern int  find_statements (void);
extern struct Cvex_BlockMode *get_block_mode (char *);
extern struct Cvex_ChipMode *get_chip_mode (char *);
extern struct Cvex_BoardParms *get_corr_bd_parms (char *);
extern struct Cvex_Mode *get_corr_mode (char *);
extern struct def *get_def (char *, char *, int *, int *);
extern int  get_drive_init (char *, struct drive_init_struct *);
extern int  get_global_deflist (struct def_list **);
extern int  get_logscan (struct def *, char *, struct station_log *);
extern int  get_mode_deflist (struct def *, char *, struct def_list **);
extern int  get_pbs_init (char *, struct pbs_init_struct *);
extern int  get_pcm_config (char *, struct svex_struct *);
extern int  get_pcm_tables (char *, struct svex_struct *);
extern struct Cvex_SectionMode *get_section_mode (char *);
extern char *get_statement (char *, char **, char **);
extern int  get_station_deflist (char *, struct def_list **);
extern int  get_su_chan_out (char *, struct svex_struct *);
extern int  get_su_connect (char *, struct su_connect_struct *);
extern int  get_trm_config (char *, struct TRM_config_struct *);
extern int  get_val_list (char *, char * []);
extern int  get_version (int);
extern int  get_vex (char *, int, char *, struct vex *);
extern int  in_comment (char *);
extern void init_scan (struct scan_struct *, int);
extern int  in_quote (char *);
extern struct ivex_struct *ivex_info (char *);
extern int  locate_blocks (void);
extern int  locate_cq (void);
extern struct lvex_struct *lvex_info (char *);
extern char nextchar (char *);
extern int  param_formats (void);
extern int  parse_date (char *, struct date *);
extern int  parse_dec (char *, struct sky_coord *);
extern int  parse_pval (char *, char *, int, struct param_val *);
extern int  parse_ra (char *, struct sky_coord *);
extern int  parse_ref (int, struct ref *);
extern int  parse_units (char *, int, double *);
extern int  parse_vexfile (char *);
extern void print_location (int);
extern int  process_qstring (char *, char *);
extern int  read_file (FILE *);
extern struct scan_struct *scan_info (char *, char *);
extern int  strip_text (char *, char *, char *);
extern struct svex_struct *svex_info (char *);
extern int  vex_init (void);
extern int  write_vexfile (int *, struct insert *, FILE *);
extern int  bitstream (char *);
#endif

