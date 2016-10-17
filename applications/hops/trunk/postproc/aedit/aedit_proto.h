/*
 * Prototypes for functions to allow compiler checking
 */
#include <stdio.h>
#include "mk4_data.h"
#include "aedata.h"
#include "psplot.h"
#include "pstruct.h"
#include "summary.h"
#include "usearray.h"

extern int active_filter(void);
extern void add_station (char station, char *slist);
extern void aeclr_corel (corelarray *cdatum);
extern void aeclr_fringe (fringearray *fdatum);
extern void aeclr_quad (quadarray *qdatum);
extern void aeclr_root (rootarray *rdatum);
extern void aeclr_triangle (trianglearray *tdatum);
extern int allocate_parms (struct usearray *user_param);
extern int alloc_btq (char **btq, int *allocated, int type);
extern int auto_hardcopy(void);
extern int axis (char *y_axis, char *x_axis);
extern void axis_scale (int index, float min, float max, float *win1, float *win2);
extern int calc_close (esum *data);
extern int cfilter (corelarray *cdatum, int mode);
extern void check_env (void);
extern int cleanup (void);
extern void cleanup_psplot (struct ps_array *psarray);
extern int clear (esum *data, char *string);
extern void clear_fqex (struct frqexp *fqex);
extern void clear_pstruct (struct plot_info *ps);
extern void clear_source (srcsum *sptr);
extern void clear_summ (struct datasumm *summ);
extern int command_id(char *string);
extern int cross_link (esum *data);
extern int cursor_select (esum *data, int *npoint, int option);
extern void datum_value (int index, int aux, int toffset, int refrem,
    fringearray *fdatum, float *value, float *errh, float *errl);
extern int display_psdata (struct ps_array *psarray);
extern int dup_check (fringearray *fdata, int i, int j);
extern int dup_flag (esum *data, int option);
extern int edit (esum *data, char *arg1, char *arg2);
extern int edit_close (esum *data, int mode, int *fedit, int *tedit);
extern void edit_families (esum *data, int mode, int *nedit1, int *nedit2);
extern void erase_point (float x, float y, float xh, float xl,
                  float yh, float yl, int symbol);
extern int esdesp_check (int esdesp);
extern int execute (esum *data, struct com *command);
extern int extract_parms (esum *data, struct usearray *user_param);
extern int ffilter (fringearray *fdatum, int mode);
extern int fill4_parms (struct mk4_fringe *fringe4,
    fringesum *data, struct usearray *user_param);
extern int fill_closure (trianglearray *tptr, char triangle[4],
    int indices[3], fringearray *fdata);
extern int fill_tdata (int nfe, esum *data, int *dptr);
extern int fplot (esum *data);
extern int get_axis (char *string, char *ax_name, int *ax_index,
    char *ax_units, int *aux, int *plotby);
extern int get_param (esum *data, char *arg1, char *arg2, char *remarg);
extern int get_param_list (struct usearray *user_param, int maxfreq, char *args);
extern int get_plot_data (esum *data, struct plot_info *pd, struct frqexp fqex,
    srcsum *sptr, char *plot_id, struct plot_points *pp, int symbol[3]);
extern void get_plot_datum (char *plot_id, esum *data, srcsum *sptr,
    int ptr, int n, int toffset, struct plot_ptqual *pt);
extern int get_ps_indices4 (esum *data, struct ps_array *psarray);
extern void help(char *string);
extern int init_inputs(void);
extern void init_summ (struct datasumm *summ, int type);
extern int locate_pscurs (float *x, float *y, struct ps_array *psarray,
    struct psplot_cell **cell);
extern int makekey (void *dptr, int key, int type);
extern int make_psarray4 (struct ps_array *psarray);
extern char *make_tri (char *stations, int *ntri);
extern char mk3_qf (fringesum *datum);
extern char *make_tri (char *stations, int *ntri);
extern int newbase (char *baselist, char *triangle);
extern int param_list (esum *data);
extern int parse_cmdline (int argc, char **argv, int *xwindow, char *run_fname,
    char *batch_string, int *filelist);
extern int parse_commands (char *line, struct com *commands, int *n);
extern int plot (esum *data, char *y_axis, char *x_axis);
extern int plot_fqex (esum *data, struct frqexp fqex);
extern void plot_header (struct frqexp fqex, srcsum *sptr);
extern void plot_points (struct plot_info *pd, struct plot_points *pp, int symbol[3]);
extern int plot_quality (fringesum *fdatum, trianglesum *tdatum);
extern int pr_allsumm(void);
extern int pr_csumm(void);
extern int pr_fsumm(void);
extern int pr_inputs(char *string);
extern int print_data (fringearray *fdata);
extern int pr_rsumm(void);
extern int pr_source(srcsum *src);
extern int pr_ssumm(char *arg);
extern int pr_summary (esum *data, char *arg);
extern int pr_tsumm(void);
extern void ps_baselabel (struct ps_array *psarray);
extern int psfile4 (esum *data, char *fname, int mode);
extern int ps_fplot (struct ps_array *psarray, fringesum *fdatum);
extern void ps_free (struct ps_array *psarray);
extern int ps_inside (float x, float y,
    float xmin, float xmax, float ymin, float ymax);
extern int psplot4 (esum *data);
extern int psplot (esum *data);
extern int psplot_defaults4 (struct ps_array *psarray);
extern void ps_proc_datum (esum *data, struct ps_array *psarray,
    float x, float y, struct psplot_cell *cell, char key, int do_fplot);
extern void ps_scanlabel (struct ps_array *psarray);
extern void ps_selbase (int baseline, struct ps_array *psarray);
extern void ps_selqual (int colour, struct ps_array *psarray);
extern void ps_selscan (int scan, struct ps_array *psarray);
extern void pstag_process (struct ps_array *psarray, esum *data);
extern int qarray_index (esum *data);
extern int qfilter (quadarray *qdatum, int mode);
extern int read_cursor(float *x, float *y);
extern int read_data (esum *data, char *filename);
extern int rfilter (rootarray *rdatum, int mode);
extern int run_com_file (esum *data, char *filename);
extern int run_pscursor (struct ps_array *psarray, esum *data);
extern int save (esum *data, int mode);
extern int set_baselines(char *arg1, char *arg2, char *remarg);
extern int set_device (char *string);
extern int set_fraction(char *arg1, char *arg2, char *remarg);
extern int set_frequencies(char *arg1, char *arg2, char *remarg);
extern int set_mode(char *arg1);
extern int set_nfreq(char *arg1, char *arg2, char *remarg);
extern int set_pols (char *arg1, char *arg2, char *remarg);
extern int set_prange (char *arg1, char *arg2, char *remarg);
extern int set_procrange(char *arg1, char *arg2);
extern int set_pscodes (esum *data, struct ps_array *psarray);
extern int set_psparam (struct ps_array *psarray);
extern int set_qcodes(char *arg1, char *arg2, char *remarg);
extern int set_quads (char *arg1, char *arg2, char *remarg);
extern int set_sources(char *arg1, char *arg2, char *remarg);
extern int set_stations(char *arg1, char *arg2, char *remarg);
extern int set_timerange (char *arg1, char *arg2);
extern int set_triangles (char *arg1, char *arg2, char *remarg);
extern int set_type(char *arg1, char *arg2, char *remarg);
extern int setup_plot (struct plot_info *pd, int nplot, struct frqexp fqex);
extern int setup_psplot (struct ps_array *psarray);
extern int set_year (fringearray *fdata, char *arg1);
extern int smatch(char *s1, char *s2);
extern int sorter (void *dptr, char *arg1, int type);
extern void station_reduce (fringearray *fdata, char station, char *source,
    int expt, char freq);
extern int summ_corel (corelarray *cdata, int mode);
extern int summ_data (esum *data, int mode);
extern int summ_fringe (esum *data, int mode);
extern int summ_quad (quadarray *qdata, int mode);
extern int summ_root (rootarray *rdata, int mode);
extern int summ_triangle (trianglearray *tdata, int mode);
extern void symbol_key (struct frqexp *fqex);
extern int tarray_index (esum *data);
extern void test1 (esum *data, char *arg1, char *arg2, char *remarg);
extern int tfilter (trianglearray *tdatum, int mode);
extern void time_axis (struct plot_info *pd, struct frqexp fqex);
extern void triangle_value (int index, trianglearray *ta, int toffset,
    float *value, float *errh, float *errl);
extern int tricheck (trianglesum *datum, char *triangle);
extern int trngl_present (char triangle[4], struct ibaselist *blist,
    int n, int indices[3]);
extern int unflag (esum *data, char *arg1);
extern int update_fqex (void *datum, struct datasumm *summ, int type);
extern int update_sinfo (struct source_info *sinfo, char *source, int nsrc);
extern int write_data (esum *data, char *filename);
extern int write_families (esum *data, FILE *fp);
extern int write_param (esum *data, char *filename, char *precision);
extern int write_prmline (fringearray *fdata, int ndigits, FILE *fp);
extern int write_psfile (struct ps_array *psarray, char *filename, int mode);
extern void write_pshdr (FILE *fp, struct ps_array *psarray, int len);
extern int write_reproc (struct ps_array *psarray, char *filename);
extern int write_tdata (esum *data, char *filename);
extern int zoom (esum *data);

/*
 * eof
 */
