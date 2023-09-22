#ifndef PLOT_DATA_DIR_H_
#define PLOT_DATA_DIR_H_
/*
 * Machinery to support dumping plot data to one or more files
 *
 * Everything hides behind one function, dump_plot_data2dir()
 */

/* output.c imports vex.h, mk4_vex.h imports vex.h */
#ifndef VEX_H
#include "mk4_vex.h"
#include "pass_struct.h"
#include "param_struct.h"
#include "meta_struct.h"
#endif /* MK4_VEX_H */

/* struct vex in mk4_vex.h,
 * struct type_pass in pass_struct.h
 * struct type_param in param_struct.h
 * struct type_meta in meta_struct.h
 * struct type_plot in plot_struct.h via param_struct.h
 */
struct type_dump {
    struct vex *root;
    struct type_pass *pass;
    struct type_param *param;
    struct type_status *status;
    struct type_meta *meta;
    struct type_plot *plot;
    struct mk4_fringe *fringe;
};
extern void dump_plot_data2dir(struct type_dump *dump);

/* disable the capability for testing */
#ifndef USE_PLOT_DATA_DIR
#define USE_PLOT_DATA_DIR 1
#endif /* USE_PLOT_DATA_DIR */
#if USE_PLOT_DATA_DIR
#define DUMP_PLOT_DATA2DIR(ROOT,PASS,PP,ST,PM,PL,PF) do{\
    static int once = 1;                                \
    struct type_dump dump;                              \
    dump.root = (ROOT);                                 \
    dump.pass = (PASS);                                 \
    dump.param = (PP);                                  \
    dump.status = (ST);                                 \
    dump.meta = (PM);                                   \
    dump.plot = (PL);                                   \
    dump.fringe = (PF);                                 \
    if (once) {                                         \
        msg("DPD(%s)",1,dump.param->plot_data_dir[0]);  \
        msg("DPD(%s)",1,dump.param->plot_data_dir[1]);  \
        once = 0;                                       \
    }                                                   \
    if (dump.param->plot_data_dir[0][0] ||              \
        dump.param->plot_data_dir[1][0])                \
        dump_plot_data2dir(&dump);                      \
} while (0)
#else /* USE_PLOT_DATA_DIR */
#define DUMP_PLOT_DATA2DIR(DP)
#endif /* USE_PLOT_DATA_DIR */

#endif
/*
 * eof
 */
