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

/* implementation details, not needed by callers */
#ifndef PLOT_DATA_DIR_IMPLEMENTATION
#define PLOT_DATA_DIR_IMPLEMENTATION 0
#endif /* PLOT_DATA_DIR_IMPLEMENTATION */
#if PLOT_DATA_DIR_IMPLEMENTATION
#define PDDCOLS 132
/* double expansion required for some reason */
#define PDD_DESCRIBE(F,X)   fprintf(F,"#  %-21.20s 0x%08X\n", #X, X)
#define PDD_DESCRIBe(F,X,C) fprintf(F,"#  %-21.20s 0x%08X  (%s)\n", #X, X, C)
/* components of HOPS_PLOT_DATA_MASK */
#define PDD_HEADER          0x00000001
#define PDD_SBD_AMP         0x00000002
#define PDD_MBD_AMP         0x00000004
#define PDD_DLYRATE         0x00000008
#define PDD_XPSPEC          0x00000010
#define PDD_PHASOR          0x00000020
#define PDD_WEIGHTS         0x00000040
#define PDD_MEAN_AP         0x00000080
#define PDD_SEG_AMP         0x00000100
#define PDD_SEG_PHS         0x00000200
#define PDD_SEG_FRAC_USB    0x00000400
#define PDD_SEG_FRAC_LSB    0x00000800
#define PDD_SEG_REFSCNT_USB 0x00001000
#define PDD_SEG_REFSCNT_LSB 0x00002000
#define PDD_SEG_REMSCNT_USB 0x00004000
#define PDD_SEG_REMSCNT_LSB 0x00008000
#define PDD_SEG_REFBIAS_USB 0x00010000
#define PDD_SEG_REFBIAS_LSB 0x00020000
#define PDD_SEG_REMBIAS_USB 0x00040000
#define PDD_SEG_REMBIAS_LSB 0x00080000
#define PDD_SEG_REFPCAL     0x00100000
#define PDD_SEG_REMPCAL     0x00200000
#define PDD_SEG_PLOT_INFO   0x00400000
#define PDD_SEG_RESERVED1   0x00800000
#define PDD_SEG_AMP_FILTER  0x01000000
#define PDD_MODELINFO       0x02000000
#define PDD_FINEPRINT       0x04000000
#define PDD_SEG_RESERVED2   0x08000000
#define PDD_SEG_RESERVED3   0x10000000
#define PDD_SEG_RESERVED4   0x20000000
#define PDD_SEG_RESERVED5   0x40000000
#define PDD_LEGEND          0x80000000
#define PDD_ALL             0x83FFFFFF
#define PDD_SOME \
    (PDD_HEADER|PDD_SBD_AMP|PDD_MBD_AMP|PDD_DLYRATE|PDD_XPSPEC|PDD_PHASOR \
    |PDD_WEIGHTS|PDD_MEAN_AP|PDD_SEG_AMP|PDD_SEG_PHS|PDD_LEGEND)
#endif /* PLOT_DATA_DIR_IMPLEMENTATION */

/*
 * eof
 */
