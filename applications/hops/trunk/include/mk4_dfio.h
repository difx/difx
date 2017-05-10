#ifndef MK4_DFIO_H
#define MK4_DFIO_H

/*----------------------------------------------------------------------------*/
#include <stdio.h>
#include <sys/types.h>

#include "type_000.h"
#include "type_100.h"
#include "type_101.h"
#include "type_110.h"
#include "type_120.h"
#include "type_200.h"
#include "type_201.h"
#include "type_202.h"
#include "type_203.h"
#include "type_204.h"
#include "type_205.h"
#include "type_206.h"
#include "type_207.h"
#include "type_208.h"
#include "type_210.h"
#include "type_212.h"
#include "type_220.h"
#include "type_221.h"
#include "type_222.h"
#include "type_230.h"
#include "type_300.h"
#include "type_301.h"
#include "type_302.h"
#include "type_303.h"
#include "type_304.h"
#include "type_305.h"
#include "type_306.h"
#include "type_307.h"
#include "type_308.h"
#include "type_309.h"
#include "mk4_typedefs.h"
#include "mk4_data.h"


/*----------------------------------------------------------------------------*/
extern struct type_100 * addr_100 (short, void *, int *);
extern struct type_101 * addr_101 (short, void *, int *);
extern struct type_110 * addr_110 (short, void *, int *);
extern struct type_120 * addr_120 (short, void *, int *);
extern struct type_200 * addr_200 (short, void *, int *);
extern struct type_201 * addr_201 (short, void *, int *);
extern struct type_202 * addr_202 (short, void *, int *);
extern struct type_203 * addr_203 (short, void *, int *);
extern struct type_204 * addr_204 (short, void *, int *);
extern struct type_205 * addr_205 (short, void *, int *);
extern struct type_206 * addr_206 (short, void *, int *);
extern struct type_207 * addr_207 (short, void *, int *);
extern struct type_208 * addr_208 (short, void *, int *);
extern struct type_210 * addr_210 (short, void *, int *);
extern struct type_212 * addr_212 (short, void *, int *);
extern struct type_220 * addr_220 (short, void *, int *);
extern struct type_221 * addr_221 (short, void *, int *);
extern struct type_222 * addr_222 (short, void *, int *);
extern struct type_230 * addr_230 (short, void *, int *);
extern struct type_300 * addr_300 (short, void *, int *);
extern struct type_301 * addr_301 (short, void *, int *);
extern struct type_302 * addr_302 (short, void *, int *);
extern struct type_303 * addr_303 (short, void *, int *);
extern struct type_304 * addr_304 (short, void *, int *);
extern struct type_305 * addr_305 (short, void *, int *);
extern struct type_306 * addr_306 (short, void *, int *);
extern struct type_307 * addr_307 (short, void *, int *);
extern struct type_308 * addr_308 (short, void *, int *);
extern struct type_309 * addr_309 (short, void *, int *);

/*----------------------------------------------------------------------------*/
extern void clear_100 (struct type_100 *);
extern void clear_101 (struct type_101 *);
extern void clear_110 (struct type_110 *);
extern void clear_120 (struct type_120 *);
extern void clear_200 (struct type_200 *);
extern void clear_201 (struct type_201 *);
extern void clear_202 (struct type_202 *);
extern void clear_203 (struct type_203 *);
extern void clear_204 (struct type_204 *);
extern void clear_205 (struct type_205 *);
extern void clear_206 (struct type_206 *);
extern void clear_207 (struct type_207 *);
extern void clear_208 (struct type_208 *);
extern void clear_210 (struct type_210 *);
extern void clear_212 (struct type_212 *);
extern void clear_220 (struct type_220 *);
extern void clear_221 (struct type_221 *);
extern void clear_222 (struct type_222 *);
extern void clear_230 (struct type_230 *);
extern void clear_300 (struct type_300 *);
extern void clear_301 (struct type_301 *);
extern void clear_302 (struct type_302 *);
extern void clear_303 (struct type_303 *);
extern void clear_304 (struct type_304 *);
extern void clear_305 (struct type_305 *);
extern void clear_306 (struct type_306 *);
extern void clear_307 (struct type_307 *);
extern void clear_308 (struct type_308 *);
extern void clear_309 (struct type_309 *);

/*----------------------------------------------------------------------------*/
extern int copy_100 (struct type_100 *, char **);
extern int copy_101 (struct type_101 *, char **);
extern int copy_110 (struct type_110 *, char **);
extern int copy_120 (struct type_120 *, char **);
extern int copy_200 (struct type_200 *, char **);
extern int copy_201 (struct type_201 *, char **);
extern int copy_202 (struct type_202 *, char **);
extern int copy_203 (struct type_203 *, char **);
extern int copy_204 (struct type_204 *, char **);
extern int copy_205 (struct type_205 *, char **);
extern int copy_206 (struct type_206 *, char **);
extern int copy_207 (struct type_207 *, char **);
extern int copy_208 (struct type_208 *, char **);
extern int copy_210 (struct type_210 *, char **);
extern int copy_212 (struct type_212 *, char **);
extern int copy_220 (struct type_220 *, char **);
extern int copy_221 (struct type_221 *, char **, int*);
extern int copy_222 (struct type_222 *, char **, int*);
extern int copy_230 (struct type_230 *, char **);
extern int copy_300 (struct type_300 *, char **);
extern int copy_301 (struct type_301 *, char **);
extern int copy_302 (struct type_302 *, char **);
extern int copy_303 (struct type_303 *, char **);
extern int copy_304 (struct type_304 *, char **);
extern int copy_305 (struct type_305 *, char **);
extern int copy_306 (struct type_306 *, char **);
extern int copy_307 (struct type_307 *, char **);
extern int copy_308 (struct type_308 *, char **);
extern int copy_309 (struct type_309 *, char **);

/*----------------------------------------------------------------------------*/
extern int  alloc_t120_array (int, int, struct index_tag *);
extern void clear_coord (struct sky_coord *);
extern void clear_mk4corel (struct mk4_corel *);
extern void clear_mk4fringe (struct mk4_fringe *);
/* extern void clear_mk4root (struct mk4_root *); */
extern void clear_mk4sdata (struct mk4_sdata *);
extern void compress (U16, U8 *, U8 *, U32, U8 *, U32 *);
extern void compress_compress (U8 *, U8 *, U32, U8 *, U32 *);
extern void compress_decompress (U8 *, U8 *, U32, U8 *, U32 *);
extern int  corel_alloc (struct mk4_corel *, int, int);
extern char display_221 (struct type_221 *, int);
extern int  init_000 (struct type_000 *, char *);
extern int  open_datafile (char [], int *, FILE **);
extern int  read_mk4corel (char *, struct mk4_corel *);
extern int  read_mk4file (FILE *, char **);
extern int  read_mk4fringe (char *, struct mk4_fringe *);
/* extern int  read_mk4root (char *, struct mk4_root *); */
extern int  read_mk4sdata (char *, struct mk4_sdata *);
extern int  write_err (FILE *, char *);
extern int  write_mk4corel (struct mk4_corel *, char *);
extern int  write_mk4fringe (struct mk4_fringe *, char *);
extern int  write_mk4root (char *, char *);
extern int  write_mk4sdata (struct mk4_sdata *, char *);
extern int  write_record (char *, FILE *, int *);
#endif
