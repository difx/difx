/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifprc.h 5806 2023-04-04 22:00:36Z gbc $
 *
 * This provides support for the vdifuse process area.
 */
#ifndef vdifprc_h
#define vdifprc_h

#include <sys/stat.h>

#ifndef VDIFUSE_TOPDIR_SIZE
#define VDIFUSE_TOPDIR_SIZE 16
#endif /* VDIFUSE_TOPDIR_SIZE */

/* this limits us to 32 files per directory */
#define VPROC_CACHE_CHUNK   8
/* except the toplevel will have many files */
#define VPROC_ROOTDIR_MULT  1024
/* and the cache will have even greater needs */
#define VPROC_CACHE_MULT    8192

#define VPROC_MAX_DIR_SIZE VDIFUSE_TOPDIR_SIZE
#define VPROC_MAXFILE_SIZE  32
#define VPROC_MAXDEPTH      4
#define VPROC_MAX_PATH \
    (VPROC_MAXDEPTH * (5+VPROC_MAX_DIR_SIZE) + VPROC_MAXFILE_SIZE)

/*
 * Holder for process information
 */
typedef struct vproc_entry {
    char        name[VPROC_MAX_PATH];   /* dir or file */
    int         nlen;                   /* avoid repeated strlen()s */
    int         ctype;                  /* VPROC_ENTRY* */
    union vproc{
        char    *bytes;                 /* for files */
        struct vproc_entry **files;     /* for dirs */
    } content;                          /* information */
    int         fcount;                 /* count of files if dir */
    int         falloc;                 /* current allocation if dir */
    int         flast;                  /* last file accessed */
    struct stat vpsb;                   /* stat buf */
} VPROCEntry;

/* content type is one of the following: EMPTY -> file|dir -> FREED */
#define VPROC_ENTRY_EMPTY   0
#define VPROC_ENTRY_ISDIR   1
#define VPROC_ENTRY_AFILE   2
#define VPROC_ENTRY_FREED   3

/* bits for the vproc_update_file(...create) flag */
#define VPROC_CREATE        0x1
#define VPROC_APPEND        0x2
/* copies of these from vdifuse.h */
#define COPY_VPROC_IF_FOUND  0x0     /* update only if it exists */
#define COPY_VPROC_TRUNCATE  0x1     /* (make) and replace contents */
#define COPY_VPROC_APPENDIF  0x2     /* append only if it is found */
#define COPY_VPROC_LENGTHEN  0x3     /* (make) and append contents */

/* methods called from vdifuse.c which includes this file */
extern int vdifuse_vprocdir(const char *path, struct stat *stbuf);
extern int get_vdifuse_vprocdir(int index, char **name, struct stat **stbuf);
extern int get_vprocdir_subdir(const char *path);
extern int get_vprocdir_subdir_item(int nn,
    int ndx, char **name, struct stat **stbuf);
extern void create_vproc_cache(void);
extern void delete_vproc_cache(void);

/* methods that are used in other places (and appear in vdifuse.h) */
extern void report_vproc(const char *where);
extern char *vproc_realpath(const char *fusepath, FFInfo *ffi);
extern int vproc_read(char *buf, FFInfo *ffi);
extern void vproc_status_subdir(FFInfo *ffi, const char *base,
    const char *info);
extern void vproc_update_file(const FFInfo *ffi, const char *base,
    const char *info, int create);

#endif /* vdifprc_h */
/*
 * eof
 */
