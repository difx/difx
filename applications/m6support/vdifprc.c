/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifprc.c 5766 2023-03-26 20:22:29Z gbc $
 *
 * This file supports information about the running vdifuse process.
 * The general notion is similar to /proc in that one should be able
 * to look at specific files to get specific information.  The data
 * management is similar to the main cache, but here we worry less
 * in the event of errors as this is for information only.
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "vdifuse.h"
#include "vdifprc.h"

/*
 * For readability, we introduce this macro to extract
 * and assign the basename from a file full path name, e.g.
 *
 *    assign_basename(*name,(vfe[index])->name,".damaged-entry");
 *    assign_basename(*name,vfe->name,".damaged-item");
 *
 * as seen below.
 */
#define assign_basename(X,Y,Z) do {     \
        (X) = strrchr((Y), '/');        \
        if (!(X)) (X) = (Z);            \
        else ++((X)); /* past '/' */    \
    } while(0)

/*
 * The cache manipulation similar to/simpler than vd_cache
 * One difference is that we can live without it, so use
 * vp_num_alloced to flag whether this is viable.
 */
#define is_cache_full() ((vp_num_entries+1) >= vp_num_alloced)
static VPROCEntry *vp_cache = 0;
static uint32_t vp_num_entries = 0;
static uint32_t vp_num_alloced = 0;
static uint32_t vp_mod_counter = 0;
static int create_vp_cache(void)
{
    vp_cache = (VPROCEntry*)calloc(VPROC_CACHE_CHUNK, sizeof(VPROCEntry));
    if (!vp_cache) return(perror("calloc"),1);
    vp_num_alloced = VPROC_CACHE_CHUNK;
    vp_num_entries = 0;
    return(0);
}
static int extend_vp_cache(void)
{
    size_t new_bytes, tot_bytes;
    if (!vp_cache) return(1);
    if (vp_num_alloced > (0xFFFFFFFEU - VPROC_CACHE_CHUNK))
        return(fprintf(stderr, "Cache limit exceeded\n"));
    vp_num_alloced += VPROC_CACHE_CHUNK;
    new_bytes = VPROC_CACHE_CHUNK * sizeof(VPROCEntry);
    tot_bytes = vp_num_alloced * sizeof(VPROCEntry);
    vp_cache = (VPROCEntry *)realloc(vp_cache, tot_bytes);
    if (!vp_cache) return(perror("realloc"),2);
    memset(vp_cache + vp_num_entries, 0, new_bytes);
    return(0);
}
static int extend_dir_falloc(VPROCEntry *ve)
{
    /* this is for the topdir creation */
    if (!ve) return(0);
    /* should have been allocated at birth */
    if (!ve->content.files) return(1);
    if (ve->fcount >= ve->falloc) {
        ve->falloc += VPROC_CACHE_CHUNK;
        ve->content.files = (VPROCEntry**)realloc(
            ve->content.files, ve->falloc);
        if (!ve->content.files) {
            ve->falloc -= VPROC_CACHE_CHUNK;
            vdiftrace(-1,VDT("%s falloc %d fail\n"),
                ve->name, ve->falloc);
            return(perror("realloc:epsb"),1);
        } else {
            vdiftrace(-1,VDT("%s falloc %d pass\n"),
                ve->name, ve->falloc);
        }
    }
    return(0);
}

static VPROCEntry *create_vproc_entry(int ctype)
{
    VPROCEntry *vpe;
    if (is_cache_full() && extend_vp_cache()) return(NULL);
    vpe = &vp_cache[vp_num_entries++];
    vpe->ctype = ctype;
    vpe->flast = -1;
    if (ctype == VPROC_ENTRY_ISDIR) {
        /* directories get an initial allocation */
        vpe->falloc = VPROC_CACHE_CHUNK;
        vpe->content.files =
            (VPROCEntry**)calloc(vpe->falloc, sizeof(VPROCEntry*));
        if (!vpe->content.files)
            return(perror("calloc:dir"),(vpe->falloc = 0),NULL);
    } else if (ctype == VPROC_ENTRY_AFILE) {
        /* files just get a null pointer to start */
        vpe->content.bytes = NULL;
        vpe->falloc = 0;
    }
    vpe->fcount = 0;
    set_creation_time(&vpe->vpsb);
    return(vpe);
}
static void report_vproc_entry(int ee, VPROCEntry *vpe)
{
    int nn;
    char *ct[4] = {"EMPTY", "ISDIR", "AFILE", "FREED"};
    VPROCEntry *fce;
    vdiftrace(-1,VDT(
        "vp[%04d] is %s[%d] (%06o) %p, '%s' files: %d/%d ptr %p\n"),
        ee, ct[vpe->ctype],vpe->vpsb.st_nlink,
        vpe->vpsb.st_mode, vpe, vpe->name,
        vpe->fcount, vpe->falloc, vpe->content);
    if (vpe->fcount > 0) {  /* for directories, list the files */
        for (fce = vpe->content.files[nn=0]; nn < vpe->fcount; nn++, fce++) {
            vdiftrace(-1,VDT("vp[%04d] file %3d (%06o) %p, '%s'\n"),
                ee, nn, fce->vpsb.st_mode, fce, fce->name);
        }
    } else {                /* for files, just the content */
        vdiftrace(-1,VDT("vp[%04d] file contents.bytes:  %-.40s%s"),
            ee, vpe->content.bytes, /* assumes newline termination */
            strlen(vpe->content.bytes) > 40 ? "...\n" : "");
    }
}
void report_vproc(const char *where)
{
    VPROCEntry *vpe = vp_cache;
    int ee;
    if (vp_mod_counter == 0) return;    /* nothing new */
    vdiftrace(-1,VDT("%s: %s\n"), where, vdifuse_mount_realpath);
    for (ee = 0; ee < vp_num_entries; ee++, vpe++) {
        report_vproc_entry(ee, vpe);
        vdifuse_flush_trace();
    }
    vp_mod_counter = 0;
}

/*
 * Open, read, write support for the vprocdir area.
 *
 * vorr_{open,release} are handled as for fragments, with the file handle
 * being set to indicate a real file.  However when it comes time to read,
 * we need to provide the read from our memory not via pread().  The FFInfo
 * area is born calloc'd and 0 would refer to the top dir, so that is a
 * good invalid indicator.  If we find the file, we save the index so that
 * on the second trip through with vorr_read() we don't have to hunt.
 */
char *vproc_realpath(const char *fusepath, FFInfo *ffi)
{
    VPROCEntry *vpp = vp_cache;
    int ee;
    if (!ffi) return(NULL);
    if (strncmp(fusepath,vdifproc_topdir,vdifproc_topdir_len)) return(NULL);
    for (ee = 0; ee < vp_num_entries; ee++, vpp++)
        if (!strcmp(fusepath, vpp->name)) {
            ffi->vpendx = ee;
            vdiftrace(-1,VDT("vproc orr[%d]\n"), ffi->vpendx);
            vdifuse_flush_trace();
            return((char*)1);       /* i.e. not NULL */
        }
    return(NULL);
}
/*
 * The read itself is a trivial memcpy once we figure out which bytes.
 * The request is for ffi->size bytes at offset ffi->offset.
 */
int vproc_read(char *buf, FFInfo *ffi)
{
    size_t blen;
    char *bytes = NULL;
    if (!ffi) return(-1);
    if (ffi->vpendx <= 0) return(-1);
    bytes = vp_cache[ffi->vpendx].content.bytes;
    blen = strlen(bytes);
    if (ffi->offset >= blen) return(0);   /* no bytes to xfer */
    /* ignore initial run of bytes */
    blen -= ffi->offset;
    bytes += ffi->offset;
    /* if less that the full thing is requested */
    if (ffi->size < blen) blen = ffi->size;
    memcpy(buf, bytes, blen);
    return((int)blen);
}

/*
 * Filler support for vdifuse_getattr() for subdirs and files below toplevel.
 * Returns information about path....update a standard response with
 * file size and update times.  Similar to vdifuse_fragment().
 * Returning 0 implies stbuf info is correct.
 */
int vdifuse_vprocdir(const char *path, struct stat *stbuf)
{
    VPROCEntry *vpp = vp_cache;
    int ii;
    for (ii = 0; ii < vp_num_entries; ii++, vpp++)
        if (!strcmp(path, vpp->name)) {
            memcpy(stbuf, &vpp->vpsb, sizeof(struct stat));
            return(0);  /* found it */
        }
    return(1);  /* not found */
}

/*
 * Filler support for vdifuse_readdir() which provides the names in
 * the main vprocdir directory.  Return 1 for each entry in the list for
 * the files of the toplevel vprocdir...and 0 when all have been supplied.
 * This is similar to get_vdifuse_fragment() we know where the top-level
 * dir is in the cache, and we merely need to report on its files.
 *
 * index is the entry to supply (starting with 0).
 */
int get_vdifuse_vprocdir(int index, char **name, struct stat **stbuf)
{
    VPROCEntry **vfe = vp_cache->content.files;
    if (!vfe || index >= vp_cache->fcount) return(0);
    assign_basename(*name,(vfe[index])->name,".damaged-entry");
    *stbuf = &((vfe[index++])->vpsb);
    return(index);  /* yes, a top-dir file for filler() */
}

/*
 * Filler support for vdifuse_readdir():
 * Returns vp_cache site of the matching directory or 0 if not.
 */
int get_vprocdir_subdir(const char *path)
{
    int nn;
    for (nn = 0; nn < vp_num_entries; nn++)
        if (!strcmp(path, vp_cache[nn].name))
            return(nn);
    return(0);
}

/*
 * Filler support for vdifuse_readdir():
 * as with get_vdifuse_vprocdir() above, supply the items.
 * Returns the index of the next file to report or 0 (done).
 * nn is the index of the parent directory in the vp_cache.
 */
int get_vprocdir_subdir_item(int nn, int nx, char **name, struct stat **stbuf)
{
    VPROCEntry *vpe = vp_cache+nn, *vfe;
    if (!vpe || nx >= vpe->fcount) return(0);
    vfe = vpe->content.files[nx];
    assign_basename(*name,vfe->name,".damaged-item");
    *stbuf = &((vfe[nx++]).vpsb);
    return(nx);     /* yes, a file in this directory for filler() */
}

/*
 * dirname is specified for the toplevel, otherwise we build a hierarchy
 * based on read requests (similar to pid in /proc).  The stat sb is
 * of a relative that should be copied and then modified.  The last
 * argument is the parent, whose falloc'ation may need enhancement.
 */
static VPROCEntry *create_a_dir(char *dirname, struct stat *sb, VPROCEntry *ve)
{
    VPROCEntry *dce;
    if (!dirname || !sb || strlen(dirname) > VPROC_MAX_PATH) return(NULL);
    dce = create_vproc_entry(VPROC_ENTRY_ISDIR);
    if (!dce || extend_dir_falloc(ve)) return(NULL);
    strncpy(dce->name, dirname, sizeof(dce->name)-1);
    dce->ctype = VPROC_ENTRY_ISDIR;
    dce->vpsb = *sb;
    dce->vpsb.st_mode = S_IFDIR | 0555;
    dce->vpsb.st_nlink = 3;
    if (ve) {
        ve->vpsb.st_nlink ++;
        ve->content.files[ve->fcount++] = dce;
    }
    vp_mod_counter ++;
    return(dce);
}
/* this one is for vdifuse itself */
static VPROCEntry *create_vproc_dir(char *dirname, struct stat *sb)
{
    VPROCEntry *dce = create_a_dir(dirname, sb, NULL);
    /* nothing else is required */
    return(dce);
}

/*
 * A simple case, create a pid file for the running vdifuse process
 * If we encounter errors, just move on and don't provide the info.
 * ve is the parent directory for this file.  The estimate parameter
 * is a guess for an initial malloc of content bytes.
 *
 * We start by creating the general utility good for ANY file.
 */
static VPROCEntry *create_a_file(VPROCEntry *ve, size_t estimate)
{
    VPROCEntry *fce;
    /* check on space in dir to add the file */
    if (extend_dir_falloc(ve)) return(NULL);
    /* proceed to create an entry */
    if (!(fce = create_vproc_entry(VPROC_ENTRY_AFILE))) return(NULL);
    fce->ctype = VPROC_ENTRY_AFILE;
    fce->content.bytes = malloc(estimate+24);
    if (!fce->content.bytes) return(perror("malloc:file"),NULL);
    fce->vpsb = ve->vpsb;
    fce->vpsb.st_mode = S_IFREG | 0444;
    fce->vpsb.st_nlink = 2;
    vp_mod_counter ++;
    return(fce);
}
static VPROCEntry *create_file_details(VPROCEntry *ve, const char *base,
    const char *cont)
{
    VPROCEntry *fce;
    int contlen = strlen(cont);
    /* check on lengths to prevent truncated names */
    if (strlen(ve->name) + strlen(base) > VPROC_MAX_PATH-2) return(NULL);
    if (!(fce = create_a_file(ve, contlen))) return(NULL);
    /* update the entry with the content */
    snprintf(fce->name, VPROC_MAX_PATH, "%s/%s", ve->name, base);
    strcpy(fce->content.bytes, cont);
    fce->vpsb.st_size = contlen;
    if (cont[contlen-1] != '\n') {      /* be nice and end with newline */
        strcat(fce->content.bytes, "\n");
        fce->vpsb.st_size ++;
    }
    /* if we get here, the file exists (virtually) */
    ve->vpsb.st_nlink ++;
    ve->content.files[ve->fcount++] = fce;
    return(fce);
}
static void create_pid_file(VPROCEntry *ve)
{
    char pidval[16];
    snprintf(pidval, sizeof(pidval), "%d", getpid());
    (void)create_file_details(ve, "pid", pidval);
}

/*
 * Locate a vp_cache entry by name.
 */
static VPROCEntry *find_dir_by_name(const char *dirname)
{
    VPROCEntry *vpe;
    int ee;
    for (ee = 0, vpe = vp_cache; ee < vp_num_entries; ee++, vpe++) {
        if (!strcmp(dirname, vpe->name)) return(vpe);
    }
    return(NULL);
}
/*
 * Append an iteration suffix to dirname as long as we find duplicate
 * names in the cache so that we can return a unique name.  We'll go
 * with 8-char hex names which makes it unlikely that we will run out.
 */
static char *uniq_dirname(char *dirname, int dnlen)
{
    VPROCEntry *dce;
    uint32_t iteration = 0, fixedlen = strlen(dirname);
    for (iteration = 0; iteration < 0xFFFFFFF0; iteration++) {
        sprintf(dirname+fixedlen, "%08x", iteration);
        dce = find_dir_by_name(dirname);
        if (!dce) return(dirname);
    }
    return(NULL);
}

/*
 * This creates a vproc subdir when a long read (sgv2) is initiated.
 * FFInfo vpendx is used as above to read files within this subdir.
 * FFInfo vparnt holds the vp_cache index of the directory created.
 * 
 * vproc_status_subdir() is the external call to create a subdir that
 *   initially holds a file with the status of the read (e.g. starting
 *   with open_sgv2_seq).  ffi->sindex should be the entry in the
 *   vd_cache and thus a repeatable and  unique value for the sequence
 *   being read.  Subsequent file entries are created with the
 *   vproc_update_file() call (which adds files to this directory).
 */
void vproc_status_subdir(FFInfo *ffi, const char *base, const char *info)
{
    VPROCEntry *dce, *ve = vp_cache;
    int dnlen = vdifproc_topdir_len + 20;
    char *dirname = malloc(dnlen);
    if (!ffi) return;
    snprintf(dirname, dnlen, "%s/%04d-", vdifproc_topdir, ffi->sindex);
    /* build dirname from seq index and borrow top-level sb */
    dce = create_a_dir(uniq_dirname(dirname, dnlen), &vp_cache->vpsb, ve);
    if (!dce) return;
    /* make it easy to find again */
    ffi->vparnt = (int)(dce - vp_cache);
    /* create one or more files in it */
    (void)create_file_details(dce, base, info);
    vdiftrace(-1,VDT("vproc_status_subdir[%d]: %s\n"), ffi->vparnt, dirname);
    vp_mod_counter ++;
}

/*
 * Find a file by base name within the directory entry, returning
 * either the file entry or NULL for not found.  If the create flag
 * is nonzero, create the file if it does not exist.  For efficiency
 * on repeat access, we try flast first if it is non-negative.
 */
VPROCEntry *find_in_dir(const char *base, VPROCEntry *dce, int create)
{
    VPROCEntry *fce;
    int ee;
    char *be;
    /* try the last referenced file first ... */
    if (dce->flast >= 0 && (fce = dce->content.files[dce->flast])) {
        be = strrchr(fce->name, '/');
        if (be && !strcmp(be+1, base)) return(fce);
    }
    /* ... otherwise hunt for it ... */
    for (ee = 0, fce = dce->content.files[0]; ee < dce->fcount; ee++, fce++) {
        if (!(be = strrchr(fce->name, '/'))) continue;
        if (!strcmp(be+1, base)) {
            dce->flast = ee;
            return(fce);
        }
    }
    /* ... otherwise maybe create it. */
    if (create & VPROC_CREATE) {
        fce = create_file_details(dce, base, (create & VPROC_APPEND)
            ? "" : "...file created empty, content to come...");
        dce->flast = dce->fcount - 1;
    } else {
        fce = NULL;
        dce->flast = -1;
    }
    return(fce);
}
/*
 * This is a general routine to change the info in a file
 * (named base).  The directory parent is searched for the
 * file named base, and if found (or created) contents are
 * updated with info.  The create flag:
 *   if VPROC_CREATE set in create it ensures that the file exists
 *   if VPROC_APPEND set in create it appends rather than overwrites
 */
void vproc_update_file(const FFInfo *ffi, const char *base,
    const char *info, int create)
{
    int infolen = strlen(info)+8;
    VPROCEntry *dce, *fce;
    if (!ffi) return;
    dce = vp_cache + ffi->vparnt;
    fce = find_in_dir(base, dce, create);
    if (!fce) return;
    if (create & VPROC_APPEND) infolen += strlen(fce->content.bytes);
    fce->content.bytes = (char *)realloc(fce->content.bytes, infolen);
    if (!fce->content.bytes) return;
    if (create & VPROC_APPEND) strcat(fce->content.bytes, info);
    else                       strcpy(fce->content.bytes, info);
    fce->vpsb.st_size = infolen = strlen(fce->content.bytes);
    /* be nice and end with newline if not there already */
    if (fce->content.bytes[infolen-1] != '\n') {
        strcat(fce->content.bytes, "\n");
        fce->vpsb.st_size ++;
    }
}

/*
 * This function creates the storage area for the proc information.
 *
 * Unlike other fuse directories, this area is more virtual in that
 * there is no backing store for it within the vdifuse cache.  Instead
 * we really just need a table of things-available that can be managed
 * without much overhead; this information is held in VPROCEntry.
 *
 * The activity is similar to the other parts of fuse, however, we do
 * not store information in the main cache, but rather in a simpler
 * version of it as this activity is largely independent of the other.
 *
 * vdifsup_opts() will have called vdifuse_implement() and loaded the
 * main cache, so we can steal some things from the toplevel dir.
 *
 * Here we create the infrastructure and create the process dir entry.
 */
void create_vproc_cache(void)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    VPROCEntry *ve;
    struct stat *stbuftop = &vd_cache[VDIFUSE_TOPDIR_VPROCDIR].u.vfuse;
    if (create_vp_cache()) return;
    /* vdifproc_topdir gets the same stbuf details as mount point */
    ve = create_vproc_dir(vdifproc_topdir, stbuftop);
    if (!ve) return;
    create_pid_file(ve);
    (void)create_file_details(ve, "bread", vdifuse_breadname());
    (void)create_file_details(ve, "trace", vdifuse_tracename());
    (void)create_file_details(ve, "mount", vdifuse_mount_realpath);
    return;
}
/*
 * This function releases the proc storage (when we are done with it).
 * Free all the entries, then destroy the cache itself.  This presumes
 * that dirs are allocated prior to the files they reference, but
 * VPROC_ENTRY_FREED is set to prevent a double-free if not.
 */
void delete_vproc_cache(void)
{
    VPROCEntry *ve;
    int ee;
    for (ee = 0, ve = vp_cache; ee < vp_num_alloced; ee++, ve++) {
        if (ve->ctype == VPROC_ENTRY_ISDIR) {
            if (ve->content.files) {
                free(ve->content.files);
                ve->ctype = VPROC_ENTRY_FREED;
            }
        } else if (ve->ctype != VPROC_ENTRY_FREED) {
            if (ve->content.bytes) {
                free(ve->content.bytes);
                ve->ctype = VPROC_ENTRY_FREED;
            }
        }
    }
    free(vp_cache);
    return;
}

/*
 * eof
 */
