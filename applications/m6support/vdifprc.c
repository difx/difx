/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifprc.c 5804 2023-04-04 21:11:55Z gbc $
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
 * For directory debugging, you can adjust these defines.
 */
#define VPX 2       /* -1 for details */
#define VPE 1       /* 0 for directory report */
#define VDF 0       /* to skip the special flushing */
#define VDFT do{if(VDF)vdifuse_flush_trace();}while(0)

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
#define is_cache_full() ((vp_num_entries+2) >= vp_num_alloced)
static VPROCEntry *vp_cache = 0;
static uint32_t vp_num_entries = 0;
static uint32_t vp_num_alloced = 0;
static uint32_t vp_mod_counter = 0;
static int create_vp_cache(void)
{
    vp_num_alloced = VPROC_CACHE_CHUNK * VPROC_CACHE_MULT;
    vp_cache = (VPROCEntry*)calloc(vp_num_alloced, sizeof(VPROCEntry));
    if (!vp_cache) return(perror("calloc"),1);
    vp_num_entries = 0;
    return(0);
}
static int extend_vp_cache(void)
{
    size_t new_bytes, tot_bytes;
    size_t increase = VPROC_CACHE_CHUNK * VPROC_CACHE_MULT;
    if (!vp_cache) return(1);
    /* not sure what the real limit is...a research project */
    if (vp_num_alloced > (0xFFFFFFFEU - increase))
        return(fprintf(stderr, "Cache limit exceeded\n"));
    vp_num_alloced += increase;
    new_bytes = increase * sizeof(VPROCEntry);
    tot_bytes = vp_num_alloced * sizeof(VPROCEntry);
    vp_cache = (VPROCEntry *)realloc(vp_cache, tot_bytes);
    if (!vp_cache) return(perror("realloc"),2);
    memset(vp_cache + vp_num_entries, 0, new_bytes);
    vdiftrace(-1,VDT("Extend vp_cache %p for %d,%d->%d\n"),
        vp_cache, vp_num_alloced - increase,
        vp_num_entries, vp_num_alloced);
    return(0);
}
static int extend_dir_falloc(VPROCEntry *ve)
{
    size_t ptr_bytes, increase = VPROC_CACHE_CHUNK;
    /* this is for the topdir creation */
    if (!ve) return(0);
    /* only directories may be so extended */
    if (ve->ctype != VPROC_ENTRY_ISDIR) return(1);
    /* should have been allocated at birth */
    if (!ve->content.files) return(2);
    /* ok, proceed to expand the allocation */
    if (ve->fcount >= ve->falloc) {
        if (ve == vp_cache) increase *= VPROC_ROOTDIR_MULT;
        ve->falloc += increase;
        ptr_bytes = increase * sizeof(VPROCEntry *);
        /* note, realloc() aborts on real issues */
        vdiftrace(-1,VDT("Extend %s falloc %p for %d->%d\n"),
            ve->name, ve->content.files, ve->fcount, ve->falloc);
        VDFT;
        /* in which case the above helps for gdb */
        ve->content.files = (VPROCEntry**)realloc(
            ve->content.files, ptr_bytes);
        if (!ve->content.files) {
            /* well, this directory is now blown */
            ve->flast = ve->fcount = ve->falloc = 0;
            memset(&ve->vpsb, 0, sizeof(struct stat));
            vdiftrace(-1,VDT("%s falloc %d fail\n"),
                ve->name, ve->falloc);
            return(perror("realloc:epsb"),1);
        } else {
            /* we should be in good shape */
            vdiftrace(-1,VDT("Extend %s falloc %p %d->%d pass\n"),
                ve->name, ve->content.files, ve->fcount, ve->falloc);
            /* nuke the newly allocated file pointers */
            memset(ve->content.files + ve->fcount, 0, ptr_bytes);
        }
    }
    return(0);
}

/*
 * Create an entry of the appropriate type.
 * If we extend vp_cache, the pointer *pve will need an update
 */
static VPROCEntry *create_vproc_entry(int ctype, VPROCEntry **pve)
{
    VPROCEntry *vpe;
    off_t pve_delta = *pve - vp_cache;
    if (is_cache_full() && extend_vp_cache()) return(NULL);
    *pve = vp_cache + pve_delta;
    vpe = &vp_cache[vp_num_entries++];
    vpe->ctype = ctype;
    vpe->flast = -1;
    vpe->fcount = 0;
    if (ctype == VPROC_ENTRY_ISDIR) {
        /* directories get an initial allocation */
        vpe->falloc = VPROC_CACHE_CHUNK;
        if (pve_delta == 0) vpe->falloc *= VPROC_ROOTDIR_MULT;
        vpe->content.files =
            (VPROCEntry**)calloc(vpe->falloc, sizeof(VPROCEntry*));
        if (!vpe->content.files)
            return(perror("calloc:dir"),(vpe->falloc = 0),NULL);
        /* pointers automatically cleared */
        vdiftrace(-1, VDT("Created edir entry %05d with content %p[%d]\n"),
            vpe - vp_cache, vpe->content.files, vpe->falloc);
    } else if (ctype == VPROC_ENTRY_AFILE) {
        /* files just get a null pointer to start */
        vpe->content.bytes = NULL;
        vpe->falloc = 0;
        vdiftrace(VPX, VDT("Created efil entry %05d with no content\n"),
            vpe - vp_cache);
    }
    set_creation_time(&vpe->vpsb);
    VDFT;
    return(vpe);
}
static void report_vproc_entry(int ee, VPROCEntry *vpe)
{
    int nn;
    char *ct[4] = {"EMPTY", "ISDIR", "AFILE", "FREED"};
    VPROCEntry *fce;
    vdiftrace(-1,VDT(
        "vp[%05d] is %s[%d] (%06o) %p, '%s'(%d) files: %d/%d ptr %p .. %p\n"),
        ee, ct[vpe->ctype],vpe->vpsb.st_nlink,
        vpe->vpsb.st_mode, vpe, vpe->name, vpe->nlen,
        vpe->fcount, vpe->falloc, vpe->content, (vpe->fcount > 0)
            ? &(vpe->content.files[vpe->falloc-1]) : NULL );
    if (vpe->fcount > 0) {  /* for directories, list the files */
        for (nn = 0; nn < vpe->fcount; nn++) {
            fce = vpe->content.files[nn];
            vdiftrace(-1,VDT("vp[%05d] file %3d (%06o) %p->%p, '%s'(%d)\n"),
                ee, nn, fce->vpsb.st_mode,
                vpe->content.files + nn, fce, fce->name, fce->nlen);
        }
        vdiftrace(-1,VDT("vp[%05d] allo %3d (%06o) %p, '(last slot)'\n"),
            ee, vpe->falloc-1, 0, vpe->content.files[vpe->falloc-1]);
    } else {                /* for files, just the content */
        vdiftrace(-1,VDT("vp[%05d] file contents.bytes:  %-.10s%s"),
            ee, vpe->content.bytes, /* assumes newline termination */
            strlen(vpe->content.bytes) > 10 ? "...\n" : "");
    }
    VDFT;
}
void report_vproc(const char *where)
{
    VPROCEntry *vpe = vp_cache;
    int ee;
    if (vp_mod_counter == 0) return;    /* nothing new */
    vdiftrace(-1,VDT("%s: %s\n"), where, vdifuse_mount_realpath);
    if (VPE) return;
    for (ee = 0; ee < vp_num_entries; ee++, vpe++) {
        report_vproc_entry(ee, vpe);
        VDFT;
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
    /* check first that it is one of ours */
    if (strncmp(fusepath,vdifproc_topdir,vdifproc_topdir_len)) return(NULL);
    /* if so, search for it */
    for (ee = 0; ee < vp_num_entries; ee++, vpp++)
        if (!strcmp(fusepath, vpp->name)) {
            ffi->vpendx = ee;
            vdiftrace(-1,VDT("vproc orr[%d]\n"), ffi->vpendx);
            VDFT;
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
    VPROCEntry *vpp = vp_cache;
    vdiftrace(VPX, VDT("get_vprocdir_subdir: path %s\n"), path);
    for (nn = 0; nn < vp_num_entries; nn++, vpp++)
        if (!strcmp(vpp->name, path)) {
            vdiftrace(VPX, VDT("get_vprocdir_subdir: found at %d\n"), nn);
            return(nn);
        }
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
    if (nn == 0) return(0);     /* not top-level */
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
 * argument is the parent (vp_cache), whose falloc'ation may need enhancement
 * to support the new directory.
 */
static VPROCEntry *create_a_dir(char *dirname, struct stat *sb, VPROCEntry *ve)
{
    VPROCEntry *dce;
    if (!dirname || !sb || strlen(dirname) > VPROC_MAX_PATH) return(NULL);
    dce = create_vproc_entry(VPROC_ENTRY_ISDIR, &ve);
    if (!dce || extend_dir_falloc(ve)) return(NULL);
    /* we now have the new dir and its parent has room */
    strncpy(dce->name, dirname, sizeof(dce->name)-1);
    dce->nlen = strlen(dce->name);
    dce->ctype = VPROC_ENTRY_ISDIR;
    dce->vpsb = *sb;
    dce->vpsb.st_mode = S_IFDIR | 0555;
    dce->vpsb.st_nlink = 3;
    if (ve) {
        vdiftrace(VPX,VDT("fcount %d/%d now %s in vp[%05d]\n"),
            ve->fcount, ve->vpsb.st_nlink, dce->name, ve - vp_cache);
        ve->vpsb.st_nlink ++;
        ve->content.files[ve->fcount++] = dce;
    }
    vp_mod_counter ++;
    vdiftrace(-1,
        VDT("Created adir %s(%d) in %s\n"), dce->name, dce->nlen, ve->name);
    VDFT;
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
 * dce is the parent directory for this file.  The estimate parameter
 * is a guess for an initial malloc of content bytes.
 *
 * We start by creating the general utility good for ANY file.
 */
static VPROCEntry *create_a_file(VPROCEntry **dce, size_t estimate)
{
    VPROCEntry *fce;
    /* check on space in dir to add the file */
    if (extend_dir_falloc(*dce)) return(NULL);
    /* proceed to create an entry */
    if (!(fce = create_vproc_entry(VPROC_ENTRY_AFILE, dce))) return(NULL);
    fce->ctype = VPROC_ENTRY_AFILE;
    fce->content.bytes = malloc(estimate+24);
    if (!fce->content.bytes) return(perror("malloc:file"),NULL);
    fce->vpsb = (*dce)->vpsb;
    fce->vpsb.st_mode = S_IFREG | 0444;
    fce->vpsb.st_nlink = 2;
    vp_mod_counter ++;
    return(fce);
}
/*
 * This creates the file; ve is the parent directory
 */
static VPROCEntry *create_file_details(VPROCEntry *ve, const char *base,
    const char *cont)
{
    VPROCEntry *fce;
    int contlen = strlen(cont);
    vdiftrace(VPX, VDT("create_file_details %p %s\n"), ve, base);
    /* check on lengths to prevent truncated names */
    if (strlen(ve->name) > VPROC_MAX_PATH - VPROC_MAXFILE_SIZE) return(NULL);
    if (strlen(base) > VPROC_MAXFILE_SIZE-2) return(NULL);
    if (!(fce = create_a_file(&ve, contlen))) return(NULL);
    /* update the entry with the content */
    //snprintf(fce->name, VPROC_MAX_PATH, "%s/%s", ve->name, base);
    strcpy(fce->name, ve->name);
    strcat(fce->name, "/");
    strcat(fce->name, base);
    strcpy(fce->content.bytes, cont);
    fce->nlen = strlen(fce->name);
    fce->vpsb.st_size = contlen;
    if (cont[contlen-1] != '\n') {      /* be nice and end with newline */
        strcat(fce->content.bytes, "\n");
        fce->vpsb.st_size ++;
    }
    /* if we get here, the file exists (virtually) */
    vdiftrace(VPX,VDT("fcount %d/%d now %s in vp[%05d]\n"),
        ve->fcount, ve->vpsb.st_nlink, fce->name, ve - vp_cache);
    ve->vpsb.st_nlink ++;
    ve->content.files[ve->fcount++] = fce;
    vdiftrace(-1, VDT("Created file %s(%d) with %d bytes, parent %d\n"),
        fce->name, fce->nlen, fce->vpsb.st_size, ve - vp_cache);
    VDFT;
    return(fce);
}
static void create_pid_file(VPROCEntry *ve)
{
    char pidval[16];
    snprintf(pidval, sizeof(pidval), "%d", getpid());
    (void)create_file_details(ve, "pid", pidval);
}

/*
 * Locate a vp_cache entry by name.  Used only in uniq_dirname.
 */
static inline VPROCEntry *find_dir_by_name(const char *dirname)
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
static inline char *uniq_dirname(char *dirname)
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
    snprintf(dirname, dnlen, "%s/%05d-", vdifproc_topdir, ffi->sindex);
    /* build dirname from seq index and borrow top-level sb */
    dce = create_a_dir(uniq_dirname(dirname), &vp_cache->vpsb, ve);
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
static VPROCEntry *find_in_dir(const char *base, VPROCEntry *dce, int create)
{
    VPROCEntry *fce;
    int ee;
    char *be;
    vdiftrace(VPX, VDT("Looking for %s in vp[%05d] create %04X\n"),
        base, dce - vp_cache, create);
    /* try the last referenced file first ... */
    if (dce->flast >= 0 && dce->flast < dce->fcount &&
        (fce = dce->content.files[dce->flast])) {
        vdiftrace(VPX, VDT(" trylast? %s...\n"), fce->name);
        be = strrchr(fce->name, '/');
        if (be && !strcmp(be+1, base)) {
            vdiftrace(VPX, VDT(" GOT IT at %d\n"), dce->flast);
            return(fce);
        }
    }
    /* ... otherwise hunt for it ... */
    for (ee = 0; ee < dce->fcount; ee++) {
        fce = dce->content.files[ee];
        vdiftrace(VPX, VDT(" trying   %s...\n"), fce->name);
        if (!(be = strrchr(fce->name, '/'))) continue;
        if (!strcmp(be+1, base)) {
            dce->flast = ee;
            vdiftrace(VPX, VDT(" GOT IT at %d\n"), ee);
            return(fce);
        }
    }
    /* ... otherwise maybe create it in the appropriate subdir */
    if ((create & VPROC_CREATE) && (dce > vp_cache)) {
        fce = create_file_details(dce, base, (create & VPROC_APPEND)
            ? "" : "...file created empty, content to come...");
        dce->flast = dce->fcount - 1;
        if (dce->flast < 0) dce->flast = 0;
        vdiftrace(VPX, VDT("Created find %s in %s\n"), base, dce->name);
    } else {
        fce = NULL;
        dce->flast = -1;
        vdiftrace(VPX, VDT("Not Found %s\n"), base);
    }
    VDFT;
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
    /* this only applies to subdirectories */
    if (!ffi || (ffi->vparnt == 0)) return;
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
    vdiftrace(VPX, VDT("Updated %s in %s\n"), base, dce->name);
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
            ve->content.files = NULL;
        } else if (ve->ctype != VPROC_ENTRY_FREED) {
            if (ve->content.bytes) {
                free(ve->content.bytes);
                ve->ctype = VPROC_ENTRY_FREED;
            }
            ve->content.bytes = NULL;
        }
    }
    free(vp_cache);
    return;
}

/*
 * eof
 */
