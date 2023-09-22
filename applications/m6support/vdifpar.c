/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifpar.c 5790 2023-04-02 15:27:35Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 * This file provides basic parameter setup and the cache.
 *
 * vdiflog should be used for this file as it is non-fuse work.
 */

#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "vdifuse.h"
#include "sg_access.h"
#include "vdifthr.h"
#include "vdifprc.h"

/* cache manipulation parameters */
#define VDIFUSE_CACHE_CHUNK 32
#define current_cache_full() ((vd_num_entries+1) >= vd_num_alloced)
static char *vd_cache_file = NULL;
static VDIFUSEntry *vd_cache = 0;
static uint32_t vd_num_entries = 0;
static uint32_t vd_num_alloced = 0;
static int vd_cache_modified = 0;

/*
 * Internal parameters that are preserved in the cache.
 * This temporary copy is for command-line adjustments,
 * then they are copied to the cache at the start.
 *
 * Note that during construction, the cache grows as needed
 * with realloc, so cache pointers should be considered dynamic,
 * but that the indices in the cache should be stable.
 */
static VDIFUSEpars clvpo = {
    .station_mask = SG_STATION_MASK,
    .vthr_per_seq = 1,
    .reserved_tre = 0.666,
    .creation.tv_sec = 1,
    .vthread_list = {
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS,
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS,
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS,
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS },
    .reserved_fiv = {
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS,
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS,
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS,
        VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS, VTHREAD_BOGUS }
};
char *filelistprefix = ".";

/*
 * Ok, time for real work
 */
static int accepted_as_frag(VDIFUSEntry *vc)
{
    int rigor, cfe;
    VDIFUSEpars *vp = &vd_cache[0].u.vpars;

    vdiflog(1, "  Considering %s\n", vc->path);
    /* see if the file is acceptable as a fragment */
    rigor = vdif_rigor_frag(vc->path, vp);

    /* if it actually passed the tests that matter */
    if ((rigor & vp->how_rigorous) == vp->how_rigorous) {
        vdiflog(1, "  Create_fragment_vfuse ... %s "
            VDIFUSE_RIGOR_PRINTF "\n", vc->path, rigor);
        /* try to create the fragment (via analyze_fragment())... */
        if ((cfe = create_fragment_vfuse(vc, vd_num_entries, vp, rigor))) {
            /* ... we not able to accept this as a fragment */
            vc->etype = VDIFUSE_ENTRY_INVALID;
            vdiflog(2, "    but failed [create:%d]\n", cfe);
            vdiflog(1, "Problematic file %s [%d]\n", vc->path, cfe);
            vdiflog(1, "  ... Create_fragment_vfuse nogo %s\n", vc->path);
        } else {
            /* then it was officially added as a fragment */
            vc->etype = VDIFUSE_ENTRY_FRAGMENT;
            vdiflog(3, "    Accepted %s\n"
                "      " VDIFUSE_RIGOR_PRINTF " sub-type %u\n",
                vc->path, rigor, vc->stype);
            vdiflog(1, "  ... Create_fragment_vfuse good %s\n", vc->path);
            return(1);
        }
    }
    vdiflog(2, "    Rejected %s " VDIFUSE_RIGOR_PRINTF "\n", vc->path, rigor);
    return(0);
}

/*
 * Helper to extend the cache and zero the new space (to prevent accidents).
 * Upon entry vd_num_entries are assumed to be valid.
 * calloc works in objects, realloc works in bytes.
 */
static int extend_vd_cache(void)
{
    size_t new_bytes, tot_bytes;
    if (vd_num_alloced > (0xFFFFFFFEU - VDIFUSE_CACHE_CHUNK))
        return(fprintf(stderr, "Cache limit exceeded\n"));
    vd_num_alloced += VDIFUSE_CACHE_CHUNK;
    new_bytes = VDIFUSE_CACHE_CHUNK * sizeof(VDIFUSEntry);
    tot_bytes = vd_num_alloced * sizeof(VDIFUSEntry);
    vd_cache = (VDIFUSEntry *)realloc(vd_cache, tot_bytes);
    if (!vd_cache) return(perror("realloc"),1);
    memset(vd_cache + vd_num_entries, 0, new_bytes);
    vdiflog(2, "Extended cache to %d/%d entries (to %p)\n",
        vd_num_entries, vd_num_alloced, vd_cache);
    return(0);
}

/*
 * Convenience functions which let us access the cache from another
 * module.  (Otherwise we'd have to put all the sequence stuff here.)
 *
 * We have vd_num_entries+1 to allow sg_access() to eventually be called
 * sooner.  It doesn't hurt to add an extra entry.
 *
 * In other files:
 *  VDIFUSEntry * vd_cache = current_cache_start();
 *  uint32_t vd_num_entries = current_cache_entries();
 */
VDIFUSEntry *current_cache_start(void) { return(vd_cache); }
uint32_t current_cache_entries(void) { return(vd_num_entries); }
void increment_cache_modified(void) {
    vd_cache_modified++;
}
VDIFUSEntry *create_cache_entry(void) {
    if (current_cache_full() && extend_vd_cache()) return(NULL);
    vd_cache[vd_num_entries].index = vd_num_entries;
    vd_cache[vd_num_entries].etype = VDIFUSE_ENTRY_INVALID;
    vd_cache_modified++;
    return(&vd_cache[vd_num_entries++]);
}
VDIFUSEpars *current_cache_pars(void) {
    return(&(vd_cache[VDIFUSE_TOPDIR_PARAMS].u.vpars));
}

/* defined below */
static int find_fragments(char *dir);

/*
 * Support code for readability in find_fragments()
 */
static int handle_frag(char *file, char *parent)
{
    char *tp;
    VDIFUSEntry *vt = create_cache_entry();
    if (!vt) return(1);
    tp = vt->path;
    if (snprintf(tp, VDIFUSE_MAX_PATH, "%s/%s", parent, file) >=
        VDIFUSE_MAX_PATH) return(fprintf(stderr,
            "Path too long on %s/%s\n", parent, file));
    if (accepted_as_frag(vt)) {
        /* only sgv2 requires an anc entry */
        if (vt->stype == VDIFUSE_STYPE_SGV2)
            attach_sgv2_ancillary(vt->index);
    } else {
        vt->etype = VDIFUSE_ENTRY_INVALID;
        vd_num_entries--;
    }
    return(0);
}
static char *tmp_abspath(char *dir, char *parent)
{
    char *abspath;
    int abslen;
    abslen = strlen(parent) + strlen(dir) + 2;
    if (abslen > VDIFUSE_MAX_PATH) {
        fprintf(stderr, "Path exceeded on %s/%s\n", parent, dir);
        return(NULL);
    }
    abspath = malloc(abslen + 1);
    if (!abspath) return(perror("malloc"),NULL);
    snprintf(abspath, abslen, "%s/%s", parent, dir);
    return(abspath);
}
static int handle_dir(char *dir, char *parent)
{
    char *abspath;
    abspath = tmp_abspath(dir, parent);
    if (!abspath) return(1);
    if (find_fragments(abspath)) return(fprintf(stderr,
        "Problem with subdir %s/%s\n", parent, dir));
    free(abspath);
    return(0);
}

/*
 * For filesystems that do not present the type in the dirent struct,
 * we need to use stat or lstat to figure out what to do.  This version
 * uses stat (so it will ignore symbolic links) and passes the buck.
 */
static int handle_bogey(unsigned char type, char *dir, char *parent)
{
    struct stat sb;
    char *abspath;
    int rv;
    abspath = tmp_abspath(dir, parent);
    if (!abspath) return(1);
    if (stat(abspath, &sb)) return(perror("stat"),2);
    if (S_ISREG(sb.st_mode)) {          /* is it a regular file? */
        rv = handle_frag(dir, parent);
    } else if (S_ISDIR(sb.st_mode)) {   /* directory? */
        rv = find_fragments(abspath);
    } else {
        vdiflog(1, "Type %d on %s/%s mode %x ignored\n",
            type, parent, dir, sb.st_mode);
        rv = 2;
    }
    free(abspath);
    return(rv);
}

/*
 * Walk through a directory hierachy.  For each entry,
 *   ignore certain things
 *   if a file: test it for vdif
 *   if a dir: recurse into it
 * We assume here that we have enough stack space (which seems to be true).
 */
 #if USE_READDIR_R
 #warning "WARNING: using original (deprecated) re-entrant readdir_r()"
 #warning "WARNING: need to go back to svn revision before 5704"
 #endif /* USE_READDIR_R */
/* The new version is similar; NULL is returned at the end, or upon errors  */
static int process_dirent(DIR *dp, char *dir)
{
    struct dirent *dep;
    errno = 0;
    while ((dep = readdir(dp)) && (errno == 0)) {
        if (dep->d_name[0] == '.') {
            continue;   /* ignore hidden files, . and .. */
        } else if (dep->d_type == DT_REG) {
            if (handle_frag(dep->d_name, dir)) return(fprintf(stderr,
                "Problem with fragment %s of %s\n", dep->d_name, dir));
        } else if (dep->d_type == DT_DIR) {
            if (handle_dir(dep->d_name, dir)) return(fprintf(stderr,
                "Problem with directory %s of %s\n", dep->d_name, dir));
        } else {
            if (handle_bogey(dep->d_type, dep->d_name, dir))
                return(fprintf(stderr,
                    "Problem with entry %s of %s\n", dep->d_name, dir));
        }
    }
    if (errno) return(perror("readdir"),2);
    return(0);
}

static int find_fragments(char *dir)
{
    DIR *dp;
    int rv;
    vdiflog(1, "Searching %s\n", dir);
    dp = opendir(dir);
    if (!dp && errno == ENOTDIR) {
        vdiflog(1, "which is not a dir\n");
        return(0);
    }
    if (!dp) return(perror("opendir"),1);
    rv = process_dirent(dp, dir);
    if (rv) fprintf(stderr, "not previous issue with readdir\n");
    return(closedir(dp));
}

/*
 * Called from vdifuse_create_metadata()
 *
 * Cache creation has four steps:
 *   first:  find the fragments of vdif files
 *   second: create a tree of fragments
 *   third:  link them together into sequences
 *   next:   link them together into vthreads
 *   next:   (possibly vdifproc area setup)
 */
int populate_cache(int ndirs, char **dirs)
{
    char *abspath;
    int had;
    vdiflog(0, "Populating cache with %d directories\n", ndirs);
    while (ndirs-- > 0) {
        had = vd_num_entries;
        abspath = realpath(*dirs, 0);
        if (!abspath)
            return(fprintf(stderr, "No Abspath for %s\n", *dirs));
        dirs++;
        if (find_fragments(abspath))
            return(fprintf(stderr, "Error seaching %s\n", abspath));
        vdiflog(0, "Searched %s: %u -> %u entries\n",
            abspath, had, vd_num_entries);
    }
    if (create_fragtree())
        return(fprintf(stderr, "Problem creating fragment tree\n"));
    if (create_sequences())
        return(fprintf(stderr, "Problem creating sequences\n"));
    if (vthreads_dir && create_vthreads())
        return(fprintf(stderr, "Problem creating vthreads\n"));
    /* NOTION: populate vdifproc slot in vd_cache */
    vdiflog(0,
        "Constructed sequences, have %u entries\n", vd_num_entries);
    return(0);
}

static int create_params_entry(void)
{
    int ee = VDIFUSE_TOPDIR_PARAMS, vv;
    vd_cache[ee].index = ee;
    vd_cache[ee].etype = VDIFUSE_ENTRY_PARAMS;
    vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
    /* command line supplied items */
    vd_cache[ee].u.vpars.vdifuse_vers = VDIFUSE_VERSION;
    vd_cache[ee].u.vpars.prefix_bytes = clvpo.prefix_bytes;
    vd_cache[ee].u.vpars.offset_bytes = clvpo.offset_bytes;
    vd_cache[ee].u.vpars.searchwindow = clvpo.searchwindow; // nyi
    vd_cache[ee].u.vpars.catchbuserrs = clvpo.catchbuserrs;
    vd_cache[ee].u.vpars.max_pkts_gap = clvpo.max_pkts_gap; // nyi
    vd_cache[ee].u.vpars.max_secs_gap = clvpo.max_secs_gap; // nyi
    vd_cache[ee].u.vpars.how_rigorous = clvpo.how_rigorous;
    vd_cache[ee].u.vpars.noduplicates = clvpo.noduplicates;
    vd_cache[ee].u.vpars.seqhierarchy = clvpo.seqhierarchy;
    vd_cache[ee].u.vpars.writeblocker = clvpo.writeblocker;
    vd_cache[ee].u.vpars.pkts_per_sec = clvpo.pkts_per_sec;
    vd_cache[ee].u.vpars.dropfraction = clvpo.dropfraction; // nyi
    vd_cache[ee].u.vpars.station_mask = clvpo.station_mask;
    sg_set_station_id_mask(clvpo.station_mask & SG_STATION_MASK);
    vd_cache[ee].u.vpars.vthr_per_seq = clvpo.vthr_per_seq;
    vd_cache[ee].u.vpars.reserved_tre = clvpo.reserved_tre; // nyi

    /* topdir names for fragments, sequences, vthreads and vdifproc */
    if (clvpo.frag_top_dir[0] != '/')
        strcpy(clvpo.frag_top_dir, VDIFUSE_FRAGMENTS);
    strcpy(vd_cache[ee].u.vpars.frag_top_dir, clvpo.frag_top_dir);
    strcpy(fragments_topdir, clvpo.frag_top_dir);
    fragments_topdir_len = strlen(fragments_topdir);
    if (clvpo.seqs_top_dir[0] != '/')
        strcpy(clvpo.seqs_top_dir, VDIFUSE_SEQUENCES);
    strcpy(vd_cache[ee].u.vpars.seqs_top_dir, clvpo.seqs_top_dir);
    strcpy(sequences_topdir, clvpo.seqs_top_dir);
    sequences_topdir_len = strlen(sequences_topdir);
    if (clvpo.vthr_top_dir[0] != '/')
        strcpy(clvpo.vthr_top_dir, VDIFUSE_VTHREADS);
    strcpy(vd_cache[ee].u.vpars.vthr_top_dir, clvpo.vthr_top_dir);
    strcpy(vthreads_topdir, clvpo.vthr_top_dir);
    vthreads_topdir_len = strlen(vthreads_topdir);
    if (clvpo.proc_top_dir[0] != '/')
        strcpy(clvpo.proc_top_dir, VDIFUSE_VPROCDIR);
    strcpy(vd_cache[ee].u.vpars.proc_top_dir, clvpo.proc_top_dir);
    strcpy(vthreads_topdir, clvpo.proc_top_dir);
    vthreads_topdir_len = strlen(vdifproc_topdir);

    /* defaults for discovered items which will be updated*/
    vd_cache[ee].u.vpars.est_pkt_rate = 1;
    vd_cache[ee].u.vpars.maxfrcounter = 0;
    vd_cache[ee].u.vpars.vthreadseper = VTHREAD_BOGUS;
    if (gettimeofday(&vd_cache[ee].u.vpars.creation, 0))
        return(perror("gettimeofday"),1);

    /* in _dir case a list was given, in _seq case it is discovered */
    for (vv = 0; vv < VDIFUSE_MAX_VTHREADS; vv++)
        vd_cache[ee].u.vpars.vthread_list[vv] = clvpo.vthread_list[vv];

    /* not to match anything else, ever */
    strcpy(vd_cache[ee].path, "--cache-parameters-entry-path--");
    strcpy(vd_cache[ee].fuse, "--cache-parameters-entry-fuse--");
    strcpy(vd_cache[ee].hier, "--cache-parameters-entry-hier--");

    vd_cache_modified++;
    return(0);
}

/*
 * Allocate memory for the working cache.
 * This version just callocs a starter and (later) writes it out.
 */
int create_cache(char *cache)
{
    vd_num_alloced = VDIFUSE_CACHE_CHUNK;
    vd_cache = (VDIFUSEntry*)calloc(vd_num_alloced, sizeof(VDIFUSEntry));
    if (!vd_cache) return(perror("calloc"),1);
    /* no reason not to put params entry first */
    if (create_params_entry()) return(2);
    if (create_root_entry(NULL)) return(3);
    if (create_fragments_entry(NULL)) return(4);
    if (create_sequences_entry(NULL)) return(5);
    if (create_vprocdir_entry(NULL)) return(6);
    if (create_vthreads_entry(NULL)) return(7);
    //vd_num_entries = VDIFUSE_TOPDIR_SEQUENCE + 1 + vthreads_dir;
    vd_num_entries = VDIFUSE_TOPDIR_VARIABLE;
    vd_cache_file = cache;
    return(0);
}
int load_cache(char *cache)
{
    int ee = VDIFUSE_TOPDIR_PARAMS;
    struct stat sb;
    FILE *fp;
    size_t ne;

    if (vd_num_entries && vd_cache && vd_cache_file) {
        vdiflog(0, "cache already loaded %d %p %s\n",
            vd_num_entries, vd_cache, vd_cache_file);
        return(0);
    }

    if (stat(cache, &sb)) return(perror("stat"),1);
    vd_num_entries = sb.st_size / sizeof(VDIFUSEntry);
    vd_num_alloced = vd_num_entries;
    vd_cache = (VDIFUSEntry*)calloc(vd_num_entries, sizeof(VDIFUSEntry));
    if (!vd_cache) return(perror("calloc"),1);
    fp = fopen(cache, "r");
    if (!fp) return(fprintf(stderr,"%s: ", cache),
        perror("load_cache fopen"),2);
    ne = fread(vd_cache, sizeof(VDIFUSEntry), vd_num_entries, fp);
    if (ne != vd_num_entries)
        return(fprintf(stderr, "Failed to load complete cache\n"));
    vd_cache_file = cache;

    strcpy(fragments_topdir, vd_cache[ee].u.vpars.frag_top_dir);
    strcpy(sequences_topdir, vd_cache[ee].u.vpars.seqs_top_dir);
    strcpy(vthreads_topdir, vd_cache[ee].u.vpars.vthr_top_dir);
    strcpy(vdifproc_topdir, vd_cache[ee].u.vpars.proc_top_dir);
    fragments_topdir_len = strlen(fragments_topdir);
    sequences_topdir_len = strlen(sequences_topdir);
    vthreads_topdir_len = strlen(vthreads_topdir);
    vdifproc_topdir_len = strlen(vdifproc_topdir);
    sg_set_station_id_mask(vd_cache[ee].u.vpars.station_mask & SG_STATION_MASK);

    return(fclose(fp));
}
int dump_cache(void)
{
    FILE *fp;
    size_t ne;
    if (!vd_cache || vd_num_entries == 0 || !vd_cache_file)
        return(0);   /* no work */
    fp = fopen(vd_cache_file, "w");
    if (!fp) return(fprintf(stderr,"%s: ",vd_cache_file),
        perror("dump_cache fopen"),1);
    ne = fwrite(vd_cache, sizeof(VDIFUSEntry), vd_num_entries, fp);
    if (ne != vd_num_entries)
        return(fprintf(stderr, "Failed to write complete cache\n"));
    vdiflog(0, "Wrote cache to %s (%d mods)\n",
        vd_cache_file, vd_cache_modified);
    return(fclose(fp));
}

int describe_params(VDIFUSEntry *vp)
{
    time_t birth = vp->u.vpars.creation.tv_sec;
    if (vdifuse_debug>2) describe_struct();
    vdiflog(1, 
        "[%05d]Cache (vers.%g) %lu.%06lu (%24.24s)\n",
        vp->index,
        vp->u.vpars.vdifuse_vers,
        vp->u.vpars.creation.tv_sec, vp->u.vpars.creation.tv_usec,
        ctime(&birth));
    vdiflog(2, 
        "  prefix_bytes=%u offset_bytes=%u searchwindow=%u\n"
        "  catchbuserrs=%u station_mask=%x vthr_per_seq=%d\n"
        "  max_pkts_gap=%u max_secs_gap=%f "
            "how_rigorous=" VDIFUSE_RIGOR_PRINTF "\n"
        "  noduplicates=%u seqhierarchy=%u writeblocker=%u\n"
        "  pkts_per_sec=%u dropfraction=%f maxfrcounter=%u\n"
        "  est_pkt_rate=%u frag_top_dir=%s seqs_top_dir=%s\n"
        "  proc_top_dir=%s vthr_top_dir=%s\n" ,
        vp->u.vpars.prefix_bytes,
        vp->u.vpars.offset_bytes,
        vp->u.vpars.searchwindow,   // nyi
        vp->u.vpars.catchbuserrs,
        vp->u.vpars.station_mask,
        vp->u.vpars.vthr_per_seq,
        vp->u.vpars.max_pkts_gap,   // nyi
        vp->u.vpars.max_secs_gap,   // nyi
        vp->u.vpars.how_rigorous,
        vp->u.vpars.noduplicates,
        vp->u.vpars.seqhierarchy,
        vp->u.vpars.writeblocker,
        vp->u.vpars.pkts_per_sec,
        vp->u.vpars.dropfraction,   // nyi
        vp->u.vpars.maxfrcounter,
        vp->u.vpars.est_pkt_rate,
        vp->u.vpars.frag_top_dir,
        vp->u.vpars.seqs_top_dir,
        vp->u.vpars.proc_top_dir,
        vp->u.vpars.vthr_top_dir
    );
    if (vp->u.vpars.vthr_per_seq > 0) vdiflog(2,
        "  vthread_list | 0: %d,%d,%d,%d,%d,%d,%d,%d :7\n"
        "  sep %5u    | 8: %d,%d,%d,%d,%d,%d,%d,%d :15\n",
        vp->u.vpars.vthread_list[0], vp->u.vpars.vthread_list[1],
        vp->u.vpars.vthread_list[2], vp->u.vpars.vthread_list[3],
        vp->u.vpars.vthread_list[4], vp->u.vpars.vthread_list[5],
        vp->u.vpars.vthread_list[6], vp->u.vpars.vthread_list[7],
        vp->u.vpars.vthreadseper,
        vp->u.vpars.vthread_list[8], vp->u.vpars.vthread_list[9],
        vp->u.vpars.vthread_list[10], vp->u.vpars.vthread_list[11],
        vp->u.vpars.vthread_list[12], vp->u.vpars.vthread_list[13],
        vp->u.vpars.vthread_list[14], vp->u.vpars.vthread_list[15]);
    return(0);
}

/*
 * Show the warts
 */
static int vdifuse_issues(void)
{
    vdiflog(-1, "\n");
    vdiflog(-1, "Single-threaded VDIF is the most tested.\n");
    vdiflog(-1, "Multi-threaded VDIF is minimally supported for sg2:\n");
    vdiflog(-1, "  (VGOS) every sg2 block is a single thread.\n");
    vdiflog(-1, "  (NOEMA) vpts threads expected per slot.\n");
    vdiflog(-1, "A single data rate is expected and must be\n");
    vdiflog(-1, "  supplied for good results: i.e. -xrate=pkts/sec.\n");
    vdiflog(-1, "Many cases of data corruption are handled, but\n");
    vdiflog(-1, "  seriously corrupt data is likely to be tossed.\n");
    vdiflog(-1, "\n");
    return(1);
}

/*
 * Yet more help
 */
static int vdifuse_examples(void)
{
    vdiflog(-1, "\n");
    vdiflog(-1, "With single-threaded Mark6 scatter-gather files,\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "  vdifuse -a <label>.cache -xm6sg -xrate=125000 \\ \n");
    vdiflog(-1, "    ./mnt-<label> /mnt/disks/[<group>]/?/data\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "would examine the data directors of the recorded\n");
    vdiflog(-1, "group on a set of modules <label> (e.g. one session\n");
    vdiflog(-1, "from some experiment) and mount it locally.\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "The packet rate is required for best results.\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "For a subset of scans, -xinclpatt=<RE> can be used\n");
    vdiflog(-1, "to restrict attention to the scans of interest.\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "See prep-one-scan.sh for more sophisticated usage.\n");
    vdiflog(-1, "\n");
    return(1);
}

static int vdifuse_vthrhelp(void)
{
    vdiflog(-1, "\n");
    vdiflog(-1, "There are three flavors of support for VDIF threads.\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "In the normal case, the fragments are assumed to be\n");
    vdiflog(-1, "single-threaded (vtps=1) and a single-threaded\n");
    vdiflog(-1, "segment is generated.\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "THIS IS IN DEVELOPMENT:\n");
    vdiflog(-1, "If there are multiple threads in the fragments (the\n");
    vdiflog(-1, "NOEMA case) then vtps=4 must be supplied (and the\n");
    vdiflog(-1, "processing must be per-slot).\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "THIS IS NOT YET IMPLEMENTED:\n");
    vdiflog(-1, "If there are multiple threads in the fragments, but\n");
    vdiflog(-1, "each block is homogeneous (VGOS), then single-threaded\n");
    vdiflog(-1, "segments can be created under the vthreads directory.\n");
    vdiflog(-1, "\n");
    return(1);
}

static int vdifuse_env(void)
{
    vdiflog(-1, "\n");
    vdiflog(-1, "For Mark6 scatter-gather (sg) files, two\n");
    vdiflog(-1, "environment variables may be used to adjust\n");
    vdiflog(-1, "the performance:\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "  SG_ACCESS_BLOCKS\n");
    vdiflog(-1, "    suggests a memory management block size.  The\n");
    vdiflog(-1, "    default is 40000000 which has been found optimal\n");
    vdiflog(-1, "    for some Mark6's used in testing.\n");
    vdiflog(-1, "  SG_ACCESS_ADVICE\n");
    vdiflog(-1, "    an integer suggesting the memory management\n");
    vdiflog(-1, "    advice strategy:\n");
    vdiflog(-1, "    0   disabled\n");
    vdiflog(-1, "    1   madvise 'sequential'\n");
    vdiflog(-1, "    2   madvise 'will need' (default)\n");
    vdiflog(-1, "    3   fadvise 'will need'\n");
    vdiflog(-1, "    4   spawns threads to read ahead\n");
    vdiflog(-1, "    5   same thing, different plan (broken)\n");
    vdiflog(-1, "1-3 depend on the kernel, OS and read-ahead (which");
    vdiflog(-1, "may be adjusted with the set-read-ahead.sh script).\n");
    vdiflog(-1, "If those do not work, method 4 should do better.\n");
    vdiflog(-1, "\n");
    vdiflog(-1, "You can do this:\n");
    vdiflog(-1, "  SG_ACCESS_ADVICE=? ... prep-one-scan.sh\n");
    vdiflog(-1, "or make a custom script\n");
    vdiflog(-1, "\n");
    return(1);
}

/*
 * Additional support options, supplied in key=value form.
 * Nonzero return indicates an error.
 */
static int vdifuse_options_help(void)
{
    vdiflog(-1, "\n");
    vdiflog(-1, "General help on -x <option> and -x <key=val>:\n");
    vdiflog(-1, "  help          provides this help\n");
    vdiflog(-1, "  issues        provides some additional info\n");
    vdiflog(-1, "  examples      provides some examples\n");
    vdiflog(-1, "  vthreads      provides some help on vdif threads\n");
    vdiflog(-1, "  env           help on environment variables\n");

    vdiflog(-1, "\n");
    vdiflog(-1, "Options that may be set to affect cache creation:\n");
    vdiflog(-1, "(On options marked with *, using \"help\" instead\n");
    vdiflog(-1, "of a value will provide further information.  And\n");
    vdiflog(-1, "options marked ! are envisioned, but not yet useful.)\n");
    vdiflog(-1, "  debug=<int>   set internal debugging level (%d)\n",
        vdifuse_debug);
    vdiflog(-1, "  delete        deletes any existing cache %s\n",
        vdifuse_protect ? "(protect)" : "(delete)");
    vdiflog(-1, "  protect       protects existing cache %s\n",
        vdifuse_protect ? "(protect)" : "(delete)");
    vdiflog(-1, "  reuse         coopts a mount point in use (%s)\n",
        vdifuse_reuse ? "(reuse)" : "(respect)");
    vdiflog(-1, "  respect       respects a mount point in use (%s)\n",
        vdifuse_reuse ? "(reuse)" : "(respect)");

    vdiflog(-1, "  list=<str>    # filelist prefix (.) for -m option\n");

    vdiflog(-1, "  prefix=<int>  # of bytes (%u) before 1st packet\n",
        clvpo.prefix_bytes);
    vdiflog(-1, "  offset=<int>  # of bytes (%u) before header\n",
        clvpo.offset_bytes);
    vdiflog(-1, "! window=<int>  # of bytes (%u) slop on these\n",
        clvpo.searchwindow);    // nyi
    vdiflog(-1, "  buserr=<int>  catch bus errors (%u)\n",
        clvpo.catchbuserrs);
    vdiflog(-1, "! gap=<int>     max # of packets between (%u) files\n",
        clvpo.max_pkts_gap);    // nyi
    vdiflog(-1, "! gaps=<float>  max # of seconds between (%g) files\n",
        clvpo.max_secs_gap);    // nyi
    vdiflog(-1, "* rigor=<int>   methods to examine files (%u)\n",
        clvpo.how_rigorous);
    vdiflog(-1, "  uniq=<int>    asserts that filenames are unique (%u)\n",
        clvpo.noduplicates);
    vdiflog(-1, "  hier=<int>    directory hierarchy limit (%u)\n",
        clvpo.seqhierarchy);
    vdiflog(-1, "  block=<int>   write blocking factor (%u)\n",
        clvpo.writeblocker);
    vdiflog(-1, "  rate=<int>    of packets/sec (%u) in files\n",
        clvpo.pkts_per_sec);
    vdiflog(-1, "! drop=<float>  fraction of dropped packets (%g)\n",
        clvpo.dropfraction);    // nyi
    vdiflog(-1, "  frag=<string> top-dir name for fragments (%s)\n",
        clvpo.frag_top_dir);
    vdiflog(-1, "  seqs=<string> top-dir name for sequences (%s)\n",
        clvpo.seqs_top_dir);
    vdiflog(-1, "  vthr=<string> top-dir name for vthreads (%s)\n",
        clvpo.vthr_top_dir);
    vdiflog(-1, "  proc=<string> top-dir name for vdifproc (%s)\n",
        clvpo.proc_top_dir);
    vdiflog(-1, "  smask=<float> station mask for sg files (0x%X)\n",
        clvpo.station_mask);
    vdiflog(-1, "  vtps=<int>    # of threads per sequence (%d)\n",
        clvpo.vthr_per_seq);
    regexhelp();

    vdiflog(-1, "\n");
    vdiflog(-1, "Some standard setups:\n");
    vdiflog(-1, "  files         setup for general files\n");
    vdiflog(-1, "  m6sg          setup for Mark6 scatter-gather dirs\n");
    vdiflog(-1, "  m6raid        setup for Mark6 raid dirs\n");
    vdiflog(-1, "  m6noema       setup for Mark6 (sg) NOEMA\n");
    vdiflog(-1, "  m6vgos        setup for Mark6 (sg) VGOS (NYI)\n");
    vdiflog(-1, "(i.e. -xm6sg -xrate=125000)\n");
    vdiflog(-1, "\n");

    return(1);
}

/*
 * More detailed help
 */
static void rigor_help(void)
{
    vdiflog(-1, "\n");
    vdiflog(-1, "This is a mask of ways to check files:\n");
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  will pass anthing.\n",
        VDIFUSE_RIGOR_NOCHECK);
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  tests for *.vdif\n",
        VDIFUSE_RIGOR_SUFFIX);
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  tests for "
        "<exp>_<sc>_<scan><any>.vdif\n",
        VDIFUSE_RIGOR_ENAME);
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  tests for "
        "scatter-gather vers. 2\n",
        VDIFUSE_RIGOR_MAGIC);
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  tests for min file size (%d)\n",
        VDIFUSE_RIGOR_MINSIZE, VDIFUSE_MIN_FILE_SIZE);
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  tests based on regex on name\n",
        VDIFUSE_RIGOR_REGEX);
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  tests based "
        "on vthreads for sequences\n",
        VDIFUSE_RIGOR_VTHRSEQ);
    vdiflog(-1, "  " VDIFUSE_RIGOR_HPRINTF "  tests based "
        "on vthreads for directory\n",
        VDIFUSE_RIGOR_VTHRDIR);
    vdiflog(-1, "\n");
}

/*
 * Handler for command-line options.
 * clvpo should be initialized to 0, and a few things have non-zero
 * default values, now.
 */
int vdifuse_options(char *arg)
{
    if (!strncmp(arg, "help", 4)) {
        return(vdifuse_options_help());
    } else if (!strncmp(arg, "issues", 6)) {
        return(vdifuse_issues());
    } else if (!strncmp(arg, "examples", 8)) {
        return(vdifuse_examples());
    } else if (!strncmp(arg, "vthreads", 8)) {
        return(vdifuse_vthrhelp());
    } else if (!strncmp(arg, "env", 3)) {
        return(vdifuse_env());

    } else if (!strncmp(arg, "debug=", 6)) {
        vdifuse_debug = atoi(arg + 6);
        vdiflog(-1, "Debugging level set to %u\n", vdifuse_debug);
    } else if (!strncmp(arg, "delete", 6)) {
        vdifuse_protect = 0;
        vdiflog(-1, "Deleting any pre-existing cache\n");
    } else if (!strncmp(arg, "protect", 7)) {
        vdifuse_protect = 1;
        vdiflog(-1, "Protecting any pre-existing cache\n");
    } else if (!strncmp(arg, "reuse", 5)) {
        vdifuse_reuse = 1;
        vdiflog(-1, "Fusermount will be invoked if the mount is in use\n");
    } else if (!strncmp(arg, "respect", 7)) {
        vdifuse_reuse = 0;
        vdiflog(-1, "Existing mounts will be respected;"
            " fusermount will not be called to unmount anything\n");

    } else if (!strncmp(arg, "list=", 5)) {
        filelistprefix = arg+5;
        vdiflog(-1, "Using %s as path prefix in file list\n", filelistprefix);

    } else if (!strncmp(arg, "prefix=", 7)) {
        clvpo.prefix_bytes = atoi(arg + 7);
        vdiflog(-1, "Assuming %u bytes prior to first packet\n",
            clvpo.prefix_bytes);
    } else if (!strncmp(arg, "offset=", 7)) {
        clvpo.offset_bytes = atoi(arg + 7);
        vdiflog(-1, "Assuming %u bytes prior each header\n",
            clvpo.offset_bytes);
    } else if (!strncmp(arg, "window=", 7)) {   // nyi
        clvpo.searchwindow = atoi(arg + 7);
        vdiflog(-1, "Allowing %u bytes of slop in searches\n",
            clvpo.searchwindow);
    } else if (!strncmp(arg, "buserr=", 7)) {
        clvpo.catchbuserrs = atoi(arg + 7);
        vdiflog(-1, "Bus error trapping: %s\n",
            clvpo.catchbuserrs ? "On" : "Off");
    } else if (!strncmp(arg, "gaps=", 5)) { // nyi
        clvpo.max_secs_gap = atof(arg + 5);
        vdiflog(-1, "At most %g seconds between fragments\n",
            clvpo.max_secs_gap);
    } else if (!strncmp(arg, "gap=", 4)) {  // nyi
        clvpo.max_pkts_gap = atoi(arg + 4);
        vdiflog(-1, "At most %u packets between fragments\n",
            clvpo.max_pkts_gap);
    } else if (!strncmp(arg, "rigor=help", 10)) {
        rigor_help();
    } else if (!strncmp(arg, "rigor=", 6)) {
        clvpo.how_rigorous = atoi(arg + 6);
        vdiflog(-1, "VDIF testing at rigor level %u\n", clvpo.how_rigorous);
    } else if (!strncmp(arg, "uniq=", 5)) {
        clvpo.noduplicates = atoi(arg + 5);
        vdiflog(-1, clvpo.noduplicates ?
            "Filenames assumed unique\n" : "Expect duplicate filenames\n");
    } else if (!strncmp(arg, "hier=", 5)) {
        clvpo.seqhierarchy = atoi(arg + 5);
        if (clvpo.seqhierarchy > VDIFUSE_MAX_HIER)
            clvpo.seqhierarchy = VDIFUSE_MAX_HIER;
        vdiflog(-1, "Directory hierachy limit now %u\n", clvpo.seqhierarchy);
    } else if (!strncmp(arg, "block=", 6)) {
        clvpo.writeblocker = atoi(arg + 6);
        vdiflog(-1, "Packets assumed blocked within %uB buffer\n",
            clvpo.writeblocker);
    } else if (!strncmp(arg, "rate=", 5)) {
        clvpo.pkts_per_sec = atoi(arg + 5);
        vdiflog(-1, "Assuming %u packets per second\n", clvpo.pkts_per_sec);
    } else if (!strncmp(arg, "drop=", 5)) { // nyi
        clvpo.dropfraction = atof(arg + 5);
        vdiflog(-1, "Assuming packets-dropped/total is %g\n",
            clvpo.dropfraction);
    } else if (!strncmp(arg, "smask=", 6)) {
        clvpo.station_mask = atoi(arg + 6);
        vdiflog(-1, "Masking sg signatures with 0x%4X\n", clvpo.station_mask);
    } else if (!strncmp(arg, "vtps=", 5)) {
        clvpo.vthr_per_seq = atoi(arg + 5);
        vdiflog(-1, "Expecting %d vthreads per file sequence\n",
            clvpo.vthr_per_seq);
        vthreads_seq = 1;

    } else if (!strncmp(arg, "frag=", 5)) {
        if (arg[5] == '/' && strlen(arg+5) < VDIFUSE_TOPDIR_SIZE) {
            strcpy(clvpo.frag_top_dir, arg+5);
            vdiflog(-1, "Using %s for top-dir fragments directory name\n",
                clvpo.frag_top_dir);
        } else {
            fprintf(stderr, "%s is not suitable for fragments\n", arg+5);
        }
    } else if (!strncmp(arg, "seqs=", 5)) {
        if (arg[5] == '/' && strlen(arg+5) < VDIFUSE_TOPDIR_SIZE) {
            strcpy(clvpo.seqs_top_dir, arg+5);
            vdiflog(-1, "Using %s for top-dir sequences directory name\n",
                clvpo.seqs_top_dir);
        } else {
            fprintf(stderr, "%s is not suitable for sequences\n", arg+5);
        }
    } else if (!strncmp(arg, "vthr=", 5)) {
        if (arg[5] == '/' && strlen(arg+5) < VDIFUSE_TOPDIR_SIZE) {
            strcpy(clvpo.vthr_top_dir, arg+5);
            vdiflog(-1, "Using %s for top-dir vthreads directory name\n",
                clvpo.vthr_top_dir);
        } else {
            fprintf(stderr, "%s is not suitable for sequences\n", arg+5);
        }
    } else if (!strncmp(arg, "proc=", 5)) {
        if (arg[5] == '/' && strlen(arg+5) < VDIFUSE_TOPDIR_SIZE) {
            strcpy(clvpo.proc_top_dir, arg+5);
            vdiflog(-1, "Using %s for top-dir vdifproc directory name\n",
                clvpo.vthr_top_dir);
        } else {
            fprintf(stderr, "%s is not suitable for sequences\n", arg+5);
        }

    /*
     * Support for exclude/include capabilities
     *   exclpatt=<pattern> exclfile=<file-with-patterns>
     *   inclpatt=<pattern> inclfile=<file-with-patterns>
     */
    } else if (!strncmp(arg, "exclpatt=", 9)) {
        if (!regexclude(arg+9)) {
            vdiflog(-1, "Excluding any files matching %s\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s was not accepted for exclusion\n", arg+9);
        }
    } else if (!strncmp(arg, "inclpatt=", 9)) {
        if (!reginclude(arg+9)) {
            vdiflog(-1, "Including only files matching %s\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s was not accepted for inclusion\n", arg+9);
        }
    } else if (!strncmp(arg, "exclfile=", 9)) {
        if (!regexcludefile(arg+9)) {
            vdiflog(-1, "Excluding any files matching patterns in %s\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s had no useful exclusion patterns\n", arg+9);
        }
    } else if (!strncmp(arg, "inclfile=", 9)) {
        if (!regincludefile(arg+9)) {
            vdiflog(-1, "Including only files matching %s patterns\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s had no useful include patterns\n", arg+9);
        }

    /* other quick options might be useful here */
    } else if (!strncmp(arg, "files", 5)) {
        vdiflog(-1, "Development setup for general file usage.\n");
        clvpo.noduplicates = 1;
        clvpo.writeblocker = 0;
        clvpo.how_rigorous = VDIFUSE_RIGOR_ENAME | VDIFUSE_RIGOR_MINSIZE;
    } else if (!strncmp(arg, "m6raid", 6)) {
        vdiflog(-1, "Development setup for Mark6 Raid.\n");
        clvpo.noduplicates = 1;
        clvpo.writeblocker = 0;
        clvpo.how_rigorous = VDIFUSE_RIGOR_ENAME | VDIFUSE_RIGOR_MINSIZE;
    } else if (!strncmp(arg, "m6sg", 4)) {
        vdiflog(-1, "Development setup Mark6 SG vers 2.\n");
        vdifuse_protect = 1;
        vdifuse_reuse = 1;
        clvpo.noduplicates = 0;
        clvpo.catchbuserrs = 1;
        clvpo.how_rigorous = VDIFUSE_RIGOR_MAGIC | VDIFUSE_RIGOR_MINSIZE;
        clvpo.seqhierarchy = 3;
        clvpo.writeblocker = 10000000;
        clvpo.pkts_per_sec = 125000;
     /* clvpo.vthr_per_seq = 1 by default */
    } else if (!strncmp(arg, "m6noema", 7)) {
        vdiflog(-1, "Development setup Mark6 SG vers 2 for NOEMA.\n");
        vdifuse_protect = 1;
        vdifuse_reuse = 1;
        clvpo.noduplicates = 0;
        clvpo.catchbuserrs = 1;
        clvpo.how_rigorous = VDIFUSE_RIGOR_ENAME | VDIFUSE_RIGOR_MINSIZE;
        clvpo.how_rigorous |= VDIFUSE_RIGOR_MAGIC | VDIFUSE_RIGOR_VTHRSEQ;
        clvpo.seqhierarchy = 3;
        clvpo.writeblocker = 10000000;
        clvpo.pkts_per_sec = 125000;
        clvpo.vthr_per_seq = 4;
    } else if (!strncmp(arg, "m6vgos", 6)) {
        vdiflog(0, "Development setup Mark6 SG vers 2 for VGOS.\n");
        fprintf(stderr, "ERROR: m6vgos is not YET implmented\n");
        /* the following requires support of _dir vthreads */
        /* FIXME: VGOS not yet implemented ... */
        vdifuse_protect = 1;
        vdifuse_reuse = 1;
        clvpo.noduplicates = 0;
        clvpo.catchbuserrs = 1;
        clvpo.how_rigorous = VDIFUSE_RIGOR_ENAME | VDIFUSE_RIGOR_MINSIZE;
        clvpo.how_rigorous |= VDIFUSE_RIGOR_MAGIC | VDIFUSE_RIGOR_VTHRDIR;
        clvpo.seqhierarchy = 3;
        clvpo.writeblocker = 10000000;
        clvpo.pkts_per_sec = 125000;
        clvpo.vthr_per_seq = 0;     /* vthreads doesn't use sequences */
        return(1);
    } 

    return(0);
}

/*
 * Final cleanup actions
 */
int vdifuse_finish(void)
{
    if (vd_cache_modified) return(dump_cache());
    vdiflog(0, "Cache was not modified\n");
    return(0);
}

/*
 * eof
 */
