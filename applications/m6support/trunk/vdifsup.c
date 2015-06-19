/*
 * $Id: vdifsup.c 2706 2014-12-15 15:07:17Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 * This file does the work related to the metadata cache.
 */

#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "vdifuse.h"

/*
 * Internal parameters that are preserved in the cache.
 * This temporary copy is for command-line adjustments,
 * then they are copied to the cache at the start.
 *
 * Note that during construction, the cache grows as needed
 * with realloc, so cache pointers should be considered dynamic,
 * but that the indices in the cache should be stable.
 */
static VDIFUSEpars clvpo = { .creation.tv_sec = 1 };

#define VDIFUSE_CACHE_CHUNK 8
static char *vd_cache_file = NULL;
static VDIFUSEntry *vd_cache = 0;
static uint32_t vd_num_entries = 0;
static uint32_t vd_num_alloced = 0;
static int vd_cache_modified = 0;

/*
 * Ok, time for real work
 */
static int accepted_as_frag(VDIFUSEntry *vc)
{
    int rigor = 0, cfe = 0;
    VDIFUSEpars *vp = &vd_cache[0].u.vpars;

    if (vdifuse_debug>1) fprintf(vdflog, "  Considering %s\n", vc->path);
    rigor |= vdif_rigor_by_nocheck(vc->path, vp);
    rigor |= vdif_rigor_by_suffix(vc->path, vp);
    rigor |= vdif_rigor_by_ename(vc->path, vp);
    rigor |= vdif_rigor_by_magic(vc->path, vp);
    rigor |= vdif_rigor_by_minsize(vc->path, vp);
    rigor |= vdif_rigor_by_regex(vc->path, vp);
    if (vdifuse_debug>3) fprintf(vdflog,
        "    passed tests " VDIFUSE_RIGOR_PRINTF "\n", rigor);

    /* if it actually passed the tests that matter */
    if ((rigor & vp->how_rigorous) == vp->how_rigorous) {
        if ((cfe = create_fragment_vfuse(vc, vd_num_entries, vp, rigor))) {// ||
            /* then we not able to accept this as a fragment */
            vc->etype = VDIFUSE_ENTRY_INVALID;
	    if (vdifuse_debug>2) fprintf(vdflog,
		"    but failed [create:%d]\n", cfe);
            if (vdifuse_debug>0) fprintf(vdflog,
                "Problematic file %s [%d]\n", vc->path, cfe);
        } else {
            /* then it is officially added as a fragment */
            vc->etype = VDIFUSE_ENTRY_FRAGMENT;
            if (vdifuse_debug>3) fprintf(vdflog,
                "    Accepted %s\n"
                "      " VDIFUSE_RIGOR_PRINTF " sub-type %u\n",
                vc->path, rigor, vc->stype);
            return(1);
        }
    }
    if (vdifuse_debug>2) fprintf(vdflog,
        "    Rejected %s " VDIFUSE_RIGOR_PRINTF "\n", vc->path, rigor);
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
    VDIFUSEntry *old = vd_cache;
    VDIFUSEntry *new_cache;
    if (vd_num_alloced > (0xFFFFFFFEU - VDIFUSE_CACHE_CHUNK))
        return(fprintf(stderr, "Cache limit exceeded\n"));
    vd_num_alloced += VDIFUSE_CACHE_CHUNK;
    new_bytes = VDIFUSE_CACHE_CHUNK * sizeof(VDIFUSEntry);
    tot_bytes = vd_num_alloced * sizeof(VDIFUSEntry);
    new_cache = (VDIFUSEntry *)realloc(vd_cache, tot_bytes);
    if (!new_cache) return(perror("realloc"),1);
    memset(new_cache + vd_num_entries, 0, new_bytes);
    vd_cache = new_cache;
    if (vdifuse_debug>2) fprintf(vdflog,
        "Extended cache to %d/%d entries (from %p to %p)\n",
            vd_num_entries, vd_num_alloced, old, new_cache);
    return(0);
}

/*
 * Convenience functions which let us access the cache from another
 * module.  (Otherwise we'd have to put all the sequence stuff here.)
 *
 * We have vd_num_entries+1 to allow sg_access() to eventually be called
 * sooner.  It doesn't hurt to add an extra entry.
 */
#define current_cache_full() ((vd_num_entries+1) >= vd_num_alloced)
VDIFUSEntry *current_cache_start(void) { return(vd_cache); }
uint32_t current_cache_entries(void) { return(vd_num_entries); }
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
            attach_sgv2_anc(vt->index);
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
        if (vdifuse_debug>1) fprintf(vdflog,
            "Type %d on %s/%s mode %x ignored\n",
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
 * TODO: we assume here that we have enough stack space.
 */
static int find_fragments(char *dir)
{
    DIR *dp;
    struct dirent de, *dep;
    if (vdifuse_debug>1) fprintf(vdflog, "Searching %s\n", dir);
    dp = opendir(dir);
    if (!dp && errno == ENOTDIR) {
        if (vdifuse_debug>1) fprintf(vdflog, "which is not a dir\n");
        return(0);
    }
    if (!dp) return(perror("opendir"),1);
    do {
        if (readdir_r(dp, &de, &dep)) return(perror("readdir_r"),2);
        if (dep == &de) {
            if (de.d_name[0] == '.') {
                continue;   /* ignore hidden files, . and .. */
            } else if (de.d_type == DT_REG) {
                if (handle_frag(de.d_name, dir)) return(fprintf(stderr,
                    "Problem with fragment %s of %s\n", de.d_name, dir));
            } else if (de.d_type == DT_DIR) {
                if (handle_dir(de.d_name, dir)) return(fprintf(stderr,
                    "Problem with directory %s of %s\n", de.d_name, dir));
            } else {
                if (handle_bogey(de.d_type, de.d_name, dir))
                    return(fprintf(stderr,
                        "Problem with entry %s of %s\n", de.d_name, dir));
            }
        }
    } while (dep);
    return(closedir(dp));
}

/*
 * Cache creation has four steps:
 *   first:  find the fragments of vdif files
 *   second: create a tree of fragments
 *   third:  link them together into sequences
 *   fourth: finalize bookkeeping data
 */
static int populate_cache(int ndirs, char **dirs)
{
    char *abspath;
    int had;
    if (vdifuse_debug>0) fprintf(vdflog,
        "Populating cache with %d directories\n", ndirs);
    while (ndirs-- > 0) {
        had = vd_num_entries;
        abspath = realpath(*dirs, 0);
        if (!abspath)
            return(fprintf(stderr, "No Abspath for %s\n", *dirs));
        dirs++;
        if (find_fragments(abspath))
            return(fprintf(stderr, "Error seaching %s\n", abspath));
        if (vdifuse_debug>0) fprintf(vdflog,
            "Searched %s: %u -> %u entries\n",
                abspath, had, vd_num_entries);
    }
    if (create_fragtree())
        return(fprintf(stderr, "Problem creating fragment tree\n"));
    if (create_sequences())
        return(fprintf(stderr, "Problem creating sequences\n"));
    if (vdifuse_debug>0) fprintf(vdflog,
        "Constructed sequences, have %u entries\n", vd_num_entries);
    return(0);
}

static int create_params_entry(void)
{
    int ee = VDIFUSE_TOPDIR_PARAMS;
    vd_cache[ee].index = ee;
    vd_cache[ee].etype = VDIFUSE_ENTRY_PARAMS;
    vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
    /* command line supplied items */
    vd_cache[ee].u.vpars.vdifuse_vers = VDIFUSE_VERSION;
    vd_cache[ee].u.vpars.prefix_bytes = clvpo.prefix_bytes;
    vd_cache[ee].u.vpars.offset_bytes = clvpo.offset_bytes;
    vd_cache[ee].u.vpars.searchwindow = clvpo.searchwindow;
    vd_cache[ee].u.vpars.max_pkts_gap = clvpo.max_pkts_gap;
    vd_cache[ee].u.vpars.max_secs_gap = clvpo.max_secs_gap;
    vd_cache[ee].u.vpars.how_rigorous = clvpo.how_rigorous;
    vd_cache[ee].u.vpars.noduplicates = clvpo.noduplicates;
    vd_cache[ee].u.vpars.seqhierarchy = clvpo.seqhierarchy;
    vd_cache[ee].u.vpars.writeblocker = clvpo.writeblocker;
    vd_cache[ee].u.vpars.pkts_per_sec = clvpo.pkts_per_sec;
    vd_cache[ee].u.vpars.dropfraction = clvpo.dropfraction;

    /* topdir names for fragments and sequences */
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

    /* discovered items */
    vd_cache[ee].u.vpars.est_pkt_rate = 1;
    vd_cache[ee].u.vpars.maxfrcounter = 0;
    if (gettimeofday(&vd_cache[ee].u.vpars.creation, 0))
        return(perror("gettimeofday"),1);

    /* not to match anything else, ever */
    strcpy(vd_cache[ee].path, "--cache-parameters-entry-path--");
    strcpy(vd_cache[ee].fuse, "--cache-parameters-entry-fuse--");
    strcpy(vd_cache[ee].hier, "--cache-parameters-entry-hier--");

    vd_cache_modified++;
    return(0);
}

/*
 * Should modify for m(first) c(final) a(delta) to agree with frag usage.
 */
void set_creation_time(struct stat *stbuf)
{
    stbuf->st_mtime =
    stbuf->st_ctime =
    stbuf->st_atime = vd_cache[0].u.vpars.creation.tv_sec;
    stbuf->st_mtim.tv_nsec =
    stbuf->st_ctim.tv_nsec =
    stbuf->st_atim.tv_nsec = vd_cache[0].u.vpars.creation.tv_usec * 1000;
}

/*
 * For the following three methods, mp == NULL in initial cache
 * creation.  If it is not null, we actually have a mount point
 * and we want to inherit almost all of the stbuf info from it.
 */
static int create_root_entry(struct stat *mp)
{
    int ee = VDIFUSE_TOPDIR_ROOT_DIR;
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 4;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].fuse, "/");
        vd_cache_modified++;
    }
    return(0);
}
static int create_fragments_entry(struct stat *mp)
{
    int ee = VDIFUSE_TOPDIR_FRAGMENT;
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 2;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].fuse, fragments_topdir);
        vd_cache_modified++;
    }
    return(0);
}
static int create_sequences_entry(struct stat *mp)
{
    int ee = VDIFUSE_TOPDIR_SEQUENCE;
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 2;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].hier, sequences_topdir);
        strcpy(vd_cache[ee].fuse, sequences_topdir);
        vd_cache_modified++;
    }
    return(0);
}

/*
 * Sequences and Directories inherit ownership from the mount point
 */
static int update_other_entries(struct stat *mp)
{
    int ee;
    for (ee = VDIFUSE_TOPDIR_SEQUENCE+1; ee < vd_num_entries; ee++) {
        if (vd_cache[ee].etype != VDIFUSE_ENTRY_SEQUENCE &&
            vd_cache[ee].etype != VDIFUSE_ENTRY_DIRECTORY) continue;
        if (vdifuse_debug>2) fprintf(vdflog,
            "%s: uid %d -> %d gid %d -> %d &c.\n", vd_cache[ee].fuse,
            vd_cache[ee].u.vfuse.st_uid, mp->st_uid,
            vd_cache[ee].u.vfuse.st_gid, mp->st_gid);
        vd_cache[ee].u.vfuse.st_uid = mp->st_uid;
        vd_cache[ee].u.vfuse.st_gid = mp->st_gid;
    }
    return(0);
}

/*
 * Allocate memory for the working cache.
 * This version just callocs a starter and (later) writes it out.
 * TODO: A later version could be more sophisticated with mmap.
 */
static int create_cache(char *cache)
{
    vd_num_alloced = VDIFUSE_CACHE_CHUNK;
    vd_cache = (VDIFUSEntry*)calloc(vd_num_alloced, sizeof(VDIFUSEntry));
    if (!vd_cache) return(perror("calloc"),1);
    /* no reason not to put params entry first */
    if (create_params_entry()) return(2);
    if (create_root_entry(NULL)) return(3);
    if (create_fragments_entry(NULL)) return(4);
    if (create_sequences_entry(NULL)) return(5);
    vd_num_entries = VDIFUSE_TOPDIR_SEQUENCE + 1;
    vd_cache_file = cache;
    return(0);
}
static int load_cache(char *cache)
{
    int ee = VDIFUSE_TOPDIR_PARAMS;
    struct stat sb;
    FILE *fp;
    size_t ne;

    if (vd_num_entries && vd_cache && vd_cache_file) return(0);

    if (stat(cache, &sb)) return(perror("stat"),1);
    vd_num_entries = sb.st_size / sizeof(VDIFUSEntry);
    vd_num_alloced = vd_num_entries;
    vd_cache = (VDIFUSEntry*)calloc(vd_num_entries, sizeof(VDIFUSEntry));
    if (!vd_cache) return(perror("calloc"),1);
    fp = fopen(cache, "r");
    if (!fp) return(fprintf(stderr,"%s: ",cache),perror("load_cache fopen"),2);
    ne = fread(vd_cache, sizeof(VDIFUSEntry), vd_num_entries, fp);
    if (ne != vd_num_entries)
        return(fprintf(stderr, "Failed to load complete cache\n"));
    vd_cache_file = cache;

    strcpy(fragments_topdir, vd_cache[ee].u.vpars.frag_top_dir);
    strcpy(sequences_topdir, vd_cache[ee].u.vpars.seqs_top_dir);
    fragments_topdir_len = strlen(fragments_topdir);
    sequences_topdir_len = strlen(sequences_topdir);

    return(fclose(fp));
}
static int dump_cache(void)
{
    FILE *fp;
    size_t ne;
    if (!vd_cache || vd_num_entries == 0 || !vd_cache_file)
        return(0);   /* no work */
    fp = fopen(vd_cache_file, "w");
    if (!fp) return(fprintf(stderr,"%s: ",vd_cache_file),perror("dump_cache fopen"),1);
    ne = fwrite(vd_cache, sizeof(VDIFUSEntry), vd_num_entries, fp);
    if (ne != vd_num_entries)
        return(fprintf(stderr, "Failed to write complete cache\n"));
    if (vdifuse_debug>0) fprintf(vdflog,
        "Wrote cache to %s (%d mods)\n", vd_cache_file, vd_cache_modified);
    return(fclose(fp));
}

/*
 * Create the VDIF meta-data cache.  Return 0 if the cache
 * created is ready for use, otherwise return nonzero.
 */
int vdifuse_create_metadata(char *cache, int ndirs, char **dirs)
{
    int err;
    if (vdifuse_debug>0) fprintf(vdflog,
        "Creating Meta Data cache for fuse in %s\n", cache);
    if ((err = create_cache(cache))) return(fprintf(stderr,
        "Unable to create cache %s (%d)\n", cache, err));
    if ((err = populate_cache(ndirs, dirs))) return(fprintf(stderr,
        "Unable to build cache with the %d directories provided (%d)\n",
            ndirs, err));
    /* at this point the cache is created, and in memory */
    return(0);
}

/*
 *  stat buf usage for fragments:
 *  (normal usage except for + items)
 *+ dev_t     st_dev;     // prefix_bytes (to first packet)
 *+ ino_t     st_ino;     // pkts_per_sec
 *  mode_t    st_mode;    // protection
 *  nlink_t   st_nlink;   // number of hard links
 *  uid_t     st_uid;     // user ID of owner
 *  gid_t     st_gid;     // group ID of owner
 *+ dev_t     st_rdev;    // offset_bytes (VDIF hdr in packet)
 *  off_t     st_size;    // total size, in bytes
 *+ blksize_t st_blksize; // packet size
 *+ blkcnt_t  st_blocks;  // number of packets
 *+ time_t    st_mtime;   // data start time
 *+ time_t    st_ctime;   // data end time (inc. last packet)
 *+ time_t    st_atime;   // total scan duration
 */
int describe_fragment(VDIFUSEntry *vp)
{   
    if (vdifuse_debug>1) fprintf(vdflog,
        "[%04d]Frg %s\n", vp->index, vp->path);
    if (vdifuse_debug>2) fprintf(vdflog,
        "  (index=%u vsig=%016llX)\n"
        "  (size=%llu pktsize=%d pkts=%d prefix=%u offset=%u rate=%u)\n"
        "  (from=%u.%09llu to=%u.%09llu dur=%u.%09llu)\n"
        "  (hier %s)\n  (frag %s)\n",
        vp->index, vp->vsig,
        vp->u.vfuse.st_size, vp->u.vfuse.st_blksize, vp->u.vfuse.st_blocks,
        vp->u.vfuse.st_dev, vp->u.vfuse.st_rdev, vp->u.vfuse.st_ino,
        vp->u.vfuse.st_mtime, vp->u.vfuse.st_mtim.tv_nsec,
        vp->u.vfuse.st_ctime, vp->u.vfuse.st_ctim.tv_nsec,
        vp->u.vfuse.st_atime, vp->u.vfuse.st_atim.tv_nsec,
        vp->hier, vp->fuse
    );
    return(0);
}

int describe_sequence(VDIFUSEntry *vp)
{
    if (vdifuse_debug>1) fprintf(vdflog,
        "[%04d]Seq %s anc [%d] %u links %lluB\n",
        vp->index, vp->fuse, vp->cindex,
        vp->u.vfuse.st_nlink, vp->u.vfuse.st_size);
    return(0);
}
int describe_directory(VDIFUSEntry *vp)
{
    if (vdifuse_debug>1) fprintf(vdflog,
        "[%04d]Dir %s anc [%d]\n", vp->index, vp->fuse, vp->cindex);
    return(0);
}

int describe_struct(void)
{
    fprintf(vdflog,
        "The cache consists of entries of size %d including\n"
        "a union of size %d ( vfuse=%d vfdir=%d vpars=%d ),\n"
        "paths of no more than %d bytes and sgv2 data (size=%d)\n",
        sizeof(VDIFUSEntry), sizeof(union vdifuse_union),
        sizeof(struct stat), sizeof(struct dirent), sizeof(VDIFUSEpars),
        VDIFUSE_MAX_PATH, sg_info_size());
}

int describe_params(VDIFUSEntry *vp)
{
    time_t birth = vp->u.vpars.creation.tv_sec;
    if (vdifuse_debug>2) describe_struct();
    if (vdifuse_debug>1) fprintf(vdflog, 
        "[%04d]Cache (vers.%g) %lu.%06lu (%24.24s)\n",
        vp->index,
        vp->u.vpars.vdifuse_vers,
        vp->u.vpars.creation.tv_sec, vp->u.vpars.creation.tv_usec,
        ctime(&birth));
    if (vdifuse_debug>2) fprintf(vdflog, 
        "  prefix_bytes=%u offset_bytes=%u searchwindow=%u\n"
        "  max_pkts_gap=%u max_secs_gap=%f "
            "how_rigorous=" VDIFUSE_RIGOR_PRINTF "\n"
        "  noduplicates=%u seqhierarchy=%u writeblocker=%u\n"
        "  pkts_per_sec=%u dropfraction=%f maxfrcounter=%u\n"
        "  est_pkt_rate=%u frag_top_dir=%s seqs_top_dir=%s\n" ,
        vp->u.vpars.prefix_bytes,
        vp->u.vpars.offset_bytes,
        vp->u.vpars.searchwindow,
        vp->u.vpars.max_pkts_gap,
        vp->u.vpars.max_secs_gap,
        vp->u.vpars.how_rigorous,
        vp->u.vpars.noduplicates,
        vp->u.vpars.seqhierarchy,
        vp->u.vpars.writeblocker,
        vp->u.vpars.pkts_per_sec,
        vp->u.vpars.dropfraction,
        vp->u.vpars.maxfrcounter,
        vp->u.vpars.est_pkt_rate,
        vp->u.vpars.frag_top_dir,
        vp->u.vpars.seqs_top_dir
    );
    return(0);
}

char *describe_stype(int stype)
{
    char *anctype;
    switch (stype) {
    case VDIFUSE_STYPE_NULL: anctype = "Empty fragment"; break;
    case VDIFUSE_STYPE_VDIF: anctype = "VDIF fragment"; break;
    case VDIFUSE_STYPE_SGV2: anctype = "SGv2 fragment"; break;
    case VDIFUSE_STYPE_INFO: anctype = "SGv2 access"; break;
    case VDIFUSE_STYPE_SDIR: anctype = "Dir entries"; break;
    case VDIFUSE_STYPE_PART: anctype = "Part of seq"; break;
    default:
        fprintf(stderr, "Illegal stype %d\n", stype);
        anctype = "ERROR";
        break;
    }
    return(anctype);
}

int describe_ancillary(VDIFUSEntry *vp)
{
    char *anctype, *prefix, *suffix;
    int ee;
    anctype = describe_stype(vp->stype);
    if (vdifuse_debug>1) fprintf(vdflog,
        "[%04d]Anc %s (%s) with %d\n",
            vp->index, vp->fuse, anctype, vp->ccount);
    if (vdifuse_debug>2) {
        if (vp->stype == VDIFUSE_STYPE_INFO)
            return(describe_ancillary_sgv2(vp));
        for (ee = 0; ee < vp->ccount; ee++) {
            prefix = (ee%8 == 0) ? "  (" : "";
            suffix = (ee%8 == 7) ? ")\n" :
                (ee == vp->ccount-1) ? ")\n" : " ";
            fprintf(vdflog, "%s[%04d]%s", prefix, vp->u.vseqi[ee], suffix);
        }
    }
    return(0);
}

/*
 * A debugging version of the guts of the following.
 */
void describe_cache_entry(VDIFUSEntry *vp)
{
    switch (vp->etype) {
    case VDIFUSE_ENTRY_FRAGMENT:
        (void)describe_fragment(vp);
        break;
    case VDIFUSE_ENTRY_SEQUENCE:
        (void)describe_sequence(vp);
        break;
    case VDIFUSE_ENTRY_DIRECTORY:
        (void)describe_directory(vp);
        break;
    case VDIFUSE_ENTRY_PARAMS:
        (void)describe_params(vp);
        break;
    case VDIFUSE_ENTRY_ANCILLARY:
        (void)describe_ancillary(vp);
        break;
    default:
    case VDIFUSE_ENTRY_INVALID:
        fprintf(vdflog, "Entry (%p) marked invalid\n", vp);
        break;
    }
}

/*
 * Walk through the list, check and comment as required.
 */
static int report_on_cache(void)
{
    long ee;
    long frags = 0, seqs = 0, dirs = 0, params = 0, anc = 0, invalids = 0;
    VDIFUSEntry *vp;
    for (ee = 0, vp = vd_cache; ee < vd_num_entries; ee++, vp++)
        switch (vp->etype) {
        case VDIFUSE_ENTRY_FRAGMENT:
            frags++;
            if (describe_fragment(vp)) return(1);
            break;
        case VDIFUSE_ENTRY_SEQUENCE:
            seqs++;
            if (describe_sequence(vp)) return(2);
            break;
        case VDIFUSE_ENTRY_DIRECTORY:
            dirs++;
            if (describe_directory(vp)) return(3);
            break;
        case VDIFUSE_ENTRY_PARAMS:
            params++;
            if (describe_params(vp)) return(4);
            break;
        case VDIFUSE_ENTRY_ANCILLARY:
            anc++;
            if (describe_ancillary(vp)) return(5);
            break;
        default:
        case VDIFUSE_ENTRY_INVALID:
            invalids++;
            fprintf(vdflog, "Entry %d (%s) marked invalid\n", ee, vp->path);
            break;
        }

    if (vdifuse_debug>0) fprintf(vdflog,
        "Have %lu frags %lu seqs %lu dirs %lu params %lu anc %lu invalids\n",
        frags, seqs, dirs, params, anc, invalids);
    return(0);
}

static int check_topdirs(void)
{
    char *x;
    x = vd_cache[VDIFUSE_TOPDIR_PARAMS].u.vpars.frag_top_dir;
    if (strcmp(x, fragments_topdir))
        return(fprintf(stderr, "%s != %s\n", x, fragments_topdir));
    x = vd_cache[VDIFUSE_TOPDIR_PARAMS].u.vpars.seqs_top_dir;
    if (strcmp(x, sequences_topdir))
        return(fprintf(stderr, "%s != %s\n", x, sequences_topdir));
    return(0);
}

/*
 * At the moment, nothing has been released, so we have no obligation
 * for supporting old versions.  So we check and die we have to.
 * TODO: update memory to current version and perhaps continue?
 */
static int check_cache_version(void)
{
    if (check_topdirs()) return(2);
    if (vd_cache[0].u.vpars.vdifuse_vers == (float)VDIFUSE_VERSION) return(0);
    fprintf(stderr, "File version %f != code version %f (%e)\n",
        vd_cache[0].u.vpars.vdifuse_vers, (float)VDIFUSE_VERSION,
        vd_cache[0].u.vpars.vdifuse_vers - (float)VDIFUSE_VERSION);
    return(1);
}
/*
 * Check the cache file which is (at this point) known to exist
 * and contain at least one entry.  Return 0 if the cache
 * passes some sanity checks.  Return nonzero otherwise.
 *
 * If vdifuse_debug is nonzero, provide a verbose report.
 */
int vdifuse_report_metadata(char *cache, struct stat *sb)
{
    int err;
    if (vdifuse_debug>0) fprintf(vdflog,
        "Reporting Meta Data cache '%s' for use with fuse.\n", cache);
    if (vdifuse_debug>0) fprintf(vdflog,
        "This cache contains %d entries of size %d\n",
            sb->st_size / sizeof(VDIFUSEntry), sizeof(VDIFUSEntry));
    if ((err = load_cache(cache))) return(fprintf(stderr,
        "Unable to open cache '%s' (%d)\n", cache, err));
    if (check_cache_version()) return(fprintf(stderr,
        "Incompatible cache '%s' -- cannot continue.\n", cache));
    /* at this point the cache is in memory */
    if (vd_num_entries && vd_cache) return(report_on_cache());
    return (fprintf(stderr, "Cache wasn't loaded properly\n"));
}

/*
 * Update the cache with data from the mount point
 */
static int update_mount_point(struct stat *mp)
{
    set_creation_time(mp);
    if (create_root_entry(mp) ||
        create_fragments_entry(mp) ||
        create_sequences_entry(mp) ||
        update_other_entries(mp))
            return(fprintf(stderr, "Issues with mount point\n"));
    return(0);
}

/*
 * If not creating or reporting, we're using
 */
int vdifuse_access_metadata(char *cache, struct stat *sb, struct stat *mp)
{
    int err;
    /* check if already loaded */
    if (vd_num_entries && vd_cache) return(update_mount_point(mp));
    if (vdifuse_debug>0) fprintf(vdflog,
        "Accessing Meta Data cache for fuse in %s\n", cache);
    if (vdifuse_debug>0) fprintf(vdflog,
        "Cache contains %d entries of size %d\n",
            sb->st_size / sizeof(VDIFUSEntry), sizeof(VDIFUSEntry));
    if ((err = load_cache(cache))) return(fprintf(stderr,
        "Unable to open cache %s (%d)\n", cache, err));
    if (check_cache_version()) return(fprintf(stderr,
        "Incompatible cache %s -- cannot continue.\n", cache));
    /* at this point the cache is in memory */
    if (vd_num_entries && vd_cache) return(update_mount_point(mp));
    return (fprintf(stderr, "Cache wasn't loaded properly\n"));
}

/*
 * Top-level directories are somewhat special.
 */
void vdifuse_topdir(int which, struct stat *stbuf)
{
    switch (which) {
    case VDIFUSE_TOPDIR_ROOT_DIR:
        *stbuf = vd_cache[1].u.vfuse;
        break;
    case VDIFUSE_TOPDIR_FRAGMENT:
        *stbuf = vd_cache[2].u.vfuse;
        break;
    case VDIFUSE_TOPDIR_SEQUENCE:
        *stbuf = vd_cache[3].u.vfuse;
        break;
    default:
        fprintf(stderr, "Developer Error\n");
        break;
    }
}

/*
 * Support for attributes; these return 0 if the path is found.
 * TODO: this lookup isn't terribly efficient
 */
int vdifuse_fragment(const char *path, struct stat *stbuf)
{
    int ii;
    for (ii = 0; ii < vd_num_entries; ii++) {
        /* this directory tree contains only fragments */
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_FRAGMENT) continue;
        if (!strcmp(path, vd_cache[ii].fuse)) {
            if (vdifuse_debug>1) fprintf(vdflog, "Found %s\n", path);
            memcpy(stbuf, &vd_cache[ii].u.vfuse, sizeof(struct stat));
            return(0);
        }
    }
    if (vdifuse_debug>1) fprintf(vdflog, "Did not find %s\n", path);
    return(1);
}
int vdifuse_sequence(const char *path, struct stat *stbuf)
{
    int ii;
    for (ii = 0; ii < vd_num_entries; ii++) {
        /* this directory tree contains subdirs and sequences */
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_DIRECTORY &&
            vd_cache[ii].etype != VDIFUSE_ENTRY_SEQUENCE) continue;
        if (!strcmp(path, vd_cache[ii].fuse)) {
            if (vdifuse_debug>1) fprintf(vdflog, "Found %s\n", path);
            memcpy(stbuf, &vd_cache[ii].u.vfuse, sizeof(struct stat));
            return(0);
        }
    }
    if (vdifuse_debug>1) fprintf(vdflog, "Did not find %s\n", path);
    return(1);
}

/*
 * Support for readdir.
 * Starting with index, provide the fuse name and stbuf data.
 * If no more, return 0.
 */
int get_vdifuse_fragment(int ii, char **name, struct stat **stbuf)
{
    for (ii++ ; ii < vd_num_entries; ii++) {
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_FRAGMENT) continue;
        *name = strrchr(vd_cache[ii].fuse, '/');
        if (!*name) *name = "/.damaged-fragment";
        (*name) ++ ;
        *stbuf = &vd_cache[ii].u.vfuse;
        return(ii);
    }
    return(0);  /* no more */
}
/*
 * Support for readdir.
 * Provide the top-level sequences directory listing.
 * There should be an ANCILLARY file with the info for the entries.
 * if seqhierarchy is 0, we're actually providing a list of sequences.
 * if seqhierarchy > 0, we're providing exp subdirs
 * Returns of 0 indicate we are done, otherwise return the
 * nonzero index of the subdirectory or sequence entry.
 */
int get_vdifuse_sequence(int ii, char **name, struct stat **stbuf)
{
    static uint32_t sndx, sdir;
    static VDIFUSEntry *vseq;
    VDIFUSEntry *vc;

    if (ii == 0) {                  /* first time through */
        vseq = &vd_cache[vd_cache[VDIFUSE_TOPDIR_SEQUENCE].cindex];
        sdir = vseq->u.vseqi[sndx = 0];
    } else {                        /* pick up where we left off */
        if (++sndx == VDIFUSE_MAX_SEQI) {
            vseq = &vd_cache[vseq->cindex];
            sndx = 0;
        }
        sdir = vseq->u.vseqi[sndx];
    }

    if (sdir == 0) {
        ii = 0;                     /* no more entries */
    } else {
        vc = &vd_cache[sdir];
        *name = strrchr(vc->fuse, '/');
        if (!*name) *name = "/.damaged-sequence";
        (*name) ++;
        *stbuf = &vc->u.vfuse;
        ii = vseq - vd_cache;       /* index of current entry */
    }
    return(ii);
}

/*
 * Return the index of the sequence subdirectory matching path, if found.
 * If found, subsequence calls to get_vdifuse_subdir() fill in the entries.
 * This pair is similar to get_vdifuse_sequence(), except that the first
 * is needed to locate the parent subdirectory so that the second can
 * unload starting there.
 */
int get_sequence_subdir(const char *path)
{
    int ii;
    for (ii = 0 ; ii < vd_num_entries; ii++) {
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_SEQUENCE &&
            vd_cache[ii].etype != VDIFUSE_ENTRY_DIRECTORY) continue;
        if (strcmp(path, vd_cache[ii].fuse)) continue;
        if (vdifuse_debug>1) fprintf(vdflog, "Found %s at %d\n", path, ii);
        return(ii);
    }
    return(0);
}
int get_vdifuse_subdir(int ii, char **name, struct stat **stbuf)
{
    static uint32_t sndx, sdir;
    static VDIFUSEntry *vseq;
    VDIFUSEntry *vc;

    if (vd_cache[ii].etype == VDIFUSE_ENTRY_SEQUENCE ||
        vd_cache[ii].etype == VDIFUSE_ENTRY_DIRECTORY) {    /* first time */
        if (vd_cache[ii].cindex == 0) return(0);
        vseq = &vd_cache[vd_cache[ii].cindex];
        sdir = vseq->u.vseqi[sndx = 0];
    } else {
        if (++sndx == VDIFUSE_MAX_SEQI) {   /* pick up where we left off */
            vseq = &vd_cache[vseq->cindex];
            sndx = 0;
        }
        sdir = vseq->u.vseqi[sndx];
    }

    if (sdir == 0) {
        ii = 0;                     /* no more entries */
    } else {
        vc = &vd_cache[sdir];
        *name = strrchr(vc->fuse, '/');
        if (!*name) *name = "/.damaged-sequence";
        (*name) ++;
        *stbuf = &vc->u.vfuse;
        ii = vseq - vd_cache;       /* index of current entry */
    }
    return(ii);
}

/* support for open, release, and read moved to vdirorr.c */

/*
 * Show the warts
 */
static int vdifuse_issues(void)
{
    fprintf(vdflog, "Single-threaded VDIF is expected.\n");
    fprintf(vdflog, "Multi-threaded VDIF is considered an error, today.\n");
    fprintf(vdflog, "A single data rate is expected, i.e. -xrate=pkts/sec.\n");
    fprintf(vdflog, "The packet rate (packets/sec) MUST be supplied\n");
    fprintf(vdflog, "This implementation is not yet optimally efficient.\n");
    fprintf(vdflog, "No allowance for missing packets is (yet) made,\n");
    fprintf(vdflog, "  so seriously corrupt data is likely to be tossed.\n");
    fprintf(vdflog, "\n");
    return(1);
}

/*
 * Yet more help
 */
static int vdifuse_examples(void)
{
    fprintf(vdflog, "With single-threaded Mark6 scatter-gather files,\n");
    fprintf(vdflog, "\n");
    fprintf(vdflog, "  vdifuse -a <label>.cache -xm6sg -xrate=125000 \\ \n");
    fprintf(vdflog, "    ./mnt-<label> /mnt/disks/[<group>]/?/data\n");
    fprintf(vdflog, "\n");
    fprintf(vdflog, "would examine the data directors of the recorded\n");
    fprintf(vdflog, "group on a set of modules <label> (e.g. one session\n");
    fprintf(vdflog, "from some experiment) and mount it locally.\n");
    fprintf(vdflog, "\n");
    fprintf(vdflog, "At the moment the packet rate is required.\n");
    fprintf(vdflog, "\n");
    fprintf(vdflog, "For a subset of scans, -xinclpatt=<RE> can be used\n");
    fprintf(vdflog, "to restrict attention to the scans of interest.\n");
    fprintf(vdflog, "\n");
}

/*
 * Additional support options, supplied in key=value form.
 * Nonzero return indicates an error.
 */
static int vdifuse_options_help(void)
{
    fprintf(vdflog, "\n");
    fprintf(vdflog, "General help:\n");
    fprintf(vdflog, "  help          provides this help\n");
    fprintf(vdflog, "  issues        provides some additional info\n");
    fprintf(vdflog, "  examples      provides some examples\n");

    fprintf(vdflog, "\n");
    fprintf(vdflog, "Options that may be set to affect cache creation:\n");
    fprintf(vdflog, "(On options marked with *, using \"help\" instead\n");
    fprintf(vdflog, "of a value will provide further information.  And\n");
    fprintf(vdflog, "options marked ! are envisioned, but not yet useful.)\n");
    fprintf(vdflog, "  debug=<int>   set internal debugging level (%d)\n",
        vdifuse_debug);
    fprintf(vdflog, "  delete        deletes any existing cache %s\n",
        vdifuse_protect ? "(protect)" : "(delete)");
    fprintf(vdflog, "  protect       protects existing cache %s\n",
        vdifuse_protect ? "(protect)" : "(delete)");

    fprintf(vdflog, "  prefix=<int>  # of bytes (%u) before 1st packet\n",
        clvpo.prefix_bytes);
    fprintf(vdflog, "  offset=<int>  # of bytes (%u) before header\n",
        clvpo.offset_bytes);
    fprintf(vdflog, "! window=<int>  # of bytes (%u) slop on these\n",
        clvpo.searchwindow);
    fprintf(vdflog, "! gap=<int>     max # of packets between (%u) files\n",
        clvpo.max_pkts_gap);
    fprintf(vdflog, "! gaps=<float>  max # of seconds between (%g) files\n",
        clvpo.max_secs_gap);
    fprintf(vdflog, "* rigor=<int>   methods to examine files (%u)\n",
        clvpo.how_rigorous);
    fprintf(vdflog, "  uniq=<int>    asserts that filenames are unique (%u)\n",
        clvpo.noduplicates);
    fprintf(vdflog, "  hier=<int>    directory hierarchy limit (%u)\n",
        clvpo.seqhierarchy);
    fprintf(vdflog, "  block=<int>   write blocking factor (%u)\n",
        clvpo.writeblocker);
    fprintf(vdflog, "  rate=<int>    of packets/sec (%u) in files\n",
        clvpo.pkts_per_sec);
    fprintf(vdflog, "! drop=<float>  fraction of dropped packets (%g)\n",
        clvpo.dropfraction);
    fprintf(vdflog, "! frag=<string> top-dir name for fragments (%s)\n",
        clvpo.frag_top_dir);
    fprintf(vdflog, "! seqs=<string> top-dir name for sequences (%s)\n",
        clvpo.seqs_top_dir);
    regexhelp(vdflog);

    fprintf(vdflog, "\n");
    fprintf(vdflog, "Some standard setups:\n");
    fprintf(vdflog, "  files         setup for general files\n");
    fprintf(vdflog, "  m6sg          setup for Mark6 scatter-gather dirs\n");
    fprintf(vdflog, "  m6raid        setup for Mark6 raid dirs\n");
    fprintf(vdflog, "(i.e. -xm6sg -xrate=125000)\n");
    fprintf(vdflog, "\n");

    return(1);
}

/*
 * More detailed help
 */
static void rigor_help(void)
{
    fprintf(vdflog, "\n");
    fprintf(vdflog, "This is a mask of ways to check files:\n");
    fprintf(vdflog,
        "  " VDIFUSE_RIGOR_HPRINTF "  will pass anthing.\n",
        VDIFUSE_RIGOR_NOCHECK);
    fprintf(vdflog,
        "  " VDIFUSE_RIGOR_HPRINTF "  tests for *.vdif\n",
        VDIFUSE_RIGOR_SUFFIX);
    fprintf(vdflog,
        "  " VDIFUSE_RIGOR_HPRINTF "  tests for <exp>_<sc>_<scan><any>.vdif\n",
        VDIFUSE_RIGOR_ENAME);
    fprintf(vdflog,
        "  " VDIFUSE_RIGOR_HPRINTF "  tests for scatter-gather vers. 2\n",
        VDIFUSE_RIGOR_MAGIC);
    fprintf(vdflog,
        "  " VDIFUSE_RIGOR_HPRINTF "  tests for min file size (%d)\n",
        VDIFUSE_RIGOR_MINSIZE, VDIFUSE_MIN_FILE_SIZE);
    fprintf(vdflog, "\n");
}

/*
 * Handler for command-line options.
 * clvpo is probably initialized to 0, but making sure is ok.
 */
int vdifuse_options(char *arg)
{
    if (clvpo.creation.tv_sec) memset(&clvpo, 0, sizeof(clvpo));

    if (!strncmp(arg, "help", 4)) {
        return(vdifuse_options_help());
    } else if (!strncmp(arg, "issues", 6)) {
        return(vdifuse_issues());
    } else if (!strncmp(arg, "examples", 8)) {
        return(vdifuse_examples());

    } else if (!strncmp(arg, "debug=", 6)) {
        vdifuse_debug = atoi(arg + 6);
        if (vdifuse_debug>0) fprintf(vdflog,
            "Debugging level set to %u\n", vdifuse_debug);
    } else if (!strncmp(arg, "delete", 6)) {
        vdifuse_protect = 0;
        if (vdifuse_debug>0) fprintf(vdflog,
            "Deleting any pre-existing cache\n");
    } else if (!strncmp(arg, "protect", 7)) {
        vdifuse_protect = 1;
        if (vdifuse_debug>0) fprintf(vdflog,
            "Protecting any pre-existing cache\n");

    } else if (!strncmp(arg, "prefix=", 7)) {
        clvpo.prefix_bytes = atoi(arg + 7);
        if (vdifuse_debug>0) fprintf(vdflog,
            "Assuming %u bytes prior to first packet\n", clvpo.prefix_bytes);
    } else if (!strncmp(arg, "offset=", 7)) {
        clvpo.offset_bytes = atoi(arg + 7);
        if (vdifuse_debug>0) fprintf(vdflog,
            "Assuming %u bytes prior each header\n", clvpo.offset_bytes);
    } else if (!strncmp(arg, "window=", 7)) {
        clvpo.searchwindow = atoi(arg + 7);
        if (vdifuse_debug>0) fprintf(vdflog,
            "Allowing %u bytes of slop in searches\n", clvpo.searchwindow);
    } else if (!strncmp(arg, "gaps=", 5)) {
        clvpo.max_secs_gap = atof(arg + 5);
        if (vdifuse_debug>0) fprintf(vdflog,
            "At most %g seconds between fragments\n", clvpo.max_secs_gap);
    } else if (!strncmp(arg, "gap=", 4)) {
        clvpo.max_pkts_gap = atoi(arg + 4);
        if (vdifuse_debug>0) fprintf(vdflog,
            "At most %u packets between fragments\n", clvpo.max_pkts_gap);
    } else if (!strncmp(arg, "rigor=help", 10)) {
        rigor_help();
    } else if (!strncmp(arg, "rigor=", 6)) {
        clvpo.how_rigorous = atoi(arg + 6);
        if (vdifuse_debug>0) fprintf(vdflog,
            "VDIF testing at rigor level %u\n", clvpo.how_rigorous);
    } else if (!strncmp(arg, "uniq=", 5)) {
        clvpo.noduplicates = atoi(arg + 5);
        if (vdifuse_debug>0) fprintf(vdflog, clvpo.noduplicates ?
            "Filenames assumed unique\n" : "Expect duplicate filenames\n");
    } else if (!strncmp(arg, "hier=", 5)) {
        clvpo.seqhierarchy = atoi(arg + 5);
        if (clvpo.seqhierarchy > VDIFUSE_MAX_HIER)
            clvpo.seqhierarchy = VDIFUSE_MAX_HIER;
        if (vdifuse_debug>0) fprintf(vdflog,
            "Directory hierachy limit now %u\n", clvpo.seqhierarchy);
    } else if (!strncmp(arg, "block=", 6)) {
        clvpo.writeblocker = atoi(arg + 6);
        if (vdifuse_debug>0) fprintf(vdflog,
            "Packets assumed blocked within %uB buffer\n", clvpo.writeblocker);
    } else if (!strncmp(arg, "rate=", 5)) {
        clvpo.pkts_per_sec = atoi(arg + 5);
        if (vdifuse_debug>0) fprintf(vdflog,
            "Assuming %u packets per second\n", clvpo.pkts_per_sec);
    } else if (!strncmp(arg, "drop=", 5)) {
        clvpo.dropfraction = atof(arg + 5);
        if (vdifuse_debug>0) fprintf(vdflog,
            "Assuming packets-dropped/total is %g\n", clvpo.dropfraction);

    } else if (!strncmp(arg, "frag=", 5)) {
        if (arg[5] == '/' && strlen(arg+5) < VDIFUSE_TOPDIR_SIZE) {
            strcpy(clvpo.frag_top_dir, arg+5);
            if (vdifuse_debug>0) fprintf(vdflog,
                "Using %s for top-dir fragments directory name\n",
                clvpo.frag_top_dir);
        } else {
            fprintf(stderr, "%s is not suitable for fragments\n", arg+5);
        }
    } else if (!strncmp(arg, "seqs=", 5)) {
        if (arg[5] == '/' && strlen(arg+5) < VDIFUSE_TOPDIR_SIZE) {
            strcpy(clvpo.seqs_top_dir, arg+5);
            if (vdifuse_debug>0) fprintf(vdflog,
                "Using %s for top-dir sequences directory name\n",
                clvpo.seqs_top_dir);
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
            if (vdifuse_debug>0) fprintf(vdflog,
                "Excluding any files matching %s\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s was not accepted for exclusion\n", arg+9);
        }
    } else if (!strncmp(arg, "inclpatt=", 9)) {
        if (!reginclude(arg+9)) {
            if (vdifuse_debug>0) fprintf(vdflog,
                "Including only files matching %s\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s was not accepted for inclusion\n", arg+9);
        }
    } else if (!strncmp(arg, "exclfile=", 9)) {
        if (!regexcludefile(arg+9)) {
            if (vdifuse_debug>0) fprintf(vdflog,
                "Excluding any files matching patterns in %s\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s had no useful exclusion patterns\n", arg+9);
        }
    } else if (!strncmp(arg, "inclfile=", 9)) {
        if (!regincludefile(arg+9)) {
            if (vdifuse_debug>0) fprintf(vdflog,
                "Including only files matching patterns in %s\n", arg+9);
            clvpo.how_rigorous |= VDIFUSE_RIGOR_REGEX;
        } else {
            fprintf(stderr, "%s had no useful include patterns\n", arg+9);
        }

    /*
     * TODO: Revise these once we have everything working.
     */
    } else if (!strncmp(arg, "files", 5)) {
        if (vdifuse_debug>0) fprintf(vdflog,
            "Development setup for general file usage.\n");
        clvpo.noduplicates = 1;
        clvpo.how_rigorous = VDIFUSE_RIGOR_ENAME | VDIFUSE_RIGOR_MINSIZE;
        clvpo.seqhierarchy = 3;
    } else if (!strncmp(arg, "m6sg", 4)) {
        if (vdifuse_debug>0) fprintf(vdflog,
            "Development setup for Mark6 SG vers 2.\n");
        vdifuse_protect = 0;
        clvpo.noduplicates = 0;
        clvpo.how_rigorous = VDIFUSE_RIGOR_MAGIC | VDIFUSE_RIGOR_MINSIZE;
        clvpo.seqhierarchy = 3;
        clvpo.writeblocker = 10000000;
        clvpo.pkts_per_sec = 125000;
    } else if (!strncmp(arg, "m6raid", 6)) {
        if (vdifuse_debug>0) fprintf(vdflog,
            "Development setup for Mark6 Raid.\n");
        clvpo.noduplicates = 1;
        clvpo.how_rigorous = VDIFUSE_RIGOR_ENAME | VDIFUSE_RIGOR_MINSIZE;
        clvpo.seqhierarchy = 3;
    } 

    return(0);
}

/*
 * Final cleanup actions
 */
int vdifuse_finish(void)
{
    if (vd_cache_modified) return(dump_cache());
    if (vdifuse_debug>2) fprintf(vdflog, "Cache was not modified\n");
    return(0);
}

/*
 * eof
 */
