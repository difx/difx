/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifseq.c 5716 2023-03-11 15:04:07Z gbc $
 *
 * This file does the work of building sequences.
 *
 * vdiflog should be used for this file as it is non-fuse work.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "vdifuse.h"

/*
 * tdestroy() is marked as supplied by _GNU_SOURCE,
 * but __USE_GNU is maybe needed as well.
 */
#define _GNU_SOURCE
#define __USE_GNU
#include <search.h>

static int twalk_thier_errors = 0;

/*
 * Tsearch support: tree for a sequence directory hierachy
 * Nodes contain hierarchy path.
 * Ordering is alphabetic in hierachy name.
 */
static void *thier_root = NULL;
typedef struct THIERnode {
    char path[VDIFUSE_MAX_PATH];
    char type;  /* 'X' or 'D' or 'S' */
    uint32_t hvdei;
} THIERnode;
static int thier_comp(const void *p1, const void *p2)
{
    THIERnode *e1 = (THIERnode *)p1;
    THIERnode *e2 = (THIERnode *)p2;
    vdiflog(6,
        "H-CMP: %s and %s\n", e1->path, e2->path);
    return(strcmp(e1->path, e2->path));
}
static void thier_dump(const void *nodep, const VISIT which, const int depth)
{
    char *what;
    THIERnode *hn;
    int print;
    switch (which) {
    case preorder:  what = " preorder:-"; print = 4;  break;
    case endorder:  what = " endorder:-"; print = 4;  break;
    case postorder: what = "postorder:*"; print = 2;  break;
    case leaf:      what = "     leaf:*"; print = 2;  break;
    default:        what = "    error";   print = -1; break;
    }
    hn = *(THIERnode **)nodep;
    vdiflog(print, "hier:%s%d:'%47s' %p %c\n",
        what, depth, hn->path, hn, hn->type);
}
static void thier_free(void *nodep)
{
    vdiflog(4, "Freeing (hier) %p\n", nodep);
    free(nodep);
}

/*
 * Construct the parent name and search for it with tfind.
 */
static VDIFUSEntry *find_seq_parent(char *path)
{
    static THIERnode tmp;
    char *basename;
    THIERnode **hp;
    VDIFUSEntry *vp;

    basename = strrchr(strcpy(tmp.path, path), '/');
    vdiflog(3, " (tfind)child: %s\n", tmp.path);
    if (!basename) return((VDIFUSEntry *)0);
    *basename++ = 0;

    hp = tfind(&tmp, &thier_root, &thier_comp);
    if (!hp) vp = current_cache_start() + VDIFUSE_TOPDIR_SEQUENCE;
    else vp = (current_cache_start() + (*hp)->hvdei);

    vdiflog(3, "(tfind)parent: %s\n", vp->fuse);
    return(vp);
}

/*
 * The logic of continuations is that cindex is the index of
 * a continuation entry.  That entry has a particular stype
 * indicating how it will be used:
 *  VDIFUSE_STYPE_INFO
 *      there is just one continuation which holds additional
 *      data (sgv2 access information is the only case)
 *  VDIFUSE_STYPE_SDIR
 *      the continuation contains ccount subdir entries in vseqi
 *      and it may itself be continued if more than VDIFUSE_MAX_SEQI
 *      entries are needed
 *  VDIFUSE_STYPE_PART
 *      the continuation contains ccount fragment entries in vseqi
 *      that refer to the fragments of the sequence
 */

/*
 * Add a continuation of the specified s(ub)type
 * Since create_cache_entry() might extend the cache, we pass
 * the index of the entry that needs continuation.
 */
static VDIFUSEntry *add_continuation(uint32_t vx_index, int stype)
{
    VDIFUSEntry *vc = create_cache_entry();
    VDIFUSEntry *vx = current_cache_start() + vx_index;
    vc->etype = VDIFUSE_ENTRY_ANCILLARY;
    memset(&vc->u, 0, sizeof(union vdifuse_union));
    vc->stype = stype;
    vc->cindex = 0;
    vc->ccount = 0;
    strcpy(vc->fuse, vx->fuse);
    vx->cindex = vc->index;
    return(vc);
}

/*
 * Walk the list of (type) entries to get to the last one,
 * append if it is full, and return pointer to the last one,
 * i.e. one with room for adding entries.
 */
static VDIFUSEntry *check_continuation(VDIFUSEntry *vx, int stype)
{
    if (vx->cindex == 0) {
        vx = add_continuation(vx->index, stype);
    } else if (vx->cindex < current_cache_entries()) {
        do vx = current_cache_start() + vx->cindex;
        while (vx->ccount == VDIFUSE_MAX_SEQI && vx->cindex > 0 &&
            vx->cindex < current_cache_entries());
        if (vx->ccount == VDIFUSE_MAX_SEQI)
            vx = add_continuation(vx->index, stype);
    } else {
        vdiflog(-1, "Illegal cindex value %d in...\n", vx->cindex);
        describe_cache_entry(vx);
    }
    return(vx);
}

/*
 * Add this directory to its parent.
 */
static void add_to_parent_dir(VDIFUSEntry *vc)
{
    VDIFUSEntry *vp, *vx;
    uint32_t vp_index, vc_index = vc->index;
    //uint32_t vx_index;
    if (!(vp = find_seq_parent(vc->hier))) {
        vdiflog(-1, "No parent for %s\n", vc->hier);
        twalk_thier_errors ++;
        return;
    }
    vp_index = vp->index;
    if (!(vx = check_continuation(vp, VDIFUSE_STYPE_SDIR))) {
        // vc = current_cache_start() + vc_index;
        vdiflog(-1, "No room in parent for %s\n", vc->hier);
        twalk_thier_errors ++;
        return;
    }
    //vx_index = vx->index;

    vc = current_cache_start() + vc_index;
    vp = current_cache_start() + vp_index;

    /* ok, safe to do the update */
    vdiflog(3,
        "Subdir: %s ->\n   Dir: %s\n", vc->hier, vp->hier);
    vp->u.vfuse.st_nlink ++;
    vp->u.vfuse.st_size += sizeof(VDIFUSEntry);
    vx->u.vseqi[vx->ccount++] = vc_index;
}



/*
 * Create the subdir cache entry for this sequence node
 *
 * Failures within add_to_parent_dir() will result in an orphaned cache item.
 */
static void thier_sdir(const void *nodep, const VISIT which, const int depth)
{
    THIERnode *hn = *(THIERnode **)nodep;
    VDIFUSEntry *vc;

    if (which != postorder && which != leaf) return;
    if (hn->type == 'X') {      /* top-level /segments */
        vdiflog(-1, "Have 'X' type subdir entry!\n");
        twalk_thier_errors ++;
        return;
    }

    vc = create_cache_entry();
    hn->hvdei = vc->index;

    if (hn->type == 'D') {
        vc->etype = VDIFUSE_ENTRY_DIRECTORY;
        vc->stype = VDIFUSE_STYPE_NULL;
        vc->u.vfuse.st_mode = S_IFDIR | 0555;
    } else if (hn->type == 'S') {
        vc->etype = VDIFUSE_ENTRY_SEQUENCE;
        vc->stype = VDIFUSE_STYPE_NULL;
        vc->u.vfuse.st_mode = S_IFREG | 0444;
    }
    vc->u.vfuse.st_nlink = 0;
    vc->u.vfuse.st_size = 0;
    set_creation_time(&(vc->u.vfuse));

    vc->cindex = 0;
    vc->ccount = 0;
    vc->path[0] = 0; /* not a real path */
    strcpy(vc->hier, hn->path);
    strcpy(vc->fuse, hn->path);

    add_to_parent_dir(vc);
}

/*
 * Tsearch support: tree for ordering fragments
 * Nodes contain fragment entries.
 * Ordering is by signature and first/final times.
 * Note that as the underlying cache is growing, pointers
 * are unstable, so we MUST make copies of data here.
 */
static void *tfrag_root = NULL;
typedef struct TFRAGnode {
    uint64_t vsig;
    struct timespec first;
    struct timespec final;
    char path[VDIFUSE_MAX_PATH];
    uint32_t fvdei;
} TFRAGnode;
static int tfrag_comp(const void *p1, const void *p2)
{
    TFRAGnode *e1 = (TFRAGnode *)p1;
    TFRAGnode *e2 = (TFRAGnode *)p2;
    vdiflog(6,
        "F-CMP: %s and %s\n", e1->path, e2->path);
    /* check the signatures */
    if (e1->vsig < e2->vsig) return(-1);
    if (e1->vsig > e2->vsig) return( 1);
    /* check the start times */
    if (e1->first.tv_sec < e2->first.tv_sec) return(-1);
    if (e1->first.tv_sec > e2->first.tv_sec) return( 1);
    if (e1->first.tv_nsec < e2->first.tv_nsec) return(-1);
    if (e1->first.tv_nsec > e2->first.tv_nsec) return( 1);
    /* check the end times */
    if (e1->final.tv_sec < e2->final.tv_sec) return(-1);
    if (e1->final.tv_sec > e2->final.tv_sec) return( 1);
    if (e1->final.tv_nsec < e2->final.tv_nsec) return(-1);
    if (e1->final.tv_nsec > e2->final.tv_nsec) return( 1);
    /* well shucks, duplicate data? */
    /* NOTION: invalidate one of them? */
    vdiflog(-1,
        "F-CMP: %s and %s are duplicates\n", e1->path, e2->path);
    return(0);
}
static void tfrag_dump(const void *nodep, const VISIT which, const int depth)
{
    char *what;
    TFRAGnode *vf;
    int print = 0;
    switch (which) {
    case preorder:  what = " preorder:-"; print = 4;  break;
    case endorder:  what = " endorder:-"; print = 4;  break;
    case postorder: what = "postorder:*"; print = 2;  break;
    case leaf:      what = "     leaf:*"; print = 2;  break;
    default:        what = "    error";   print = -1; break;
    }
    vf = *(TFRAGnode **)nodep;
    vdiflog(print, "frag:%s%d:'%47s' %p %c\n", what, depth, vf->path, vf, 'f');
}
static void tfrag_free(void *nodep)
{
    vdiflog(4, "Freeing (frag) %p\n", nodep);
    free(nodep);
}

/*
 * Find the sequence this fragment belongs to with tfind()
 */
static VDIFUSEntry *find_frag_parent(char *path, VDIFUSEntry *vc)
{
    static THIERnode tmp;
    THIERnode **hp;
    VDIFUSEntry *vp = 0;
    vdiflog(4,
        "   thier lookup '%s' ...\n", path); 
    describe_fragment(vc);
    strcpy(tmp.path, path);
    if ((hp = tfind(&tmp, &thier_root, &thier_comp)))
        vp = (current_cache_start() + (*hp)->hvdei);
    vdiflog(4,
        "   thier lookup '%s' produced [%d] %p\n"
        "                '%s'\n",
            path, (*hp)->hvdei, vp, vp->hier); 
    return(vp);
}

/*
 * Attach this fragment to its sequence
 */
static void tfrag_sadd(const void *nodep, const VISIT which, const int depth)
{
    TFRAGnode *vf = *(TFRAGnode **)nodep;
    VDIFUSEntry *vp, *vx, *vz;
    uint32_t vp_index, vz_index;

    if (which != postorder && which != leaf) return;

    vdiflog(4,
        "tfrag_sadd: node %p %s\n", vf, vf->path);
    vz = (current_cache_start() + vf->fvdei);
    vz_index = vz->index;

    if (!(vp = find_frag_parent(vz->hier, vz))) {
        vdiflog(3,
            "No sequence entry for fragment %s\n as '%s'\n",
            vf->path, vz->hier);
        return;
    }
    vp_index = vp->index;
    vdiflog(4,
        "tfrag_sadd: parent %s\n", vp->hier);

    if (!(vx = check_continuation(vp, VDIFUSE_STYPE_PART))) {
        vdiflog(3,
            "No room in parent for %s\n", vf->path);
        return;
    }
    vp = current_cache_start() + vp_index;
    vz = current_cache_start() + vz_index;

    /* first fragment sets type, thereafter we check */
    if (vp->stype == VDIFUSE_STYPE_NULL) {
        vp->stype = vz->stype;
    } else if (vp->stype != vz->stype) {
        vdiflog(3,
            "Inhomogeneous sequence %s\n", vp->path);
        return;
    }
    vdiflog(3,
        "Frag %s ->\n seq %s\n", vf->path, vp->hier);
    /* ok, safe to do the update */
    /* st_size updated later */
    vp->u.vfuse.st_nlink ++;
    vx->u.vseqi[vx->ccount++] = vz->index;
}

/*
 * Fix the vfuse size to reflect actual size of all the fragments
 * rather than the temporary u.vfuse.st_size = 0 inserted above.
 *
 * We presume that the sequences are homogeneous and have been so marked.
 */
static int finalize_sequences(void)
{
    int ii, vne = current_cache_entries();
    VDIFUSEntry *vdc = current_cache_start();
    int errs = 0;
    for (ii = 0; ii < vne; ii++) {
        if (vdc[ii].etype != VDIFUSE_ENTRY_SEQUENCE) continue;
        if (!vdc[ii].cindex) {
            vdiflog(-1, "No continuation for seq %s\n", vdc[ii].hier);
            errs ++;
            continue;
        }
        switch (vdc[ii].stype) {
        case VDIFUSE_STYPE_SGV2:
            errs += finalize_sgv2_sequence(&vdc[ii]);
            break;
        case VDIFUSE_STYPE_VDIF:
            errs += finalize_vdif_sequence(&vdc[ii]);
            break;
        case VDIFUSE_STYPE_NULL:
        default:
            fprintf(stderr, "Illegal stype %d\n", vdc[ii].stype);
            errs += 1;
        }
    }
    return(errs);
}

/*
 * When called (after all fragments have been found), we should have a
 * tree (thier_root) with the sequence directory hierarchy.
 * At this point, we
 *   create the cache entries for the required (sub)directories (thier_sdir)
 *   load them with their subdirectories or fragments (tfrag_sadd),
 *   provide the sequence sizes in vfuse data (finalize_sequences())
 * NOTION: collapse fragments within the gap threshold into same sequence
 * NOTION: split sequences of different threads?
 *   delete the trees (thier_free, tfrag_free) since we no longer need them.
 */
int create_sequences(void)
{
    int errors = 0;

    /* create subdirs for seqs */
    twalk_thier_errors = 0;
    twalk(thier_root, &thier_dump);
    twalk(thier_root, &thier_sdir);
    errors += twalk_thier_errors;

    /* put fragments into seqs */
    twalk(tfrag_root, &tfrag_dump);
    twalk(tfrag_root, &tfrag_sadd);

    /* finalize vfuse data */
    errors += finalize_sequences();

    /* delete the working trees */
    tdestroy(thier_root, &thier_free);
    tdestroy(tfrag_root, &tfrag_free);
    return(errors);
}

/*
 * Something to add fragments to the frag tree
 * Returns non-zero if there are problems.
 */
static int fragment_in_fragtree(VDIFUSEntry *vc)
{
    TFRAGnode *node = (TFRAGnode *)malloc(sizeof(TFRAGnode));
    TFRAGnode **leaf;
    if (!node) { perror("malloc-frag"); return(1); }
    vdiflog(4, "frag:malloc %p\n", node);
    VDIFUSEntry *vx;
    node->vsig = vc->vsig;
    node->first.tv_sec = vc->u.vfuse.st_mtime;
    node->first.tv_nsec = vc->u.vfuse.st_mtim.tv_nsec;
    node->final.tv_sec = vc->u.vfuse.st_ctime;
    node->final.tv_nsec = vc->u.vfuse.st_ctim.tv_nsec;
    strncpy(node->path, vc->fuse, VDIFUSE_MAX_PATH-1);
    node->fvdei = vc->index;
    leaf = tsearch(node, &tfrag_root, &tfrag_comp);
    if (!leaf) { perror("tsearch-frag"); return(2); }
    describe_fragment(vc);
    vdiflog(4,
        "    Leaf %p %016lX %p\n"
        "    Node %p %016lX %lu.%09lu..%lu.%09lu\n"
        "    Path '%s'\n"
        "    Hier '%s'\n",
        leaf, (*leaf)->vsig, (current_cache_start() + node->fvdei),
        node, node->vsig,
        node->first.tv_sec, node->first.tv_nsec,
        node->final.tv_sec, node->final.tv_nsec,
        node->path, (current_cache_start() + node->fvdei)->hier);
    vx = (current_cache_start() + node->fvdei);
    vdiflog(4, "   VC: %d:%d %p:%p %s\n",
        vc->index, node->fvdei, vc, vx, vc->hier);
    return(0);
}

/*
 * Helper function for hierarchy_name(); called with
 *   (NULL,limit) to start a new path (in temp[])
 *   (part,depth) to add to it
 *      entering it in the hierarchy as a subdir "/..."
 *      we mark subdirs through a character after the string.
 *   (NULL,-1) to finish it
 *      which enters it into the hierachy "/...", and
 *      returns a temporary string for copying to the cache entry.
 * NOTION: force vdifuse termination.  This must be done carefully to
 *       avoid breaking the heirarchy (a tdelete() and a tsearch()
 *       are needed on the revised leaf path).
 * NOTION: might consider putting the full scan name at depth.
 */
static THIERnode *dir_hier_add(char *part, int depth)
{
    static THIERnode tmp;
    THIERnode **leaf, *node;
    if (!part && (depth<0)) { /* finish up */
        leaf = tfind(&tmp, &thier_root, &thier_comp);
        if (!leaf) return(vdiflog(-1,
            "tfind-hier error: no leaf\n"), NULL);
        if ((*leaf)->type == 'D') (*leaf)->type = 'S';
        return(*leaf);
    } else if (!part) { /* initialize temp with the root */
        memset(&tmp, 0, sizeof(THIERnode));
        tmp.type = 'X';
        strncat(tmp.path, sequences_topdir, VDIFUSE_MAX_PATH-1);
    } else { /* add a component, force null termination */
        strncat(tmp.path, "/", VDIFUSE_MAX_PATH-1);
        strncat(tmp.path, part, VDIFUSE_MAX_PATH-1);
        tmp.path[VDIFUSE_MAX_PATH-1] = 0;
        tmp.type = 'D';
        node = (THIERnode *)malloc(sizeof(THIERnode));
        if (!node) return(vdiflog(-1,
            "malloc-hier error: no node\n"), NULL);
        vdiflog(4,
            "hier:malloc %p %s\n", node, tmp.path);
        memcpy(node, &tmp, sizeof(THIERnode));
        leaf = tsearch(node, &thier_root, &thier_comp);
        if (!leaf) return(vdiflog(-1,
            "tsearch-hier error: find/create: no leaf\n"), NULL);
        if (strcmp(tmp.path, (*leaf)->path)) vdiflog(-1,
            "Tree Search Corrupt Dir with %s v %s\n", tmp.path, (*leaf)->path);
    }
    return(NULL);
}

/*
 * Use this fragment to construct sequence hierarchy.
 * Called after fragment is created from accepted_as_frag()
 *
 * Parse names for canonical format <exp>_<sc>_<scan>_<...>.vdif
 * '.' is not allowed within the <> parts.
 * name is the absolute path, and we just need to work with the basename.
 * 
 * Returns 0 if all is well.
 */
static int fragment_in_hierarchy(VDIFUSEntry *vc, uint32_t limit)
{
    int depth = 0;
    char *copy = malloc(strlen(vc->path)+3), *part, *slash;
    THIERnode *hn;
    if (!copy) { perror("malloc"); return(1); }
    slash = strrchr(vc->path, '/');
    if (!slash) { free(copy); return(2); }
    (void)dir_hier_add(NULL, limit);
    if (limit == 0) {
        /* use the whole name */
        (void)dir_hier_add(slash+1, depth);
    } else {
        /* copy contains the basename of the fragment */
        part = strtok(strcpy(copy, slash+1), "_.");
        /* expect to find "vdif" as the last bit */
        while (part && strcmp(part, "vdif") && (depth < limit)) {
            (void)dir_hier_add(part, depth);
            depth ++;
            part = strtok(NULL, (depth < limit-1) ? "_." : "_");
        }
    }
    free(copy);
    hn = dir_hier_add(NULL, -1);
    if (!hn) return(vdiflog(-1,
        "frag_in_hier: dir_hier_add misbehaved\n"), 3);
    strcpy(vc->hier, hn->path);
    return( fragment_in_fragtree(vc) );
}

/*
 * Create the fragment tree.  Here we walk through the cache,
 * and identify fragments for continued processing in the routines
 * above.  This consists of parsing the name and adding the bits to
 * the sequence heirarchy.
 */
int create_fragtree(void)
{
    VDIFUSEntry *vc = current_cache_start();
    int ii, fhe, vdne = current_cache_entries();
    VDIFUSEpars *vp = current_cache_pars();
    uint32_t limit = vp->seqhierarchy;
    int errors = 0;

    vdiflog(0, limit
        ? "Sequence hierarchy depth is %u.\n"
        : "Sequence hierarchy disabled (%u).\n", limit);

    for (ii = 0; ii < vdne; ii++) {
        if (vc[ii].etype != VDIFUSE_ENTRY_FRAGMENT) continue;
        fhe = fragment_in_hierarchy(&vc[ii], limit);
        if (fhe) {
            /* NOTION: mark frag as invalid as an alternative to error return */
            errors ++;
            fprintf(stderr, "Problem adding frag %d to tree\n", ii);
        }
    }
    return(errors);
}

/*
 * eof
 */
