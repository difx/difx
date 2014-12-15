/*
 * $Id: vdifuse.c 2392 2014-08-19 20:09:07Z gbc $
 *
 * Fuse for vdif files for use with Mark6 or other
 * applications where vdif files are scattered around.
 *
 * This file contains the interface to fuse.
 *
 * Derived from vdifuse.c and other fuse examples at
 * http://fuse.sourceforge.net:
 *
 *  FUSE: Filesystem in Userspace
 *  Copyright (C) 2001-2007 Miklos Szeredi <miklos@szeredi.hu>
 *  This program can be distributed under the terms of the GNU GPL.
 *  See the file COPYING.
 *
 */

#define FUSE_USE_VERSION 30

#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#include "vdifuse.h"

/*
 * mntpoint/
 *  fragments/
 *      basename[_vf<index>].vdif
 *  sequences/
 *  [   exp/sc/... heirarchy | noncanon ] /
 *              exp_sc_numb_....[_vf<index>].vdif
 *
 * where index is inserted to guarantee uniqueness of filenames.
 * The meaning of struct stat items have been adjusted to provide
 * some more useful information, since this isn't really a filesystem.
 */

char fragments_topdir[VDIFUSE_TOPDIR_SIZE] = VDIFUSE_FRAGMENTS;
char sequences_topdir[VDIFUSE_TOPDIR_SIZE] = VDIFUSE_SEQUENCES;
int fragments_topdir_len;
int sequences_topdir_len;

/*
 * Support for file attributes
 *  stbuf provides the stat(2) data for listings, &c.
 */
static int vdifuse_getattr(const char *path, struct stat *stbuf)
{
    int res = 0;
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_getattr(%s)\n", path);
    memset(stbuf, 0, sizeof(struct stat));
    if (!strcmp(path, "/")) {
        /* return data about the fuse root directory */
        vdifuse_topdir(VDIFUSE_TOPDIR_ROOT_DIR, stbuf);
    } else if (!strncmp(path, fragments_topdir, fragments_topdir_len)) {
        if (!strcmp(path, fragments_topdir)) {
            /* return data about the fragment directory */
            vdifuse_topdir(VDIFUSE_TOPDIR_FRAGMENT, stbuf);
        } else {
            if (vdifuse_debug>2) fprintf(vdflog,
                "vdifuse_getattr: fragment lookup on %s\n", path);
            if (vdifuse_fragment(path, stbuf)) {
                if (vdifuse_debug>3) fprintf(vdflog,
                    "vdifuse_getattr: fragment not found %s\n", path);
                res = -ENOENT;
            } /* else stbuf is valid */
        }
    } else if (!strncmp(path, sequences_topdir, sequences_topdir_len)) {
        if (!strcmp(path, sequences_topdir)) {
            /* return data about the sequences directory */
            vdifuse_topdir(VDIFUSE_TOPDIR_SEQUENCE, stbuf);
        } else {
            if (vdifuse_debug>2) fprintf(vdflog,
                "vdifuse_getattr: sequence lookup on %s\n", path);
            if (vdifuse_sequence(path, stbuf)) {
                if (vdifuse_debug>3) fprintf(vdflog,
                    "vdifuse_getattr: sequence not found %s\n", path);
                res = -ENOENT;
            } /* else stbuf is valid */
        }
    } else {
        res = -ENOENT;
    }
    return(res);
}

/*
 * Support for directory operations
 *   int fuse_fill_dir_t(void *buf, const char *name,
 *                       const struct stat *stbuf, off_t off);
 *   stbuf gets that sort of data
 *   name is the name of the entry
 *   off = 0 tells fuse to manage the offsets in the directory structure
 */
static int vdifuse_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                         off_t offset, struct fuse_file_info *fi)
{
    int res = 0;
    int ndx = 0;
    char *name;
    struct stat *sb;
    (void) offset;  /* shut up compiler */
    (void) fi;  /* shut up compiler */
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_readdir(%s)\n", path);
    if (!strcmp(path, "/")) {
        filler(buf, ".", NULL, 0);
        filler(buf, "..", NULL, 0);
        filler(buf, fragments_topdir+1, NULL, 0);
        filler(buf, sequences_topdir+1, NULL, 0);
    } else if (!strcmp(path, fragments_topdir)) {
        /* top-level fragments listing */
        filler(buf, ".", NULL, 0);
        filler(buf, "..", NULL, 0);
        while ((ndx = get_vdifuse_fragment(ndx, &name, &sb)) > 0)
            filler(buf, name, sb, 0);
    } else if (!strcmp(path, sequences_topdir)) {
        /* top-level sequences listing */
        filler(buf, ".", NULL, 0);
        filler(buf, "..", NULL, 0);
        while ((ndx = get_vdifuse_sequence(ndx, &name, &sb)) > 0)
            filler(buf, name, sb, 0);
    } else {
        /* look for it in the sequence hierarchy */
        ndx = get_sequence_subdir(path);
        if (ndx > 0) {
            filler(buf, ".", NULL, 0);
            filler(buf, "..", NULL, 0);
            while ((ndx = get_vdifuse_subdir(ndx, &name, &sb)) > 0)
                filler(buf, name, sb, 0);
        } else {
            res = -ENOENT;
        }
    }
    return(res);
}

/*
 * Support for open.  For every open() there is a release().
 * vorr_open() may open a file.  The only part of fuse_file_info
 * that is used are fh & flags on input and fh on output.
 */
static int vdifuse_open(const char *fusepath, struct fuse_file_info *fi)
{
    int res;
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_open(%s)\n", fusepath);
    FFInfo ffi;
    ffi.fh = fi->fh;
    ffi.flags = fi->flags;
    res = vorr_open(fusepath, &ffi);
    fi->fh = ffi.fh;
    return(res);
}

/*
 * This releases whatever vorr_open() left open.
 */
static int vdifuse_release(const char *fusepath, struct fuse_file_info *fi)
{
    int res;
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_release(%s)\n", fusepath);
    FFInfo ffi;
    ffi.fh = fi->fh;
    ffi.flags = fi->flags;
    res = vorr_release(fusepath, &ffi);
    fi->fh = ffi.fh;
    return(res);
}

/*
 * Support for read operation: put size bytes into buffer from offset.
 */
static int vdifuse_read(const char *fusepath, char *buf,
                        size_t size, off_t offset, struct fuse_file_info *fi)
{
    int res;
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_read(%s)\n", fusepath);
    FFInfo ffi;
    ffi.fh = fi->fh;
    ffi.flags = fi->flags;
    ffi.size = size;
    ffi.offset = offset;
    res = vorr_read(fusepath, buf, &ffi);
    fi->fh = ffi.fh;
    return(res);
}

/*
 * Not clear if we need this
 */
static int vdifuse_flush(const char *fusepath, struct fuse_file_info *fi)
{
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_flush(%s)\n", fusepath);
    return(0);
}

/*
 * Stubs for possible future capabilities -- 
 * unlink and rmdir would remove/invalidate cache entries
 * rename would allow rename within a directory
 * (moving to another directory would be harder).
 * TODO: manipulate cache entries to satisfy these needs.
 */
static int vdifuse_unlink(const char *path)
{
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_unlink(%s)\n", path);
    fprintf(stderr, "Unlink not allowed on %s.\n", path);
    return(-EPERM);
}
static int vdifuse_rmdir(const char *path)
{
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_rmdir(%s)\n", path);
    fprintf(stderr, "Rmdir not allowed on %s.\n", path);
    return(-EPERM);
}
static int vdifuse_rename(const char *from, const char *to)
{
    if (vdifuse_debug>4) fprintf(vdflog, "vdifuse_rename(%s)\n", from);
    fprintf(stderr, "Rename of %s to %s not allowed.\n", from, to);
    return(-EPERM);
}

/*
 * Supported/stubbed filesystem operations
 */
static struct fuse_operations vdifuse_oper = {
    .getattr = vdifuse_getattr,
    .readdir = vdifuse_readdir,
    .open = vdifuse_open,
    .flush = vdifuse_flush,
    .release = vdifuse_release,
    .read = vdifuse_read,
    .unlink = vdifuse_unlink,
    .rmdir = vdifuse_rmdir,
    .rename = vdifuse_rename,
};

/*
 * Main entry point.
 * 
 * fuse_main uses -o -h/--help -V/--version -d (-o debug) -f -s
 * the main option is the mount point
 *
 * Our options are handled in vdifsup_opts():
 * -c/-r/-u cache specifies a file with information about them
 * -x... to set options and then directories to scan.
 */
int main(int argc, char *argv[])
{
    int rv = 0;
    void *h0, *h1, *h2, *h3;
    h0 = malloc(0x20);
    if (vdifsup_opts(&argc, &argv)) return(1);
    h1 = malloc(0x20);
    if (vdifuse_enable != VDIFUSE_ENABLE_SKIP) {
        if (vdifuse_debug>4) fprintf(vdflog, "fuse_main...starting\n");
        h2 = malloc(0x20);
        if (vdifuse_debug>2) fprintf(vdflog,
            "initial heap %p + 0x20\n"
            "support heap %p + 0x20\n"
            "to fuse heap %p + 0x20\n", h0, h1, h2);
        rv = fuse_main(argc, argv, &vdifuse_oper, NULL);
        h3 = malloc(0x20);
        if (vdifuse_debug>2) fprintf(vdflog,
            "alldone heap %p + 0x20\n", h3);
        if (vdifuse_enable == VDIFUSE_ENABLE_HELP) rv = 0;
        if (rv) fprintf(stderr, "Fuse error %d\n", rv);
        if (rv<0) rv = -rv;
    }
    if (vdifuse_debug>4) fprintf(vdflog, "fuse_main...finished.\n");
    return(rv);
}

/*
 * eof
 */
