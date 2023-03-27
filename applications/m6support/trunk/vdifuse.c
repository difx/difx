/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifuse.c 5775 2023-03-27 14:21:32Z gbc $
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
 * vdifuse_trace is available in this file once we enter fuse_main to "use".
 * vdiftrace is an equivalent call, but gated on verbosity.
 */

#define FUSE_USE_VERSION 30

#include <fuse.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#include "vdifuse.h"
#include "vdifsig.h"
#include "vdifprc.h"

#ifndef EROFS
#define EROFS EPERM
#endif /* EROFS */

/*
 * mntpoint/
 *  fragments/
 *      basename[_vf<index>].vdif
 *  sequences/
 *      [   exp/sc/... heirarchy | noncanon ] /
 *              exp_sc_numb_....[_vf<index>].vdif
 *  vprocdir/
 *      top-level-status files
 *      dir per orr in process
 *          status files for that read
 *  if vthreads_dir != 0
 *  vthreads/
 *      [ ... ]
 *
 * where index is inserted to guarantee uniqueness of filenames.
 * The meaning of struct stat items have been adjusted to provide
 * some more useful information, since this isn't really a filesystem.
 */

char fragments_topdir[VDIFUSE_TOPDIR_SIZE] = VDIFUSE_FRAGMENTS;
char sequences_topdir[VDIFUSE_TOPDIR_SIZE] = VDIFUSE_SEQUENCES;
char vthreads_topdir[VDIFUSE_TOPDIR_SIZE] = VDIFUSE_VTHREADS;
char vdifproc_topdir[VDIFUSE_TOPDIR_SIZE] = VDIFUSE_VPROCDIR;
char *vdifuse_mount_realpath = NULL;
size_t fragments_topdir_len = 0;
size_t sequences_topdir_len = 0;
size_t vthreads_topdir_len = 0;
size_t vdifproc_topdir_len = 0;
size_t vdifuse_mount_realpath_len = 0;

/*
 * Support for file attributes
 *  stbuf provides the stat(2) data for listings, &c.
 */
static int vdifuse_getattr(const char *path, struct stat *stbuf)
{
    int res = 0;
    memset(stbuf, 0, sizeof(struct stat));
    if (!strcmp(path, "/")) {
        /* return data about the fuse root directory */
        vdifuse_topdir(VDIFUSE_TOPDIR_ROOT_DIR, stbuf);
    } else if (!strncmp(path, fragments_topdir, fragments_topdir_len)) {
        if (!strcmp(path, fragments_topdir)) {
            /* return data about the fragment directory */
            vdifuse_topdir(VDIFUSE_TOPDIR_FRAGMENT, stbuf);
        } else {
            vdiftrace(0,
                VDT("vdifuse_getattr: fragment lookup on %s\n"), path);
            if (vdifuse_fragment(path, stbuf)) {
                res = -ENOENT;
                vdiftrace(-1,
                    VDT("vdifuse_getattr: not found %s, rv=%d\n"), path, res);
            } /* else stbuf is valid */
        }
    } else if (!strncmp(path, sequences_topdir, sequences_topdir_len)) {
        if (!strcmp(path, sequences_topdir)) {
            /* return data about the sequences directory */
            vdifuse_topdir(VDIFUSE_TOPDIR_SEQUENCE, stbuf);
        } else {
            vdiftrace(-1,
                VDT("vdifuse_getattr: sequence lookup on %s\n"), path);
            if (vdifuse_sequence(path, stbuf)) {
                res = -ENOENT;
                vdiftrace(0,
                    VDT("vdifuse_getattr: not found %s, rv=%d\n"), path, res);
            } /* else stbuf is valid */
        }
    } else if (!strncmp(path, vdifproc_topdir, vdifproc_topdir_len)) {
        if (!strcmp(path, vdifproc_topdir)) {
            report_vproc(path);
            /* return data about the vdifproc directory */
            vdifuse_topdir(VDIFUSE_TOPDIR_VPROCDIR, stbuf);
        } else {
            vdiftrace(-1,
                VDT("vdifuse_getattr: vdifproc lookup on %s\n"), path);
            if (vdifuse_vprocdir(path, stbuf)) {
                res = -ENOENT;
                vdiftrace(0,
                    VDT("vdifuse_getattr: not found %s, rv=%d\n"), path, res);
            } /* else stbuf is valid */
        }
    } else if (vthreads_dir &&
               !strncmp(path, vthreads_topdir, vthreads_topdir_len)) {
        if (!strcmp(path, vthreads_topdir)) {
            /* return data about the vthreads directory */
            vdifuse_topdir(VDIFUSE_TOPDIR_VTHREADS, stbuf);
        } else {
            vdiftrace(-1,
                VDT("vdifuse_getattr: vthreads lookup on %s\n"), path);
            if (vdifuse_vthreads(path, stbuf)) {
                res = -ENOENT;
                vdiftrace(0,
                    VDT("vdifuse_getattr: not found %s, rv=%d\n"), path, res);
            } /* else stbuf is valid */
        }
    } else {
        res = -ENOENT;
        vdiftrace(-1,
            VDT("vdifuse_getattr: unsupported path=%s, rv=%d\n"), path, res);
    }
    return(res);
}

/*
 * Support for directory operations using the filler (provided by the
 * fuse API) to load each directory with entries.  It loads a buffer (buf)
 * with struct stat data for each named dictionary entry.
 *
 *   int fuse_fill_dir_t(void *buf, const char *name,
 *                       const struct stat *stbuf, off_t off);
 *      stbuf   gets that sort of data
 *      name    is the name of the entry
 *      off=0   tells fuse to manage the offsets in the directory structure
 *
 * (which is to say we are not volunteering to manage that).
 */
static int vdifuse_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                         off_t offset, struct fuse_file_info *fi)
{
    int res = 0;
    int ndx = 0, ref = 0;
    char *name;
    struct stat *sb;
    (void) offset;  /* shut up compiler */
    (void) fi;  /* shut up compiler */
    if (!strcmp(path, "/")) {
        /* directories within the mount point */
        filler(buf, ".", NULL, 0);
        filler(buf, "..", NULL, 0);
        filler(buf, fragments_topdir+1, NULL, 0);
        filler(buf, sequences_topdir+1, NULL, 0);
        filler(buf, vdifproc_topdir+1, NULL, 0);
        if (vthreads_dir)
            filler(buf, vthreads_topdir+1, NULL, 0);
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
    } else if (!strcmp(path, vdifproc_topdir)) {
        /* top-level vdifproc listing */
        filler(buf, ".", NULL, 0);
        filler(buf, "..", NULL, 0);
        while ((ndx = get_vdifuse_vprocdir(ndx, &name, &sb)) > 0)
            filler(buf, name, sb, 0);
    } else if (vthreads_dir && !strcmp(path, vthreads_topdir)) {
        /* top-level vthreads listing */
        filler(buf, ".", NULL, 0);
        filler(buf, "..", NULL, 0);
        while ((ndx = get_vdifuse_vthreads(ndx, &name, &sb)) > 0)
            filler(buf, name, sb, 0);
    } else {
        res = -ENOENT;  /* a switch to failure-oriented is clearer below */
        /* look for it in the sequence hierarchy */
        ndx = get_sequence_subdir(path);
        if (ndx > 0) {
            res = 0;
            filler(buf, ".", NULL, 0);
            filler(buf, "..", NULL, 0);
            while ((ndx = get_vdifuse_subdir(ndx, &name, &sb)) > 0)
                filler(buf, name, sb, 0);
            return(res);
        }
        /* or in the vprocdir world */
        ref = get_vprocdir_subdir(path);
        if (ref > 0) {
            res = 0;
            filler(buf, ".", NULL, 0);
            filler(buf, "..", NULL, 0);
            while ((ndx = get_vprocdir_subdir_item(ref, ndx, &name, &sb)) > 0)
                filler(buf, name, sb, 0);
            return(res);
        }
        /* or perhaps some day a vthreads_dir */
        if (vthreads_dir) {
            ndx = get_vthreads_subdir(path);
            if (ndx > 0) {
                res = 0;
                filler(buf, ".", NULL, 0);
                filler(buf, "..", NULL, 0);
                while ((ndx = get_vdifuse_subdir(ndx, &name, &sb)) > 0)
                    filler(buf, name, sb, 0);
                return(res);
            }
        }
        /* if we get here, -ENOENT it shall be */
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
 * We have nothing to flush; however the fuse API expects to have it defined.
 */
static int vdifuse_flush(const char *fusepath, struct fuse_file_info *fi)
{
    return(0);
}

/*
 * Stubs for possible future capabilities -- 
 *
 * NOTION: support unlink, rmdir, rename (missing capabilities)
 *         it is not clear that anyone would need to do this,
 *         but they can be done by manipulating the cache here.
 *
 * unlink and rmdir would remove/invalidate cache entries
 * rename would allow rename within a directory
 * (moving to another directory would be harder).
 */
static int vdifuse_unlink(const char *path)
{
    vdiftrace(-1, VDT("Unlink not allowed on %s rv=%d\n"), path, -EROFS);
    return(-EROFS); /* vdifuse_unlink not supported */
}
static int vdifuse_rmdir(const char *path)
{
    vdiftrace(-1, VDT("Rmdir not allowed on %s rv=%d.\n"), path, -EROFS);
    return(-EROFS); /* vdifuse_rmdir not supported */
}
static int vdifuse_rename(const char *from, const char *to)
{
    vdiftrace(-1, VDT("Rename %s -> %s not allowed rv=%d\n"), from, to, -EROFS);
    return(-EROFS); /* vdifuse_rename not supported */
}

/*
 * Supported/stubbed filesystem operations (see /usr/include/fuse/fuse.h
 * under "file system operations" for the complete list.  Operations not
 * defined will return -ENOSYS (Invalid system call number).  The full
 * list of error numbers is in one of /usr/include/[*]/errno.h
 * On RedHat it seems to be in /usr/include/asm-generic/errno.h.
 *
 * As of v2.6 (2019) the full list appears to be this:
 *  readlink mknod mkdir symlink rename link chmod chown truncate utimens
 *  write statfs fsync setxattr getxattr listxattr removexattr opendir
 *  releasedir fsyncdir init destroy access create ftruncate fgetattr lock
 *  bmap (various flags) ioctl poll write_buf read_buf flock fallocate
 * and the default version is 2.1 (FUSE_USE_VERSION 21).
 */
static struct fuse_operations vdifuse_oper = {
    .getattr = vdifuse_getattr,
    .readdir = vdifuse_readdir,
    .open = vdifuse_open,
    .flush = vdifuse_flush,     /* noop */
    .release = vdifuse_release,
    .read = vdifuse_read,
    .unlink = vdifuse_unlink,   /* stub */
    .rmdir = vdifuse_rmdir,     /* stub */
    .rename = vdifuse_rename,   /* stub */
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
        h2 = malloc(0x20);
        vdiftrace(0, VDT("initial heap %p + 0x20\n"), h0);
        vdiftrace(0, VDT("support heap %p + 0x20\n"), h1);
        vdiftrace(0, VDT("to fuse heap %p + 0x20\n"), h2);
        vdsig_info();
        create_vproc_cache();
        rv = fuse_main(argc, argv, &vdifuse_oper, NULL);
        delete_vproc_cache();
        h3 = malloc(0x20);
        vdiftrace(0, VDT("alldone heap %p + 0x20\n"), h3);
        if (vdifuse_enable == VDIFUSE_ENABLE_HELP) rv = 0;
        if (rv) fprintf(stderr, "Fuse error %d\n", rv);
	vdifuse_rmtrace(rv);
    }
    return(rv);
}

/*
 * eof
 */
