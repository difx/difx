#!/usr/bin/env python3
"""pagecache.py - drop files from this node's page cache, or report how much of them is cached.

Usage:  pagecache.py drop <file>...       fsync, then posix_fadvise(DONTNEED) each file
        pagecache.py resident <file>...   print the percentage of the files' pages in the cache

Used by bench-askap-staging.sh to time cold reads without root (no drop_caches).  A drop
only removes clean pages that no other process holds, so always check with "resident".
"""
import ctypes
import ctypes.util
import mmap
import os
import sys

PAGE = os.sysconf("SC_PAGE_SIZE")


def drop(path):
    fd = os.open(path, os.O_RDONLY)
    try:
        os.fsync(fd)                      # dirty pages (a fresh copy) cannot be dropped
        os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_DONTNEED)
    finally:
        os.close(fd)


def resident_pages(path, libc):
    """(pages in cache, pages in file), from mincore() on a read-only mapping."""
    size = os.path.getsize(path)
    npages = (size + PAGE - 1) // PAGE
    if npages == 0:
        return 0, 0
    fd = os.open(path, os.O_RDONLY)
    try:
        libc.mmap.restype = ctypes.c_void_p
        libc.mmap.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_int, ctypes.c_int,
                              ctypes.c_int, ctypes.c_long]
        addr = libc.mmap(None, size, mmap.PROT_READ, mmap.MAP_SHARED, fd, 0)
        if addr in (None, ctypes.c_void_p(-1).value):
            raise OSError(ctypes.get_errno(), "mmap failed: " + path)
        try:
            vec = (ctypes.c_ubyte * npages)()
            libc.mincore.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_void_p]
            if libc.mincore(ctypes.c_void_p(addr), size, vec) != 0:
                raise OSError(ctypes.get_errno(), "mincore failed: " + path)
            return sum(v & 1 for v in vec), npages
        finally:
            libc.munmap.argtypes = [ctypes.c_void_p, ctypes.c_size_t]
            libc.munmap(ctypes.c_void_p(addr), size)
    finally:
        os.close(fd)


def main():
    if len(sys.argv) < 3 or sys.argv[1] not in ("drop", "resident"):
        sys.exit(__doc__)
    files = sys.argv[2:]
    if sys.argv[1] == "drop":
        for f in files:
            drop(f)
        return
    libc = ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True)
    have = total = 0
    for f in files:
        h, t = resident_pages(f, libc)
        have += h
        total += t
    print("%.1f" % (100.0 * have / total if total else 0.0))


if __name__ == "__main__":
    main()
