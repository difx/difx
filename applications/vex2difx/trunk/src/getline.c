/* getline.c
 * version 0.1, released 2004-08-31.
 *
 * http://ioioio.net./misc/getline.c
 *
 * Copyright (C) 2004 Andy Goth <unununium@openverse.com>
 *
 * This file is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2.0 of the License, or (at your option)
 * any later version.  To obtain a copy of the License, see the following:
 *
 * http://www.gnu.org./licenses/gpl.html  (HTML format)
 * http://www.gnu.org./licenses/gpl.txt   (Plain text )
 *
 * This implementation of getline() is loosely based on _IO_getdelim() as found
 * in the GNU C Library, glibc-2.3.3.
 *
 * You are encouraged to incorporate this file into your own projects, provided
 * you observe the terms of the License.  Feel free to send me any bug reports,
 * patches, or news related to this file. */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* getline() - line-based string input with automatic allocation
 *
 * getline() reads an entire line, storing the address of the buffer containing
 * the text into *buf.  The buffer is NUL-terminated and includes the newline
 * character, if a newline delimiter was found.
 *
 * If *buf is NULL, getline() allocates a buffer for containing the line, which
 * must be freed by the user program.  Alternatively, before calling getline(),
 * *buf can contain a poiner to a malloc()-allocated buffer *len bytes in
 * size.  If the buffer isn't large enough to hold the line read in, getline()
 * grows the buffer with realloc(), updating *buf and *len as necessary.
 *
 * On success, getline() returns the number of characters read, including the
 * newline, but not including the terminating NUL.  This value can be used to
 * handle embedded NUL characters in the line read.  On failure to read a line
 * (including end-of-file condition), -1 is returned, and errno may be set.
 * getline() always updates *buf and *len to reflect the buffer address and
 * size.  errno is set to EINVAL if bad parameters are passed to getline().
 *
 * XXX: Unlike GNU getline(), this function cannot correctly handle files whose
 * last line contains embedded NUL bytes but lacks a final newline character.
 * However, the only time this is likely to happen is if getline() is used to
 * read binaries.  In this exceptional condition, bytes including and following
 * the first NUL are not counted as part of the return value. */
ssize_t getline(char** buf, size_t* len, FILE* stream)
{
    int i = 0, new_len, my_malloc = 0;
    char* nl, *new_buf;

    if (buf == NULL || len == NULL) {
        errno = EINVAL;
        return -1;
    }

    if (*buf == NULL || *len == 0) {
        *buf = NULL; *len = 0;
        my_malloc = 1;
    }

    if (*len <= 60) goto alloc;

    while (1) {
        if (fgets(*buf + i, *len - i, stream) == NULL) {
            if (!feof(stream) || i == 0) {
                /* The read failed with an error, or the file is empty. */
                goto error;
            } else {
                /* The final line contains no newline, and the previous fgets()
                 * read exactly as many characters as remained in the line. */
                return i;
            }
        }

        if (feof(stream)) {
            /* We were able to successfully read at least one byte before
             * encountering EOF, but the file did not end in a newline.
             * Let's hope the last line doesn't contain any NUL bytes. */
            return i + strlen(*buf + i);
        }

        if ((nl = memchr(*buf + i, '\n', *len - i - 1)) == NULL) {
            /* No newline found.  Either we're at the end of a file with no
             * newline after its final line, or we need to grow the buffer.
             * This chunk of code is also used to allocate the initial buffer,
             * since realloc(NULL, x) works the same as malloc(x). */
            i = *len - 1;
alloc:      new_len = *len < 60 ? 120 : *len * 2;
            if ((new_buf = realloc(*buf, new_len)) == NULL) goto error;
            *buf = new_buf;
            *len = new_len;
        } else {
            /* We have the newline, so we're done. */
            return nl - *buf + 1;
        }

        /* Try again. */
    }

error:
    if (my_malloc) {
        free(*buf);
        *buf = NULL; *len = 0;
    }
    return -1;
}

/* Uncomment to provide a test utility.  Delete or rename the above getline()
 * function to test GNU getline(), if present in your libc. */
#if 0
int main(int argc, char** argv)
{
    char* buf = NULL; int len = 0, ret;

    while (1) {
        printf("getline() = %d", ret = getline(&buf, &len, stdin));
        if (ret == -1) {
            if (feof(stdin)) printf("; EOF\n");
            else perror("getline");
            break;
        } else {
            printf("; buf = \"");
            fwrite(buf, ret, 1, stdout);
            printf("\"; len = %d\n", len);
        }
    }

    free(buf);
    return EXIT_SUCCESS;
}
#endif

/* vim: set ts=4 sts=4 sw=4 tw=80 et: */
