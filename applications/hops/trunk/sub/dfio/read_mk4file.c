/************************************************************************/
/*                                                                      */
/* This routine is responsible for creating a memory image of a MkIV    */
/* disk file.  It allocates the necessary memory space, then reads the  */
/* file into that space in a reasonably efficient manner.               */
/*                                                                      */
/*      Inputs:         fp              Open file pointer               */
/*                                                                      */
/*      Output:         ptr             Pointer to memory image         */
/*                      return value    Number of bytes read            */
/*                                                                      */
/* Created August 6 1996 by CJL                                         */
/* Rewritten Jan 2 1997 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "mk4_dfio.h"
#include "mk4_util.h"

int
read_mk4file (FILE *fp,
              char **ptr)
    {
    int nb;
    int fd, size;
    struct stat file_status;
                                        /* Map stream pointer onto file */
                                        /* descriptor, and make stat() call */
                                        /* to figure out file size */
    if ((fd = fileno (fp)) < 0)
        {
        msg ("Problem with stream pointer in read_mk4file()", 2);
        return (-1);
        }
    if (fstat (fd, &file_status) != 0)
        {
        msg ("Problem making stat call in read_mk4file()", 2);
        return (-1);
        }
    size = file_status.st_size;
                                        /* Allocate memory for entire file */
    if ((*ptr = (char *)malloc (size)) == NULL)
        {
        msg ("Memory allocation error in read_mk4file()", 2);
        return (-1);
        }
    msg ("Allocated %d bytes in read_mk4file, address %p", 0, size, *ptr);
                                        /* Make sure we are at start of file */
    rewind (fp);
                                        /* Read file in a single call, let */
                                        /* system figure out best buffering */
    nb = fread (*ptr, sizeof(char), size, fp);
                                        /* Did it go OK? */
    if (nb != size)
        {
        msg ("Error, expected %d bytes, read %d bytes", 2, size, nb);
        return (-1);
        }

    return (nb);
    }
