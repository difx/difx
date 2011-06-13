/************************************************************************/
/*                                                                      */
/* This routine is responsible for writing a fringe file, given a       */
/* complete fringe structure.  It writes out records one at a time,     */
/* using the write_record() interface routine                           */
/*                                                                      */
/*      Inputs:         fringe           Filled-in fringe structure     */
/*                      filename        Full pathname to write to       */
/*                                                                      */
/*      Output:         return value    total bytes written, <=0 if bad */
/*                                                                      */
/* Created 2 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mk4_data.h"
#include "fstruct.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define quit {fclose(fp); return (-1);}

int
write_mk4fringe (struct mk4_fringe *fringe,
                 char *filename)
    {
    int i, totbytes, bytes;
    char *ptr;
    fstruct f_info;
    FILE *fp;
    struct type_000 t000;
                                        /* Check for legal and sensible */
                                        /* filename input */
    ptr = (char *)strrchr (filename, '/');
    if (ptr == NULL) ptr = filename - 1;
    if (check_name (ptr+1, &f_info) != 0)
        {
        msg ("Badly formed file name '%s'", 2, ptr+1);
        return (-1);
        }
    if (f_info.type != 2)
        {
        msg ("File '%s' is a type %d filename, not a type 2 filename", 2, 
                                                ptr+1, f_info.type);
        return (-1);
        }
                                        /* Fill in the id record */
    fringe->id = &t000;
    if (init_000 (fringe->id, filename) != 0)
        {
        msg ("Failure in init_000()", 2);
        return (-1);
        }
                                        /* Open the output file */
    if ((fp = fopen (filename, "w")) == NULL)
        {
        msg ("Could not open file '%s'", 2, filename);
        return (-1);
        }
                                        /* Loop through fringe structure, */
                                        /* writing records to disk, closing */
                                        /* file and quitting on error */
    totbytes = 0;
    if (write_record ((char*)fringe->id, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t200, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t201, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t202, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t203, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t204, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t205, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t206, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t207, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t208, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t210, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)fringe->t221, fp, &bytes) < 0) quit;
    totbytes += bytes;
    for (i=0; i<fringe->n212; i++)
        {
        if (write_record ((char*)fringe->t212[i], fp, &bytes) < 0) quit;
        totbytes += bytes;
        }
    for (i=0; i<fringe->n230; i++)
        {
        if (write_record ((char*)fringe->t230[i], fp, &bytes) < 0) quit;
        totbytes += bytes;
        }
                                        /* Close file and quit */
    fclose (fp);
    return (totbytes);
    }
