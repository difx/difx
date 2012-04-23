/************************************************************************/
/*                                                                      */
/* This routine is responsible for writing a sdata file, given a        */
/* complete sdatafile structure.  It writes out records one at a time,  */
/* using the write_record() interface routine                           */
/*                                                                      */
/*      Inputs:         sdata           Filled-in sdata structure       */
/*                      filename        Full pathname to write to       */
/*                                                                      */
/*      Output:         return value    total bytes written, <=0 if bad */
/*                                                                      */
/* Created 12 March 1998 by CJL                                         */
/* Added support for type_309 records            rjc  2006.2.6          */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mk4_data.h"
#include "fstruct.h"
#include "mk4_sizes.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define quit {fclose(fp); return (-1);}

int
write_mk4sdata (struct mk4_sdata *sdata,
                char *filename)
    {
    int i, j, totbytes, bytes;
    char *ptr;
    fstruct f_info;
    FILE *fp;
    struct type_000 t000;
    struct type_301 *t301;
    struct type_302 *t302;
    struct type_302 *t303;
                                        /* Check for legal and sensible */
                                        /* filename input */
    ptr = (char *)strrchr (filename, '/');
    if (ptr == NULL) ptr = filename - 1;
    if (check_name (ptr+1, &f_info) != 0)
        {
        msg ("Badly formed file name '%s'", 2, ptr+1);
        return (-1);
        }
    if (f_info.type != 3)
        {
        msg ("File '%s' is a type %d file, not a type 3 file", 2, 
                                                ptr+1, f_info.type);
        return (-1);
        }
                                        /* Fill in the id record */
    sdata->id = &t000;
    if (init_000 (sdata->id, filename) != 0)
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
                                        /* Loop through sdata structure, */
                                        /* writing records to disk, closing */
                                        /* file and quitting on error */
    totbytes = 0;
    if (write_record ((char *)sdata->id, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)sdata->t300, fp, &bytes) < 0) quit;
    totbytes += bytes;

    for (i=0; i<MAX_CHAN_PP; i++)
        for (j=0; j<MAXSPLINES; j++)
            {
            t301 = sdata->model[i].t301[j];
            t302 = sdata->model[i].t302[j];
            t303 = sdata->model[i].t303[j];
            if (t301 != NULL)
                {
                if (write_record ((char*)t301, fp, &bytes) < 0) quit;
                totbytes += bytes;
                }
            if (t302 != NULL)
                {
                if (write_record ((char*)t302, fp, &bytes) < 0) quit;
                totbytes += bytes;
                }
            if (t303 != NULL)
                {
                if (write_record ((char*)t303, fp, &bytes) < 0) quit;
                totbytes += bytes;
                }
        }

    for (i=0; i<MAXSTATPER; i++)
        {
        if (i < sdata->n304)
            {
            if (write_record ((char*)sdata->t304[i], fp, &bytes) < 0) quit;
            totbytes += bytes;
            }
        if (i < sdata->n305)
            {
            if (write_record ((char*)sdata->t305[i], fp, &bytes) < 0) quit;
            totbytes += bytes;
            }
        if (i < sdata->n306)
            {
            if (write_record ((char*)sdata->t306[i], fp, &bytes) < 0) quit;
            totbytes += bytes;
            }
        if (i < sdata->n307)
            {
            if (write_record ((char*)sdata->t307[i], fp, &bytes) < 0) quit;
            totbytes += bytes;
            }
        if (i < sdata->n308)
            {
            if (write_record ((char*)sdata->t308[i], fp, &bytes) < 0) quit;
            totbytes += bytes;
            }
        if (i < sdata->n309)
            {
            if (write_record ((char*)sdata->t309[i], fp, &bytes) < 0) quit;
            totbytes += bytes;
            }
        }

                                        /* Close file and quit */
    fclose (fp);
    return (totbytes);
    }
