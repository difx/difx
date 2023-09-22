/************************************************************************/
/*                                                                      */
/* This routine is responsible for writing a corel file, given a        */
/* complete corelfile structure.  It writes out records one at a time,  */
/* after copying to the current overlay structure if needed.  After the */
/* operation, the disk file should be sorted into index number and AP   */
/* order, regardless of how the corel structure was filled originally.  */
/*                                                                      */
/*      Inputs:         corel           Filled-in corel structure       */
/*                      filename        Full pathname to write to       */
/*                                                                      */
/*      Output:         return value    total bytes written, <=0 if bad */
/*                                                                      */
/* Created 2 January 1997 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "fstruct.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define quit {fclose(fp); return (-1);}

int
write_mk4corel (struct mk4_corel *corel,
                char *filename)
    {
    int totbytes, bytes, nlags, i, maxindex = -1, ap, maxap, nblocks;
    char *ptr;
    fstruct f_info;
    FILE *fp;
    struct type_101 *t101;
    struct type_120 *t120;
                                        /* Check for legal and sensible */
                                        /* filename input */
    ptr = (char *)strrchr (filename, '/');
    if (ptr == NULL) ptr = filename - 1;
    if (check_name (ptr+1, &f_info) != 0)
        {
        msg ("Badly formed file name '%s'", 2, ptr+1);
        return (-1);
        }
    if (f_info.type != 1)
        {
        msg ("File '%s' is a type %d file, not a type 1 file", 2, 
                                                ptr+1, f_info.type);
        return (-1);
        }
                                        /* Open the output file */
    if ((fp = fopen (filename, "w")) == NULL)
        {
        msg ("Could not open file '%s'", 2, filename);
        return (-1);
        }

    nblocks = corel->t100->nblocks;     // save pristine version (not byte flipped)
                                        /* Need these to figure out how big the */
                                        /* type 120 records really are */
    nlags = corel->t100->nlags;         // save pristine version
                                        /* Loop through corel structure, */
                                        /* writing records to disk, closing */
                                        /* file and quitting on error */
    totbytes = 0;
    if (write_record ((char*)corel->id, fp, &bytes) < 0) quit;
    totbytes += bytes;
    if (write_record ((char*)corel->t100, fp, &bytes) < 0) quit;
    totbytes += bytes;
                                        /* Get last index with data */
    for (i=0; i<corel->index_space; i++)
        {
        if (corel->index[i].t101 == NULL) continue;
        maxindex = i;
        }
                                        /* Loop over index numbers */
    for (i=0; i<=maxindex; i++)
        {
        t101 = corel->index[i].t101;
        if (t101->nblocks != nblocks)
            {
            msg ("Inconsistent type 101 record encountered", 2);
            fclose (fp);
            return (-1);
            }
        if (write_record ((char*)t101, fp, &bytes) < 0) quit;
        totbytes += bytes;
                                        /* How many APs ? */
        maxap = 0;
        for (ap=0; ap<corel->index[i].ap_space; ap++)
            {
            if (corel->index[i].t120[ap] == NULL) continue;
            maxap = ap;
            }
                                        /* Loop over APs */
        for (ap=0; ap<=maxap; ap++)
            {
            t120 = corel->index[i].t120[ap];
            if (t120 == NULL) continue;
            if (t120->nlags != nlags)
                {
                msg ("Inconsistent type 120 record encountered", 2);
                fclose (fp);
                return (-1);
                }
            if (write_record ((char*)t120, fp, &bytes) < 0) quit;
            totbytes += bytes;
            }
        }
                                        /* Close file and quit */
    fclose (fp);
    return (totbytes);
    }
