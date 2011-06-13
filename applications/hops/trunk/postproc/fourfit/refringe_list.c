/************************************************************************/
/*                                                                      */
/* This routine converts a user-supplied A-file into a fourfit-         */
/* digestible list of root files to process.  It also generates and     */
/* fills in an array of baseline and subgroup information which         */
/* specifies what subset of the data represented by each root is to     */
/* be processed, independent of any filtering based on control structure*/
/* information.  This is the refringing mechanism of fourfit.           */
/*                                                                      */
/*      Inputs:         afile_name              User-specified with -r  */
/*                      files                   fstruct array to fill   */
/*                      base_sgrp               Baseline and subgroup   */
/*                                              array to fill           */
/*                                                                      */
/*      Output:         filled in structure arrays                      */
/*                      return value            0 is OK, !=0 is bad     */
/*                                                                      */
/* Created January 20 1994 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include "adata.h"
#include "refringe.h"
#include "fstruct.h"

int
refringe_list (afile_name, files, base_sgrp)
char *afile_name;
fstruct **files;
bsgstruct **base_sgrp;
    {
    int ret, i, version, type, nfalloc, nballoc, nroot, nbsg;
    int len, nbadline, nbadname;
    char *fname, source[32], exp_scan[20], rname[256], line[512];
    char *fringename();
    bsgstruct *bsg;
    fringesum fdata;
    FILE *fp;
    extern char datadir[];
                                        /* Make some space to start with */
    *files = (fstruct *) calloc (1000, sizeof(fstruct));
    *base_sgrp = (bsgstruct *) calloc (5000, sizeof(bsgstruct));
    nfalloc = 1000;
    nballoc = 5000;
    nroot = nbsg = 0;
    if ((*base_sgrp == NULL) || (*files == NULL))
        {
        msg ("Unable to allocate space in refringe_list()", 2);
        return (1);
        }
                                        /* Open up the a-file */
    if ((fp = fopen (afile_name, "r")) == NULL)
        {
        msg ("Could not open A-file '%s'", 2, afile_name);
        return (1);
        }
                                        /* Read the file, looking only */
                                        /* at type 2 lines */
    nbadline = nbadname = 0;
    while (fgets (line, 511, fp) != NULL)
        {
        if (line[0] == '*') continue;
                                        /* Check this is a type 2 line */
        aline_id (line, &version, &type);
        if (type != 2) continue;

        if (parse_fsumm (line, &fdata) != 0)
            {
            msg ("Failed to parse line, skipping", 1);
            msg ("%s", 1, line);
            nbadline++;
            continue;
            }
                                        /* More robust than just looking */
                                        /* at the structure elements to figure */
                                        /* it out */
        if ((fname = fringename (&fdata)) == NULL)
            {
            nbadname++;
            continue;
            }
                                        /* Construct the root filename */
                                        /* First, copy directory part */
        len = strlen (fname);
        for (i = len-1; i >= 0; i--) if (fname[i] == '/') break;
        strncpy (exp_scan, fname, i);
        exp_scan[i] = '\0';
                                        /* Do proper source name mapping */
        strcpy (source, fdata.source);
        for (i=0; i<strlen(source); i++) if (source[i] == '.') source[i] = '_';
        sprintf (rname, "%s/%s/%s.%s", datadir, exp_scan, source, fdata.root_id);
                                        /* Now find root in files list */
        for (i=0; i<nroot; i++)
            if (strcmp (rname, (*files)[i].name) == 0) break;
                                        /* Not found, make new files entry */
                                        /* fourfit only needs these fstruct */
                                        /* elements */
        if (i == nroot)
            {
            (*files)[i].name = strdup (rname);
            (*files)[i].order = 0;
            nroot++;
            }
                                        /* Add to list of baselines/subgroups */
        bsg = *base_sgrp + nbsg;
        bsg->files_index = i;
        strcpy (bsg->baseline, fdata.baseline);
        bsg->subgroup = fdata.freq_code;
        nbsg++;
                                        /* Expand arrays as needed */
                                        /* Leave space for termination */
        if (nroot > nfalloc-2)
            {
            nfalloc += 1000;
            *files = (fstruct *) realloc (*files, nfalloc*sizeof(fstruct));
            }
        if (nbsg > nballoc - 2)
            {
            nballoc += 5000;
            *base_sgrp = (bsgstruct *) realloc (*base_sgrp, nballoc*sizeof(bsgstruct));
            }
        if ((*files == NULL) || (*base_sgrp == NULL))
            {
            msg ("Unable to reallocate space in refringe_list()", 2);
            fclose (fp);
            return (1);
            }
        }                               /* End loop through A-file */
                                        /* Terminate lists */
    (*files)[nroot].order = -1;
    (*base_sgrp)[nbsg].files_index = END_OF_LIST;

    fclose (fp);
    return (0);
    }
