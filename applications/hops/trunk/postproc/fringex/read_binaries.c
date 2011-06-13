/************************************************************************/
/*                                                                      */
/* Reads in the fringe file and associated root file.  It leaves a      */
/* pointer to the fringe file image and the relevant baseline of the    */
/* root file in the fxp structure.  A certain amount of cross-checking  */
/* is done.  The output A-file line is filled in with all the values    */
/* that do not depend on the segment.  Each output line then need only  */
/* have the segment-dependent pieces filled in later.                   */
/*                                                                      */
/*      Inputs:         file            standard fstruct structure      */
/*                                                                      */
/*      Output:         fxp             fringe and rootb pointers set   */
/*                                      and adata fields filled in      */
/*                      return value    0=OK, 1=bad                     */
/*                                                                      */
/* Created October 10 1995 by CJL                                       */
/* Modified for Mk4 use                               rjc  2007.9.28    */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "general.h"
#include "fringex.h"
#include "fstruct.h"
#include "vex.h"

int read_binaries (fstruct *file,
                   struct fxparam *fxp)
    {
    int i, j, lastslash, fourfit;
    float minlo, maxlo, freq;
    static struct mk4_fringe fringe;
    static struct mk4_sdata sdata[2];
    static struct vex root;
    struct datec date;
    char rootname[256], fringename[256], directory[256], 
         stnfile_name[256], source[9], rootcode[7];
                                        /* Get relevant strings */
    strcpy (fringename, file->name);
    strcpy (rootcode, file->rootcode);
                                        /* Read the fringe file */
    fringe.nalloc = 0;                 // no memory blocks yet allocated
    if (read_mk4fringe (fringename, &fringe) != 0)
        {
        msg ("Failure reading fringe file %s", 2, fringename);
        return (1);
        }
    msg ("Read fringe file %s", 0, fringename);
                                        // kludge to make backward compatible fix to
                                        // type 206 start.day problem. fourfit was calculating
                                        // 0-based day, instead of doy

    if (fringe.t206->start.day + 1 == fringe.t200->scantime.day)
        fringe.t206->start.day += 1;

                                        /* Now need root associated with */
                                        /* this fringe file */
                                        /* Find directory by truncating fringe */
                                        /* name at last slash */
    strcpy (directory, fringename);
    lastslash = -1;
    for (i=0; i<strlen(directory); i++)
        if (directory[i] == '/') lastslash = i;
    directory[lastslash+1] = '\0';
                                        /* Get source name and convert . to _ */
                                        /* Also trim trailing blanks */
                                        // note that 32 char source trimmed to 8 chars
    strncpy (source, fringe.t201->source, 8);
    source[8] = '\0';
    for (i=0; i<8; i++)
        {
        if (source[i] == '.') source[i] = '_';
        if (source[i] == ' ') source[i] = '\0';
        }
                                        /* Concatenate for full root name */
                                        /* and read in the root file if not */
                                        /* already in memory.  Point to current */
                                        /* baseline */
    sprintf (rootname, "%s%s.%s", directory, source, rootcode);
    if (get_vex (rootname, OVEX | EVEX, "", &root) != 0)
        {
        msg ("Failure reading root file %s", 2, rootname);
        return (1);
        }
    msg ("Read root file %s", 0, rootname);
                                        // read station file name into memory structure
    for (j=0; j<2; j++)
        {                               // first construct name of each station file
        strcpy (stnfile_name, fringename);
        lastslash = -1;
        for (i=0; i<strlen(stnfile_name); i++)
            if (stnfile_name[i] == '/') lastslash = i;

        stnfile_name[lastslash+1] = stnfile_name[lastslash+1+j];
        stnfile_name[lastslash+2] = '.';
        stnfile_name[lastslash+3] = '.';
        
        strcpy (stnfile_name+lastslash+4, stnfile_name+strlen(fringename)-6);

                                        // now read data into sdata array
        if (read_mk4sdata (stnfile_name, &sdata[j]) != 0)
            {
            msg ("Failure reading station %d data file %s", 2, j, stnfile_name);
            return (1);
            }
	msg ("Read station file %s", 0, stnfile_name);
        }

                                        /* Set pointers in fxp */
    fxp->fringe = &fringe;
    for (i=0; i<2; i++)
        fxp->sdata[i] = &sdata[i];
                                        /* Fill in parts of the A-file struct */
                                        /* which are segment-invariant */
                                        /* Borrowed heavily from alist code */
    if (fill_aline (&fringe, &root, fringename, &(fxp->adata)) != 0)
        {
        msg ("Problem with input file '%s'", 2, fringename);
        return (1);
        }
    msg ("Proceeding with input file %s", 0, fringename);
                                        /* Fill in the calculated parameters */
                                        /* in fxp */
    fxp->ffit_reffreq = fringe.t205->ref_freq;
    fxp->amp_corr_fact = 1.0;           // there is no corr. factor saved in mk4
    fxp->reftime = time_to_int ((int)fringe.t200->frt.year,
                                (int)fringe.t200->frt.day,
                                (int)fringe.t200->frt.hour,
                                (int)fringe.t200->frt.minute,
                                (int)fringe.t200->frt.second);
    fxp->acc_period = root.evex->ap_length;
                                        // for fourfit, dprate is cleared
    fxp->dprate = 0;
                                        /* The coherence time was stored in */
                                        /* the intparm parameter by filelist(), */
                                        /* need to retrieve and remember it now */
    fxp->srch_cotime = file->intparm[0];
    fxp->noloss_cotime = file->intparm[1];
    fxp->adata.srch_cotime = file->intparm[0];
    fxp->adata.noloss_cotime = file->intparm[1];
                                        /* Same thing for rate and delay */
    fxp->rate = file->floatparm[0];
    fxp->delay = file->floatparm[1];
                                        /* Finally, must compute the spanned */
                                        /* bandwidth of the current file */
                                        // just do for reference station freq.
    maxlo = 0.0;
    minlo = 1.0e30;
    for (i=0; i<NFX_SB_32; i++)
        {                               // need total bandwidth in MHz
        freq = fringe.t203->channels[i].ref_freq / 1e6;
        if (freq > maxlo) 
            maxlo = freq;
        if ((freq > 0.0) && (freq < minlo)) 
            minlo = freq;
        fxp->bandwidth = maxlo - minlo;
//      for now, commment out bw correction, as sample rate is wrong in the t203 rjc 2007.10.18
//      fxp->bandwidth += (double)fringe.t203->channels[i].sample_rate / 2e6;
        }

    return (0);
    }
