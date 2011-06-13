/************************************************************************/
/*									*/
/* This routine converts a user-supplied A-file into a fringex-		*/
/* digestible list of fringe files to process.				*/
/*									*/
/*	Inputs:		afile_name		User-specified with -r	*/
/*			mode			for coh. time modes	*/
/*			files			fstruct array to fill	*/
/*									*/
/*	Output:		filled in structure array			*/
/*			return value		0 is OK, !=0 is bad	*/
/*									*/
/* Created November 27 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "mk4_afio.h"
#include "fringex.h"
#include "fstruct.h"

int
filelist (afile_name, mode, files)
char *afile_name;
int mode;
fstruct **files;
    {
    int version, type, nfalloc, nfiles;
    int nbadtype, nbadline, nbadname, badcoh, nbadcoh;
    char *fname, c, fullname[256], line[512];
    fringesum fdata;
    FILE *fp;
    extern char datadir[];
					/* For full pathnames we need the */
					/* environment variable DATADIR */
    environment();
					/* Make some space to start with */
    *files = (fstruct *) calloc (1000, sizeof(fstruct));
    nfalloc = 1000;
    if (*files == NULL)
        {
        msg ("Unable to allocate space in filelist()", 2);
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
    nfiles = nbadtype = nbadline = nbadname = nbadcoh = 0;
    while (fgets (line, 511, fp) != NULL)
	{
	if (line[0] == '*') continue;
					/* Check that this is a type 2 line */
	aline_id (line, &version, &type);
	if (type != 2) continue;

	if (parse_fsumm (line, &fdata) != 0)
	    {
	    msg ("Failed to parse line, skipping", 1);
	    msg ("%s", 1, line);
	    nbadline++;
	    continue;
	    }
					/* Only scan-based A-data allowed */
	c = fdata.datatype[0];
	if ((c != 'J') && (c != 'K') && (c != 'L') && (c != 'S'))
	    {
	    nbadtype++;
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
					/* Skip if needed coherence times */
					/* not present */
	badcoh = FALSE;
	if ((mode & SEARCH) && (fdata.srch_cotime < 0)) badcoh = TRUE;
	if ((mode & NOLOSS) && (fdata.noloss_cotime < 0)) badcoh = TRUE;
	if (badcoh)
	    {
	    nbadcoh++;
	    continue;
	    }
					/* Fill in fstruct entry */
	sprintf (fullname, "%s/%s", datadir, fname);
	(*files)[nfiles].name = (char *)strdup (fullname);
	(*files)[nfiles].order = 0;
	(*files)[nfiles].type = 2;
	(*files)[nfiles].freq_code = fdata.freq_code;
	strcpy ((*files)[nfiles].baseline, fdata.baseline);
	strcpy ((*files)[nfiles].rootcode, fdata.root_id);
					/* Remember coherence times, rate */
					/* and delay */
	(*files)[nfiles].intparm[0] = fdata.srch_cotime;
	(*files)[nfiles].intparm[1] = fdata.noloss_cotime;
	(*files)[nfiles].floatparm[0] = fdata.delay_rate;
	(*files)[nfiles].floatparm[1] = fdata.mbdelay;
	nfiles++;
					/* Expand arrays as needed */
					/* Leave space for termination */
	if (nfiles > nfalloc-2)
	    {
            nfalloc += 1000;
            *files = (fstruct *) realloc (*files, nfalloc*sizeof(fstruct));
            }
	if (*files == NULL)
	    {
	    msg ("Unable to reallocate space in filelist()", 2);
	    fclose (fp);
	    return (1);
	    }
	}				/* End loop through A-file */
					/* Terminate lists */
    (*files)[nfiles].order = -1;
					/* Error report */
    if ((nbadline + nbadname) > 0)
	msg ("Ignored %d unparseable lines and %d failures in name generation",
				2, nbadline, nbadname);
    if (nbadtype > 0)
	msg ("Warning - ignored %d lines which were not scan-based datatypes",
				2, nbadtype);
    if (nbadcoh > 0)
	msg ("Warning - ignored %d lines with undetermined coherence times",
				2, nbadcoh);

    fclose (fp);
    return (0);
    }
