/************************************************************************/
/*                                                                      */
/*      This routine takes a pointer to a structure containing all the  */
/*      information about a root file necessary to generate a data      */
/*      file name.  The structure is defined in the header file         */
/*      adata.h.  The return value is a pointer to a string             */
/*      containing the pathname of the data file, relative to the main  */
/*      data directory (it is assumed that the caller knows what this   */
/*      is)                                                             */
/*                                                                      */
/*      On failure caused by bad inputs, a NULL pointer is returned     */
/*                                                                      */
/*      Created 881027 by CJL                                           */
/*      Modified to handle '.' characters in source name properly       */
/*      930303 by CJL                                                   */
/*      Moved to AFIO library for more generic utility 931018 by CJL    */
/*      islower -> isalnum for new root code Nov 2017 GBC               */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "adata.h"
#include "general.h"
#include "mk4_afio.h"
#include "mk4_util.h"

char *
rootname(rootsum *rsumm)
    {
    static char pathname[256];
    char source[32];
    int bad, i, sday, shour, smin, ssec, year;

    bad = FALSE;
                                                /* Generate root code */
    if (strlen(rsumm->root_id) != 6) bad = TRUE;
    for (i=0; i<6; i++) if (! isalnum(rsumm->root_id[i])) bad = TRUE;
    if (bad)
        {
        msg ("Bad root id code '%s'", 3, rsumm->root_id);
        msg ("Cannot generate filename without root id or creation date\n", 3);
        return (NULL);
        }
                                                /* Check inputs */
    if (rsumm->expt_no < 0) bad = TRUE;
    if (bad)
        {
        msg ("Bad experiment number (%d/%d)", 3, rsumm->expt_no);
        return (NULL);
        }
                                        /* scantime not needed for Mk4 */
    if (rsumm->version <= 4)
        {
        int_to_time (rsumm->time_tag, &year, &sday, &shour, &smin, &ssec);
        if(sday < 1 || sday > 366) bad = TRUE;
        if(shour < 0 || shour > 23) bad = TRUE;
        if(smin < 0 || smin > 59) bad = TRUE;
        if(ssec < 0 || ssec > 59) bad = TRUE;
        if (bad)
            {
            msg ("Encoded time tag makes no sense!", 3);
            return (NULL);
            }
        }
                                                /* Construct pathname */
                                /* First, map '.' to '_' in source name */
    strcpy (source, rsumm->source);
    for (i=0; i<strlen(source); i++) if (source[i] == '.') source[i] = '_';

                                /* If new version, construct the whole thing */
                                /* For Mk4, scan_id replaces encoded scantime */
    if (rsumm->version > 4)
        (void)sprintf(pathname,"%d/%s/%s.%s",
                rsumm->expt_no, rsumm->scan_id,
                source, rsumm->root_id);

    else if (rsumm->version > 1)
        (void)sprintf(pathname,"%d/%03d-%02d%02d%02d/%s.%s",
                rsumm->expt_no, sday, shour, smin, ssec,
                source, rsumm->root_id);

                                /* The * below reflects the fact that no second info */
                                /* is available in version 1 A-file data */
                                /* The resulting ambiguity is resolved in */
                                /* the get_unique_name() call below */
    else
        {
        (void)sprintf(pathname,"%d/%03d-%02d%02d*/%s.%s",
                rsumm->expt_no, sday, shour, smin,
                source, rsumm->root_id);
        get_unique_name (pathname, pathname);
        }

    return(pathname);
    }

