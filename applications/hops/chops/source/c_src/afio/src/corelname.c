/************************************************************************/
/*                                                                      */
/*      This routine takes a pointer to a structure containing all the  */
/*      information about a corel file necessary to generate a data     */
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
corelname (corelsum *csumm)
    {
    static char pathname[256];
    int bad, i, sday, shour, smin, ssec, year;

    bad = FALSE;
                                                /* Generate root code */
    if (strlen(csumm->root_id) != 6) bad = TRUE;
    for (i=0; i<6; i++) if (! isalnum(csumm->root_id[i])) bad = TRUE;
    if (bad)
        {
        msg ("Bad root id code '%s'", 3, csumm->root_id);
        msg ("Cannot generate filename without root id or creation date\n", 3);
        return (NULL);
        }
                                                /* Check inputs */
    if (csumm->expt_no < 0) bad = TRUE;
    if (csumm->extent_no < 0 || csumm->extent_no > 9999) bad = TRUE;
    if (bad)
        {
        msg ("Bad experiment or extent number (%d/%d)", 3, csumm->expt_no,
                                                        csumm->extent_no);
        return (NULL);
        }
                                        /* scantime not needed for Mk4 */
    if (csumm->version <= 4)
        {
        int_to_time (csumm->time_tag, &year, &sday, &shour, &smin, &ssec);
        if(sday < 1 || sday > 366) bad = TRUE;
        if(shour < 0 || shour > 23) bad = TRUE;
        if(smin < 0 || smin > 59) bad = TRUE;
        if(ssec < 0 || ssec > 59) bad = TRUE;
        if (bad)
            {
            msg ("Encoded time_tag makes no sense!", 3);
            return (NULL);
            }
        }
                                /* Construct pathname relative to datadir */
                                /* If new version, construct the whole thing */
                                /* For Mk4, scan_id replaces encoded scantime */
                                /* Also, there is no extent # for Mk4 type-1 files */
    if (csumm->version > 4)
        (void)sprintf(pathname,"%d/%s/%s..%s",
                csumm->expt_no, csumm->scan_id,
                csumm->baseline, csumm->root_id);

    else if (csumm->version > 1)
        (void)sprintf(pathname,"%d/%03d-%02d%02d%02d/%s.%d.%s",
                csumm->expt_no, sday, shour, smin, ssec,
                csumm->baseline, csumm->extent_no, csumm->root_id);

                                /* The * below reflects the fact that no second info */
                                /* is available in version 1 A-file data */
                                /* The resulting ambiguity is resolved in */
                                /* the get_unique_name() call below */
    else
        {
        (void)sprintf(pathname,"%d/%03d-%02d%02d*/%s.%d.%s",
                csumm->expt_no, sday, shour, smin,
                csumm->baseline, csumm->extent_no, csumm->root_id);
        get_unique_name (pathname, pathname);
        }

    return(pathname);
    }

