/************************************************************************/
/*                                                                      */
/*      This routine takes a pointer to a structure containing all the  */
/*      information about a fringe file necessary to generate a data    */
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
fringename(fringesum *fsumm)
    {
    static char pathname[256];
    int bad, i, scan_time, sday, shour, smin, ssec, year;

    bad = FALSE;
                                                /* Generate root code */
    if (strlen(fsumm->root_id) != 6) bad = TRUE;
    for (i=0; i<6; i++) if (! islower(fsumm->root_id[i])) bad = TRUE;
    if (bad)
        {
        msg ("Bad root id code '%s'", 3, fsumm->root_id);
        msg ("Cannot generate filename without root id or creation date\n", 3);
        return (NULL);
        }
                                                /* Check inputs */
    if (fsumm->expt_no < 0) bad = TRUE;
    if (fsumm->extent_no < 0 || fsumm->extent_no > 999) bad = TRUE;
    if (bad)
        {
        msg ("Bad experiment or extent number (%d/%d)", 3, fsumm->expt_no,
                                                        fsumm->extent_no);
        return (NULL);
        }
                                        /* Time tag not needed for Mk4 */
    if (fsumm->version <= 4)
        {
        scan_time = fsumm->time_tag - fsumm->scan_offset;
        int_to_time (scan_time, &year, &sday, &shour, &smin, &ssec);
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
                                /* Construct pathname relative to datadir */
                                /* If new version, construct the whole thing */
                                /* For Mk4, scan id replaces encoded scan time */
    if (fsumm->version > 4)
        (void)sprintf(pathname,"%d/%s/%s.%c.%d.%s",
                fsumm->expt_no, fsumm->scan_id,
                fsumm->baseline, fsumm->freq_code,
                fsumm->extent_no, fsumm->root_id);

    else if (fsumm->version > 1)
        (void)sprintf(pathname,"%d/%03d-%02d%02d%02d/%s.%c.%d.%s",
                fsumm->expt_no, sday, shour, smin, ssec,
                fsumm->baseline, fsumm->freq_code,
                fsumm->extent_no, fsumm->root_id);

                                /* The * below reflects the fact that no second info */
                                /* is available in version 1 A-file data */
                                /* The resulting ambiguity is resolved in */
                                /* the get_unique_name() call below */
    else
        {
        (void)sprintf(pathname,"%d/%03d-%02d%02d*/%s.%c.%d.%s",
                fsumm->expt_no, sday, shour, smin,
                fsumm->baseline, fsumm->freq_code,
                fsumm->extent_no, fsumm->root_id);
        if (get_unique_name (pathname, pathname) != 0) return (NULL);
        }

    return (pathname);
    }

