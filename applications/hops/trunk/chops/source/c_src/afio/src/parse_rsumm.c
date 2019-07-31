/************************************************************************/
/*                                                                      */
/* Second in a trio of routines to decode ascii lines from A-files.     */
/* This one deals with type 0 (root) lines.                             */
/*                                                                      */
/*      Inputs:         line            ASCII line from A-file          */
/*                                                                      */
/*      Output:         file            Points to adata structure       */
/*                      return value    0 for success, -1 for failure   */
/*                                      +1 indicates OK but bad year    */
/*                                      +2 indicates incomplete parse   */
/*                                                                      */
/* Created 26 April 1991 by CJL                                         */
/* Added support for version 2, 27 January 1994 by CJL                  */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "adata.h"
#include "mk4_afio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

char *rpformat1 = "%s %d %hd %hd %hd %2d%3d%c%2d%2d %d %d-%2d%2d %s %s";

char *rpformat23 = "%*d %s %d %hd %hd %hd %2d%3d%c%2d%2d %d %d-%2d%2d%2d %s %s";

char *rpformat4 = "%*d %s %d %hd %hd %hd %2d%3d%c%2d%2d%2d %d %d-%2d%2d%2d %s %s";

char *rpformat5 = "%*d %s %d %hd %hd %s %2d%3d-%2d%2d%2d %d %d-%2d%2d%2d %s %s";

char *rpformat6 = "%*d %s %d %hd %hd %s %2d%3d-%2d%2d%2d %d %d-%2d%2d%2d %s %s";

int
parse_rsumm(char *line, rootsum *file)
    {
    int n, pyear, pday, phour, pmin, psec, syear, sday, shour, smin, ssec, yearok, type;
    int version, incomplete;
    char c, name[7];

    yearok = TRUE;                              /* Flag for scan year info */
    incomplete = FALSE;

    clear_rsumm (file);
                                        /* What version is this format? */
    if (isdigit (line[0])) version = line[0] - '0';
    else version = 1;
    file->version = version;

    psec = 0;
    switch (version)
        {
        case 1:
            n = sscanf(line, rpformat1,
                name,
                &type, 
                &(file->extent_no), 
                &(file->size),
                &(file->expt_no), 
                &pyear, &pday, &(file->corel_vers), &phour, &pmin,
                &syear, &sday, &shour, &smin,
                file->source,
                file->stations);
                                        /* Check that the caller got it right */
            if (type != 0)
                {
                msg ("parse_rsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 15) return(-1);


                                        /* Try to get root code */
            if (islower(line[112])) 
                n += sscanf (line+112, "%6s", file->root_id);

            file->archiv = 0;
            sscanf(line+126,"A%c%hd", &c, &(file->archiv));
            if ((c >= '0') && (c <= '9')) file->archiv += 1000 * (c - '0');
            else file->archiv += 1000 * (10 + c - 'A');
            n++;

            if (n < 18) incomplete = TRUE;
                                        /* VFIND output */
            if (syear < 80) yearok = FALSE;
                                        /* No seconds resolution in version 1 */
            ssec = 0;
                                        /* Nonsense name non-fatal */
            if (sscanf(name,"<%s",file->fname) != 1)
                sprintf(file->fname,"00000");
            break;

        case 2:
        case 3:
                                        /* Version 2/3, unix only, Jan 94 on */
            n = sscanf (line, rpformat23,
                file->root_id,
                &type,
                &(file->extent_no),
                &(file->size),
                &(file->expt_no),
                &pyear, &pday, &(file->corel_vers), &phour, &pmin,
                &syear, &sday, &shour, &smin, &ssec,
                file->source,
                file->stations);
                                        /* Check that the caller got it right */
            if (type != 0)
                {
                msg ("parse_rsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 16) return(-1);

            if (n < 17) incomplete = TRUE;
            break;

        case 4:
                                        /* Version 2/3/4, unix only, Jan 94 on */
            n = sscanf (line, rpformat4,
                file->root_id,
                &type,
                &(file->extent_no),
                &(file->size),
                &(file->expt_no),
                &pyear, &pday, &(file->corel_vers), &phour, &pmin, &psec,
                &syear, &sday, &shour, &smin, &ssec,
                file->source,
                file->stations);
                                        /* Check that the caller got it right */
            if (type != 0)
                {
                msg ("parse_rsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 17) return(-1);

            if (n < 18) incomplete = TRUE;
            break;

        case 5:
                                        /* Version 5, Mk4 only, Sep 99 on */
            n = sscanf (line, rpformat5,
                file->root_id,
                &type,
                &(file->size),
                &(file->expt_no),
                file->scan_id,
                &pyear, &pday, &phour, &pmin, &psec,
                &syear, &sday, &shour, &smin, &ssec,
                file->source,
                file->stations);
                                        /* Check that the caller got it right */
            if (type != 0)
                {
                msg ("parse_rsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 16) return(-1);
            if (n < 17) incomplete = TRUE;
            break;

        case 6:
                                        /* Version 6, EHT era */
            n = sscanf (line, rpformat5,
                file->root_id,
                &type,
                &(file->size),
                &(file->expt_no),
                file->scan_id,
                &pyear, &pday, &phour, &pmin, &psec,
                &syear, &sday, &shour, &smin, &ssec,
                file->source,
                file->stations);
                                        /* Check that the caller got it right */
            if (type != 0)
                {
                msg ("parse_rsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 16) return(-1);
            if (n < 17) incomplete = TRUE;
            break;

        default:
            msg ("Unsupported A-file format version number '%d'", 2, version);
            return (-1);
        }
                                        /* fields independent of version # */
    if (syear < 70) syear += 2000;
    if (pyear < 70) pyear += 2000;
    file->time_tag = time_to_int (syear, sday, shour, smin, ssec);
    file->procdate = time_to_int (pyear, pday, phour, pmin, psec);

    if (incomplete) return (2);
    else if (yearok) return(0);
    else return(1);
    }
