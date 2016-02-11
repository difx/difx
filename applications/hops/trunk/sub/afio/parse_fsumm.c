/************************************************************************/
/*                                                                      */
/* This rather unwieldy routine handles the task of decoding the ASCII  */
/* information contained in a line from a standard A-file.  It uses the */
/* decoded information to fill a structure, making the information much */
/* more compact (for memory-resident arrays and efficiency), and easier */
/* to manipulate for purposes ranging from plotting to data filtering.  */
/* It returns an error code only when unable to decode the first 9      */
/* items from the line.  This means that a successful return yields a   */
/* structure containing enough information to uniquely specify the data */
/* file from which the line is derived.                                 */
/*                                                                      */
/*      Inputs:         line            ASCII line from A-file          */
/*                                                                      */
/*      Output:         file            Points to adata structure       */
/*                      return value    0 for success, -1 for failure   */
/*                                      +1 indicates OK but bad year    */
/*                                      +2 indicates incomplete parse   */
/*                                                                      */
/* Created 31 March by CJL                                              */
/* Added support for version 2, 26 January 1994 by CJL                  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <ctype.h>
#include "adata.h"
#include "mk4_afio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

char *fpformat1 = "%s %d %hd %hd %hd %2d%3d%c%2d%2d %d %d-%2d%2d %s %2s%c %s %s %c%c%hd \
%f %f %f %f %f %f %d %s %2hd%2hd %f %lf %lf %f %f %hd %hd %hd %hd %hd %s";

char *fpformat2 = "%*d %s %d %hd %hd %hd %2d%3d%c%2d%2d %d %d-%2d%2d%2d %s %2s%c \
%c%c%hd %f %f %f %s %f %f %f %f %f %f %f %f %f %f %d %2hd%2hd %lf %f %lf %lf %f %s";

char *fpformat3 = "%*d %s %d %hd %hd %hd %hd %hd %2d%3d%c%2d%2d %d %d-%2d%2d%2d %s %2s%c \
%c%c%hd %f %f %f %s %f %f %f %f %f %f %f %f %f %f %d %2hd%2hd %lf %f %lf %lf %f %s";

char *fpformat4 = "%*d %s %d %hd %hd %hd %hd %hd %2d%3d%c%2d%2d%2d %d %d-%2d%2d%2d %hd %s \
%2s%c %c%c%hd %d %f %f %f %f %s %f %f %f %f %f %f %f %f %f %f %d %2hd%2hd %lf %f %lf %lf \
%f %hd %hd %s";

char *fpformat5 = "%*d %s %d %hd %hd %hd %hd %hd %s %2d%3d-%2d%2d%2d %d %d-%2d%2d%2d %hd %s \
%2s %c%c %c%hd %2s %d %f %f %f %f %s %f %f %f %f %f %f %f %f %f %f %d %2hd%2hd %lf %f %lf %lf \
%f %hd %hd";

char *fpformat6 = "%*d %s %d %hd %hd %hd %hd %hd %s %2d%3d-%2d%2d%2d %d %d-%2d%2d%2d %hd %s \
%2s %c%c %c%hd %2s %d %f %f %f %f %s %f %f %f %f %f %f %f %f %f %f %d %2hd%2hd %lf %f %lf %lf \
%f %hd %hd";

int
parse_fsumm(char *line, fringesum *file)
    {
    int n, syear, sday, shour, smin, ssec, pyear, pday, phour, pmin, psec, yearok, type;
    int version, incomplete;
    char c, afile[6], name[7], parents[20];
    char *strcpy(), *strncpy(), *strcat();

    yearok = TRUE;                              /* Flag for scan year info */
    incomplete = FALSE;

    clear_fsumm (file);
                                        /* What version is this format? */
    if (isdigit (line[0])) version = line[0] - '0';
    else version = 1;
    file->version = version;
                                                /* This is set only for version 4 */
    psec = 0;

    switch (version)
        {
        case 1:
                                                /* Version 1, HP-1000 and pre-'94 unix */
            n = sscanf(line, fpformat1,
                name,
                &type, 
                &(file->extent_no), 
                &(file->length),
                &(file->expt_no), 
                &pyear, &pday, &(file->corel_vers), &phour, &pmin,
                &syear, &sday, &shour, &smin, 
                file->source,
                file->baseline,
                &(file->quality),
                file->reftape,
                file->remtape,
                &(file->freq_code),
                &(file->mode),
                &(file->no_freq), 
                &(file->amp), 
                &(file->snr), 
                &(file->resid_phas), 
                &(file->sbdelay), 
                &(file->mbdelay), 
                &(file->delay_rate),
                &(file->esdesp), 
                afile,
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->total_phas), 
                &(file->total_rate), 
                &(file->total_mbdelay), 
                &(file->total_sbresid), 
                &(file->ambiguity),
                &(file->parents[0]), 
                &(file->pcals[0]),
                &(file->pcals[1]),
                &(file->pcals[2]),
                &(file->pcals[3]),
                file->root_id);
                                        /* Check that the caller got it right */
            if (type != 2)
                {
                msg ("parse_fsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 20 ) return(-1);
                                        /* Try again for rootcode if bad */
            if ((n < 43) && (islower(*(line+200))) ) 
                sscanf (line+200, "%s", file->root_id);

            if (n < 43) incomplete = TRUE;
                                        /* Decode the fields which could not go */
                                        /* directly into structure */

                                        /* This must be VFIND output */
            if(syear < 80) 
                {
                file->parents[0] = syear;
                syear = 80;
                yearok = FALSE;
                }
                                        /* No seconds resolution in version 1 */
            ssec = 0;
                                        /* Done this way to make sure main sscanf */
                                        /* keeps going even with garbage in this field */
                                        /* Support ARW mod for Bonn numbers > 9999 */
            sscanf(afile,"A%c%hd", &c, &(file->archiv));
            if ((c >= '0') && (c <= '9')) file->archiv += 1000 * (c - '0');
            else file->archiv += 1000 * (10 + c - 'A');

            if (sscanf(name,"<%s",file->fname) != 1) sprintf(file->fname,"00000");
            break;

        case 2:
                                                /* Version 2, unix only, Jan 94 on */
            n = sscanf(line, fpformat2,
                file->root_id,
                &type, 
                &(file->extent_no), 
                &(file->length),
                &(file->expt_no), 
                &pyear, &pday, &(file->corel_vers), &phour, &pmin,
                &syear, &sday, &shour, &smin, &ssec, 
                file->source,
                file->baseline,
                &(file->quality),
                &(file->freq_code),
                &(file->mode),
                &(file->no_freq), 
                &(file->amp), 
                &(file->snr), 
                &(file->resid_phas), 
                file->datatype,
                &(file->sbdelay), 
                &(file->mbdelay), 
                &(file->ambiguity),
                &(file->delay_rate),
                &(file->ref_elev),
                &(file->rem_elev),
                &(file->ref_az),
                &(file->rem_az),
                &(file->u),
                &(file->v),
                &(file->esdesp), 
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq), 
                &(file->total_phas), 
                &(file->total_rate), 
                &(file->total_mbdelay), 
                &(file->total_sbresid), 
                parents);
                                        /* Check that the caller got it right */
            if (type != 2)
                {
                msg ("parse_fsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 19 ) return(-1);

            if (n < 44) 
                {
                incomplete = TRUE;
                msg ("Incomplete parse, number parsed = %d", -3, n);
                }
                                        /* Decode parent extent fields */
            sscanf (parents, "%hd,%hd,%hd,%hd",
                &(file->parents[0]),
                &(file->parents[1]),
                &(file->parents[2]),
                &(file->parents[3]));
            break;

        case 3:
                                                /* Version 3, unix only, March 95 on */
            n = sscanf(line, fpformat3,
                file->root_id,
                &type, 
                &(file->extent_no), 
                &(file->duration),
                &(file->length),
                &(file->offset),
                &(file->expt_no), 
                &pyear, &pday, &(file->corel_vers), &phour, &pmin,
                &syear, &sday, &shour, &smin, &ssec, 
                file->source,
                file->baseline,
                &(file->quality),
                &(file->freq_code),
                &(file->mode),
                &(file->no_freq), 
                &(file->amp), 
                &(file->snr), 
                &(file->resid_phas), 
                file->datatype,
                &(file->sbdelay), 
                &(file->mbdelay), 
                &(file->ambiguity),
                &(file->delay_rate),
                &(file->ref_elev),
                &(file->rem_elev),
                &(file->ref_az),
                &(file->rem_az),
                &(file->u),
                &(file->v),
                &(file->esdesp), 
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq), 
                &(file->total_phas), 
                &(file->total_rate), 
                &(file->total_mbdelay), 
                &(file->total_sbresid), 
                parents);
                                        /* Check that the caller got it right */
            if (type != 2)
                {
                msg ("parse_fsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 21 ) return(-1);

            if (n < 46) 
                {
                incomplete = TRUE;
                msg ("Incomplete parse, number parsed = %d", -3, n);
                }
                                        /* Decode parent extent fields */
            sscanf (parents, "%hd,%hd,%hd,%hd",
                &(file->parents[0]),
                &(file->parents[1]),
                &(file->parents[2]),
                &(file->parents[3]));
            break;

        case 4:
                                                /* Version 4, unix only, November 95 on */
            n = sscanf(line, fpformat4,
                file->root_id,
                &type, 
                &(file->extent_no), 
                &(file->duration),
                &(file->length),
                &(file->offset),
                &(file->expt_no), 
                &pyear, &pday, &(file->corel_vers), &phour, &pmin, &psec,
                &syear, &sday, &shour, &smin, &ssec, 
                &(file->scan_offset), 
                file->source,
                file->baseline,
                &(file->quality),
                &(file->freq_code),
                &(file->mode),
                &(file->no_freq), 
                &(file->lags), 
                &(file->amp), 
                &(file->snr), 
                &(file->resid_phas), 
                &(file->phase_snr), 
                file->datatype,
                &(file->sbdelay), 
                &(file->mbdelay), 
                &(file->ambiguity),
                &(file->delay_rate),
                &(file->ref_elev),
                &(file->rem_elev),
                &(file->ref_az),
                &(file->rem_az),
                &(file->u),
                &(file->v),
                &(file->esdesp), 
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq), 
                &(file->total_phas), 
                &(file->total_rate), 
                &(file->total_mbdelay), 
                &(file->total_sbresid), 
                &(file->srch_cotime), 
                &(file->noloss_cotime), 
                parents);
                                        /* Check that the caller got it right */
            if (type != 2)
                {
                msg ("parse_fsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 23 ) return(-1);

            if (n < 52) 
                {
                incomplete = TRUE;
                msg ("Incomplete parse, number parsed = %d", -3, n);
                }
                                        /* Decode parent extent fields */
            sscanf (parents, "%hd,%hd,%hd,%hd",
                &(file->parents[0]),
                &(file->parents[1]),
                &(file->parents[2]),
                &(file->parents[3]));
            break;

        case 5:
                                                /* Version 5, Mk4 only, Sept 99 on */
            n = sscanf(line, fpformat5,
                file->root_id,
                &type, 
                &(file->extent_no), 
                &(file->duration),
                &(file->length),
                &(file->offset),
                &(file->expt_no), 
                file->scan_id,
                &pyear, &pday, &phour, &pmin, &psec,
                &syear, &sday, &shour, &smin, &ssec, 
                &(file->scan_offset), 
                file->source,
                file->baseline,
                &(file->quality),
                &(file->errcode),
                &(file->freq_code),
                &(file->no_freq), 
                file->polarization,
                &(file->lags), 
                &(file->amp), 
                &(file->snr), 
                &(file->resid_phas), 
                &(file->phase_snr), 
                file->datatype,
                &(file->sbdelay), 
                &(file->mbdelay), 
                &(file->ambiguity),
                &(file->delay_rate),
                &(file->ref_elev),
                &(file->rem_elev),
                &(file->ref_az),
                &(file->rem_az),
                &(file->u),
                &(file->v),
                &(file->esdesp), 
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq), 
                &(file->total_phas), 
                &(file->total_rate), 
                &(file->total_mbdelay), 
                &(file->total_sbresid), 
                &(file->srch_cotime), 
                &(file->noloss_cotime)); 
                                        /* Check that the caller got it right */
            if (type != 2)
                {
                msg ("parse_fsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 24 ) return(-1);

            if (n < 52) 
                {
                incomplete = TRUE;
                msg ("Incomplete parse, number parsed = %d", -3, n);
                }
            break;

        case 6:
                                                /* Version 6, EHT era */
            n = sscanf(line, fpformat6,
                file->root_id,
                &type, 
                &(file->extent_no), 
                &(file->duration),
                &(file->length),
                &(file->offset),
                &(file->expt_no), 
                file->scan_id,
                &pyear, &pday, &phour, &pmin, &psec,
                &syear, &sday, &shour, &smin, &ssec, 
                &(file->scan_offset), 
                file->source,
                file->baseline,
                &(file->quality),
                &(file->errcode),
                &(file->freq_code),
                &(file->no_freq), 
                file->polarization,
                &(file->lags), 
                &(file->amp), 
                &(file->snr), 
                &(file->resid_phas), 
                &(file->phase_snr), 
                file->datatype,
                &(file->sbdelay), 
                &(file->mbdelay), 
                &(file->ambiguity),
                &(file->delay_rate),
                &(file->ref_elev),
                &(file->rem_elev),
                &(file->ref_az),
                &(file->rem_az),
                &(file->u),
                &(file->v),
                &(file->esdesp), 
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq), 
                &(file->total_phas), 
                &(file->total_rate), 
                &(file->total_mbdelay), 
                &(file->total_sbresid), 
                &(file->srch_cotime), 
                &(file->noloss_cotime)); 
                                        /* Check that the caller got it right */
            if (type != 2)
                {
                msg ("parse_fsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id file */
            if(n < 24 ) return(-1);

            if (n < 52) 
                {
                incomplete = TRUE;
                msg ("Incomplete parse, number parsed = %d", -3, n);
                }
            break;

        default:
            msg ("Unsupported A-file format version number '%d'", 2, version);
            return (-1);
        }
                                        /* These fields independent of version # */
    if (syear < 70) syear += 2000;
    if (pyear < 70) pyear += 2000;
    file->time_tag = time_to_int (syear, sday, shour, smin, ssec);
    file->procdate = time_to_int (pyear, pday, phour, pmin, psec);

    if (incomplete) return (2);
    else if (yearok) return(0);
    else return(1);
    }
