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
/*                                      +2 indicates incomplete parse   */
/*                                                                      */
/* Created 29 July 1994 by CJL                                          */
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

char *triformat2 = "%*d %hd %d %d %d-%2d%2d%2d %s %c%c %s %s %hd,%hd,%hd\
 %hd,%hd,%hd %c %c %d %f %f %f %f %f %f %f %f,%f,%f %f,%f,%f %2hd%2hd %lf";

char *triformat3 = "%*d %hd %d %d %d-%2d%2d%2d %s %c%c %s %s %hd,%hd,%hd\
 %hd,%hd,%hd %hd %hd %c %c %d %f %f %f %s %f %f %f %f %f,%f,%f %f,%f,%f %2hd%2hd %lf";

char *triformat4 = "%*d %hd %d %d %d-%2d%2d%2d %hd %s %c%c %d %s %s %hd,%hd,%hd\
 %hd,%hd,%hd %hd %hd %c %c %d %f %f %f %s %f %f %f %f %f,%f,%f %f,%f,%f %2hd%2hd %lf %hd";

char *triformat5 = "%*d %hd %d %s %d %d-%2d%2d%2d %hd %s %c%c %d %s %s %hd,%hd,%hd\
 %hd,%hd,%hd %hd %hd %c %c %d %f %f %f %s %f %f %f %f %f,%f,%f %f,%f,%f %2hd%2hd %lf %hd";

int
parse_tsumm(char *line, trianglesum *file)
    {
    int n, syear, sday, shour, smin, ssec, type;
    int version, incomplete;
    char roots[50];
    char *ptr;

    clear_tsumm (file);
                                        /* What version is this format? */
    if (isdigit (line[0])) version = line[0] - '0';
    else version = 1;
    file->version = version;
    incomplete = FALSE;

    switch (version)
        {
        case 2:
                                                /* Version 2, unix only, Jan 94 on */
            n = sscanf(line, triformat2,
                &(file->expt_no), 
                &type, 
                &syear, &sday, &shour, &smin, &ssec, 
                file->source,
                &(file->freq_code),
                &(file->mode),
                file->triangle,
                roots,
                &(file->extent_no[0]), 
                &(file->extent_no[1]),
                &(file->extent_no[2]),
                &(file->length[0]), 
                &(file->length[1]),
                &(file->length[2]),
                &(file->scan_quality),
                &(file->data_quality),
                &(file->esdesp), 
                &(file->bis_amp), 
                &(file->bis_snr), 
                &(file->bis_phas), 
                &(file->csbdelay), 
                &(file->cmbdelay), 
                &(file->ambiguity),
                &(file->cdelay_rate),
                &(file->elevation[0]),
                &(file->elevation[1]),
                &(file->elevation[2]),
                &(file->azimuth[0]),
                &(file->azimuth[1]),
                &(file->azimuth[2]),
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq)); 
                                        /* Check that the caller got it right */
            if (type != 3)
                {
                msg ("parse_tsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id files */
            if(n < 15 ) return(-1);
                                        /* Didn't get everything */
            if (n < 37) incomplete = TRUE;
                                        /* Extract the root codes */
            ptr = strtok (roots, ",");
            if (ptr != NULL) strcpy (file->root_id[0], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[1], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[2], ptr);
            if (strlen (file->root_id[0]) != 6)
                {
                msg ("Corrupt root code(s) in parse_tsumm()", 2);
                return (-1);
                }
                                        /* Copy last rootcode if blank */
            if (strlen (file->root_id[1]) != 6)
                strcpy (file->root_id[1], file->root_id[0]);
            if (strlen (file->root_id[2]) != 6)
                strcpy (file->root_id[2], file->root_id[1]);

            break;

        case 3:
                                                /* Version 3, unix only, March 95 on */
            n = sscanf(line, triformat3,
                &(file->expt_no), 
                &type, 
                &syear, &sday, &shour, &smin, &ssec, 
                file->source,
                &(file->freq_code),
                &(file->mode),
                file->triangle,
                roots,
                &(file->extent_no[0]), 
                &(file->extent_no[1]),
                &(file->extent_no[2]),
                &(file->length[0]), 
                &(file->length[1]),
                &(file->length[2]),
                &file->duration,
                &file->offset,
                &(file->scan_quality),
                &(file->data_quality),
                &(file->esdesp), 
                &(file->bis_amp), 
                &(file->bis_snr), 
                &(file->bis_phas), 
                file->datatype,
                &(file->csbdelay), 
                &(file->cmbdelay), 
                &(file->ambiguity),
                &(file->cdelay_rate),
                &(file->elevation[0]),
                &(file->elevation[1]),
                &(file->elevation[2]),
                &(file->azimuth[0]),
                &(file->azimuth[1]),
                &(file->azimuth[2]),
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq)); 
                                        /* Check that the caller got it right */
            if (type != 3)
                {
                msg ("parse_tsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id files */
            if(n < 15 ) return(-1);
                                        /* Didn't get everything */
            if (n < 40) incomplete = TRUE;
                                        /* Extract the root codes */
            ptr = strtok (roots, ",");
            if (ptr != NULL) strcpy (file->root_id[0], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[1], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[2], ptr);
            if (strlen (file->root_id[0]) != 6)
                {
                msg ("Corrupt root code(s) in parse_tsumm()", 2);
                return (-1);
                }
                                        /* Copy last rootcode if blank */
            if (strlen (file->root_id[1]) != 6)
                strcpy (file->root_id[1], file->root_id[0]);
            if (strlen (file->root_id[2]) != 6)
                strcpy (file->root_id[2], file->root_id[1]);

            break;

        case 4:
                                                /* Version 4, unix only, November 95 on */
            n = sscanf(line, triformat4,
                &(file->expt_no), 
                &type, 
                &syear, &sday, &shour, &smin, &ssec, 
                &(file->scan_offset), 
                file->source,
                &(file->freq_code),
                &(file->mode),
                &(file->lags),
                file->triangle,
                roots,
                &(file->extent_no[0]), 
                &(file->extent_no[1]),
                &(file->extent_no[2]),
                &(file->length[0]), 
                &(file->length[1]),
                &(file->length[2]),
                &file->duration,
                &file->offset,
                &(file->scan_quality),
                &(file->data_quality),
                &(file->esdesp), 
                &(file->bis_amp), 
                &(file->bis_snr), 
                &(file->bis_phas), 
                file->datatype,
                &(file->csbdelay), 
                &(file->cmbdelay), 
                &(file->ambiguity),
                &(file->cdelay_rate),
                &(file->elevation[0]),
                &(file->elevation[1]),
                &(file->elevation[2]),
                &(file->azimuth[0]),
                &(file->azimuth[1]),
                &(file->azimuth[2]),
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq),
                &(file->cotime)); 
                                        /* Check that the caller got it right */
            if (type != 3)
                {
                msg ("parse_tsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id files */
            if(n < 17 ) return(-1);
                                        /* Didn't get everything */
            if (n < 43) incomplete = TRUE;
                                        /* Extract the root codes */
            ptr = strtok (roots, ",");
            if (ptr != NULL) strcpy (file->root_id[0], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[1], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[2], ptr);
            if (strlen (file->root_id[0]) != 6)
                {
                msg ("Corrupt root code(s) in parse_tsumm()", 2);
                return (-1);
                }
                                        /* Copy last rootcode if blank */
            if (strlen (file->root_id[1]) != 6)
                strcpy (file->root_id[1], file->root_id[0]);
            if (strlen (file->root_id[2]) != 6)
                strcpy (file->root_id[2], file->root_id[1]);

            break;

        case 5:
                                                /* Version 5, Mk4 only, Sep 99 on */
            n = sscanf(line, triformat5,
                &(file->expt_no), 
                &type, 
                file->scan_id,
                &syear, &sday, &shour, &smin, &ssec, 
                &(file->scan_offset), 
                file->source,
                &(file->freq_code),
                &(file->mode),
                &(file->lags),
                file->triangle,
                roots,
                &(file->extent_no[0]), 
                &(file->extent_no[1]),
                &(file->extent_no[2]),
                &(file->length[0]), 
                &(file->length[1]),
                &(file->length[2]),
                &file->duration,
                &file->offset,
                &(file->scan_quality),
                &(file->data_quality),
                &(file->esdesp), 
                &(file->bis_amp), 
                &(file->bis_snr), 
                &(file->bis_phas), 
                file->datatype,
                &(file->csbdelay), 
                &(file->cmbdelay), 
                &(file->ambiguity),
                &(file->cdelay_rate),
                &(file->elevation[0]),
                &(file->elevation[1]),
                &(file->elevation[2]),
                &(file->azimuth[0]),
                &(file->azimuth[1]),
                &(file->azimuth[2]),
                &(file->epoch[0]), 
                &(file->epoch[1]), 
                &(file->ref_freq),
                &(file->cotime)); 
                                        /* Check that the caller got it right */
            if (type != 3)
                {
                msg ("parse_tsumm passed line of wrong type '%d'", 2, type);
                return (-1);
                }
                                        /* Not even enough to id files */
            if(n < 18 ) return(-1);
                                        /* Didn't get everything */
            if (n < 44) incomplete = TRUE;
                                        /* Extract the root codes */
            ptr = strtok (roots, ",");
            if (ptr != NULL) strcpy (file->root_id[0], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[1], ptr);
            ptr = strtok (NULL, ",");
            if (ptr != NULL) strcpy (file->root_id[2], ptr);
            if (strlen (file->root_id[0]) != 6)
                {
                msg ("Corrupt root code(s) in parse_tsumm()", 2);
                return (-1);
                }
                                        /* Copy last rootcode if blank */
            if (strlen (file->root_id[1]) != 6)
                strcpy (file->root_id[1], file->root_id[0]);
            if (strlen (file->root_id[2]) != 6)
                strcpy (file->root_id[2], file->root_id[1]);

            break;

        default:
            msg ("Unsupported triangle A-file format version number '%d'", 2, version);
            return (-1);
        }
                                        /* This field independent of version # */
    if (syear < 70) syear += 2000;
    file->time_tag = time_to_int (syear, sday, shour, smin, ssec);

    if (incomplete) return (2);
    else return (0);
    }
