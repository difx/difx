/************************************************************************/
/*                                                                      */
/* Fills in the fields structure array with identified, valid A-file    */
/* fields.  These fields will then be written to the output later on    */
/*                                                                      */
/*      Inputs:         argc, argv              Contains user fields    */
/*                                                                      */
/*      Output:         fields                  Filled in struct array  */
/*                      return value            # fields parsed, <0=err */
/*                                                                      */
/* Created 4 April 1995 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "adump.h"
#include "mk4_util.h"

struct flist type3_fields[] = {
    { "experiment",       T_EXPT_NO,      "%4d"    },
    { "type",             T_TYPE,         "%d"     },
    { "scanyear",         T_SCANYEAR,     "%2d"    },
    { "timetag",          T_TIMETAG,      "%s"     },
    { "source",           T_SOURCE,       "%-8s"   },
    { "freq_code",        T_FREQ_CODE,    "%c"     },
    { "mode",             T_MODE,         "%c"     },
    { "triangle",         T_TRIANGLE,     "%s"     },
    { "roots",            T_ROOT,         "%-20s"  },
    { "extents",          T_EXTENT,       "%-11s"  },
    { "lengths",          T_LENGTH,       "%-14s"  },
    { "duration",         T_DURATION,     "%4d"    },
    { "offset",           T_OFFSET,       "%4d"    },
    { "scan_quality",     T_SQUALITY,     "%c"     },
    { "data_quality",     T_DQUALITY,     "%c"     },
    { "esdesp",           T_ESDESP,       "%06d"   },
    { "bis_amp",          T_BISAMP,       "%9.2f"  },
    { "bis_snr",          T_BISSNR,       "%7.2f"  },
    { "bis_phase",        T_BISPHASE,     "%5.1f"  },
    { "datatype",         T_DATATYPE,     "%s "    },
    { "csbdelay",         T_CSBDELAY,     "%6.3f"  },
    { "cmbdelay",         T_CMBDELAY,     "%8.5f"  },
    { "ambiguity",        T_AMBIGUITY,    "%6.4f"  },
    { "cdrate",           T_CDRATE,       "%7.2f"  },
    { "elevations",       T_ELEVATION,    "%-14s"  },
    { "azimuths",         T_AZIMUTH,      "%-11s"  },
    { "epoch",            T_EPOCH,        "%s"     },
    { "ref_frequency",    T_REF_FREQ,     "%8.2f"  },
    { "scanday",          T_SCANDAY,      "%12.7f" },
    { "epochday",         T_EPOCHDAY,     "%12.7f" },
    { "\0",               0,              "\0"     }
};

int
parse_tfields (int argc,
               char **argv,
               struct flist fields[])
    {
    int i, fieldno, nfields;
    char name[20], string[20];
    extern int optind;
                                        /* Initialize fields array */
    for (i=0; i<MAXFIELDS; i++) fields[i].id = 0;
                                        /* Find out how many fields to search */
    i = 0;
    while (type3_fields[i].id != 0) i++;
    nfields = i;
                                        /* Loop over user-specified fields */
    fieldno = 0;
    for (; optind<argc; optind++)
        {
        strcpy (string, argv[optind]);
                                        /* First look for exact match */
        i = 0;
        while (type3_fields[i].id != 0)
            {
            strcpy (name, type3_fields[i].name);
            if (strcmp (string, name) == 0) break;
            i++;
            }
                                        /* If no match, look for minmatch */
        if (i == nfields)
            {
            i = 0;
            while (type3_fields[i].id != 0)
                {
                strcpy (name, type3_fields[i].name);
                if (strncmp (string, name, strlen(string)) == 0) break;
                i++;
                }
            }
                                        /* No match, error */
        if (i == nfields)
            {
            msg ("Unrecognized field '%s', abort", 3, string);
            return (-1);
            }
                                        /* We have a match, copy it into */
                                        /* next slot of fields array */
        strcpy (fields[fieldno].name, name);
        fields[fieldno].id = type3_fields[i].id;
        strcpy (fields[fieldno].format, type3_fields[i].format);
        fieldno++;
        }

    return (fieldno);
    }
