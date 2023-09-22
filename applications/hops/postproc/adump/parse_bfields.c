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
/* Created 2 March 1995 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "adump.h"
#include "mk4_util.h"

struct flist type2_fields[] = {
    { "rootcode",         ROOTCODE,       "%s"     },
    { "type",             TYPE,           "%d"     },
    { "extent",           EXTENT,         "%3d"    },
    { "duration",         DURATION,       "%4d"    },
    { "length",           LENGTH,         "%4d"    },
    { "offset",           OFFSET,         "%4d"    },
    { "experiment",       EXPT_NO,        "%4d"    },
    { "procdate",         PROCDATE,       "%s"     },
    { "scanyear",         SCANYEAR,       "%2d"    },
    { "timetag",          TIMETAG,        "%s"     },
    { "source",           SOURCE,         "%-8s"   },
    { "baseline",         BASELINE,       "%s"     },
    { "quality",          QUALITY,        "%c"     },
    { "freq_code",        FREQ_CODE,      "%c"     },
    { "mode",             MODE,           "%c"     },
    { "nfreq",            NFREQ,          "%d"     },
    { "polarization",     POL,            "%2s"    },
    { "amplitude",        AMP,            "%6.2f"  },
    { "snr",              SNR,            "%#5.4g" },
    { "phase",            PHASE,          "%5.1f"  },
    { "datatype",         DATATYPE,       "%s"     },
    { "sbdelay",          SBDELAY,        "%6.3f"  },
    { "mbdelay",          MBDELAY,        "%8.5f"  },
    { "ambiguity",        AMBIGUITY,      "%6.4f"  },
    { "drate",            DRATE,          "%7.2f"  },
    { "ref_elevation",    REF_ELEVATION,  "%4.1f"  },
    { "rem_elevation",    REM_ELEVATION,  "%4.1f"  },
    { "ref_azimuth",      REF_AZIMUTH,    "%5.1f"  },
    { "rem_azimuth",      REM_AZIMUTH,    "%5.1f"  },
    { "u",                U,              "%7.4f"  },
    { "v",                V,              "%7.4f"  },
    { "esdesp",           ESDESP,         "%06d"   },
    { "epoch",            EPOCH,          "%s"     },
    { "ref_frequency",    REF_FREQ,       "%8.2f"  },
    { "total_phase",      TOT_PHASE,      "%5.1f"  },
    { "total_drate",      TOT_DRATE,      "%11.8f" },
    { "total_mbdelay",    TOT_MBDELAY,    "%13.6f" },
    { "total_mbdsbd",     TOT_MBDSBD,     "%5.3f"  },
    { "scotime",          SCOTIME,        "%3d"    },
    { "ncotime",          NCOTIME,        "%3d"    },
    { "parents",          PARENTS,        "%s"     },
    { "procday",          PROCDAY,        "%12.7f" },
    { "scanday",          SCANDAY,        "%12.7f" },
    { "epochday",         EPOCHDAY,       "%12.7f" },
    { "\0",               0,              "\0"     }
};

int
parse_bfields (int argc,
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
    while (type2_fields[i].id != 0) i++;
    nfields = i;
                                        /* Loop over user-specified fields */
    fieldno = 0;
    for (; optind<argc; optind++)
        {
        strcpy (string, argv[optind]);
                                        /* First look for exact match */
        i = 0;
        while (type2_fields[i].id != 0)
            {
            strcpy (name, type2_fields[i].name);
            if (strcmp (string, name) == 0) break;
            i++;
            }
                                        /* If no match, look for minmatch */
        if (i == nfields)
            {
            i = 0;
            while (type2_fields[i].id != 0)
                {
                strcpy (name, type2_fields[i].name);
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
        fields[fieldno].id = type2_fields[i].id;
        strcpy (fields[fieldno].format, type2_fields[i].format);
        fieldno++;
        }

    return (fieldno);
    }
