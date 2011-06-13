/************************************************************************/
/*                                                                      */
/* This routine takes a string specifying the units, and converts it    */
/* to a generic unit type (length, time etc), plus a multiplicative     */
/* factor with which to convert a numerical value to MKS units.         */
/*                                                                      */
/*      Inputs:         unitstring      Input string                    */
/*                      units           expected (defines in vex.h)     */
/*                                                                      */
/*      Outputs:        factor          Multiplier for value            */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 15 October 1998 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

#define PI          3.14159265358979323846

                                        /* This define tries to match the */
                                        /* units string with an entry in the */
                                        /* specified static struct.  If it */
                                        /* succeeds, it sets 'found' to the */
                                        /* supplied type identifier, and */
                                        /* sets 'out' to the associated factor */

#define MATCH_UNITS(found, units, table, type, out)\
                                { int i; if(!found)\
                                    for (i=0; table[i].str !=NULL; i++)\
                                      if(strcmp(units,table[i].str)==0) {\
                                        out=table[i].factor;\
                                        found=type; } }


int
parse_units (char *unitstring,
             int units,
             double *factor)
    {
    char num[32], denom[32], *slash;
    int num_type, denom_type, type;
    double num_factor, denom_factor;
                                        /* Define the static structs */
    static struct 
        {
        char *str;
        double factor;
        } time[] = 
          { { "psec", 1e-12 },
            { "nsec", 1e-9 },
            { "usec", 1e-6 },
            { "msec", 1e-3 },
            { "sec" , 1.0 },
            { "min" , 60.0 },
            { "hr"  , 60.0*60.0 },
            { "day" , 60.0*60.0*24.0 },
            { "yr"  , 60.0*60.0*24.0*365.2425 },
            { NULL  , 0.0 } };

    static struct 
        {
        char *str;
        double factor;
        } freq[] = 
          { { "mHz", 1e-3 },
            { "Hz" , 1.0 },
            { "kHz", 1e3 },
            { "MHz", 1e6 },
            { "GHz", 1e9 },
            { NULL  , 0.0 } };

    static struct 
        {
        char *str;
        double factor;
        } sample_rate[] = 
          { { "ks/sec", 1e3 },
            { "Ms/sec", 1e6 },
            { NULL  , 0.0 } };

    static struct 
        {
        char *str;
        double factor;
        } length[] = 
          { { "um", 1e-6 },
            { "mm", 1e-3 },
            { "cm", 1e-2 },
            { "m" , 1.0 },
            { "km", 1e3 },
            { "in", 2.54*1e-2 },
            { "ft", 12*2.54*1e-2 },
            { NULL  , 0.0 } };

    static struct 
        {
        char *str;
        double factor;
        } angle[] = 
          { { "mdeg", 1e-3*PI/180.0 },
            { "deg" , PI/180.0 },
            { "amin", PI/(60.0*180.0) },
            { "asec", PI/(60.0*60.0*180.0) },
            { "rad" , 1.0 },
            { NULL  , 0.0 } };

    static struct 
        {
        char *str;
        double factor;
        } flux[] = 
          { { "mJy", 1e-29 },
            { "Jy", 1e-26 },
            { NULL  , 0.0 } };

    static struct 
        {
        char *str;
        double factor;
        } bit_density[] = 
          { {  "bpi", 1.0 },
            { "kbpi", 1e3 },
            { NULL  , 0.0 } };

                                        /* Handle null pointer */
    if (unitstring == NULL)
        {
        type = UNIT_NONE;
        *factor = 1.0;
        return (0);
        }
                                        /* Handle null string */
    if (strlen (unitstring) == 0)
        {
        type = UNIT_NONE;
        *factor = 1.0;
        return (0);
        }
                                        /* Defuse array overflows */
    if (strlen (unitstring) >= 32)
        {
        msg ("Units string '%s' too long", 2, unitstring);
        return (1);
        }
                                        /* Simple units without a slash */
    type = FALSE;
    MATCH_UNITS (type, unitstring, time, UNIT_TIME, *factor);
    MATCH_UNITS (type, unitstring, freq, UNIT_FREQ, *factor);
    MATCH_UNITS (type, unitstring, sample_rate, UNIT_SAMPLERATE, *factor);
    MATCH_UNITS (type, unitstring, length, UNIT_LENGTH, *factor);
    MATCH_UNITS (type, unitstring, angle, UNIT_ANGLE, *factor);
    MATCH_UNITS (type, unitstring, flux, UNIT_FLUX, *factor);
    MATCH_UNITS (type, unitstring, bit_density, UNIT_BITDENSITY, *factor);
    if (type) return (0);
                                        /* Not found, better have a slash */
    slash = strchr (unitstring, '/');
    if (slash == NULL) 
        {
        msg ("Invalid units string '%s'", 2, unitstring);
        return (1);
        }
                                        /* Separate out numerator and */
                                        /* denominator, and match them */
    strncpy (num, unitstring, (int)(slash - unitstring));
    num[slash - unitstring] = '\0';
    strcpy (denom, slash+1);
    num_type = denom_type = FALSE;
    MATCH_UNITS (num_type, num, angle, UNIT_ANGLE, num_factor);
    MATCH_UNITS (num_type, num, length, UNIT_LENGTH, num_factor);
    MATCH_UNITS (num_type, num, time, UNIT_TIME, num_factor);
    MATCH_UNITS (denom_type, denom, time, UNIT_TIME, denom_factor);
                                        /* Assign compound types */
    if (num_type && denom_type) 
        {
        *factor = num_factor / denom_factor;
        if ((num_type == UNIT_ANGLE) && (denom_type == UNIT_TIME)) 
            type = UNIT_ANGLE_TIME;
        else if ((num_type == UNIT_LENGTH) && (denom_type == UNIT_TIME)) 
            type = UNIT_LENGTH_TIME;
        else if (num_type == denom_type) type = UNIT_NONE;
        return (0);
        }

                                        /* No match */
    msg ("Invalid units string '%s'", 2, unitstring);
    return (1);
    }
