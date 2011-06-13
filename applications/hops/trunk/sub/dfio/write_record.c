/************************************************************************/
/*                                                                      */
/* Writes the specified type of record out to the file pointed to by fp */
/* This does the version control and byte-swapping stuff automatically, */
/* providing a simple interface for the application programmer.         */
/*                                                                      */
/*      Inputs:         record          pointer to record : WARNING -   */
/*                                      may be modified by bytflp       */
/*                      fp              open file pointer               */
/*                                                                      */
/*      Output:         bytes           Number of bytes written         */
/*                      return value    0 = OK, else error              */
/*                                                                      */
/* Created March 12 1998 by CJL                                         */
/* added 309's                             rjc  2006.2.6                */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mk4_records.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define TRUE 1
#define FALSE 0

int
write_record (char *record,
              FILE *fp,
              int *bytes)
    {
    int type, alloc_overlay;
    char *overlay;
    struct type_100 *t100; struct type_101 *t101;
    struct type_120 *t120;
    struct type_200 *t200; struct type_201 *t201;
    struct type_202 *t202; struct type_203 *t203; struct type_204 *t204;
    struct type_205 *t205; struct type_206 *t206;
    struct type_207 *t207; struct type_208 *t208; struct type_210 *t210;
    struct type_212 *t212; struct type_230 *t230;
    struct type_220 *t220; struct type_221 *t221;
    struct type_300 *t300; struct type_301 *t301;
    struct type_302 *t302; struct type_303 *t303;
    struct type_304 *t304; struct type_305 *t305;
    struct type_306 *t306; struct type_307 *t307;
    struct type_308 *t308; struct type_309 *t309;

    msg ("record header in write_record '%c%c%c%c%c'", -2,
        record[0],record[1],record[2],record[3], record[4]);
                                        /* What record type is this? */
    if (sscanf (record, "%3d", &type) != 1)
        {
        msg ("Unintelligible record header '%c%c%c%c%c' in write_record()", 2,
            record[0],record[1],record[2],record[3], record[4]);
        return (-1);
        }
                                        /* Copy to overlay according to */
                                        /* type */
    alloc_overlay = FALSE;
    switch (type)
        {
                                        /* Type 000 is special case */
        case 0:
            overlay = record;
            *bytes = sizeof (struct type_000);
            break;
                                        /* All others require version/copy */
                                        /* operation */
        case 100:
            t100 = (struct type_100 *)record;
            *bytes = copy_100 (t100, &overlay);
            break;
        case 101:
            t101 = (struct type_101 *)record;
            *bytes = copy_101 (t101, &overlay);
            break;
        case 120:
            t120 = (struct type_120 *)record;
            *bytes = copy_120 (t120, &overlay);
            break;
        case 200:
            t200 = (struct type_200 *)record;
            *bytes = copy_200 (t200, &overlay);
            break;
        case 201:
            t201 = (struct type_201 *)record;
            *bytes = copy_201 (t201, &overlay);
            break;
        case 202:
            t202 = (struct type_202 *)record;
            *bytes = copy_202 (t202, &overlay);
            break;
        case 203:
            t203 = (struct type_203 *)record;
            *bytes = copy_203 (t203, &overlay);
            break;
        case 204:
            t204 = (struct type_204 *)record;
            *bytes = copy_204 (t204, &overlay);
            break;
        case 205:
            t205 = (struct type_205 *)record;
            *bytes = copy_205 (t205, &overlay);
            break;
        case 206:
            t206 = (struct type_206 *)record;
            *bytes = copy_206 (t206, &overlay);
            break;
        case 207:
            t207 = (struct type_207 *)record;
            *bytes = copy_207 (t207, &overlay);
            break;
        case 208:
            t208 = (struct type_208 *)record;
            *bytes = copy_208 (t208, &overlay);
            break;
        case 210:
            t210 = (struct type_210 *)record;
            *bytes = copy_210 (t210, &overlay);
            break;
        case 212:
            t212 = (struct type_212 *)record;
            *bytes = copy_212 (t212, &overlay);
            break;
        case 220:
            t220 = (struct type_220 *)record;
            *bytes = copy_220 (t220, &overlay);
            break;
        case 221:
            t221 = (struct type_221 *)record;
            *bytes = copy_221 (t221, &overlay, &alloc_overlay);
            break;
        case 230:
            t230 = (struct type_230 *)record;
            *bytes = copy_230 (t230, &overlay);
            break;
        case 300:
            t300 = (struct type_300 *)record;
            *bytes = copy_300 (t300, &overlay);
            break;
        case 301:
            t301 = (struct type_301 *)record;
            *bytes = copy_301 (t301, &overlay);
            break;
        case 302:
            t302 = (struct type_302 *)record;
            *bytes = copy_302 (t302, &overlay);
            break;
        case 303:
            t303 = (struct type_303 *)record;
            *bytes = copy_303 (t303, &overlay);
            break;
        case 304:
            t304 = (struct type_304 *)record;
            *bytes = copy_304 (t304, &overlay);
            break;
        case 305:
            t305 = (struct type_305 *)record;
            *bytes = copy_305 (t305, &overlay);
            break;
        case 306:
            t306 = (struct type_306 *)record;
            *bytes = copy_306 (t306, &overlay);
            break;
        case 307:
            t307 = (struct type_307 *)record;
            *bytes = copy_307 (t307, &overlay);
            break;
        case 308:
            t308 = (struct type_308 *)record;
            *bytes = copy_308 (t308, &overlay);
            break;
        case 309:
            t309 = (struct type_309 *)record;
            *bytes = copy_309 (t309, &overlay);
            break;
        default:
            msg ("Unknown record type '%d' requested in write_record()", 2, type);
            return (-1);
        }
                                        /* Check integrity of copy, then */
                                        /* write out to disk */
    if (*bytes < 0)
        {
        msg ("type_%03d overlay failure", 2, type);
        return (-1);
        }
    if (fwrite (overlay, *bytes, 1, fp) != 1)
        {
        msg ("type_%03d write failure", 2, type);
        return (-1);
        }
    if (alloc_overlay) free (overlay);

    return (0);
        
    }

