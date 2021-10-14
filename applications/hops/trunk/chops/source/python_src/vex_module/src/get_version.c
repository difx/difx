/************************************************************************/
/*                                                                      */
/* Simple routine responsible for figuring out the version number of    */
/* the VEX file being parsed.  One call is needed for each "flavour" of */
/* VEX, and the result is placed in the return value.                   */
/*                                                                      */
/*      Inputs:         type            Flavour of vex                  */
/*                                                                      */
/*      Output:         return value    version number, V_BAD if error  */
/*                                                                      */
/* Created 21 January 1999 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
get_version (int type)
    {
    int i;
    char *str, *revstr, blockname[MAX_NAMESIZE];
    struct block *blk;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
                                        /* Find $?VEX_REV for this flavour */
    if (type == OVEX) strcpy (blockname, "OVEX_REV");
    else if (type == CVEX) strcpy (blockname, "CVEX_REV");
    else if (type == IVEX) strcpy (blockname, "IVEX_REV");
    else if (type == SVEX) strcpy (blockname, "SVEX_REV");
    else if (type == EVEX) strcpy (blockname, "EVEX_REV");
    else if (type == LVEX) strcpy (blockname, "LVEX_REV");
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, blockname) == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $%s block", 3, blockname);
        return (V_BAD);
        }
    blk = blist + i;
                                        /* Parse for revision level */
    str = stlist[blk->stno + 1].str;
    if (parse_pval (str, blockname, type | V_ALL, &p_val) != 0)
        {
        msg ("Error parsing statement '%s'", 2, str);
        print_location (blk->stno + 1);
        return (V_BAD);
        }
    if ISNAME ("rev")
        {
        revstr = p_val.dval[0].data.strval;
        if (strcmp (revstr, "1.0") == 0) return (V_1_0);
        else if (strcmp (revstr, "1.5") == 0) return (V_1_5);
        else
            {
            msg ("Unsupported %s version number '%s'", 2, blockname, revstr);
            return (V_BAD);
            }
        }
    else
        {
        msg ("Unrecognized statement in %s", 2, blockname);
        return (V_BAD);
        }

    }

