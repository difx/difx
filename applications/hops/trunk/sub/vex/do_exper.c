/************************************************************************/
/*                                                                      */
/* Fills in the experiment parts of the scan structure, from an exper   */
/* def                                                                  */
/*                                                                      */
/*      Inputs:         experdef        Pointer to the exper def        */
/*                                                                      */
/*      Output:         scan            Pointer to the scan struct      */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 13 January 1998 by CJL                                       */
/* Rewritten for Haystack parser 28 October 1998 by CJL                 */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
do_exper (struct def *experdef,
          struct scan_struct *scan)
    {
    int st;
    char *str;
    struct param_val p_val;
    extern struct statement *stlist;
    extern int ovex_ver;
                                        /* Loop over statements in def */
    for (st=experdef->start+1; st<experdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "EXPER", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }
                                        /* Only interested in 3 items */
        if (strcmp (p_val.name, "exper_num") == 0)
            scan->exper_num = p_val.dval[0].data.intval;
        else if (strcmp (p_val.name, "exper_name") == 0)
            strcpy (scan->exper_name, p_val.dval[0].data.strval);
        else if (strcmp (p_val.name, "target_correlator") == 0)
            strcpy (scan->correlator, p_val.dval[0].data.strval);
        }

    return (0);
    }
