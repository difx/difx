/************************************************************************/
/*                                                                      */
/* This routine parses the svex portion of the input file, which has    */
/* already been low-level parsed into memory by the parse_vexfile()     */
/* routine.  It fills an svex_struct struct according to the key to the */
/* $SU_CONFIG section that is passed to it.                             */
/*                                                                      */
/*      Inputs:         key             A def in the $SU_CONFIG block   */
/*                                                                      */
/*      Output:         return value    Pointer to svex_struct          */
/*                                      (null on error)                 */
/*                                                                      */
/* Created 14 February 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int svex_del_list[2048];

struct svex_struct *
svex_info (char *key)
    {
    int i, j, st, incomplete, nchar, retain, nctot;
    struct def *sdef;
    struct ref ref;
    struct block *blk;
    char *str, *ptr;
    static struct svex_struct svex;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern char svex_src[];
    extern int do_output, svex_ver;
                                        /* Initialize */
                                        /* Get the IVEX version number */
    if ((svex_ver = get_version (SVEX)) == V_BAD)
        {
        msg ("Failed to get SVEX version number", 2);
        return (NULL);
        }
                                        /* Initialize the delete list */
    if (nstmt > 2048)
        {
        msg ("SVEX source file too big, more than 2048 statements", 2);
        return (NULL);
        }
    for (i=0; i<nstmt; i++) svex_del_list[i] = TRUE;
                                        /* Retain all SVEX $BLOCK statements */
    for (i=0; i<nblock; i++) 
        {
        retain = FALSE;
        if (strcmp (blist[i].name, "SU_CONFIG") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "SU_CHAN_OUT") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "PCM_CONFIG") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "PCM_TABLES") == 0) retain = TRUE;
        if (retain) svex_del_list[blist[i].stno] = FALSE;
                                        /* Retain IVEX_REV section */
        if (strcmp (blist[i].name, "SVEX_REV") == 0)
            for (j=blist[i].stno; j<=blist[i].end; j++)
                svex_del_list[j] = FALSE;
        }
                                        /* Locate SU_CONFIG block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "SU_CONFIG") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $SU_CONFIG block", 3);
        return (NULL);
        }
                                        /* Check for null key and root file */
    blk = blist + i;
    if (key[0] == '\0')
        {
        if (blk->ndef > 1)
            {
            msg ("Multiple keys present in svex file, but none specified", 2);
            return (NULL);
            }
        sdef = blk->deflist;
        }
                                        /* Retrieve key def */
    else
        {
        for (i=0; i<blk->ndef; i++)
            if (strcmp (blk->deflist[i].name, key) == 0) break;
        if (i == blk->ndef)
            {
            msg ("Could not find key '%s' in $SU_CONFIG block", 3, key);
            return (NULL);
            }
        sdef = blk->deflist + i;
        }
                                        /* Undelete the def */
    for (st=sdef->start; st<=sdef->end; st++) svex_del_list[st] = FALSE;
                                        /* Loop over statements in sdef */
    for (st=sdef->start+1; st<sdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Only "ref" statements allowed */
                                        /* may be mixed */
                                        /* Generic ref parsing */
        if (parse_ref (st, &ref) != 0)
            {
            msg ("Bad ref statement in SVEX def '%s'", 2, key);
            return (NULL);
            }
                                        /* SU channel out assignments */
        if (strcmp (ref.blockname, "SU_CHAN_OUT") == 0)
            {
            if (get_su_chan_out (ref.keyword, &svex) != 0)
                {
                msg ("Error parsing SU_CHAN_OUT def '%s'", 2, ref.keyword);
                return (NULL);
                }
            }
                                        /* PCM configuration */
        else if (strcmp (ref.blockname, "PCM_CONFIG") == 0)
            {
            if (get_pcm_config (ref.keyword, &svex) != 0)
                {
                msg ("Error parsing PCM_CONFIG def '%s'", 2, ref.keyword);
                return (NULL);
                }
            }
        else
            {
            msg ("Reference to inappropriate block '%s' in $SU_CONFIG",
                        2, ref.keyword);
            print_location(st);
            return (NULL);
            }
        }
                                        /* Must meet minimum requirements */
    incomplete = FALSE;
    if (incomplete)
        {
        msg ("Insufficient information supplied in SVEX def '%s'", 2, key);
        return (NULL);
        }
                                        /* Create stripped ascii output */
    if (do_output)
        {
        ptr = svex_src;
        nctot = 0;
        for (i=0; i<nstmt; i++)
            {
            if (svex_del_list[i] == TRUE) continue;
            nchar = stlist[i].end - stlist[i].start + 1;
            if ((nctot + nchar) >= 32768)
                {
                msg ("Stripped SVEX contents too large for output buffer", 2);
                return (NULL);
                }
            strncpy (ptr, stlist[i].start, nchar);
            ptr += nchar;
            nctot += nchar;
            }
        *ptr = '\n';
        ptr++;
        *ptr = '\0';
        }

    return (&svex);
    }
