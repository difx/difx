/************************************************************************/
/*                                                                      */
/* This routine parses the cvex portion of the input file, which has    */
/* already been low-level parsed into memory by the parse_vexfile()     */
/* routine.  It fills a Cvex_Setup struct according to the key to the   */
/* $CORR_CONFIG section that is passed to it.                           */
/*                                                                      */
/*      Inputs:         key             Refers to def in $CORR_CONFIG   */
/*                                                                      */
/*      Output:         cvex            Pointer to Cvex_Setup struct    */
/*                                                                      */
/* Created 16 November 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int cvex_del_list[2048];

struct Cvex_Config *
cvex_info (char *key)
    {
    int i, j, nchar, nctot, retain;
    struct def *keydef;
    char *str, *ptr;
    struct ref ref;
    struct block *blk;
    struct Cvex_Mode *cm, *get_corr_mode();
    static struct Cvex_Config cconfig;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;   
    extern char cvex_src[];
    extern int do_output, cvex_ver;
                                        /* Get the CVEX version number */
    if ((cvex_ver = get_version (CVEX)) == V_BAD)
        {
        msg ("Failed to get CVEX version number", 2);
        return (NULL);
        }
                                        /* Initialize the delete list */
    if (nstmt > 2048)
        {
        msg ("CVEX source file too big, more than 2048 statements", 2);
        return (NULL);
        }
    for (i=0; i<nstmt; i++) cvex_del_list[i] = TRUE;
                                        /* Do some parser internal init */
    get_section_mode (":INIT:");
    get_corr_bd_parms (":INIT:");
                                        /* Retain all CVEX $BLOCK statements */
    for (i=0; i<nblock; i++) 
        {
        retain = FALSE;
        if (strcmp (blist[i].name, "CORR_CONFIG") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "CORR_MODE") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "CORR_BD_PARMS") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "CORR_SECTION_MODE") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "CORR_CHIP_MODE") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "CORR_BLOCK_MODE") == 0) retain = TRUE;
        if (retain) cvex_del_list[blist[i].stno] = FALSE;
                                        /* Retain CVEX_REV section */
        if (strcmp (blist[i].name, "CVEX_REV") == 0)
            for (j=blist[i].stno; j<=blist[i].end; j++)
                cvex_del_list[j] = FALSE;
        }
                                        /* Locate CORR_CONFIG block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "CORR_CONFIG") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $CORR_CONFIG block", 3);
        return (NULL);
        }
                                        /* Check for null key in root file */
    blk = blist + i;
    if (key[0] == '\0')
        {
        if (blk->ndef != 1)
            {
            msg ("Multiple keys present in cvex file, but none specified", 2);
            return (NULL);
            }
        keydef = blk->deflist;
        }
                                        /* Find def for key */
    else
        {
        for (i=0; i<blk->ndef; i++)
            if (strcmp (blk->deflist[i].name, key) == 0) break;
        if (i == blk->ndef)
            {
            msg ("Could not find key '%s' in $CORR_CONFIG block", 3, key);
            return (NULL);
            }
        keydef = blk->deflist + i;
        }
                                        /* Undelete the def */
    for (i=keydef->start; i<=keydef->end; i++)
        cvex_del_list[i] = FALSE;
                                        /* Record the def name */
    strcpy (cconfig.defId, keydef->name);
                                        /* Process statements in def */
    for (i=keydef->start+1; i<keydef->end; i++)
        {
                                        /* Set default auto_corr flag */
        cconfig.auto_corr = TRUE;
        str = stlist[i].str;
        if (strncmp (str, "ref", 3) == 0)
            {
            if (parse_ref (i, &ref) != 0)
                {
                msg ("Error, bad 'ref' in $CORR_CONFIG def '%s'", 2, keydef->name);
                return (NULL);
                }
                                        /* Refs here cannot be qualified */
            if (ref.nargs != 1)
                {
                msg ("Qualified form of ref not allowed in $CORR_CONFIG", 2);
                print_location (i);
                return (NULL);
                }
                                        /* Create requested pointers */
            if (strcmp (ref.blockname, "CORR_MODE") == 0)
                {
                cm = get_corr_mode (ref.keyword);
                if (cm == NULL)
                    {
                    msg ("Failed to get CORR_MODE def '%s'", 2, ref.keyword);
                    return (NULL);
                    }
                cconfig.mode = cm;
                }
            else
                {
                msg ("Bad $block name specified in $CORR_CONFIG block ref", 2);
                print_location (i);
                return (NULL);
                }
            continue;
            }
                                        /* Must be ignore_chan_names statement */
                                        /* or auto_corr statement */
        else if (parse_pval (str, "CORR_CONFIG", CVEX | cvex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[i].str);
            print_location (i);
            return (NULL);
            }
        if ISNAME ("ignore_chan_names") 
            {
            if (strcmp (p_val.dval[0].data.strval, "on") == 0)
                cconfig.ignoreChanNames = TRUE;
            else cconfig.ignoreChanNames = FALSE;
            }
        else if ISNAME ("auto_corr") 
            {
            if (strcmp (p_val.dval[0].data.strval, "on") == 0)
                cconfig.auto_corr = TRUE;
            else cconfig.auto_corr = FALSE;
            }

        }
                                        /* Process the delete list to generate */
                                        /* stripped output */
    if (do_output)
        {
        ptr = cvex_src;
        nctot = 0;
        for (i=0; i<nstmt; i++)
            {
            if (cvex_del_list[i] == TRUE) continue;
            nchar = stlist[i].end - stlist[i].start + 1;
            if ((nctot + nchar) >= 32768)
                {
                msg ("Stripped CVEX contents too large for output buffer", 2);
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

    return (&cconfig);
    }
