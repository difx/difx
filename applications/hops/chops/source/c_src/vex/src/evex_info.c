/************************************************************************/
/*                                                                      */
/* This routine finds and parses a def in the EVEX block.  The          */
/* low-level routine parse_vexfile() must first have been called.  This */
/* routine is normally called only from get_vex().                      */
/*                                                                      */
/*      Inputs:         key             A named def in the $EVEX block  */
/*                                                                      */
/*      Output:         return value    Pointer to evex_struct          */
/*                                      (null on error)                 */
/*                                                                      */
/* Created 1 December 1998 by CJL                                       */
/* realtime latency added by rjc 2008.7.15                              */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

struct evex_struct *
evex_info (char *key)
    {
    int i, j, nst, st, incomplete, nchar;
    struct def *edef;
    struct ref ref;
    struct block *blk;
    struct param_val p_val;
    char stcode, *str, *ptr;
    static struct evex_struct evex;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern char evex_src[];
    extern int do_output, evex_ver;
                                        /* Initialize */
    evex.exper_num = 0;
    evex.ovex_name[0] = '\0';
    evex.lvex_name[0] = '\0';
    evex.cvex_name[0] = '\0';
    evex.svex_name[0] = '\0';
    evex.ap_length = 0.0;
    evex.speedup_factor = 0.0;
    evex.corr_config_key[0] = '\0';
    evex.nst = 0;
    evex.tape_mode = TM_RANDOM;
    evex.mirror = MIR_ALLOCATE | MIR_COMPARE | MIR_NOSAVE;
    evex.realtime_latency = 0;
                                        /* Get the EVEX version number */
    if ((evex_ver = get_version (EVEX)) == V_BAD)
        {
        msg ("Failed to get EVEX version number", 2);
        return (NULL);
        }
                                        /* Locate EVEX block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "EVEX") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $EVEX block", 3);
        return (NULL);
        }
                                        /* Check for null key in root file */
    blk = blist + i;
    if (key[0] == '\0')
        {
        if (blk->ndef > 1)
            {
            msg ("Multiple keys present in evex file, but none specified", 2);
            return (NULL);
            }
        edef = blk->deflist;
        }
                                        /* Retrieve key def */
    else
        {
        for (i=0; i<blk->ndef; i++)
            if (strcmp (blk->deflist[i].name, key) == 0) break;
        if (i == blk->ndef)
            {
            msg ("Could not find key '%s' in $EVEX block", 3, key);
            return (NULL);
            }
        edef = blk->deflist + i;
        }
                                        /* Loop over statements in edef */
    nst = 0;
    for (st=edef->start+1; st<edef->end; st++)
        {
        str = stlist[st].str;
                                        /* parameter and ref statements */
                                        /* may be mixed */
        if (strncmp (str, "ref ", 4) == 0)
            {
                                        /* Generic ref parsing */
            if (parse_ref (st, &ref) != 0)
                {
                msg ("Bad ref statement in EVEX def '%s'", 2, key);
                return (NULL);
                }
                                        /* corr-config is simple keyword copy */
            if (strcmp (ref.blockname, "CORR_CONFIG") == 0)
                strcpy (evex.corr_config_key, ref.keyword);
                                        /* SU config is nasty.  We decree that */
                                        /* specified stations override global */
                                        /* config.  There must be at most one */
                                        /* global config, and no station may */
                                        /* appear twice.  We use correlator */
                                        /* single-character station codes only */
            else if (strcmp (ref.blockname, "SU_CONFIG") == 0)
                {
                                        /* This is global config */
                if (ref.nargs == 1)
                    {
                                        /* Check for previous '*' stations */
                    for (i=0; i<nst; i++)
                        {
                        if (evex.su_config[nst].station == '*')
                            {
                            msg ("Multiple global SU_CONFIG refs in", 2);
                            msg ("EVEX def '%s'", 2, key);
                            return (NULL);
                            }
                        }
                                        /* station '*' reserved for global */
                    evex.su_config[nst].station = '*';
                    strcpy (evex.su_config[nst].su_config_key, ref.keyword);
                    nst = 1;
                    }
                else
                    {
                    msg ("Sorry, station-specific SU configs are not yet", 2);
                    msg ("supported by the software", 2);
                    return (NULL);
                                        /* We have list of stations after ref */
                    for (i=0; i<ref.nargs; i++)
                        {
                                        /* Single characters only allowed */
                        if (strlen (ref.args[i]) != 1)
                            {
                            msg ("Bad station '%s' in SU_CONFIG ref", 2, 
                                                        ref.args[i]);
                            return (NULL);
                            }
                        stcode = ref.args[i][0];
                                        /* Should not exist in list yet */
                        for (j=0; j<nst; j++)
                            {
                            if (evex.su_config[nst].station == stcode)
                                {
                                msg ("Multiple SU_CONFIGs for station '%c'",
                                        2, stcode);
                                return (NULL);
                                }
                            }
                                        /* All clear, record it */
                        evex.su_config[nst].station = stcode;
                        strcpy (evex.su_config[nst].su_config_key, ref.keyword);
                        nst++;
                        }
                    }
                }
                                        /* Done with the ref */
            continue;
            }
                                        /* Not ref, must be parameter= */
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "EVEX", EVEX | evex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (NULL);
            }

        if ISNAME ("corr_exp#") 
            evex.exper_num = p_val.dval[0].data.intval;
        else if ISNAME ("ovex_file")
            strcpy (evex.ovex_name, p_val.dval[0].data.strval);
        else if ISNAME ("lvex_file")
            strcpy (evex.lvex_name, p_val.dval[0].data.strval);
        else if ISNAME ("cvex_file")
            strcpy (evex.cvex_name, p_val.dval[0].data.strval);
        else if ISNAME ("svex_file")
            strcpy (evex.svex_name, p_val.dval[0].data.strval);
        else if ISNAME ("AP_length")
            evex.ap_length = p_val.dval[0].data.realval;
        else if ISNAME ("speedup_factor")
            evex.speedup_factor = p_val.dval[0].data.realval;
        else if ISNAME ("tape_mode")
            {
            if (strcmp (p_val.dval[0].data.strval, "random") == 0)
                evex.tape_mode = TM_RANDOM;
            else if (strcmp (p_val.dval[0].data.strval, "sequential") == 0)
                evex.tape_mode = TM_SEQUENTIAL;
            else if (strcmp (p_val.dval[0].data.strval, "synchronous") == 0)
                evex.tape_mode = TM_SYNCHRONOUS;
            else if (strcmp (p_val.dval[0].data.strval, "continuous") == 0)
                evex.tape_mode = TM_CONTINUOUS;
            }
        else if ISNAME ("mirror")
            {
            if (strcmp (p_val.dval[0].data.strval, "allocate") == 0)
                evex.mirror |= MIR_ALLOCATE;
            else if (strcmp (p_val.dval[0].data.strval, "noallocate") == 0)
                evex.mirror |= MIR_NOALLOCATE;

            if (strcmp (p_val.dval[1].data.strval, "compare") == 0)
                evex.mirror |= MIR_COMPARE;
            else if (strcmp (p_val.dval[1].data.strval, "nocompare") == 0)
                evex.mirror |= MIR_NOCOMPARE;

            if (strcmp (p_val.dval[2].data.strval, "no_save") == 0)
                evex.mirror |= MIR_NOSAVE;
            else if (strcmp (p_val.dval[2].data.strval, "save_different") == 0)
                evex.mirror |= MIR_SAVEDIFFERENT;
            else if (strcmp (p_val.dval[2].data.strval, "save_all") == 0)
                evex.mirror |= MIR_SAVEALL;
            }
        else if ISNAME ("realtime_latency")
            evex.realtime_latency = p_val.dval[0].data.intval;
        }
                                        /* Remember to record nst */
    evex.nst = nst;
                                        /* Must meet minimum requirements */
    incomplete = FALSE;
    if (evex.exper_num == 0) incomplete = TRUE;
    if (evex.ovex_name[0] == '\0') incomplete = TRUE;
    if (evex.cvex_name[0] == '\0') incomplete = TRUE;
    if (evex.svex_name[0] == '\0') incomplete = TRUE;
    if (evex.ap_length == 0.0) incomplete = TRUE;
    if (evex.speedup_factor == 0.0) incomplete = TRUE;
    if (evex.corr_config_key[0] == '\0') incomplete = TRUE;
    if (evex.nst == 0) incomplete = TRUE;
    msg ("expnum=%d", -1,evex.exper_num);
    msg ("oname='%s'", -1,evex.ovex_name);
    msg ("cname='%s'", -1,evex.cvex_name);
    msg ("sname='%s'", -1,evex.svex_name);
    msg ("aplen=%g", -1,evex.ap_length);
    msg ("speedup=%g", -1,evex.speedup_factor);
    msg ("key='%s'", -1,evex.corr_config_key);
    msg ("nst=%d", -1,evex.nst);
    if (incomplete)
        {
        msg ("Insufficient information supplied in EVEX def '%s'", 2, key);
        return (NULL);
        }
                                        /* Create stripped ascii output */
    if (do_output)
        {
                                        /* Copy over EVEX_REV block */
        ptr = evex_src;
        for (i=0; i<nblock; i++)
            {
            if (strcmp (blist[i].name, "EVEX_REV") == 0)
                for (j=blist[i].stno; j<=blist[i].end; j++)
                    {
                    nchar = stlist[j].end - stlist[j].start + 1;
                    strncpy (ptr, stlist[j].start, nchar);
                    ptr += nchar;
                    }
            }
                                        /* Only one $block in EVEX */
        strcpy (ptr, "\n$EVEX;\n");
        ptr += 8;
                                        /* Copy def, lock stock + barrel */
        nchar = stlist[edef->end].end - stlist[edef->start].start + 1;
        if ((ptr - evex_src + nchar) >= 4096)
            {
            msg ("EVEX def too large for output buffer", 2);
            return (NULL);
            }
        memcpy (ptr, stlist[edef->start].start, nchar);
        ptr[nchar] = '\n';
        ptr[nchar+1] = '\0';
        }

    return (&evex);
    }
