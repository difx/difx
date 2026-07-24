/*
 * Parser support for frqs as an alias for freqs
 *
 * This file contains code to parse a frqstyle frequency code list.
 * There are two usages: after 'frqs' in which case we need to interpret
 * the string and walk through the parser steps of code setup.  OR,
 * in most other places, interpret a frqstyle frequency code list
 * for use with other parameters.
 *
 * Code is inserted if ALLOW_FRQS_CODE is defined; otherwise
 * this is a header file for required routines (if needed outside
 * of parser, which is not very likely).
 */
#ifndef frqsupport_h
#define frqsupport_h
/*
 * First the header info, if any, then code within ALLOW_FRQS_CODE
 */

#if defined(ALLOW_FRQS_CODE)
/* for code legality checking */
static char fchars[] = FCHARS;

/* convenience for readable code */
#define FREE_DEST_RETURN(MSG,PRI,VAL)   do {                 \
        msg(MSG,PRI,VAL); free(dest); return(NULL); } while(0)
/* expansion case: A~B -> A..B; @|N -> @@@..@ */
static char *frqs_expand_notches(char *chans, char *master)
{
    char *dest = calloc(MAXNOTCH+1, sizeof(char)), *scp, *dcp;
    msg("FRQS: frqs_expand_notches(%s) -- input",1,chans);
    if (!dest) {
        perror("malloc");
        return(NULL);
    }
    /* increment of scp may be more, depending */
    for (scp = chans, dcp = dest; *scp; scp++) {
        if (*scp == '~') {  // expand
            char *aa, *bb;
            aa = strchr(master, scp[-1]);
            if (!aa) FREE_DEST_RETURN("~ at start of string or after @!",3,0);
            else if (scp[1] == '\0')
                FREE_DEST_RETURN("~ at end of string or followed by @!",3,0);
            bb = strchr(master, scp[1]);
            if (!bb) FREE_DEST_RETURN("Odd, %c not in codes",3,*scp);
            while (*(++aa) != *bb && (*aa))
                *dcp++ = *aa; // pull next char from master codes
            // bb will be inserted next iteration
        } else if (scp[0] == '@' && scp[1] == '|') {    // N @
            int nn = atoi(scp+2);
            if (!isdigit(scp[2]))
                FREE_DEST_RETURN("@| followed by non-digit: %s!",3,scp);
            /* advance past @|N or @|NN -- one less than auto-increment */
            if (nn < 9) scp += 2; else scp += 3;
            while (nn-- > 0)
                *dcp++ = '@';
        } else {
            *dcp++ = *scp;
        }
    }
    msg("FRQS: frqs_expand_notches(%s) -- output",1,dest);
    return(dest);
}
#undef FREE_DEST_RETURN

/* convenience for readable code */
#define FREE_CHANS_RETURN(MSG,PRI,VAL,XIT)   do {              \
        msg(MSG,PRI,VAL);                                      \
        if(chans != ochans) free(chans); return(XIT); } while(0)
/* handler for chan_notches case: expansion, checking, and insertion */
int frqs_chan_notches(char *chans, char *master, struct c_block *cb_ptr)
{
    char *ochans = chans, *fcp, *ncp;
    msg("FRQS: frqs_chan_notches(%s) -- input",1,chans);
    /* consider expansion cases */
    if (strchr(chans, '@') || strchr(chans, '|') || strchr(chans, '~')) {
        chans = frqs_expand_notches(chans, master);
        if (!chans) return(-1);
    }
    if (strlen(chans) > cb_ptr->nnotches)
        FREE_CHANS_RETURN("Too many chan_notches %d",3,strlen(chans),-2);
    /* check and insert each freq code */
    for (fcp = chans, ncp = cb_ptr->chan_notches; *fcp; fcp++, ncp++) {
        if (*fcp != '@' && !strchr(master, *fcp))
            FREE_CHANS_RETURN("Channel %c is not active?",3,*fcp,-3);
        *ncp = *fcp;
    }
    /* (if expanded) free the expanded string and return success */
    cb_ptr->chan_notches[cb_ptr->nnotches] = '\0';
    msg("FRQS: frqs_chan_notches(%s) -- output",1,cb_ptr->chan_notches);
    if (chans != ochans) free(chans);
    return(0);
}
#undef FREE_DEST_RETURN

/* propagate clone and chid updates through all blocks */
static void frqs_clone_clones(struct c_block *cond_start, int havechid)
{
    struct c_block *cb_ptr;
    int jj, nn = strlen(cond_start->chid);
    for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain) {
        strcpy(cb_ptr->clones[0], cond_start->clones[0]);
        strcpy(cb_ptr->clones[1], cond_start->clones[1]);
        strcpy(cb_ptr->chid, cond_start->chid);
        for (jj = 0; jj < nn; jj++)
            cb_ptr->chid_rf[jj] = cond_start->chid_rf[jj];
    }
}

/* this is called on the string after CLONE_IDS_. */
static int frqs_clone_ids(char *cinfo,char *master, struct c_block *cond_start)
{
    int clen = strlen(cinfo), ncan = clen/2, havechid, ii, mclen;
    char *clones = cinfo + ncan, *cand, *copy;
    int ndx[MAXFREQ/2], kk;
    msg("FRQS: frqs_clone_ids '%s' -- input",1,cinfo);
    /* first check the length */
    if (2*(ncan) != clen || ncan >= MAXFREQ/2) {
        msg("Clone string length issues (%d is not 2*%d or > %d)", 3,
            clen, ncan, MAXFREQ/2);
        return(-1);
    }
    /* then check to see if chids were populated as chid_rf must match */
    havechid = (cond_start->chid_rf[0] != 0.0) ? 1 : 0;
    mclen = strlen(master);
    /* if havechid, then master codes and cond_start->chid should be same */
    if (havechid) {
        if (strlen(cond_start->chid) != mclen) {
            msg("chan_ids was used but illegally (%d != %d, cannot clone", 3,
                strlen(cond_start->chid), mclen);
            return(-2);
        }
    }
    /* now check that all the candidates are in master codes */
    /* while we are in there, capture index in master codes */
    for (ii = 0, cand = cinfo; cand - cinfo < ncan; cand++) {
        copy = strchr(master, *cand);
        if (!copy) {
            msg("clone candidate %c not in code list, cannot clone", 3, cand);
            return(-3);
        }
        ndx[ii++] = copy - master;
    }
    /* if chids were used, then can also check that new names are not */
    /* either way, we need to make sure the new names are legal */
    for (cand = clones; cand - clones < ncan; cand++) {
        if (havechid && strchr(master, *cand)) {
            msg("clone alias %c is already used, cannot clone", 3, cand);
            return(-4);
        }
        if (!strchr(fchars, *cand)) {
            msg("clone code %c is not legal, cannot clone", 3, cand);
        }
    }
    /* Ok, install the aliases, and if chids used, copy the freq as well */
    for (ii = 0; ii < ncan; ii++) {
        /* the clones */
        cond_start->clones[1][ii] = clones[ii];
        cond_start->clones[0][ii] = cinfo[ii];  /* the clone sources */
        if (havechid && mclen < MAXFREQ) {
            // adjust cond_start->chid and master codes synchronously
            kk = mclen+ii;
            if (kk > MAXFREQ) {
                msg("No more room for codes starting with %c", 3, clones[ii]);
                return(-6);
            }
            cond_start->chid[kk] = clones[ii];
            cond_start->chid_rf[kk] = cond_start->chid_rf[ndx[ii]];
            master[kk] = clones[ii];
            // null termination for string usages
            cond_start->chid[kk+1] = master[kk+1] = 0;
            msg("short master code path %d\n%s:  of %s (freq %f)", 1,
                kk, progname, master, cond_start->chid_rf[kk]);
        } else {    // using all codes; have to find rather than append
            kk = fcode(clones[ii], master);
            if (kk<0 || kk>MAXFREQ-1) {
                msg("64 fcodes in use, but clone label not found", 3);
                return(-5);
            }
            // this sets it equal to zero if chids not used,
            // and correctly if it chids is used.
            cond_start->chid_rf[kk] = cond_start->chid_rf[ndx[ii]];
            msg("long master code path %d\n%s:  of %s (freq %f)", 1,
                kk, progname, master, cond_start->chid_rf[kk]);
        }
        msg("found %d for %c", 1, kk, clones[ii]);
        // else clone label is in master codes and does not need to be added.
        msg(havechid
            ? "clone %c (%c -> %c) at address %d (->%d), freqs %f -> %f"
            : "clone %c (%c -> %c) at master address %d (->%d)",
            1, master[kk], clones[ii], cinfo[ii], kk, ndx[ii],
                cond_start->chid_rf[kk], cond_start->chid_rf[ndx[ii]]);
        // finally, make the clone as acceptable as the source
        cond_start->accept_sbs[kk] = cond_start->accept_sbs[ndx[ii]];
    }
    /* ensure null termination, yes, anal compulsive */
    cond_start->clones[1][ii] = cond_start->clones[0][ii] = '\0';
    /* next step will be in make passes where the cloning takes place */
    msg("FRQS: (master) fcode set is now %s with %s -> %s", 1,
        master, cond_start->clones[1], cond_start->clones[0]);
    frqs_clone_clones(cond_start, havechid);
    return(0);
}

/* this is called to notice CHAN_PARAM tokens for channel parsing */
static int frqs_CHAN_PARAM(int toknum)
{
    extern int token_cat[];     /* lives in parse_control_file.c */
    int ii;
    for (ii = 0; ii < MAX_TOKENS; ii++)
        if (token_cat[toknum] == CHAN_PARAM) return(1);
    return(0);
}

/* this is invoked from (SAVE_CODES or) NEW_CODES */
/* if chid is used, master codes will have already been rewritten */
static int frqs_new_codes(char *parsed_codes, char *master,
    struct c_block *cond_start)
{
    static char *temp_codes, sblabels[] = { 'D', '+', '-' };
    struct c_block *cb_start, *cb_ptr, *cb_tail;
    int fc, nc, pp, tt, xx, len, sideband, sbndx, one;

    temp_codes = calloc((len = strlen(parsed_codes))+8, 1);
    if (!temp_codes) { perror("malloc"); return(-1); }
    strcpy(temp_codes, parsed_codes);
    memset(parsed_codes, 0, MAXFREQ);
    msg("FRQS: frqs_old_codes: '%s' with %d (cond_start: %p)", 1,
        temp_codes, len, cond_start);
    for (pp = tt = 0; tt < len; tt++) {
        if (temp_codes[tt] == '~') {    /* expansion case--is DSB only */
            sideband = DSB;
            one = (temp_codes[tt-1] == '+' || temp_codes[tt-1] == '-') ? 2 : 1;
            /* get code of preceding channel */
            fc = fcode(temp_codes[tt-one], master);
            if (fc < 0) {
                msg("FRQS: bogus ~ channel expansion after '%c'", 3,
                    temp_codes[tt-one]);
                return(-2);
            }
            /* get code of following channel */
            nc = fcode(temp_codes[tt+1], master);
            if (nc < 0) {
                msg("FRQS: bogus ~ channel expansion before '%c'", 3,
                    temp_codes[tt+one]);
                return(-3);
            }
            while (++fc < nc) {
                parsed_codes[pp++] = master[fc];
                if (cond_start) {
                    msg("FRQS: %c accept_sbs[%d] = %d (sideband %c)", 1,
                        master[fc], fc, sideband, sblabels[0]);
                    for (cb_ptr=cond_start; cb_ptr!=NULL;
                         cb_ptr=cb_ptr->cb_chain)
                         cb_ptr -> accept_sbs[fc] = sideband;
                }
            }
        } else {                        /* normal codes */
            /* see after INSERT_V_CHAR */
            if      (temp_codes[tt+1] == '+') { sideband = USB; sbndx = 1; }
            else if (temp_codes[tt+1] == '-') { sideband = LSB; sbndx = 2; }
            else                              { sideband = DSB; sbndx = 0; }
            /* else other actions..., e.g. @ for clone? */
            fc = fcode(temp_codes[tt], master);
            if (fc >= 0) parsed_codes[pp++] = master[fc];
            else msg("FRQS: bad freq code '%c'(%d)", 3,
                temp_codes[tt], sbndx=-1);
            if (sbndx > 0) { parsed_codes[pp++] = sblabels[sbndx]; tt++; }
            if (sbndx < 0) return(-4);
            if (cond_start) {
                /* set the sideband in all code blocks if cond_start non-NULL */
                msg("FRQS: %c accept_sbs[%d] = %d (sideband %c)", 1,
                    master[fc], fc, sideband, sblabels[sbndx]);
                for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
                     cb_ptr -> accept_sbs[fc] = sideband;
            }
        }
    }
    len = strlen(parsed_codes);
    msg(cond_start
        ? "FRQS: frqs_new_codes: '%s' with %d"
        : "FRQS: frqs_exp_codes: '%s' with %d",
        1, parsed_codes, len);
    return(0);
}

/* this is invoked on (char_values+tval) when frqs are in use */
static char *frqs_expand_codes(char *cf_chan_string, char *master)
{
    char *newchans = calloc(268, 1);
    msg("FRQS: frqs_expand_codes: '%s' (%d) -- input",1,
        cf_chan_string, strlen(cf_chan_string));
    strncpy(newchans, cf_chan_string, 256);
    if (frqs_new_codes(newchans, master, NULL)) {
        free(newchans);
        return(NULL);
    }
    msg("FRQS: frqs_expand_codes: '%s' -- output",1,newchans);
    return(newchans);
}

/* deal with interpretation of display_chans */
static int frqs_display_chans(char *chans, char *master,
    struct c_block *cond_start)
{
    char *expcodes;
    char *atskip = strchr(chans, '@');
    struct c_block *cb_ptr;
    msg("FRQS: frqs_display_chans: '%s' '%s' -- input",1,
        chans, atskip ? atskip : "");
    if (atskip) *atskip = 0;
    expcodes = frqs_expand_codes(chans, master);
    if (!expcodes) {
        msg ("problem expanding display_chans %s",3, chans);
        return(-1);
    }
    if (atskip) { *atskip = '@'; strcat(expcodes, atskip); }
    for (cb_ptr=cond_start; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
        strcpy(cb_ptr->display_chans, expcodes);
    msg("FRQS: frqs_display_chans: '%s' -- output",1, expcodes);
    free(expcodes);
    return(0);
}

#else /* defined(ALLOW_FRQS_CODE) */
// no ALLOW_FRQS_CODE
#endif /* defined(ALLOW_FRQS_CODE) */

#endif /* frqsupport_h */
/*
 * eof
 */
