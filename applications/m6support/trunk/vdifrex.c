/*
 * $Id: vdifrex.c 2510 2014-09-22 21:15:04Z gbc $
 *
 * This file provides support for the fuse interface.
 * This file provides support for regex(7) matching of paths for
 * inclusion/exclusion.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <regex.h>

/* all we need from vdifuse.h */
extern int vdifuse_debug;
extern FILE *vdflog;

#include "vdifuse.h"

#define VREX_NDEF   0
#define VREX_EXCL   1
#define VREX_INCL   2
typedef struct regex_patts {
    int type;       /* one of VREX_* above */
    char *regex;    /* user-supplied pattern */
    regex_t *preg;  /* compiled pattern */
} RexPatt;

static int regcnt = 0;  /* number of patterns */
RexPatt  *rxlist = 0;   /* list of patterns */

/*
 * Provide the user with some help
 */
void regexhelp(FILE *fp)
{
    fprintf(fp, "\nFile scanning can be controlled with RE-type patterns:\n");
    fprintf(fp, "  exclpatt=<patt>  exclude paths matching pattern\n");
    fprintf(fp, "  inclpatt=<patt>  include paths matching pattern\n");
    fprintf(fp, "  exclfile=<file>  exclude paths patterns of file\n");
    fprintf(fp, "  inclfile=<file>  include paths patterns of file\n");
    fprintf(fp, "You can use any of these--but order matters.  The\n");
    fprintf(fp, "files should contain precisely one pattern per line.\n");
}

/*
 * Called when done to free temporary processing resources
 */
void regexfinal(void)
{
    int rr;
    for (rr = 0; rr < regcnt; rr++) {
        free(rxlist[rr].regex);
        regfree(rxlist[rr].preg);
        free(rxlist[rr].preg);
    }
    free(rxlist);
    rxlist = 0;
}

/*
 * Common code for regex management
 */
static int regcommon(char *patt, RexPatt **rxpp)
{
    static char errbuf[1000];
    RexPatt *rxp;
    int rxcr, nerr;
    rxlist = (RexPatt *)realloc(rxlist, (1 + regcnt)*sizeof(RexPatt));
    if (!rxlist) return(perror("regexcommon:realloc"),1);
    rxp = &rxlist[regcnt];
    rxp->type = VREX_NDEF;
    rxp->regex = malloc(strlen(patt)+2);
    if (!rxp->regex) return(perror("regexcommon:malloc:regex"),2);
    strcpy(rxp->regex, patt);
    rxp->preg = malloc(sizeof(regex_t));
    if (!rxp->preg) return(perror("regexcommon:malloc:preg"),3);
    rxcr = regcomp(rxp->preg, rxp->regex, REG_EXTENDED|REG_NOSUB|REG_NEWLINE);
    if (rxcr) {
        nerr = regerror(rxcr, rxp->preg, errbuf, sizeof(errbuf));
        fprintf(vdflog, "regexclude: %s\n", errbuf);
        return(4);
    }
    regcnt++;
    *rxpp = rxp;
    return(0);
}

/*
 * Register an exclude/include patterns -- only difference is the type.
 */
int regexclude(char *patt)
{
    RexPatt *rxp;
    if (regcommon(patt, &rxp)) return(VREX_EXCL);
    rxp->type = VREX_EXCL;
    return(0);
}
int reginclude(char *patt)
{
    RexPatt *rxp;
    if (regcommon(patt, &rxp)) return(VREX_INCL);
    rxp->type = VREX_INCL;
    return(0);
}

/*
 * Common code for file patterns
 */
static int regcommonfile(char *file, int type)
{
    static char rb[1000];
    FILE *rp = fopen(file, "r");
    int rs;
    if (!rp) return(fprintf(stderr, "Unable to open %s\n", file));
    while (fgets(rb, sizeof(rb), rp)) {
        if (type == VREX_EXCL) rs = regexclude(rb);
        else                   rs = reginclude(rb);
        if (vdifuse_debug>0) fprintf(vdflog,
            "%sclude patt %s from %s was %sted\n",
            type == VREX_EXCL ? "Ex" : "In", rb, file, rs ? "rejec" : "accep");
    }
    fclose(rp);
    return(0);
}

/*
 * File methods -- only difference is the type.
 */
int regexcludefile(char *file)
{
    return(regcommonfile(file, VREX_EXCL));
}
int regincludefile(char *file)
{
    return(regcommonfile(file, VREX_INCL));
}

/*
 * This does the test: returns 0 to retain file, 1 to exclude it.
 * regexec() returns 0 or REG_NOMATCH (!0) for failure.
 * Then we need to account for the pattern type.
 * Then we need to modify based on each additional rule.
 */
int regexcheck(char *path)
{
    int rr, rs, rv = 0;     /* initially marked to be retained */
    char *exin;
    RexPatt *rxp = rxlist;
    for (rr = 0; rr < regcnt; rr++, rxp++) {
        rs = regexec(rxp->preg, path, 0, 0, 0);
        if (rxp->type == VREX_EXCL) {
            exin = "ex";
            /* if it matches an exclusion rule it is rejected */
            rv = rs ? rv : 1;
        } else {
            exin = "in";
            /* if it doesn't match an inclusion rule it is rejected */
            rv = rs ? 1 : rv;
        }
        if (vdifuse_debug>1) fprintf(vdflog,
            "    Path %s\n      %s match %sclude patt %s [%s]\n", path,
            rs ? "didn't" : "did", exin, rxp->regex, rv?"drop":"keep");
    }
    return(rv);
}

/*
 * eof
 */
