/*
 * exam_edit() handles the needs of [-g] processing.
 * exam_defaults() transfers items from command line processing
 *   to each examdata structure pointer.
 *
 * The semantics of [-g] are altered to allow creating a template file or
 * alternatively to pull the RxC information from such a file.  These
 * arrangements are evolutionary, to allow minimal changes at the higher
 * level (i.e. the original command-line parsing) and to allow the edit
 * file to override command-line choices (which may eventually be made
 * simpler).
 *
 * As written exam_edit returns >0 on error,  0 if edit file found and
 * used without issue, or <0 to indicate the template was created (and
 * cohfit should exit without doing work).
 */
#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include "cohfit.h"

/* private routine, but global for debugger:
 *  return 0 for template creation and *rcserr == ENOENT,
 *  1 if file exists and is readable, -1 otherwise */
int edit_exists(char *optarg, int *rcserr)
{
    struct stat sb;
    /* return of 0 passes directly to edit_template() */
    if (stat(optarg, &sb) < 0 && (*rcserr = errno) == ENOENT) return(0);
    if (*rcserr) {      /* this will not end well */
        msg("Some error to fix with %s:", 3, optarg);
        msg("  %s", 3, strerror(*rcserr));
        return(-1);
    }
    return(1);          /* file exists and is e.g. readable */
}

/* private routine:
 *  create the template config file, returning -1 if successful, 2 otherwise */
int create_template(char *optarg, examdata *exdp)
{
    FILE *fpg = fopen(optarg, "w");
    if (!fpg) {
        perror("create_template:fopen(w)");
        return(204);    /* errno values are < 133 today */
    }
    fprintf(fpg,
        "#cohfit configuration file -- this line is mandatory\n"
        "#\n"
        "#Lines that begin with # are comments and\n"
        "# in any case lines must have < %d characters.\n"
        "#\n"
        "#Parameters are set with something= ... where\n"
        "# there should be no space before the '=' character\n"
        "# and an exact match to 'something' is required.\n"
        "# There should be only one such assignment per line.\n"
        "#\n"
        "#Values are either integer or floating point and are\n"
        "# correspondingly scanned by %%d or %%f or %%lf.  Booleans\n"
        "# are expressed as 0 for FALSE and nonzero for TRUE.\n"
        "#\n"
        "#The following line sets the number of rows x columns\n"
        "# in the PGPLOT output as well as the montage:\n"
        "RxC= %dx%d\n"
        "#\n", MAX_TXT, exdp->rnc[0], exdp->rnc[1]);
    fprintf(fpg,
        "#These three booleans control first whether plots are made\n"
        "# with gnuplot, and if so, whether montaged with -tile CxR.\n"
        "# Then if TRUE, customlimits sets the scale of the axes to be\n"
        "# on a per-plot basis.  If FALSE, all plots share the same scale.\n"
        "#(Note that the gnuplot/montage functions on a per-scan-boundary\n"
        "# basis, whereas PGPLOT tiles all the plots on as many pages as\n"
        "# are required, with per-plot scalings.):\n"
        "gnuplot=%d\n"
        "montage=%d\n"
        "customlimits=%d\n"
        "#\n", exdp->gnuplot, exdp->montage, exdp->customlimits);
    fprintf(fpg,
        "#The montage converts the gnuplot PDFs into images for tiling\n"
        "# using a density set by -density geometry.  Here we assume the\n"
        "# same density in both axes that may be adjusted with:\n"
        "density=%d\n"
        "#\n", exdp->density);
    fprintf(fpg,
        "#The coherence loss [-s], maximum coherence time [-c], and\n"
        "# label flag [-l] can also be set here.  The first two are\n"
        "# floating point quantities, and the zero for false:\n"
        "coherenceloss=%g\n"
        "maxcoheretime=%g\n"
        "labels=%d\n"
        "#\n", exdp->closs, exdp->mxcotime, exdp->labels);
    fprintf(fpg,
        "#For testing on systems that support the older GSL nonlinear\n"
        "# fitting library of <gsl/gsl_multifit_nlin.h> you may set\n"
        "# this option to 1 from its 0 default to use the older\n"
        "# framework for the amplitude fits (PS, PO or SO):\n"
        "gslegacy=%d\n"
        "#\n", exdp->gslegacy);
    fprintf(fpg, "#\n# eoc\n#\n");
    fclose(fpg);
    msg("Created graphic edit file '%s'", 2, exdp->edit = optarg);
    return(-1);
}

/* private support: print msg and exit */
int barf(int lno, char *wye, char *line, int errval)
{
    msg(wye, 3, lno);
    msg("'%s'", 3, line);
    return(errval);
}

/* private routine:
 *  return 0 if successfully parsed or nonzero for parse error.
 *  the default row and col numbers come from main() */
int edit_parse(char *optarg, examdata *exdp)
{
    FILE *fpg = fopen(optarg, "r");
    char line[MAX_TXT + 10];
    int lno = 0, ncs;
    if (!fpg) {
        perror("edit_parse:fopen(r)");
        return(200);    /* errno values are < 133 today */
    }
    while (++lno && fgets(line, MAX_TXT, fpg)) {
        line[strlen(line)-1] = 0;                   /* stomp newline */
        if (1 == lno && strcmp(line,
            "#cohfit configuration file -- this line is mandatory")) {
            msg("Config file %s missing mandatory first line", 3, optarg);
            return(201);
        } else if (1 == lno) {
            continue;                               /* do nothing */
        } else if (line[0] == '#') {
            continue;                               /* do nothing */
        } else if (!strncmp(line, "RxC=", 4)) {
            ncs = sscanf(line, "RxC=%dx%d", &exdp->rnc[0], &exdp->rnc[1]);
            if (2 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else if (!strncmp(line, "gnuplot=", 8)) {
            ncs = sscanf(line, "gnuplot=%d", &exdp->gnuplot);
            if (1 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else if (!strncmp(line, "montage=", 8)) {
            ncs = sscanf(line, "montage=%d", &exdp->montage);
            if (1 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else if (!strncmp(line, "customlimits=", 13)) {
            ncs = sscanf(line, "customlimits=%d", &exdp->customlimits);
            if (1 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else if (!strncmp(line, "density=", 8)) {
            ncs = sscanf(line, "density=%d", &exdp->density);
            if (1 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else if (!strncmp(line, "coherenceloss=", 14)) {
            ncs = sscanf(line, "coherenceloss=%f", &exdp->closs);
            if (1 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else if (!strncmp(line, "maxcoheretime=", 14)) {
            ncs = sscanf(line, "maxcoheretime=%lg", &exdp->mxcotime);
            if (1 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else if (!strncmp(line, "labels=", 7)) {
            ncs = sscanf(line, "labels=%d", &exdp->labels);
            if (1 == ncs) continue;
            return(barf(lno, "Line %d did not parse properly:", line, 202));
        } else {
            return(barf(lno, "Line %d is beyond the pale:", line, 254));
        }
    }
    /* save a copy of the file name */
    exdp->edit = malloc(strlen(optarg)+1);
    strcpy(exdp->edit, optarg);
    return(0);
}

/* public entry, see masthead comment */
int exam_edit(char *optarg, examdata *exdp)
{
    int staterr = 0;
    /* try RowsxCols parsing first */
    msg("Considering -g '%s'", 0, optarg);
    if (2 == sscanf(optarg, "%dx%d", &exdp->rnc[0], &exdp->rnc[1])) return(0);
    msg("exam edit pattern is '%s'", 0, exdp->epat);
    if (!exdp->epat) {
        msg("RxC expected; config file illegal without -e ...", 3);
        return(253);
    }
    if (1 == edit_exists(optarg, &staterr)) {
        msg("Found graphic edit file '%s'", 2, optarg);
        return(edit_parse(optarg, exdp));
    } else {
        msg("exists staterr is %d =? (%d)", 0, staterr, ENOENT);
        if (ENOENT != staterr) return(staterr);
        msg("Creating template graphic edit file '%s'", 2, optarg);
        return(create_template(optarg, exdp));
    }
    msg("-g RowsxCols, '<int>x<int>' or graphic edit file is required", 3);
    return(1);
}

/*
 * eof vim:nospell
 */
