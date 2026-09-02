/*
 * Check that get_filelist() works
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fstruct.h>
#include "msg.h"

extern void   environment (void);

int print_files(char *ind, int verb, int skip, fstruct *files)
{
    int n = 0;
    if (files == NULL) {
        if (verb) printf("%sNO FILES REPORTED\n", ind);
        return(-1);
    }
    while (files->order >= 0) {
        n++;
        printf(
            "%sfile %d: order=%d type=%d source=%s bl=%s st=%c fc=%c fn=%d rc=%s %s\n"
            "%sfile %d: (%C) pol='%s' ...%s\n",
            ind, n, files->order, files->type, files->source, files->baseline,
                files->station, files->freq_code, files->filenum, files->rootcode,
                files->done == FALSE ? "no" : "yes",
            ind, n, files->namealloc ? 'A' : 'N', files->poln, skip + files->name
        );
        files ++;
    }
    return(n);
}

int main(int argc, char **argv)
{
    int verb, answ, type, nfls, bytes, nn;
    char *datadir;
    fstruct *files = NULL;

    if (--argc < 1) return(-1);
    /* number of arguments is fixed */
    if (**++argv == '-') verb = 1;  /* -v or +v*/

    if (verb) for (nn = 0; nn < argc && argv[nn]; nn++)
        printf("    argv[%d]='%s'\n", nn, argv[nn]);
    if (argc < 4) return(77);

    type = atoi(*++argv);
    bytes = strlen(*++argv)+10;
    argc -= 2;
    datadir = malloc(bytes);
    if (!datadir) return(perror("malloc"), -1);
    snprintf(datadir, bytes, "DATADIR=%s", *argv);
    if (verb) printf("  put '%s'\n", datadir);
    putenv(datadir);
    if (verb) printf("  got 'DATADIR=%s'\n", getenv("DATADIR"));
    environment();

    bytes -= 10;     /* back to path length */

    argc--;
    argv++;
    if (verb) for (nn = 0; nn < argc && argv[nn]; nn++)
        printf("    path[%d]='%s'\n", nn, argv[nn]);
    fflush(stdout);

    msglev = -3;
    answ = get_filelist(argc, argv, type, &files);
    if (verb) {
        printf("  Got Error return %d:\n", answ);
        nfls = print_files("    ", verb, bytes, files);
        printf("  in all %d files found\n", nfls);
    }

    free (datadir);
    /* answ should be zero and nfls is number of files */
    return(100000*answ + nfls);
}

/*
 * eof
 */
