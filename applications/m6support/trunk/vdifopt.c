/*
 * $Id: vdifopt.c 3022 2015-04-15 18:20:49Z gbc $
 *
 * This file provides support for the fuse interface.
 * Here we do the command-line processing to drive the support for fuse.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "vdifuse.h"

/* global args */
int vdifuse_debug = 0;
int vdifuse_enable = VDIFUSE_ENABLE_SKIP;   /* invoke fuse_main() */
int vdifuse_protect = 0;
FILE *vdflog = 0;

/* principal support options */
static int vdifuse_report = 0;
static int vdifuse_create = 0;
static int vdifuse_use_it = 0;
static char *vdifuse_cache = NULL;
static char *vdifuse_mount = NULL;

/* directory list to cache */
static int vdifuse_ndirs = 0;
static char **vdifuse_dirs = NULL;

/*
 * Handlers for help and version
 */
static int vdifsup_help(char *fullname, char *help)
{
    char *name = strrchr(fullname, '/');
    if (!name) name = fullname;
    else name++;
    fprintf(stderr,
        "usage: %s [options] mount-point [directories]\n"
        "\n"
        "  -v           verbose commentary, repeat for more\n"
        "  -c <file>    create cache <file> for metadata (and exit)\n"
        "  -r <file>    check & report on cache <file> (and exit)\n"
        "  -u <file>    use cache <file> and go into background\n"
        "  -a <file>    all of the above with cache <file>\n"
        "  -x <key=val> set various processing parameters\n"
        "\n"
        "%s is expecting to scan some set of directories for valid VDIF\n"
        "files or valid Mark6 scatter-gather files, to build a cache of\n"
        "what it finds (for future access) and to prepare to supply a\n"
        "FUSE filesystem filled with \"fragments\" (what it found) or\n"
        "\"sequences\" (what it assembles into virtual files).\n"
        "\n"
        "Normal usages is to create and use the cache in one step:\n"
        "\n"
        "  %s -a cache mount <directories...>\n"
        "\n"
        "and when finished unmount it with\n"
        "\n"
        "  fusermount -u mount-point\n"
        "\n"
        "The -c/-r and -u option are available to step through the\n"
        "process (perhaps checking for errors), or to reuse a cache\n"
        "that was created previously.\n"
        "\n"
        "For usage examples,  use \"-xexamples\".\n"
        "For details on the fuse mounting options, use \"--HELP\".\n"
        "For details on additional processing parameters, use \"-xhelp\".\n"
        "For details on a variety of known issues, use \"-xissues\".\n"
        "\n"
        "The fuse -f option keeps %s in the foreground, and if\n"
        "combined with -xdebug=N is the best way to debug issues.\n"
        "For convenience -v (repeated N) is equivalent to -xdebug=N.\n"
        "\n",
        name, name, name, name
    );
    if (help) {
        help[0] = 'h'; help[1] = 'e'; help[2] = 'l'; help[3] = 'p';
        fprintf(stderr, "standard FUSE ");  /* FUSE usage follows */
        vdifuse_enable = VDIFUSE_ENABLE_HELP;   /* just to fuse_main() help */
    } else {
        vdifuse_enable = VDIFUSE_ENABLE_SKIP;
    }
    return(1);                                  /* yes, this was help */
}
static int vdifsup_vers(void)
{
    fprintf(stderr, "vdifuse version %.1f\n", VDIFUSE_VERSION);
    vdifuse_enable = VDIFUSE_ENABLE_HELP;   /* just to fuse_main() help */
    return(1);                              /* yes, this was version */
}

/*
 * Args to handle directly, precluding more work.
 */
static int horv(int *argc, char **argv[])
{
    int aa;
    char *name = (*argv)[0];
    if (*argc < 2) return(fprintf(stderr, "Arguments are required\n"));
    for (aa = 0; aa < *argc; aa++) {
        if (!strncmp((*argv)[aa], "-h", 2)) return(vdifsup_help(name,NULL));
        if (!strncmp((*argv)[aa], "--HELP", 6))
            return(vdifsup_help(name,((*argv)[aa])+2));
        if (!strncmp((*argv)[aa], "--help", 6)) return(vdifsup_help(name,NULL));
        if (!strncmp((*argv)[aa], "-V", 2)) return(vdifsup_vers());
        if (!strncmp((*argv)[aa], "--version", 6)) return(vdifsup_vers());
    }
    return(0);
}

/*
 * Run through the list of arguments, separating int our args and fuse's.
 * After return, (n)argc and (n)argv refer only to what fuse should see.
 */
#define vdiferr(cc) (fprintf(stderr,\
    "Cache name conflict with option -%c: %s != %s\n"\
    ,cc,vdifuse_cache,optarg),NULL) 
static char **separate_options(int *argc, char **argv[])
{
    int cc, nargc = 0;
    char **nargv = malloc(sizeof(char*)*(*argc));
    if (!nargv) return(perror("malloc"),NULL);

    vdflog = stdout;
    nargv[nargc++] = (*argv)[0];   /* program name for fuse invocation */

    while ((cc = getopt(*argc, *argv, "l:a:dfso:c:r:u:vx:")) != -1)
    switch(cc) {
    case 'l':
        vdflog = fopen(optarg, "w");
        break;
    case 'a':
        vdifuse_create = 1;
        vdifuse_report = 1;
        vdifuse_use_it = 1;
        if (!vdifuse_cache) vdifuse_cache = optarg;
        else if (strcmp(vdifuse_cache, optarg)) return(vdiferr(cc));
        break;
    case 'd':
        if (!vdifuse_debug) vdifuse_debug = 1;
        nargv[nargc++] = "-d";      /* for fuse */
        break;
    case 'f':
        nargv[nargc++] = "-f";      /* for fuse */
        break;
    case 's':
        nargv[nargc++] = "-s";      /* for fuse */
        break;
    case 'o':
        nargv[nargc++] = "-o";      /* for fuse */
        nargv[nargc++] = optarg;
        break;
    case 'c':
        vdifuse_create = 1;
        if (!vdifuse_cache) vdifuse_cache = optarg;
        else if (strcmp(vdifuse_cache, optarg)) return(vdiferr(cc));
        break;
    case 'r':
        vdifuse_report = 1;
        if (!vdifuse_cache) vdifuse_cache = optarg;
        else if (strcmp(vdifuse_cache, optarg)) return(vdiferr(cc));
        break;
    case 'u':
        vdifuse_use_it = 1;
        if (!vdifuse_cache) vdifuse_cache = optarg;
        else if (strcmp(vdifuse_cache, optarg)) return(vdiferr(cc));
        break;
    case 'v':
        vdifuse_debug ++;
        break;
    case 'x':
        if (vdifuse_options(optarg)) return(NULL);
        break;
    }
    vdifuse_mount = nargv[nargc++] = (*argv)[optind];

    /* first non-opt arg is mount point, the rest are directories */
    vdifuse_dirs = *argv + optind + 1;
    vdifuse_ndirs = *argc - optind - 1;

    *argc = nargc;
    return(nargv);
}

/*
 * A debugging function to show what the result of the command-line
 * parsing produced.
 */
static void vdifuse_plans(int oargc, char **oargv, int nargc, char **nargv)
{
    int aa;
    if (vdifuse_debug>2) {
        fprintf(vdflog, "Original args: ");
        for (aa = 0; aa < oargc; aa++)
            fprintf(vdflog, " %s", oargv[aa]);
        fprintf(vdflog, "\n");
        fprintf(vdflog, "Revised args:  ");
        for (aa = 0; aa < nargc; aa++)
            fprintf(vdflog, " %s", nargv[aa]);
        fprintf(vdflog, "\n");
        fprintf(vdflog, "Directories [%d]: ", vdifuse_ndirs);
        for (aa = 0; aa < vdifuse_ndirs; aa++)
            fprintf(vdflog, " %s", vdifuse_dirs[aa]);
        fprintf(vdflog, "\n");
    }
    if (vdifuse_debug>3) fprintf(vdflog,
        "Cache: '%s' Debug=%d Create=%d Report=%d Access=%d\n",
        vdifuse_cache, vdifuse_debug, vdifuse_create,
        vdifuse_report, !vdifuse_create && !vdifuse_report);
    if (!vdifuse_use_it)
        fprintf(vdflog, "Mount with:   %s -u '%s' '%s'\n",
            nargv[0], vdifuse_cache, vdifuse_mount);
    else 
        fprintf(vdflog, "Unmount with: fusermount -u '%s'\n", vdifuse_mount);
}

/*
 * Implement aforementioned plans.
 */
static int vdifuse_implement(void)
{
    struct stat sb, mp;

    if (!vdifuse_cache) return(fprintf(stderr, "A cache is required.\n"));
    // TODO: shifted this test until later...ok?
    //if (!vdifuse_mount || stat(vdifuse_mount, &mp))
    //    return(fprintf(stderr, "A valid mount point is required\n"));

    if (!vdifuse_create && stat(vdifuse_cache, &sb)) return(fprintf(stderr,
        "VDIFuse metadata cache %s is missing.\n", vdifuse_cache));
    if (!vdifuse_create && (sb.st_size < sizeof(VDIFUSEntry)))
        return(fprintf(stderr, "Cache is too small (%u)\n", sb.st_size));

    if (vdifuse_create && !stat(vdifuse_cache, &sb)) {
        if (!vdifuse_protect) unlink(vdifuse_cache);
        else return(fprintf(stderr,
            "VDIFuse metadata cache %s exists: remove it.\n", vdifuse_cache));
    }
    if (vdifuse_create && !vdifuse_ndirs) return(fprintf(stderr,
        "No search directories given for cache creation\n"));

    if (vdifuse_create &&
        vdifuse_create_metadata(vdifuse_cache, vdifuse_ndirs, vdifuse_dirs))
            return(fprintf(stderr, "Problem creating cache\n"));
    if (vdifuse_report &&
        vdifuse_report_metadata(vdifuse_cache, &sb))
            return(fprintf(stderr, "Metadata cache is unusable\n"));

    // TODO: none of this is needed if we don't have a mount
    if (vdifuse_mount) {
        if (stat(vdifuse_mount, &mp))
            return(fprintf(stderr, "A valid mount point is required\n"));

        if (vdifuse_access_metadata(vdifuse_cache, &sb, &mp))
            return(fprintf(stderr, "Unable to use the cache\n"));

        if (vorr_init())
            return(fprintf(stderr, "Unable to initialize for use.\n"));

        vdifuse_enable =
            vdifuse_use_it ? VDIFUSE_ENABLE_DOIT : VDIFUSE_ENABLE_SKIP;
    } else {
        if (vdifuse_use_it)
            return(fprintf(stderr, "A valid mount point is required\n"));

        vdifuse_enable = VDIFUSE_ENABLE_SKIP;
    }
    return(vdifuse_finish());
}

/*
 * Pull our options out from the general option list and do
 * our setup work.  Then hand the list off to fuse to drive.
 */
int vdifsup_opts(int *argc, char **argv[])
{
    char **fuse_argv;
    int oargc = *argc;
    if (horv(argc, argv)) return(0);
    fuse_argv = separate_options(argc, argv);
    if (!fuse_argv) return(1);
    if (vdifuse_debug>0) vdifuse_plans(oargc, *argv, *argc, fuse_argv);
    if (vdifuse_implement()) return(1);
    *argv = fuse_argv;
    return(0);
}

/*
 * eof
 */
