/****************************************************************/
/*                                                              */
/* A test program shamelessly ripped off from fplot for testing */
/*                                                              */
/****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "fstruct.h"

//#ifdef P_tmpdir
//# define P_tmpdir "/tmp"
//#endif /* P_tmpdir */

//#define NONE     0
#define XWINDOW  1
//#define HARDCOPY 2
//#define PRINTLPR 3
#define GSDEVICE 4
//#define DISKFILE 5

char progname[6] = "fplot";
int msglev = 2;

extern void msg(char*, int, ...);

#define SAMPLE "AI.X.2.oifhak"

int main (int argc, char* argv[])
    {
    int i, display, ret, mk4, size, quit, prompt;
    char c, cmd[128], pmt[128], *file_name;
    struct mk4_fringe fringe4;
    fstruct myfile[2], *files = myfile;
    FILE *fp;
    static char ps_file[1024] = "fplot_";
    char *srcdir = getenv("srcdir");
    int sleepy;

    msg ("DISPLAY is '%s'", 3, getenv("DISPLAY"));
    if (!getenv("DISPLAY") || (strlen(getenv("DISPLAY")) == 0))
        {
        msg ("no DISPLAY, no test", 3);
        exit(0);
        }
                                        /* Initialize */
    fringe4.nalloc = 0;
    if (!srcdir)
        {
        msg ("srcdir not defined",3);
        exit(1);
        }
                                        /* options as needed for test */
    display = GSDEVICE;
    files[0].order = 0;
    files[0].name = malloc(strlen(srcdir) + 4 + strlen(SAMPLE));
    sprintf(files[0].name, "%s/%s", srcdir, SAMPLE);
    files[1].order = -1;
                                        /* Loop over all filenames */
    i = 0;
    quit = FALSE;
    prompt = FALSE;
    while (files[i].order >= 0)
        {
        if (read_mk4fringe (files[i++].name, &fringe4) != 0)
            {
            msg ("Failure reading fringe file %s", 2, files[i-1].name);
            continue;
            }
                                    /* Display on screen if xwindow */
        if (display == XWINDOW || display == GSDEVICE)
            {
            if (display == XWINDOW) putenv("GS_DEVICE=x11");
            else                    prompt = TRUE;
            if (prompt) msg ("File %d: %s", 2, i-1, files[i-1].name);
            sleepy = getenv("FP_SLEEP") ? atoi(getenv("FP_SLEEP")) : -4;
            c = display_221 (fringe4.t221, sleepy);
            switch (c)
                {
                case 'q':
                    quit = TRUE;
                    break;
                case 'p':
                    if (i > 1) i -= 2;
                    else i -= 1;
                    break;
                case 'n':
                default:
                    break;
                }
            if (quit) break;
            }

        else
            {
            msg ("developer error", 2);
            exit (1);
            }

        }

    return(0);
    }
