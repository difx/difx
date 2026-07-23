#include "msg.h"

char progname[256];
int msglev = 5;

void set_progname(const char* local_progname)
{
    int i;
    for(i=0;i<256;i++){progname[i] = '\0';}
    strncpy(progname, local_progname, 255);
    progname[255] = '\0';
}

const char* get_progname()
{
    return &(progname[0]);
}

void set_msglev(int lev)
{
    msglev = lev;
}

					/* To understand this, check stdarg man pages */
void msg (const char *string, 
          int level, 
          ...)	

    {
    // extern int msglev;
    // extern char progname[];
    va_list args;

    if (level >= msglev)
        {
        va_start (args, level);
        if (*progname) fprintf (stderr, "%s: ", progname);
        vfprintf (stderr, string, args);
        putc ('\n', stderr);
        va_end (args);
        fflush (stderr);
        }
    }
