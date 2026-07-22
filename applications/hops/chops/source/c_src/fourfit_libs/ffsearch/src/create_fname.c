/*****************************************************/
/*                                                   */
/* This figures out the correct, full pathname of    */
/* the fringe output file.  It uses the full root    */
/* filename (via parsed ovex) as a shortcut to the   */
/* scan id, root code, and directory path parts of   */
/* name, but must look at the data to decide the     */
/* band and extent number to use                     */
/* Error checking is minimised because the input is  */
/* already sanitized.                                */
/*                                                   */
/* Created October 3 1991 by CJL                     */
/* Modified for Mk4, August 30 1999 by CJL           */
/*                                                   */
/*****************************************************/

#include <stdio.h>
#include <string.h>
#include "msg.h"
#include "vex.h"
#include "mk4_data.h"                   /* Definitions of data structures */
#include "param_struct.h"               /* Definition of 2 structures (param & status) */
#include "pass_struct.h"

/* ripped off and modified from generate_text() */
static int zpolab(struct type_pass *pass, char *polstr)
{
    switch (pass->pol) {
    case 0:  strcpy(polstr, "LL"); break;
    case 1:  strcpy(polstr, "RR"); break;
    case 2:  strcpy(polstr, "LR"); break;
    case 3:  strcpy(polstr, "RL"); break;
    default: return(1);            break;
    }
    modify_pol(pass, polstr);
    if (polstr[0] && polstr[1]) return(0);
    return(1);
}

int
create_fname (char *rootfilename, struct type_pass *pass,
    int the_seq_no, char fname[])
    {
    static char buf[256], buf2[256], polstr[3], format[40];
    char *directory, *rname, *scan, *rootcode, *frngfmt = format;
    extern struct type_param param;

    strcpy (buf, rootfilename);        /* Local copy is better */ 

    rname = strrchr (buf, '/') + 1;
    *(rname - 1) = '\0';                /* null terminate directory */
    directory = buf;
    rootcode = strrchr (rname, '.') + 1;
                                        /* Update sequence number */
    if (pass->control.polfringnames && !zpolab(pass, polstr)) {
        snprintf(format, 40, "%%s/%%c%%c.%%c.%%d-%2s.%%s", polstr);
    } else {
        frngfmt="%s/%c%c.%c.%d.%s";
    }
    sprintf (buf2, frngfmt, directory, param.baseline[0],
         param.baseline[1], pass->pass_data[0].fgroup, the_seq_no, rootcode);

    msg ("fringe filename = %s",1, buf2);
    strcpy (fname, buf2);
    return (0);
    }
