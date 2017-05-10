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

#include "vex.h"
#include "mk4_data.h"                   /* Definitions of data structures */
#include "param_struct.h"               /* Definition of 2 structures (param & status) */
#include "pass_struct.h"

int
create_fname (struct scan_struct *root, struct type_pass *pass, char fname[])
    {
    static char buf[256], buf2[256];
    char *directory, *rname, *strrchr(), *strtok(), *scan, *rootcode;
    extern struct type_param param;
    extern int max_seq_no;

    strcpy (buf, root->filename);        /* Local copy is better */ 

    rname = strrchr (buf, '/') + 1;
    *(rname - 1) = '\0';                /* null terminate directory */
    directory = buf;
    rootcode = strrchr (rname, '.') + 1;
                                        /* Update sequence number */
    max_seq_no++;
    sprintf (buf2, "%s/%c%c.%c.%d.%s", directory, param.baseline[0],
             param.baseline[1], pass->pass_data[0].fgroup, max_seq_no, rootcode);

    msg ("fringe filename = %s",1, buf2);
    strcpy (fname, buf2);
    return (0);
    }
