/*****************************************************************
* Subroutine to read file-based phase corrections, calculate     *
* them for the given time and frequency, and return the          *
* remote - reference difference                                  *
*                                                                *
*  initial version                                rjc 2013.6.6   *
*****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "param_struct.h"
#include "pass_struct.h"


double diff_file_phase (struct type_pass *p, // ptr to current pass structure
                        char fcode,          // frequency code [a..zA..Z$%]
                        double t)            // ap center time in days since boy
    {
    int i,
        k,
        n,
        rc,
        nch,
        in_comment,
        lleng,                      // size of a memory chunk for one input line
        nchunks;                    // number of chunks of memory allocated

    double phase[2],
           *pd,
           t_bound,
           t_a, t_b,
           phi_a, phi_b;

    char *pch;

    extern struct type_param param;
    FILE *fstr;
                                    // persistent variables
    static char bl_mem[2] = {'\0', '\0'};
    static int nl[2],
               nchan[2];
    static void *pmall[2] = {NULL, NULL};

    for (i=0; i<2; i++)
        if (param.baseline[i] != bl_mem[i])
            {
                                    // free previous memory if it's there
            if (pmall[i] != NULL)
                free (pmall[i]);
                                    // open baseline files if non-null
            if (p->control.adhoc_file[i][0] != '\0')
                {
                msg("new station[%d] %c file <%s> channels <%s>",1,
                    i, param.baseline[i], p->control.adhoc_file[i],
                    p->control.adhoc_file_chans[i]);
                fstr = fopen (p->control.adhoc_file[i], "r");
                if (fstr == NULL)
                    {
                    msg ("Error opening adhoc_file %s, ignoring request", 
                          2, p->control.adhoc_file[i]);
                    p->control.adhoc_file[i][0] = '\0';
                    }
                }
                                    // skip reading if no file or open was bad
            if (p->control.adhoc_file[i][0] != '\0')
                {
                                    // calculate the length of one line in memory
                nchan[i] = strlen (p->control.adhoc_file_chans[i]);
                lleng = 8 * (nchan[i] + 1);
                k = 0;
                                    // malloc space for the first line
                pmall[i] = malloc (lleng);
                pd = (double *) pmall[i];
                nchunks = 1;
                                    // loop over whole file
                nl[i] = 0;          // line counter
                in_comment = FALSE;
                while (TRUE)
                    {
                                    // read a line into heap memory
                    do              // swallow white space, and all non-numeric input
                        {
                        rc = fgetc (fstr);
                        if (rc == EOF)
                            break;
                        else if (rc < 0)
                           {
                           msg ("read error on %s", 2,  p->control.adhoc_file[i]);
                           break;
                           }
                        if (rc == '*')
                            in_comment = TRUE;
                        else if (rc == '\n')
                            in_comment = FALSE;
                        }
                    while
                        (in_comment || rc < '0' || rc > '9');
                                    // break outer while loop, too?
                    if (rc < 0)
                        break;  
                                    // valid character, push it back
                    ungetc (rc, fstr);
                                    // first the time field...
                    rc = fscanf (fstr, "%lf", &(pd[k++]));
                                    // exit on EOF
                    if (rc == EOF)
                        break;
                    if (rc != 1)
                        {
                        msg ("read error on %s", 2,  p->control.adhoc_file[i]);
                        break;
                        }
                                    // ...then all of the channels
                    for (n=0; n<nchan[i]; n++)
                        rc = fscanf (fstr, "%lf", &(pd[k++]));

                    if (rc != 1)
                        {
                        msg ("read error on %s", 2,  p->control.adhoc_file[i]);
                        break;
                        }
                                    // realloc enough space to store the next line
                    nchunks++;
                    pmall[i] = realloc (pmall[i], nchunks * lleng);
                    if (pmall[i] == NULL)
                        {
                        msg ("realloc failure in diff_file_phase; skipping rest of file", 2);
                        break;
                        }
                    pd = (double *) pmall[i];
                    ++nl[i];
                    }
                fclose (fstr);
                                    // duplicate line iff there is only one
                if (nl[i] == 1)
                    {
                    for (n=0; n<nchan[i]+1; n++)
                        pd[nchan[i]+1+n] = pd[n];
                    nl[i] = 2;
                    }
                                    // denote new baseline in memory
                bl_mem[i] = param.baseline[i];
                }
            }
                                    // data are in memory array 
                                    // now find differential phase
    for (i=0; i<2; i++)

    for (i=0; i<2; i++)
        if (p->control.adhoc_file[i][0] != '\0')
            {
                                    // determine frequency index
            pch = strchr (p->control.adhoc_file_chans[i], fcode);
            if (pch == NULL)
                {
                msg ("freq code %c not in file %s with chans %s", 2, fcode,
                     p->control.adhoc_file[i], p->control.adhoc_file_chans[i]);
                phase[i] = 0.0;
                continue;
                }
            nch = pch - p->control.adhoc_file_chans[i];
            pd = (double *) pmall[i];
                                    // bound t to be within data coverage region
            if (t < pd[0])          // earlier than first time?
                t_bound = pd[0];
                                    // later than last time?
            else if (t > pd[(nl[i] - 1) * (nchan[i] + 1)])
                t_bound = pd[(nl[i] - 1) * (nchan[i] + 1)];
            else
                t_bound = t;        // already within spanned interval
                

            for (n=1; n<nl[i]; n++)
                if (pd[n * (nchan[i] + 1)] > t_bound)
                    break;      
                                    // on exit, containing interval is n-1 .. n
            t_a = pd[(n - 1) * (nchan[i] + 1)];
            t_b = pd[(n    ) * (nchan[i] + 1)];
            phi_a = pd[(n - 1) * (nchan[i] + 1) + nch + 1];
            phi_b = pd[(n    ) * (nchan[i] + 1) + nch + 1];
                                    // linear interpolation between times t_a and t_b
            phase[i] = (t_bound * (phi_b-phi_a) - t_a*phi_b + t_b*phi_a) / (t_b-t_a);
            msg ("fcode %c t %f file_phase[%d] %7.2f", 0, fcode, t, i, phase[i]);
                                    // convert answer to radians
            phase[i] *= M_PI / 180.0;
            }
        else
            phase[i] = 0.0;
                                    // match sign sense of manual pcal
    return (phase[0] - phase[1]);
    }

