/************************************************************************/
/*                                                                      */
/* The purpose of this routine is to extract from the user a            */
/* specification of the quantities in the fringe files that are         */
/* desired, for future plotting or tabular ascii output.  The list of   */
/* available parameters (defined in param_list.h) is printed on the     */
/* screen, and the user selects the ones he/she wants by entering a     */
/* list of parameter index numbers at the terminal.  Some error checking*/
/* (only to catch overflow of number of parameters at this point) is    */
/* done.  In the future, a more user-friendly mouse-driven interface    */
/* is envisaged.  Also, to permit aedit scripts that are immune to      */
/* changes in parameter index number assignments, a minimum-match       */
/* parameter name recognition capability should be implemented.         */
/*                                                                      */
/*      Inputs:         maxfreq         Up to this many elements needed */
/*                                      in each per-freq parameter      */
/*                                                                      */
/*      Output:         user_param      nparms and type structure       */
/*                                      elements filled in.             */
/*                      return value    0=success, !=0 = failure        */
/*                                                                      */
/* Created 21 July 1993 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "usearray.h"
#include "aedit.h"

#define TRUE 1
#define FALSE 0

int get_param_list (struct usearray *user_param, int maxfreq, char *args)
    {
    struct udat *p, *p1, *p2;
    char buf[256], string1[40], string2[40], idstr[10], *str, *eol;
    int i, j, nparms, param_id, nelement, try_again, ask, split_at;
    extern struct udat parameter_list[];
    extern char progname[];
    extern int batch;
                                        /* Do we have numbers in arg list? */
    ask = TRUE;
    if (sscanf (args, "%d", &param_id) == 1) ask = FALSE;
    if (batch) ask = FALSE;
                                        /* Tell the user what to do */
    if (ask)
        {
        msg ("Below is a list of available user-defined parameters that", 2);
        msg ("can be extracted from the fringe files.  Please indicate", 2);
        msg ("which ones you want with a space-delimited list of parameter", 2);
        msg ("index numbers.  The total number of parameters allowed is", 2);
        msg ("currently %d, and each array of parameters (denoted by the", 2, MAX_PARMS);
        msg ("parentheses below) counts one for each array element", 2);
        msg ("An index number in parentheses indicates that the", 2);
        msg ("parameter is already in memory, but can be selected as", 2);
        msg ("a parameter for manipulation and output like the others", 2);
        msg ("", 2);
        msg ("INDEX  PARAMETER NAME                   INDEX  PARAMETER NAME", 2);
        msg ("-----  --------------                   -----  --------------", 2);
                                        /* This puts a list of available */
                                        /* parameters on the screen, according */
                                        /* to the current state of parameter_list[] */
                                        /* which is in param_list.h (included */
                                        /* just once, declaring initialized extern) */

                                        /* How many are there? */
        nparms = 0;
        while (parameter_list[nparms++].parameter_name != NULL) ;
        nparms--;
                                        /* Set up for 2-column print, fiddly stuff */
        split_at = nparms/2 + nparms%2;
        for (i = 0; i < split_at; i++)
            {
            p1 = parameter_list + i;
            p2 = parameter_list + i + split_at;
            if (p1->parameter_id > IN_ALINE) sprintf (idstr, "(%2d):",
                        p1->parameter_id);
            else sprintf (idstr, "%2d:", p1->parameter_id);
            if (p1->parameter_index > 1)
                sprintf (string1, "%-7s%s (%d)", idstr, p1->parameter_name, maxfreq);
            else sprintf (string1, "%-7s%s", idstr, p1->parameter_name);
                                        /* Don't run off end of parameter_list */
            if ((i + split_at) >= nparms) string2[0] = '\0';
            else
                {
                if (p2->parameter_id > IN_ALINE) sprintf (idstr, "(%2d):  ",
                        p2->parameter_id);
                else sprintf (idstr, "%2d:    ", p2->parameter_id);
                if (p2->parameter_index > 1)
                    sprintf (string2, "%-7s%s (%d)", idstr, p2->parameter_name, maxfreq);
                else sprintf (string2, "%-7s%s", idstr, p2->parameter_name);
                }
            msg ("%-40s%s", 2, string1, string2);
            }
        msg ("", 2);
        }
                                        /* Now have to get user input */
    user_param->nparms = 0;
    while (TRUE)
        {
                                        /* Prompt, and read response into buf */
        if (ask)
            {
            printf ("%s: Response: ", progname);
            if (!fgets (buf, sizeof(buf), stdin)) exit(0);
	    eol = strrchr(buf, '\n');
	    if (eol) *eol = 0;		/* Drop newline */
            }
        else strcpy (buf, args);
                                        /* Will exit loop unless foulup */
        try_again = FALSE;
                                        /* Parse response for integers */
        nparms = i = 0;
        str = strtok (buf, " ");
        while (str != NULL)
            {
            if (sscanf (str, "%d", &param_id) != 1)
                {
                msg ("Unrecognized field '%s', try again", 2, str);
                try_again = TRUE;
                break;
                }
                                        /* Locate this parameter in list */
            p = parameter_list;
            while (p->parameter_name != NULL)
                {
                if (p->parameter_id == param_id) break;
                p++;
                }
            if (p->parameter_id == 0)
                {
                msg ("Bad parameter id: %d, try again", 2, param_id);
                try_again = TRUE;
                break;
                }
                                        /* Add to user_param, one entry for */
                                        /* each element of a parameter array */
            nelement = p->parameter_index;
            if (nelement > maxfreq) nelement = maxfreq;
            for (j=0; j<nelement; j++)
                {
                if (user_param->nparms >= MAX_PARMS)
                    {
                    msg ("Too many parameters specified (> %d), try again!", 2,
                         MAX_PARMS);
                    user_param->nparms = 0;
                    try_again = TRUE;
                    break;
                    }
                if (try_again) break;
                                        /* OK, add this one */
                user_param->type[user_param->nparms].parameter_name =
                                        p->parameter_name;
                user_param->type[user_param->nparms].parameter_id = param_id;
                                        /* 1-relative for user comfort */
                if (p->parameter_index > 1)
                    user_param->type[user_param->nparms].parameter_index = j+1;
                else
                    user_param->type[user_param->nparms].parameter_index = 0;
                user_param->nparms++;
                }
                                        /* And get the next token */
            str = strtok (NULL, " ");
            }
                                        /* Non-interactive, message and return */
        if (! ask)
            if (try_again)
                {
                msg ("Error in user parameter specification '%s'", 2, args);
                return (1);
                }
                                        /* If we failed, prompt again */
        if (! try_again) break;
        }

    return (0);
    }
