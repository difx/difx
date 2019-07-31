/*************************************************************/
/*                                                           */
/* Converts a possibly relative pathname for an input root   */
/* filename to an absolute pathname                          */
/*                                                           */
/* Initial version CJL 12 August 1991                        */
/* Rewritten completely sometime in '92 by CJL               */
/* Modified to find absolute path for any file -TAC 10-2009  */
/*                                                           */
/*************************************************************/

#define FALSE 0
#define TRUE 1

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "fstruct.h"
#include "mk4_util.h"

int
get_abs_path(char input[], char rootname[])
    {
    char directory[256], abs_directory[256], r_fname[256];
    char workdir[256], temp[256];
    char *ptr, *strrchr();

    if (getcwd (workdir, 256) == NULL)
        {
        msg ("error getting current working directory", 3);
        return (-1);
        }
                                        /* Look for last occurrence of '/' */
                                        /* in input */
    ptr = strrchr (input, '/');
    if (ptr == NULL)                    /* File in current working directory */
        {
        strcpy (r_fname, input);
        strcpy (abs_directory, workdir);
        }
    else 
        {
        strcpy (r_fname, ptr+1);        /* filename follows last '/' */
        ptr[0] = '\0';
        strcpy (directory, input);
                                        /* Absolute pathname given */
        if ((directory[0] == '/') || (directory[0] == '\0'))
            strcpy (abs_directory, directory);

        else if (directory[0] == '.')   /* dot form of relative pathname */
            {
                                        /* This is current working directory */
            if (strlen (directory) == 1) strcpy (abs_directory, workdir);
                                        /* Must tack on additional directory path */
            else if (strncmp (directory, "./", 2) == 0)
                sprintf (abs_directory, "%s%s", workdir, directory + 1);
                                        /* Too nasty to do for now */
            else if (strncmp (directory, "..", 2) == 0)
                {
                msg ("Sorry, can't do filenames of ../xxx form yet", 3);
                return (-1);
                }
            else
                {
                msg ("Strangely formed directory specification ... quitting", 3);
                return (-1);
                }
            }
        else                            /* Simple relative pathname */
            sprintf (abs_directory, "%s/%s", workdir, directory);
        }
                                        /* Ok, we have the full path to the */
                                        /* directory, and the root filename */
                                        /* (This error checking should now be */
                                        /* redundant, because we are using the */
                                        /* UTIL library to get the names, which */
                                        /* pre-checks anything that gets here) */
    /*
    if ((ret = check_name (r_fname, &fstemp)) != 0)
        {
        msg ("Badly formed root file name", 2);
        msg  ("return code = %d, name = %s",2, ret, r_fname);
        return (-1);
        }
    else if (fstemp.type != 0)
        {
        msg ("'%s' not a root file name", 3, r_fname);
        return (-1);
        }
    */
                                        /* Reconstruct full filename, and we */
                                        /* are done.  Sometimes bad practise to */
                                        /* sprintf to an argument, so ... */
    sprintf (temp, "%s/%s", abs_directory, r_fname);
    strcpy (rootname, temp);

    return (0);
    }
