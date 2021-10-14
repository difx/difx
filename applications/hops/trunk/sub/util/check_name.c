/*************************************************************/
/*                                                           */
/* This subroutine is a general purpose correlator filename  */
/* checker.  It takes the filename (stripped of directory    */
/* information), and a file specification structure to be    */
/* filled in, and applies a variety of rules to determine if */
/* the name complies with data file naming conventions.  If  */
/* all is well, zero is returned.  Otherwise, bits are set   */
/* in the return value to indicate what part of the filename */
/* is unsatisfactory.  The f_info structure is filled in to  */
/* the extent possible for use by the caller.                */
/*                                                           */
/* Initial version CJL 26 September 1991                     */
/* General version for library use, CJL 23 December 1992     */
/* Modified for Mk4 use with more file types Aug. 4 1995 CJL */
/* islower->isalnum for new rootcode Nov 2017 GBC            */
/*************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "fstruct.h"
#include "mk4_util.h"

int
check_name(char *name, fstruct *f_info)
    {
    char buf[256], *field1, *field2, *field3, *field4, *field5, *strtok();
    char *baseline, *filenum, *rootcode, *freq;
    int i, len, nfield, errcode, l;
                                        /* Init */
    clear_fstruct (f_info);
                                        /* optimism */
    errcode = 0;
                                        /* Parsing below corrupts argument */
    strcpy (buf, name);
                                        /* Can't use strtok because it doesn't */
                                        /* handle null fields, and can't use */
                                        /* strsep because HP doesn't supply it */
                                        /* with their C library, so we must do */
                                        /* this manually */
    len = strlen (buf);
    field1 = buf;
    nfield = 1;
    for (i=0; i<len; i++) if (buf[i] == '.') break;
    if (buf[i] == '.')
        {
        buf[i] = '\0';
        i++;
        field2 = buf + i;
        nfield++;
        for (; i<len; i++) if (buf[i] == '.') break;
        if (buf[i] == '.')
            {
            buf[i] = '\0';
            i++;
            field3 = buf + i;
            nfield++;
            for (; i<len; i++) if (buf[i] == '.') break;
            if (buf[i] == '.')
                {
                buf[i] = '\0';
                i++;
                field4 = buf + i;
                nfield++;
                for (; i<len; i++) if (buf[i] == '.') break;
                if (buf[i] == '.')
                    {
                    buf[i] = '\0';
                    i++;
                    field5 = buf + i;
                    nfield++;
                    }
                }
            }
        }

    if (nfield == 1) return (BADSTRING);
    (void) field5; //disable unused var compiler warning

    switch (nfield)
        {
                                        /* root file, e.g. "3C205.abcdef" */
                                        /* If source name is "log", */
                                        /* this is actually mk4 log file */
        case 2:
            if (strcmp (field1, "log") == 0) f_info->type = 4;

                                        /* Check source name ... should be */
                                        /* up to 31 printable characters */
            else
                {
                l = strlen (field1);
                if ((l == 0) || (l > 31)) errcode |= BADSRC; 
                l--;
                for ( ; l >= 0; l--)
                    if (! isprint(field1[l])) errcode |= BADSRC;
                if (! (errcode & BADSRC)) strcpy (f_info->source, field1);
                f_info->type = 0;
                }
                                        /* assign pointers */
            rootcode = field2;
            baseline = NULL;
            filenum = NULL;
            freq = NULL;
            break;
                                        /* corel file, e.g. "AB.nn.abcdef" */
                                        /* or sdata file "A.nn.abcdef" */
        case 3:
            if (strlen (field1) == 1)
                {
                baseline = NULL;
                if (! isalpha (field1[0])) errcode = BADSTAT;
                f_info->station = field1[0];
                f_info->type = 3;
                }
            else 
                {
                baseline = field1;
                f_info->type = 1;
                }
                                        /* assign pointers */
            filenum = field2;
            rootcode = field3;
            freq = NULL;
            break;
                                        /* fringe file, e.g. "AB.X.nn.abcdef" */
        case 4:
                                        /* assign pointers */
            baseline = field1;
            freq = field2;
            filenum = field3;
            rootcode = field4;
            f_info->type = 2;
            break;

        default:
            return (BADFORM);
        }

    if (baseline != NULL)                               /* Baseline, e.g. "AB" */
        {
        if (strlen (baseline) != 2) errcode |= BADBASE;
        else if((! isalpha(baseline[0])) || (! isalpha(baseline[1]))) errcode |= BADBASE;
        else strcpy (f_info->baseline, baseline);
        }

    if (freq != NULL)                                   /* Frequency code, e.g. "X" */
        {
        if (! isalpha(freq[0])) errcode |= BADFREQ;
        else if (freq[1] != '\0') errcode |= BADFREQ;
        else f_info->freq_code = freq[0];
        }
                                        /* Filenumbers only for type 2's in mk4 */
    if ((filenum != NULL) && (f_info->type == 2))
        {
        l = strlen(filenum);                            /* File #, e.g. (1-9999) */
        if ((l == 0) || (l > 4)) errcode |= BADFNUM;
        l--;
        for ( ; l >= 0; l--)
            if (! isdigit (filenum[l])) errcode |= BADFNUM;
        if ((errcode & BADFNUM) == 0) sscanf (filenum, "%d", &(f_info->filenum));
        }

    if (strlen(rootcode) != 6) errcode |= BADROOT;      /* root id code, e.g. "abcdef" */
    else
        {
        for (l = 0; l < 6; l++)
            if(! isalnum (rootcode[l])) errcode |= BADROOT;
        }
    if ((errcode & BADROOT) == 0) strcpy (f_info->rootcode, rootcode);

    return (errcode);
    }
