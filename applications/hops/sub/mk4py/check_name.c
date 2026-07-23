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
#include <stdlib.h>
#include <string.h>
#include "fstruct.h"
#include "mk4_util.h"

/* this can be used by the caller to make wipe_fstruct() safe */
void prep_fstruct(fstruct *f_info)
{
    if (!f_info) return;
    f_info->type = -1;
    f_info->name = NULL;
    f_info->namealloc = 0;
}

/* most of the defines come from fstruct.h */

/* a replacement for clear _ fstruct() which is really only used here */
static inline void wipe_fstruct(fstruct *f_info)
{
    int i;
    /* One presumes the name came from a struct dirent which is [256]
     * for the name component. However if the caller created the f_info
     * variable on the stack or malloc'd it, namealloc might be some
     * random, legal length.  prep_fstruct() is the best protection.
     * The code here only makes catastrophic failure less likely. */
    if (f_info->name) {
        if (f_info->namealloc > 0 && f_info->namealloc < 256) {
            if (f_info->type > 0 && f_info->type < 5 &&
                f_info->rootcode[6] == 0) {
                free(f_info->name);
                f_info->name = NULL;
            }
        }
    }
    f_info->order = -1;
    f_info->type = -1;
    f_info->source[0] = '\0';
    f_info->baseline[0] = '\0';
    f_info->station = ' ';
    f_info->freq_code = ' ';
    f_info->filenum = -1;
    f_info->rootcode[0] = '\0';
    f_info->done = FALSE;
    for (i=0; i<4; i++)
        {
        f_info->intparm[i] = 0;
        f_info->floatparm[i] = 0.0;
        }
    f_info->namealloc = 0;
    for (i=0; i<2; i++)
        f_info->poln[i] = ' ';
    f_info->poln[i] = 0;
}

int
check_name(char *name, fstruct *f_info)
    {
    char *buf, *field1, *field2, *field3, *field4;
    char *baseline, *filenum, *rootcode, *freq, *poln, *pp;
    int i, len, nfield, errcode, l;
                                    /* Init */
    wipe_fstruct(f_info);
    errcode = 0;
                                    /* Parsing below corrupts argument */
    buf = malloc(strlen(name)+1);
    if (!buf) { perror("malloc") ; return(BADMALLOC); }
    strcpy (buf, name);
    // just in case...
    field1 = field2 = field3 = field4 = "undefined";
                                    /* strtok() doesn't work: .. disappears */
    // why would one want to?
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
                if (buf[i] == '.')  /* one '.' too many */
                    {
                    free(buf);
                    return (BADSTRING);
                    }
                }
            }
        }

    // msg("%s -> %s %s %s %s", 3, name, field1, field2, field3, field4);

    // field1.field2.field3.field4 -> field1 field2 field3 field4
    switch (nfield)
        {
        case 1:
            free(buf);
            return (BADSTRING);     /* too short */

                                    /* root file, e.g. "3C205.abcdef" */
                                    /* If source name is "log", */
                                    /* this is actually mk4 log file */
        case 2:
            if (strcmp (field1, "log") == 0)
                {
                                    /* provide a bogus root code: */
                                    /* we don't use this case, anyway. */
                f_info->type = 4;
                rootcode = "aaaaaa";
                }
            else
                                    /* Check source name ... should be */
                                    /* up to 31 printable characters */
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
                if (! isalnum (field1[0])) errcode = BADSTAT;
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
            filenum = field3;       /* "nn" */
            rootcode = field4;
            f_info->type = 2;
            break;

        default:
            free(buf);
            return (BADFORM);
        }

    if (baseline != NULL)           /* Baseline, e.g. "AB" */
        {
        if (strlen (baseline) != 2) errcode |= BADBASE;
        else if((! isalnum(baseline[0])) ||
                (! isalnum(baseline[1]))) errcode |= BADBASE;
        else strcpy (f_info->baseline, baseline);
        }

    if (freq != NULL)               /* Frequency code, e.g. "X" */
        {
        if (! isalpha(freq[0])) errcode |= BADFREQ;
        else if (freq[1] != '\0') errcode |= BADFREQ;
        else f_info->freq_code = freq[0];
        }
                                    /* Filenumbers only for type 2's in mk4 */
                                    /* originally; -PP for poln information */
    if ((filenum != NULL) && (f_info->type == 2))
        {
        poln = strchr(filenum, '-');
        if (poln != NULL) *poln++ = 0;
        l = strlen(filenum);        /* File #, e.g. (1-9999) */
        if ((l == 0) || (l > MAXFNDIGITS)) errcode |= BADFNUM;
        l--;
        for ( ; l >= 0; l--)
            if (! isdigit (filenum[l])) errcode |= BADFNUM;
        if ((errcode & BADFNUM) == 0)
            sscanf (filenum, "%d", &(f_info->filenum));
                                    /* copy and check pol'n information */
        if (poln != NULL)
            {
            for (l=0; l<2; l++, poln++)
                /* check that each character is in the legal list */
                if ((pp = strpbrk(poln, LEGALPOLCHARS)) &&
                     pp == poln) f_info->poln[l] = *poln;
                else
                    errcode |= BADFNUM;
            f_info->poln[2] = 0;
            }
        }

    if (strlen(rootcode) != 6)
        {
        errcode |= BADROOT;         /* root id code, e.g. "abcdef" */
        }
    else
        {
        for (l = 0; l < 6; l++)
            if(! isalnum (rootcode[l])) errcode |= BADROOT;
        }
    if ((errcode & BADROOT) == 0) strcpy (f_info->rootcode, rootcode);

    if (nfield >= 2 && nfield <= 4) free(buf);
    return (errcode);
    }
