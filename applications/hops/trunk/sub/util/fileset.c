/************************************************************************/
/*                                                                      */
/* Examines the contents of the scan directory in which the supplied    */
/* root file lives, and fills a structure which summarizes the Mk4      */
/* correlator files it finds there, based on the filenames.  The        */
/* resulting information can be used by various programs to decide what */
/* data to work with.                                                   */
/*                                                                      */
/*      Inputs:         rootname        Full pathname of the root file  */
/*                                                                      */
/*      Output:         fset            fileset structure, filled in    */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created March 30 1998 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include "fileset.h"
#include "mk4_util.h"

int
fileset(char *rootname, struct fileset *fset)
    {
    int i, nfil;
    char temp[256], *ptr, *rootcode;
    DIR *dp, *opendir();
    struct dirent *ds, *readdir();
    fstruct fstemp;
                                        /* Initialize */
    fset->expno = 0;
    fset->scandir[0] = '\0';
    fset->scanname[0] = '\0';
    fset->rootname[0] = '\0';
    fset->maxfile = 0;
    for (i=0; i<MAXFSET; i++) clear_fstruct (fset->file + i);
                                        /* Dissect input rootname */
                                        /* First check for sensible input */
    if (rootname[0] != '/')
        {
        msg ("Error in fileset(), input name must be absolute pathname", 2);
        return (-1);
        }
                                        /* Save scan directory to open later */
    strcpy (fset->scandir, rootname);
    ptr = strrchr (fset->scandir, '/');
    *ptr = '\0';
                                        /* Copy out rootname, and get rootcode */
    strcpy (fset->rootname, ptr+1);
    rootcode = strrchr (fset->rootname, '.');
    rootcode++;
                                        /* Make spare copy and dissect out the */
                                        /* scan and experiment directories */
    strcpy (temp, fset->scandir);
    ptr = strrchr (temp, '/');
    if (ptr == NULL)
        {
        msg ("Invalid input to fileset()", 2);
        return (-1);
        }
    *ptr = '\0';
    strcpy (fset->scanname, ptr+1);
    ptr = strrchr (temp, '/');
    if (ptr == NULL)
        {
        msg ("Invalid input to fileset()", 2);
        return (-1);
        }
    if (sscanf (ptr+1, "%hd", &(fset->expno)) != 1)
        {
        msg ("Failure decoding experiment number in fileset()", 2);
        return (-1);
        }
                                        /* Open the scan directory */
    if ((dp = opendir (fset->scandir)) == NULL)
        {
        msg ("Cound not open scan directory '%s'", 2, fset->scandir);
        return (-1);
        }
                                        /* Read and examine all filenames */
    nfil = 0;
    while ((ds = readdir (dp)) != NULL)
        {
                                        /* Ignore these entries */
        if ((strcmp (ds->d_name, ".") == 0) || (strcmp (ds->d_name, "..") == 0))
            continue;
                                        /* Analyze the filename.  Nonzero */
                                        /* return indicates that this is not */
                                        /* a correlator related file, so skip */
        if (check_name (ds->d_name, &fstemp) != 0) continue;
                                        /* Is it a member of this fileset? */
        if (strcmp (rootcode, fstemp.rootcode) != 0) continue;
                                        /* Is this the rootfile? */
                                        /* Make sure it's right, then skip */
        if (fstemp.type == 0)
            {
            sprintf (temp, "%s.%s", fstemp.source, rootcode);
            if (strcmp (temp, fset->rootname) != 0)
                {
                msg ("Found root file with discrepant name '%s'", 2, temp);
                return (-1);
                }
            continue;
            }
                                        /* This is non-root member of fileset */
                                        /* Store the information in fset */
        if (fstemp.filenum > fset->maxfile) fset->maxfile = fstemp.filenum;
        memcpy (fset->file + nfil, &fstemp, sizeof (fstruct));
                                        /* This allocates memory, but only */
                                        /* up to a max of 16 bytes/file, so we */
                                        /* ignore memory leak problem */
        fset->file[nfil].name = strdup (ds->d_name);
        nfil++;
        }

    closedir (dp);
    return (0);
    }

