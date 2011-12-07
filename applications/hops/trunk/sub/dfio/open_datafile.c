/************************************************************************/
/*                                                                      */
/* The function of this routine is to take a data file name and return  */
/* an open file descriptor and file type for a valid Mk4 correlator     */
/* data file.  It checks the contents of the file-ID type 000 record    */
/* which is the first record of any mk4 file.  The open file is         */
/* returned ready for IO on subsequent records.  Substantial cross      */
/* checking is done to ensure the integrity of the filenaming system.   */
/*                                                                      */
/*      Inputs:         filename        name of target file             */
/*                                                                      */
/*      Output:         type            File type number (0, 1, 2 ...)  */
/*                      fp              Open stream                     */
/*                      return value    0=OK, -1=error, +1=file in      */
/*                                      nonstandard directory location  */
/*                                                                      */
/* Created August 3 1995 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include "mk4_data.h"
#include "fstruct.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
open_datafile (char filename[],
               int *type,
               FILE **fp)
    {
    char relname[100], name[20], tempname[256];
    char expdir[10], scandir[20];
    char *ptr;
    struct type_000 id_record;
    struct stat fil_status;
    fstruct f_info;
    extern int errno;
                                        /* Assume current working dir if caller */
                                        /* fails to provide full path */
    if (filename[0] != '/')
        {
        if (getcwd (tempname, 255) == NULL)
            {
            msg ("Error in open_datafile, cannot get current working dir", 3);
            return (-1);
            }
        strcat (tempname, "/");
        }
    else tempname[0] = '\0';
    strcat (tempname, filename);
        
                                        /* filename follows last '/' */
    ptr = strrchr (tempname, '/');
    strcpy (name, ptr+1);
                                        /* Now extract scan directory name */
    if (ptr != NULL) ptr[0] = '\0';
    ptr = strrchr (tempname, '/');
    if (ptr == NULL) scandir[0] = '\0';
    else strcpy (scandir, ptr+1);
                                        /* Finally get expt dir name */
    if (ptr != NULL) ptr[0] = '\0';
    ptr = strrchr (tempname, '/');
    if (ptr == NULL) expdir[0] = '\0';
    else strcpy (expdir, ptr+1);
                                        /* This fills f_info with useful data */
    if (check_name (name, &f_info) != 0)
        {
        msg ("Badly formed data file name '%s'", 3, filename);
        return (-1);
        }
    *type = f_info.type;
                                        /* Standard file status call */
    if (stat (filename, &fil_status) != 0)
        {
        msg ("Problem statting '%s' on read", 3, filename);
        msg ("open_datafile: '%s'", 3, strerror (errno));
        return (-1);
        }
                                        /* Open file */
    if ((*fp = fopen (filename, "r")) == NULL)
        {
        msg ("Could not open '%s'", 3, filename);
        msg ("open_datafile: '%s'", 3, strerror (errno));
        return (-1);
        }
                                        /* Read first 64 bytes */
    if (fread (&id_record, sizeof (struct type_000), 1, *fp) != 1)
        {
        msg ("Problem reading file '%s'", 3, filename);
        msg ("open_datafile: '%s'", 3, strerror (errno));
        fclose (*fp);
        return (-1);
        }
                                        /* Now cross-check record contents */
    if (strncmp (id_record.record_id, "000", 3) != 0)
        {
        msg ("File '%s' does not appear to be a Mk4 data file", 2, filename);
        fclose (*fp);
        return (-1);
        }
                                        /* cross-check given name versus */
                                        /* original name in type-000 record */
    ptr = strrchr (id_record.name, '/');
    if (strcmp (name, ptr+1) != 0)
        {
        msg ("Error: File has been renamed from '%s' to '%s'", 2, ptr+1, name);
        fclose (*fp);
        return (-1);
        }
                                        /* Is the relative pathname proper */
                                        /* for this file? */
    sprintf (relname, "%s/%s/%s", expdir, scandir, name);
    if (strcmp (id_record.name, relname) != 0)
        {
        msg ("Warning, '%s' is not in a proper data directory location", 
                0, filename);
        // return (1);                  only warn user  - rjc 2009.10.8
        }
                                        /* Everything seems OK */
    return (0);
    }
