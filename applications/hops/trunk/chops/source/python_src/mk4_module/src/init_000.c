/************************************************************************/
/*                                                                      */
/* Takes a type_000 record and an absolute filename, and fills in the   */
/* record using the current time for the date field.                    */
/*                                                                      */
/*      Inputs:         t000            Input record                    */
/*                      filename        Full pathname of file           */
/*                                                                      */
/*      Output:         t000            Filled in                       */
/*                                                                      */
/* Created 10 February 1997 by CJL                                      */
/* Modified 2 May 2006 by CJL                                           */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "type_000.h"
#include "fstruct.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

int
init_000 (struct type_000 *t000,
          char *filename)
    {
    int i, err, len, nslash;
    char *dataname, *stripname, date[64];
    time_t tm;
    struct tm *utc_now, *gmtime();
    fstruct f_info;
                                        /* Version fixed at zero */ 
    strncpy (t000->record_id, "000", 3);
    strncpy (t000->version_no, "00", 2);
    strncpy (t000->unused1, "   ", 3);
                                        /* File creation date in ascii */
    tm = time (NULL);
    utc_now = gmtime (&tm);
    sprintf (date, " %04d%03d-%02d%02d%02d ",
                utc_now->tm_year+1900, utc_now->tm_yday + 1,
                utc_now->tm_hour, utc_now->tm_min % 100,
                utc_now->tm_sec % 100);
    strncpy (t000->date, date, 16);
                                        /* Process filename, and check */
                                        /* for validity */
    err = FALSE;
    len = strlen (filename);
    if (len < 23) err = TRUE;
    else if (filename[0] != '/') err = TRUE;
    else
        {
        nslash = 0;
        for (i=len-1; i>=0; i--)
            {
            if (filename[i] == '/')
                {
                nslash++;
                if (nslash == 1) stripname = filename + i + 1;
                if (nslash == 3) 
                    {
                    dataname = filename + i + 1;
                    break;
                    }
                }
            }
        }
    if (nslash != 3) err = TRUE;
    if (check_name (stripname, &f_info) != 0) err = TRUE;

    if (err)
        {
        msg ("Filename '%s' not a valid correlator filename", 2, filename);
        return (-1);
        }

//                               It appears someone has started using
//                               improbably long scan directory names, 
//                               so this sanity check is inappropriate
//                               CJL, 2 May 2006
//    if (strlen (dataname) > 39)
//        {
//        msg ("Filename '%s' is too long (max 39 chars)", 2, filename);
//        return (-1);
//        }

    else
        {
        strcpy (t000->name, dataname);
        return (0);
        }
    }
