/************************************************************************/
/*                                                                      */
/* Mechanism for modifying the behaviour of various programs in a       */
/* uniform way.  In each program, one uses an extern declaration to     */
/* access the various environment variables the user may have set.      */
/* This routine should be called early by any program whose behaviour   */
/* is under environment variable control.  Any variable which has       */
/* potential or actual application to more than one program should be   */
/* processed here.                                                      */
/*                                                                      */
/*      Inputs:                                                         */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created ?? by CJL                                                    */
/* Modified for UTIL library use 931018 by CJL                          */
/* Added a few Mk4-specific directories 930323 CJL                      */
/*                                                                      */
/************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mk4_util.h"

                                        /* Declare these global, to be */
                                        /* initialized here, but used */
                                        /* throughout program */
char datadir[200];
char scheddir[200];
char afiledir[200];
char textdir[200];
char sysvexdir[200];
char taskdir[200];
char bindir[200];
char tmpdir[200];

void
environment(void)
    {                                   /* Default values */
    static char *datadef = "/correlator/data";
    static char *scheddef = "/correlator/schedules";
    static char *afiledef = "/correlator/afiles";
    static char *textdef = "/correlator/prog/text";
    static char *sysvexdef = "/correlator/sysvex";
    static char *taskdef = "/correlator/task";
    static char *bindef = "/correlator/prog/bin/hppa";
    static char *tmpdef = "/correlator/tmp";
    char *dummy;


    if ((dummy = getenv ("DATADIR")) != NULL) strcpy (datadir, dummy);
    else if ((dummy = getenv ("CORDATA")) != NULL) strcpy (datadir, dummy);
    else strcpy (datadir, datadef);

    if ((dummy = getenv ("SCHEDDIR")) != NULL) strcpy (scheddir, dummy);
    else strcpy (scheddir, scheddef);

    if ((dummy = getenv ("AFILEDIR")) != NULL) strcpy (afiledir, dummy);
    else strcpy (afiledir, afiledef);

    if ((dummy = getenv ("TEXT")) != NULL) strcpy (textdir, dummy);
    else strcpy (textdir, textdef);

    if ((dummy = getenv ("SYSVEX")) != NULL) strcpy (sysvexdir, dummy);
    else strcpy (sysvexdir, sysvexdef);

    if ((dummy = getenv ("TASK")) != NULL) strcpy (taskdir, dummy);
    else strcpy (taskdir, taskdef);

    if ((dummy = getenv ("BIN")) != NULL) strcpy (bindir, dummy);
    else strcpy (bindir, bindef);

    if ((dummy = getenv ("TMP")) != NULL) strcpy (tmpdir, dummy);
    else strcpy (tmpdir, tmpdef);
    return;
    }
