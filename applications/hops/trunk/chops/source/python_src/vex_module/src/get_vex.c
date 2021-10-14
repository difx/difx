/************************************************************************/
/*                                                                      */
/* This is the primary interface to the VEX parsing library.  It takes  */
/* a filename and a struct describing which sections of the VEX         */
/* file are wanted, and fills in the appropriate structure trees in     */
/* the main vex struct.  The defines for the vex struct and the keys    */
/* struct are in the include file "vex.h"                               */
/*                                                                      */
/*      Inputs:         filename        Name of input vex file          */
/*                      vextype         Which type of info to get       */
/*                      key             Name of def/scan in the         */
/*                                      specified flavor of vex.  Note  */
/*                                      that svex, cvex, ivex and evex  */
/*                                      are exclusive to Mk4            */
/*                                      correlators. An empty string    */
/*                                      as a vex key means read what    */
/*                                      is assumed to be the only key   */
/*                                      present in the specified        */
/*                                      section of the file.  It is an  */
/*                                      error for there to be more than */
/*                                      one in this case.               */
/*                                                                      */
/*      Output:         vex             Main vex structure              */
/*                                                                      */
/* Created 14 November 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int reread_vexfile = 0;             // flag to indicate previous failure and to
                                    // force file reread in case of possible edit

int
get_vex (char *filename,
         int vextype,
         char *key,
         struct vex *vex)
    {
    static char last_filename[256] = "";
    struct Cvex_Config *cvex_info();
    struct scan_struct *scan_info();
    struct evex_struct *evex_info();
    struct ivex_struct *ivex_info();
    struct svex_struct *svex_info();
    struct lvex_struct *lvex_info();
    extern int do_output;
   
                                       /* sanity check */
    if (*filename == '\0')
        {
        msg ("Null filename, vex parsing failed.", 2);
        return (-1);
        }
                                        /* Is this a new file? */
    if (strcmp (filename, vex->filename) != 0)
        {
        strcpy (vex->filename, filename);
        vex->vextypes = 0;
        }
                                        /* Parse the basic syntax */
                                        /* Assume file hasn't changed */
                                        /* since we last read it. */
                                        // unless there was a failure. rjc 2008.7.22
    if (strcmp (filename, last_filename) != 0 || reread_vexfile)
        {
        if (parse_vexfile (filename) != 0)
            {
            msg ("Low-level parse of '%s' failed.", 2, filename);
            reread_vexfile = 1;         // bad parse - set flag to reread
            return (-1);
            }
        strcpy (last_filename, filename);
        reread_vexfile = 0;             // good parse - clear flag
        }
                                        /* Is output requested? */
    if (vextype & WANT_OUTPUT) do_output = TRUE;
    else do_output = FALSE;
                                        /* Is ovex requested? */
    if (vextype & OVEX)
        {
                                        /* No station filtering */
        vex->ovex = scan_info (key, "");
        if (strcmp (key, "SCAN_LIST") == 0) return (0);
        if (vex->ovex == NULL)
            {
            msg ("Failure parsing observe vex portion of '%s'", 2, filename);
            reread_vexfile = 1;         // bad parse - set flag to reread
            return (-1);
            }
        vex->vextypes |= OVEX;
        reread_vexfile = 0;             // good parse - clear flag
        }
                                        /* Is cvex requested? */
    if (vextype & CVEX)
        {
        vex->cvex = cvex_info (key);
        if (vex->cvex == NULL)
            {
            msg ("Failure parsing corr vex portion of '%s'", 2, filename);
            reread_vexfile = 1;         // bad parse - set flag to reread
            return (-1);
            }
        vex->vextypes |= CVEX;
        reread_vexfile = 0;             // good parse - clear flag
        }
                                        /* Is evex requested? */
    if (vextype & EVEX)
        {
        vex->evex = evex_info (key);
        if (vex->evex == NULL)
            {
            msg ("Failure parsing exper vex portion of '%s'", 2, filename);
            reread_vexfile = 1;         // bad parse - set flag to reread
            return (-1);
            }
        vex->vextypes |= EVEX;
        reread_vexfile = 0;             // good parse - clear flag
        }
                                        /* Is ivex requested? */
    if (vextype & IVEX)
        {
        vex->ivex = ivex_info (key);
        if (vex->ivex == NULL)
            {
            msg ("Failure parsing init vex portion of '%s'", 2, filename);
            reread_vexfile = 1;         // bad parse - set flag to reread
            return (-1);
            }
        vex->vextypes |= IVEX;
        reread_vexfile = 0;             // good parse - clear flag
        }
                                        /* Is svex requested? */
    if (vextype & SVEX)
        {
        vex->svex = svex_info (key);
        if (vex->svex == NULL)
            {
            msg ("Failure parsing SU vex portion of '%s'", 2, filename);
            reread_vexfile = 1;         // bad parse - set flag to reread
            return (-1);
            }
        vex->vextypes |= SVEX;
        reread_vexfile = 0;             // good parse - clear flag
        }
                                        /* Is lvex requested? */
    if (vextype & LVEX)
        {
        vex->lvex = lvex_info (key);
        if (vex->lvex == NULL)
            {
            msg ("Failure parsing log vex portion of '%s'", 2, filename);
            reread_vexfile = 1;         // bad parse - set flag to reread
            return (-1);
            }
        vex->vextypes |= LVEX;
        reread_vexfile = 0;             // good parse - clear flag
        }

    return (0);
    }
