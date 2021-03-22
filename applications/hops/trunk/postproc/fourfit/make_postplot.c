/************************************************************************/
/*                                                                      */
/* Creates a postscript Mk4 fringe plot.  It does this in two parts,    */
/* one graphical and one textual.  The graphical part (plus some graph  */
/* labelling) is done using PGPLOT, and the output is flushed to a      */
/* scratch file.  The file is then read in, and textual information is  */
/* spliced in using a standard postscript font, yielding much more      */
/* compact files than are obtained with PGPLOT vector fonts.            */
/*                                                                      */
/*                                                                      */
/*                                                                      */
/* Created October 1999 by CJL                                          */
/* Refactored into subroutines                    2014.7.30  rjc        */
/************************************************************************/
#include "mk4_data.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <string.h>
#include "pass_struct.h"
#include "ovex.h"

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */


int make_postplot (struct scan_struct *root,
                   struct type_pass *pass,
                   char *fringename,
                   struct type_221 **t221)
    {
    extern char *sprint_char_arr();
    struct stat file_status;
    struct tm *gmtime();
    int fd;
    size_t nb, size, filesize;
    int rc;
    FILE *fp;
    char *pplot, *showpage, *end, trailer[1024];
    static char ps_file[1024];
    double tickinc;
                                        /* Create a temporary file to hold */
                                        /* the postscript output */
    strcpy(ps_file, P_tmpdir "/fourfit_XXXXXX");
    close(mkstemp(ps_file));
    msg ("Temporary postscript filename = '%s'", 0, ps_file);

                                        // insert graphs into postscript file image
    rc = generate_graphs (root, pass, fringename, ps_file, &tickinc);
    if (rc)
        {
        msg ("error generating fringe plot graphs");
        return (-1);
        }
                                        /* Now need to read in the resulting */
                                        /* postscript file.  This is done by */
                                        /* creating a type 221 record in */
                                        /* allocated memory, and reading the */
                                        /* file into the pplot member */
    fp = fopen (ps_file, "r");
                                        /* Map stream pointer onto file */
                                        /* descriptor, and make stat() call */
                                        /* to figure out file size */
    if ((fd = fileno (fp)) < 0)
        {
        msg ("Problem with stream pointer in read_mk4file()", 2);
        return (-1);
        }
    if (fstat (fd, &file_status) != 0)
        {
        msg ("Problem making stat call in read_mk4file()", 2);
        return (-1);
        }
    filesize = file_status.st_size;
                                        /* Add 50,000 to allow for text */
    size = filesize + (size_t)50000;
                                        /* Allocate memory for type_221 record */
    if ((*t221 = (struct type_221 *)malloc (size)) == NULL)
        {
        msg ("Memory allocation error in read_mk4file()", 2);
        return (-1);
        }
                                        /* Initialize it */
    clear_221 (*t221);
                                        /* Make sure we are at start of file */
    rewind (fp);
                                        /* Figure out starting address of the */
                                        /* postscript instructions */
    pplot = (*t221)->pplot;
                                        /* Read file in a single call, let */
                                        /* system figure out best buffering */
    nb = fread (pplot, sizeof(char), filesize, fp);
    pplot[filesize] = 0;                // terminate with null to be safe
                                        /* Did it go OK? */
    if (nb != filesize)
        {
        msg ("Error, expected %zu bytes, read %zu bytes", 2, filesize, nb);
        return (-1);
        }
                                        /* Tidy up */
    fclose (fp);
    unlink (ps_file);
                                        /* Forcibly null-terminate file image */
    if ((end = strstr (pplot, "EOF\n")) != NULL)
        *(end+4) = '\0';
                                        /* Store away trailing part of file */
    if ((showpage = strstr (pplot, "PGPLOT restore showpage")) != NULL)
        {
        strcpy (trailer, showpage);
                                        /* Null terminate what's left */
        showpage[0] = '\0';
        }

                                        // insert text into postscript file image
    generate_text (root, pass, fringename, pplot, tickinc);

                                        /* Re-attach trailing part of file */
    strcat (pplot, trailer);
    (*t221)->ps_length = strlen (pplot);

    return (0);
    }
