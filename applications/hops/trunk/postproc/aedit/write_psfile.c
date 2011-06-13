/************************************************************************/
/*                                                                      */
/* Takes a completed ps array and converts the information into a disk  */
/* based PS file.                                                       */
/*                                                                      */
/*      Inputs:         psarray         Complete with pointers and qc's */
/*                      mode            0=Mk3, 1=Mk4                    */
/*                                                                      */
/*      Output:         filename        created on disk                 */
/*                      return value    0=good, 1=bad                   */
/*                                                                      */
/* Created 2 May 1995 by CJL                                            */
/* Modified for Mk4, with G and H codes, Feb 1 2001 by CJL              */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include "psplot.h"

int
write_psfile (psarray, filename, mode)
struct ps_array *psarray;
char *filename;
int mode;
    {
    int i, bnd, scn, bsl, nlines, x, y, nbase, nbands, nscan;
    int max, ndig, total, year, day, hour, minute, second;
    int len, maxlen;
    char qc[4], qcstring[2000], scanstr[15], band, base[3], buf[10];
    char outline[256], format[6];
    struct ps_count *qcount;
    time_t now;
    FILE *fp;
    static char qclist[] = QUALITIES;
    struct psplot_cell *cell;
    struct stat statbuf;
    extern int data_version;

    nbase = psarray->nbaselines;
    nbands = strlen (psarray->subgroups);
    nscan = psarray->nscans;
                                        /* Set up arrays to hold quality code */
                                        /* statistics */
    qcount = (struct ps_count *)calloc ((nbase*nbands + 1), sizeof(struct ps_count));
    if (qcount == NULL)
        {
        msg ("Error allocating memory for psfile summaries", 2);
        return (1);
        }

                                        /* Check out requested filename */
    if (stat (filename, &statbuf) == 0)
        {
        if (statbuf.st_size != 0)
            if (! confirm ("File exists and contains data.  Proceed?")) return (1);
        }
    else if (errno != ENOENT)
        {
        msg ("problem %d with output file %s", 2, errno, filename);
        return (1);
        }
                                        /* Open file and insert header info */
    if ((fp = fopen (filename, "w")) == NULL)
        {
        msg ("Could not open file for output", 2);
        return (1);
        }

    now = time (NULL);
    fprintf(fp,"* This file processed by AEDIT, %s", ctime (&now));

                                        /* What is the longest Mk4 scan id? */
    maxlen = 0;
    if (mode == 1)
        for (scn=0; scn<nscan; scn++)
            {
            len = strlen (psarray->time[scn].scan_name);
            if (len > maxlen) maxlen = len;
            }
    else if (mode == 0) maxlen = 10;
                                        /* Construct output lines, nbands per scan */
    nlines = 21;
    total = 0;
    for (scn=0; scn<nscan; scn++)
        {
                                        /* Periodically write a header */
        if (nlines > 20)
            {
            write_pshdr (fp, psarray, maxlen);
            nlines = 0;
            }
                                        /* Start by constructing scan string */
        if (mode == 0)
            {
            int_to_time (psarray->time[scn].scantime,
                        &year, &day, &hour, &minute, &second);
            second = psarray->time[scn].seconds;
            if (data_version > 1)
                sprintf (scanstr, "%03d-%02d%02d%02d", day, hour, minute, second);
            else
                sprintf (scanstr, "%03d-%02d%02d  ", day, hour, minute);
            }
                                        /* This is Mk4 */
        else if (mode == 1) 
            {
            strcpy (scanstr, psarray->time[scn].scan_name);
            len = strlen (scanstr);
            if (len < maxlen)
                {
                for (i=len; i<maxlen; i++) scanstr[i] = ' ';
                scanstr[maxlen] = '\0';
                }
            }
                                        /* Now loop over bands and get baseline qcodes */
        for (bnd=0; bnd<nbands; bnd++)
            {
            band = psarray->subgroups[bnd];
            qcstring[0] = '\0';
            qc[0] = qc[2] = ' ';
            qc[3] = '\0';
            for (bsl=0; bsl<nbase; bsl++)
                {
                cell = psarray->baseline[bsl].scan + scn;
                qc[1] = qclist[cell->colour_index[bnd]];
                strcat (qcstring, qc);
                                        /* Accumulate statistics */
                x = cell->colour_index[bnd];
                y = bsl*nbands + bnd;
                qcount[y].nqual[x] += 1;
                qcount[nbase*nbands].nqual[x] += 1;
                if (x > 0) 
                    {
                    qcount[y].nqual[21] += 1;
                    qcount[nbase*nbands].nqual[21] += 1;
                    }
                }
            fprintf (fp, "%s %c %s\n", scanstr, band, qcstring);
            nlines++;
            }
        }
                                        /* How many digits shall we use? */
    max = qcount[nbase*nbands].nqual[21];
    ndig = 3;
    if (max > 99) ndig = 4;
    if (max > 999) ndig = 5;
    if (max > 9999) ndig = 6;
    if (max > 99999) ndig = 7;
    sprintf (format, "%%%dd", ndig);
                                        /* Make summary section header */
    sprintf (outline, "Qcodes ");
    sprintf (buf, "        ");
    buf[ndig] = '\0';
    for (i=1; i<21; i++)
        {
        buf[ndig-1] = qclist[i];
        if (i == 19) buf[ndig-1] = 'N';
        strcat (outline, buf);
        }
    buf[ndig-3] = 'T'; buf[ndig-2] = 'o'; buf[ndig-1] = 't';
    strcat (outline, buf);
    fprintf (fp, "\n\n%s\n", outline);
                                        /* Underline */
    for (i=0; i<7+21*ndig; i++) outline[i] = '-';
    outline[i] = '\0';
    fprintf (fp, "%s\n", outline);
                                        /* Write totals */
    for (bsl=0; bsl<nbase; bsl++)
        {
        strcpy (base, psarray->baseline[bsl].id);
        for (bnd=0; bnd<nbands; bnd++)
            {
            band = psarray->subgroups[bnd];
            sprintf (outline, "%s:%c   ", base, band);
            for (i=1; i<22; i++)
                {
                sprintf (buf, format, qcount[bsl*nbands + bnd].nqual[i]);
                strcat (outline, buf);
                }
            fprintf (fp, "%s\n", outline);
            }
        }
                                        /* Underline */
    for (i=0; i<7+21*ndig; i++) outline[i] = '-';
    outline[i] = '\0';
    fprintf (fp, "%s\n", outline);
                                        /* Grand totals */
    sprintf (outline, "Totals ");
    for (i=1; i<22; i++)
        {
        sprintf (buf, format, qcount[nbase*nbands].nqual[i]);
        strcat (outline, buf);
        }
    fprintf (fp, "%s\n", outline);
            

    fclose (fp);
    free (qcount);
    return (0);
    }
