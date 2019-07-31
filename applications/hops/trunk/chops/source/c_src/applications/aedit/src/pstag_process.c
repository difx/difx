/************************************************************************/
/*                                                                      */
/*                                                                      */
/*                                                                      */
/*      Inputs:                                                         */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created  by CJL                                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mk4_afio.h"
#include "aedata.h"
#include "flags.h"
#include "psplot.h"
#include "aedit.h"

void pstag_process (struct ps_array *psarray, esum *data)
    {
    char filename[512], line[256], *s;
    int scan, base, i, zap, nedit, band, nband;
    FILE *fp;
    extern int fscan, fflag, up_to_date;
    extern int data_version, output_version;
    int version;
    fringearray *fdata;
    struct psplot_cell *cell;
                                        /* No-op for no tags */
    if (psarray->ntagged == 0) return;

    zap = FALSE;
    fdata = data->fdata;

    msg ("You have tagged %d points.  In the future, there", 2, 
                                                psarray->ntagged);
    msg ("will be several options here, but for now you can", 2);
    msg ("either zap them (type 'zap'), type a filename", 2);
    msg ("into which the tagged scans will be written, or simply.", 2);
    msg ("hit <return> to do nothing with them", 2);
    printf ("\n\nresponse: ");

    if (!fgets (line, sizeof(line), stdin)) return;
    if ((i = strlen (line)) < 0) return;
    if (line[i-1] == '\n') line[i-1] = 0;
    if (strncmp (line, "zap", 3) == 0) zap = TRUE;
    else
        {
        sscanf (line, "%s %*s", filename);
                                        /* Open file and insert header info */
        if((fp = fopen(filename,"w")) == NULL)
            {
            msg("Could not open file '%s' for output",2, filename);
            return;
            }

        version = data_version;
        if (output_version != 0) version = output_version;
        afile_header (version, 2, fp);
        }

    nedit = 0;
    nband = strlen (psarray->subgroups);
    for (base=0; base<psarray->nbaselines; base++)
        for (scan=0; scan<psarray->nscans; scan++)
            for (band=0; band<nband; band++)
                {
                cell = psarray->baseline[base].scan + scan;
                if (cell->data_index[band] < 0) continue;
                if (cell->flag[band] == 0) continue;
                if (zap) fdata[cell->data_index[band]].flag |= ZAPPED;
#warning "LOOK AT THIS CAREFULLY"
                else if (write_fsumm (fdata+cell->data_index[band], fp) != 0)
                    {
                    msg ("Problem writing out tagged records", 2);
                    fclose (fp);
                    return;
                    }
                nedit++;
                }

    if (zap) 
        {
        fflag += nedit;
        msg("\tSummary of editing on type-2 (fringe) data only:", 2);
        msg("\tNew edits\tFlag\tTotal\tRemaining", 2);
        msg("\t%d\t\t%d\t%d\t%d", 2,nedit,fflag,fscan,fscan-fflag);
        if(nedit > 0) up_to_date = FALSE;
        return;
        }
    else
        {
        msg ("Wrote %d lines into file '%s'", 2, nedit, filename);
        fclose (fp);
        }
    }
