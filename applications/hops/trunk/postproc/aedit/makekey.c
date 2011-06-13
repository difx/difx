/************************************************************************/
/*                                                                      */
/* This routine fills up the keyval field in the data array by          */
/* converting (where necessary) the field corresponding to the key      */
/* number to an integer.  This makes the sort operation itself simpler, */
/* because we are left with a simple integer comparison.  The values    */
/* need only be calculated once, instead of on every sort pass.         */
/*                                                                      */
/* Because it does no harm and may do much good, the operation is       */
/* performed on flagged as well as unflagged records.                   */
/*                                                                      */
/*      Inputs:         key             Sort key id number              */
/*                                                                      */
/*      Output:         data            keyval fields modified          */
/*                      return value    0 = OK, -1 = bad key            */
/*                                                                      */
/* Created 10 April 1990 by CJL                                         */
/* Modified to support root and corel data, 28 February 1994 by CJL     */
/* Modified to support closure data, 29 August 1994 by CJL              */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"
#include "sort.h"

#define qflist "FDCBA0E123456789 "

int
makekey (dptr, key, type)
char *dptr;
int key, type;
    {
    extern int fscan, cscan, rscan, tscan, qscan,  data_version;
    extern struct datasumm fsumm, csumm, rsumm, tsumm, qsumm;
    struct source_info sinfo[MAXSRC];
    int i, j, n, npts, value, keyval;
    char quality, dummy[10], id[7];
    rootarray *rdata;
    corelarray *cdata;
    fringearray *fdata;
    trianglearray *tdata;
    quadarray *qdata;
    rootsum *rdatum;
    corelsum *cdatum;
    fringesum *fdatum;
    trianglesum *tdatum;
    quadsum *qdatum;
                                        /* Sort out pointers */
    if (type == 0) rdata = (rootarray *)dptr;
    else if (type == 1) cdata = (corelarray *)dptr;
    else if (type == 2) fdata = (fringearray *)dptr;
    else if (type == 3) tdata = (trianglearray *)dptr;
    else if (type == 4) qdata = (quadarray *)dptr;
    else
        {
        msg ("Invalid data type '%d' passed to makekey()", 2, type);
        return (-1);
        }

                                        /* Source name is a special case */
    n = 0;                              /* Must generate local alphabetical list */
    if (type == 0) npts = rscan;
    else if (type == 1) npts = cscan;
    else if (type == 2) npts = fscan;
    else if (type == 3) npts = tscan;
    else if (type == 4) npts = qscan;
    if (key == S_SOURCENAME)
        for (i=0; i<npts; i++)
                {
                if (n >= MAXSRC)
                    {
                    msg ("Too many (%d) sources in makekey", 2, n);
                    return (-1);
                    }
                if      (type == 0) n += update_sinfo (sinfo, rdata[i].data.source, n);
                else if (type == 1) n += update_sinfo (sinfo, cdata[i].data.source, n);
                else if (type == 2) n += update_sinfo (sinfo, fdata[i].data.source, n);
                else if (type == 3) n += update_sinfo (sinfo, tdata[i].data.source, n);
                else if (type == 4) n += update_sinfo (sinfo, qdata[i].data.source, n);
                }

    for (i=0; i<npts; i++) 
        {
        if (type == 0)      rdatum = &(rdata[i].data);
        else if (type == 1) cdatum = &(cdata[i].data);
        else if (type == 2) fdatum = &(fdata[i].data);
        else if (type == 3) tdatum = &(tdata[i].data);
        else if (type == 4) qdatum = &(qdata[i].data);

        switch (key) 
            {

                                        /* If key is timetag and versions are */
                                        /* mixed, must use only 60sec resolution */
            case S_TIMETAG:
                if (type == 0)      keyval = rdata[i].data.time_tag;
                else if (type == 1) keyval = cdata[i].data.time_tag;
                else if (type == 2) keyval = fdata[i].data.time_tag;
                else if (type == 3) keyval = tdata[i].data.time_tag;
                else if (type == 4) keyval = qdata[i].data.time_tag;

                if (data_version == 0) keyval = (keyval / 60) * 60;

                if (type == 0)      rdata[i].keyval = keyval;
                else if (type == 1) cdata[i].keyval = keyval;
                else if (type == 2) fdata[i].keyval = keyval;
                else if (type == 3) tdata[i].keyval = keyval;
                else if (type == 4) qdata[i].keyval = keyval;
                break;

            case S_PROCDATE:
                if (type == 2) fdata[i].keyval = fdata[i].data.procdate;
                else if (type == 1) cdata[i].keyval = cdata[i].data.procdate;
                else rdata[i].keyval = rdata[i].data.procdate;
                break;

            case S_SNR:
                if (type == 2) fdata[i].keyval = (int)(fdatum->snr * 1000);
                else tdata[i].keyval = (int)(tdatum->bis_snr * 100);
                break;

            case S_LENGTH:
                fdata[i].keyval = fdatum->length;
                break;

            case S_BASELINE:
                if (type == 2) fdata[i].keyval = fdata[i].data.baseline[1]
                                        + 256 * fdata[i].data.baseline[0];
                else if (type == 1) cdata[i].keyval = cdata[i].data.baseline[1]
                                        + 256 * cdata[i].data.baseline[0];
                break;

            case S_TRIANGLE:
                tdata[i].keyval = tdata[i].data.triangle[2]
                                        + 256 * tdata[i].data.triangle[1]
                                        + 65536 * tdata[i].data.triangle[0];
                break;

            case S_FREQUENCY:
                if (type == 2) fdata[i].keyval = fdatum->freq_code;
                else if (type == 3) tdata[i].keyval = tdatum->freq_code;
                else if (type == 4) qdata[i].keyval = qdatum->freq_code;
                break;

            case S_SOURCENAME:
                if (type == 0)      strcpy (dummy, rdata[i].data.source);
                else if (type == 1) strcpy (dummy, cdata[i].data.source);
                else if (type == 2) strcpy (dummy, fdata[i].data.source);
                else if (type == 3) strcpy (dummy, tdata[i].data.source);
                else if (type == 4) strcpy (dummy, qdata[i].data.source);
                for (j=0; j<n; j++) 
                    {
                    if (strcmp (dummy, sinfo[j].name) == 0) 
                        {
                        if (type == 0)      rdata[i].keyval = j;
                        else if (type == 1) cdata[i].keyval = j;
                        else if (type == 2) fdata[i].keyval = j;
                        else if (type == 3) tdata[i].keyval = j;
                        else if (type == 4) qdata[i].keyval = j;
                        break;
                        }
                    }
                    break;

            case S_QCODE:
                if      (type == 1) quality = cdata[i].data.quality;
/*                 else if (type == 2) quality = fdata[i].data.quality; */
                else if (type == 2) quality = mk3_qf (fdatum);
                else if (type == 3) quality = tdata[i].data.scan_quality;
                else if (type == 4) quality = qdata[i].data.quality;
                for (j=0; j<17; j++)
                        if (quality == qflist[j]) break;
                if      (type == 1) cdata[i].keyval = j;
                else if (type == 2) fdata[i].keyval = j;
                else if (type == 3) tdata[i].keyval = j;
                else if (type == 4) qdata[i].keyval = j;
                break;

            case S_EXPERIMENT:
                if (type == 0)      rdata[i].keyval = rdata[i].data.expt_no;
                else if (type == 1) cdata[i].keyval = cdata[i].data.expt_no;
                else if (type == 2) fdata[i].keyval = fdata[i].data.expt_no;
                else if (type == 3) tdata[i].keyval = tdata[i].data.expt_no;
                else if (type == 4) qdata[i].keyval = qdata[i].data.expt_no;
                break;

            case S_ROOTCODE:
                if (type == 2) strcpy (id, fdata[i].data.root_id);
                else if (type == 1) strcpy (id, cdata[i].data.root_id);
                else strcpy (id, rdata[i].data.root_id);
                value = 11881376 * (id[0] - 'a')
                        + 456976 * (id[1] - 'a')
                        + 17576 * (id[2] - 'a')
                        + 676 * (id[3] - 'a')
                        + 26 * (id[4] - 'a')
                        + (id[5] - 'a');
                if (type == 2) fdata[i].keyval = value;
                else if (type == 1) cdata[i].keyval = value;
                else rdata[i].keyval = value;
                break;

            default:            /* Bad key value */
                return (-1);
            }                           /* End switch */
        }
    return (0);
    }
