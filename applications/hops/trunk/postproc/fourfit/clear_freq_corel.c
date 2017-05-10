/************************************************************************/
/*                                                                      */
/* Initialize a freq_corel structure, a trivial routine                 */
/*                                                                      */
/*      Inputs:         corel           pointer to freq_corel struct    */
/*                                                                      */
/*      Output:         corel           emptied and deallocated         */
/*                                                                      */
/* Created 16 March 1993 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#include "pass_struct.h"

void
clear_freq_corel (struct freq_corel *corel)
    {
    int i, j;

    corel->freq_code = ' ';
    corel->frequency = 0.0;
    corel->fgroup = ' ';
    for (i=0; i<2; i++)
        {
        corel->ch_usb_lcp[i][0] = '\0';
        corel->ch_usb_rcp[i][0] = '\0';
        corel->ch_lsb_lcp[i][0] = '\0';
        corel->ch_lsb_rcp[i][0] = '\0';
        for (j=0; j<16; j++)
            {
            corel->trk_lcp[i][j] = -1;
            corel->trk_rcp[i][j] = -1;
            corel->mean_lcp_trk_err[i][j] = 0.0;
            corel->mean_rcp_trk_err[i][j] = 0.0;
            }
        corel->bbc_lcp[i] = -1;
        corel->bbc_rcp[i] = -1;
        }
/*     corel->pcal_freq = 0.0; */
/*     corel->pcal_location = -1; */
    for (i=0; i<8; i++) corel->index[i] = 0;
    if (corel->data_alloc) free (corel->data);
    corel->data_alloc = FALSE;
    }
