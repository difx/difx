/************************************************************************/
/*                                                                      */
/* This routine examines the data for one source.                       */
/* It makes sure the data records are consistent with each other, and   */
/* if not, tags the source as invalid (to be skipped over later in the  */
/* program).                                                            */
/*                                                                      */
/*      Inputs:         data            one big seg_data struct array   */
/*                      dsumm           summary struct for one source   */
/*                                                                      */
/*      Output:         dsumm           flagged either valid or invalid */
/*                                                                      */
/* Created May 10 1995 by CJL                                           */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include "average.h"
#include "mk4_util.h"

void
check_source (seg_data *data,
              summary *dsumm)
    {
    int i, start, end, index;
    double ref_freq, old_ref_freq;
    char origin, old_origin, phase_type, old_phase_type, freq_code, old_freq_code;
    char source[31];
    fringesum *fdatum;
    trianglesum *tdatum;
    extern int datatype;
                                        /* Convenience variables */
    start = dsumm->start_index;
    end = dsumm->end_index;
                                        /* Impose some rules */
                                        /* Loop over all data */
    for (i=start; i<=end; i++)
        {
        index = data[i].order;
        fdatum = &(data[index].u.fdata);
        tdatum = &(data[index].u.tdata);
                                        /* Extract relevant quantities */
        if (datatype == 2)
            {
            freq_code = fdatum->freq_code;
            ref_freq = fdatum->ref_freq;
            origin = fdatum->datatype[0];
            phase_type = fdatum->datatype[1];
            strcpy (source, fdatum->source);
            }
        else if (datatype == 3)
            {
            freq_code = tdatum->freq_code;
            ref_freq = tdatum->ref_freq;
            origin = tdatum->datatype[0];
            phase_type = tdatum->datatype[1];
            strcpy (source, tdatum->source);
            }
                                        /* Initialize for first pass */
        if (i == start)
            {
            old_freq_code = freq_code;
            old_ref_freq = ref_freq;
            old_origin = origin;
            old_phase_type = phase_type;
            }
                                        /* Check critical variables for */
                                        /* consistency throughout this source */
        if ((freq_code != old_freq_code) || (ref_freq != old_ref_freq))
            {
            msg ("Input data for source '%s' are mixed up (mixed freq", 3, source);
            msg ("codes and/or mixed reference freqs).  Will skip this source.", 3);
            dsumm->valid = FALSE;
            return;
            }
        if ((origin != old_origin) || (phase_type != old_phase_type))
            {
            msg ("Input data for source '%s' do not have common origin and/or,",
                        3, source);
            msg ("a common phase type.  1st record '%c%c', this record '%c%c'", 3,
                        old_origin, old_phase_type, origin, phase_type);
            msg ("Will skip this source", 3);
            dsumm->valid = FALSE;
            return;
            }
                                        /* Can't handle scan averaged data yet */
        if (origin == 'S')
            {
            msg ("Sorry, but scan-averaged data cannot be further averaged", 3);
            msg ("by this version of the program.  Skipping source '%s'", 3, source);
            dsumm->valid = FALSE;
            return;
            }
        }
                                        /* If we get this far, must be OK */
    dsumm->valid = TRUE;
    return;
    }
