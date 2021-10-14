/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_309().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t309            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created by modifying cjl's copy_307                 rjc 2006.2.1     */
/* version 1                                           rjc 2010.10.1    */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_309.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_309 (struct type_309 *t309,
          char **ptr)
    {
    int version;
    int i, j, k;
    struct type_309_v0 *t309_v0;
    struct type_309_v1 *t309_v1;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t309->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T309_VERSION) *ptr = (char *)t309;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_309_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_309()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_309()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t309_v0 = (struct type_309_v0 *) *ptr;
        strncpy (t309_v0->record_id, "309", 3);
        strncpy (t309_v0->version_no, "00", 2);

        cp_int (t309_v0->su, t309->su);
        cp_int (t309_v0->ntones, t309->ntones);

        cp_double (t309_v0->rot, t309->rot);
        cp_double (t309_v0->acc_period, t309->acc_period);
        
        for (i=0; i<16; i++)        // loop over channels
            {
            strcpy (t309_v0->chan[i].chan_name, t309->chan[i].chan_name);
            cp_double (t309_v0->chan[i].freq, t309->chan[i].freq);

            for (j=0; j<16; j++)    // loop over tones
                for (k=0; k<2; k++) // loop over quadratures
                    cp_int (t309_v0->chan[i].acc[j][k], t309->chan[i].acc[j][k]);
            }

        return (sizeof (struct type_309_v0));
        }
    else if (version == 1)
        {
        t309_v1 = (struct type_309_v1 *) *ptr;
        strncpy (t309_v1->record_id, "309", 3);
        strncpy (t309_v1->version_no, "01", 2);

        cp_int (t309_v1->su, t309->su);
        cp_int (t309_v1->ntones, t309->ntones);

        cp_double (t309_v1->rot, t309->rot);
        cp_double (t309_v1->acc_period, t309->acc_period);
        
        for (i=0; i<64; i++)        // loop over channels
            {
            strcpy (t309_v1->chan[i].chan_name, t309->chan[i].chan_name);
            cp_double (t309_v1->chan[i].freq, t309->chan[i].freq);

            for (j=0; j<64; j++)    // loop over tones
                for (k=0; k<2; k++) // loop over quadratures
                    cp_int (t309_v1->chan[i].acc[j][k], t309->chan[i].acc[j][k]);
            }

        return (sizeof (struct type_309_v1));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_309()", 2, version);
        return (-1);
        }
    }
