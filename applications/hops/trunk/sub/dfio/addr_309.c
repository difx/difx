/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.  Either way, byte flipping is performed */
/* as necessary by the architecture-dependent macros cp_xxxx() defined  */
/* in bytflp.h                                                          */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                                                                      */
/*      Output:         size            number of bytes read from input */
/*                                      address                         */
/*                      Return value    Address of filled app structure */
/*                                                                      */
/* Created by modifying cjl's addr_309                 rjc 2006.2.6     */
/* added version 1                                     rjc 2010.10.1    */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_309.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_309 *
addr_309 (short version,
          void *address,
          int *size)
    {
    int i, j, k, malloced;
    struct type_309 *t309;
    struct type_309_v0 *t309_v0;
    struct type_309_v1 *t309_v1;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T309_VERSION) t309 = (struct type_309 *)address;
    else
        {
        t309 = (struct type_309 *)malloc (sizeof (struct type_309));
        if (t309 == NULL)
            {
            msg ("Memory allocation failure in addr_309()", 2);
            return (NULL);
            }
        clear_309 (t309);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_309_v0);
        t309_v0 = (struct type_309_v0 *) address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t309->record_id, "309", 3);
        strncpy (t309->version_no, "00", 2);

        cp_int (t309->su, t309_v0->su);
        cp_int (t309->ntones, t309_v0->ntones);

        cp_double (t309->rot, t309_v0->rot);
        cp_double (t309->acc_period, t309_v0->acc_period);
        
        for (i=0; i<16; i++)        // loop over channels
            {
            strcpy (t309->chan[i].chan_name, t309_v0->chan[i].chan_name);
            cp_double (t309->chan[i].freq, t309_v0->chan[i].freq);

            for (j=0; j<16; j++)    // loop over tones
                for (k=0; k<2; k++) // loop over quadratures
                    cp_int (t309->chan[i].acc[j][k], t309_v0->chan[i].acc[j][k]);
            }
        return (t309);
        }
    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_309_v1);
        t309_v1 = (struct type_309_v1 *) address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t309->record_id, "309", 3);
        strncpy (t309->version_no, "01", 2);

        cp_int (t309->su, t309_v1->su);
        cp_int (t309->ntones, t309_v1->ntones);

        cp_double (t309->rot, t309_v1->rot);
        cp_double (t309->acc_period, t309_v1->acc_period);
        
        for (i=0; i<64; i++)        // loop over channels
            {
            strncpy (t309->chan[i].chan_name, t309_v1->chan[i].chan_name, 8);
            cp_double (t309->chan[i].freq, t309_v1->chan[i].freq);

            for (j=0; j<64; j++)    // loop over tones
                for (k=0; k<2; k++) // loop over quadratures
                    cp_int (t309->chan[i].acc[j][k], t309_v1->chan[i].acc[j][k]);
            }
        return (t309);
        }
    else 
        {
        msg ("Unrecognized type 309 record version number %d", 2, version);
        if (malloced) free (t309);
        return (NULL);
        }
    }
