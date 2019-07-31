/************************************************************************/
/*                                                                      */
/*  Fills in a type_201 record, based on root information               */
/*                                                                      */
/*      Inputs:         root        scan_info struct                    */
/*                      param       param struct pointer                */
/*                                                                      */
/*      Output:         t201        Filled in type_201 record           */
/*                                                                      */
/* Created 31 August 1999 by CJL                                        */
/* put TEC into the dispersion variable          rjc    2014.1.6        */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "param_struct.h"
#include "mk4_data.h"

int fill_201 (struct scan_struct *root,
              struct type_param *param,
              struct type_201 *t201)
    {

    clear_201 (t201);

    strncpy (t201->source, root->src.source_name, 32);
    memcpy (&(t201->coord), &(root->src.position), sizeof (struct sky_coord));
    if (root->src.position_ref_frame == J2000) t201->epoch = 2000;
    else if (root->src.position_ref_frame == B1950) t201->epoch = 1950;
    memcpy (&(t201->coord_date), &(root->src.position_epoch), 
                                         sizeof (struct date));
    t201->ra_rate = root->src.ra_rate;
    t201->dec_rate = root->src.dec_rate;

    t201->dispersion = param->ion_diff;
                                        /* Ignore pulsar parameters for now */
    return (0);
    }

