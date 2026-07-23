/************************************************************************/
/*                                                                      */
/* In order to keep memory requirements in check, and in view of the    */
/* occasional need for very large numbers of index numbers and APs,     */
/* there is a need to dynamically allocate space for the pointers to    */
/* type-101 and type-120 records.  This memory allocation needs to be   */
/* managed often enough in read_mk4corel() that it is worth a separate  */
/* routine.  This is it, as you may by now have deduced.                */
/*                                                                      */
/*      Inputs:         corel           main corel file structure ptr   */
/*                      nidx            Number of idx currently needed  */
/*                      ap              AP currently needed             */
/*                                                                      */
/*      Output:         corel           arrays expanded if necessary    */
/*                                                                      */
/* Created 17 December 1996 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
corel_alloc (struct mk4_corel *corel,
             int nidx,
             int ap)
    {
    int i, j;
                                        /* Index number above array size, */
                                        /* must expand it */
    if (nidx >= corel->index_space)
        {
        if (corel->index_space == 0)
            corel->index = (struct index_tag *)calloc (nidx + 100, 
                                        sizeof (struct index_tag));
        else
            corel->index = (struct index_tag *)realloc (corel->index,
                        (nidx + 100) * sizeof (struct index_tag));
        if (corel->index == NULL)
            {
            msg ("Memory allocation failure in corel_alloc()", 2);
            return (-1);
            }
                                        /* Initialize type 101 pointers */
        for (i=corel->index_space; i<nidx+100; i++) 
            {
            corel->index[i].t101 = NULL;
            corel->index[i].ap_space = 0;
            }
                                        /* Update space count */
        corel->index_space = nidx + 100;
        }
                                        /* Provide requested number of aps */
                                        /* in each idx */
    for (i=0; i<corel->index_space; i++)
        {
        if (ap >= corel->index[i].ap_space)
            {
            if (corel->index[i].ap_space == 0)
                {
                corel->index[i].t120 = 
                        (struct type_120 **)calloc (ap + 100, 
                                sizeof (struct type_120 *));
                }
            else
                {
                corel->index[i].t120 =
                        (struct type_120 **)realloc (corel->index[i].t120,
                                (ap+100) * sizeof (struct type_120 *));
                }
            if (corel->index[i].t120 == NULL)
                {
                msg ("Memory allocation failure in corel_alloc()", 2);
                return (-1);
                }
                                        /* Initialize pointers */
            for (j=corel->index[i].ap_space; j<ap+100; j++) 
                corel->index[i].t120[j] = NULL;
                                        /* Update space count */
            corel->index[i].ap_space = ap + 100;
            }
        }

    return (0);
    }
