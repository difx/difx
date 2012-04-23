/*****************************************************************************
*  Called by fourmer to generate merged type-1 and -3 files                  *
*                                                                            *
*       Input:  two root files to be merged, and output rootcode             *
*                                                                            *
*       Output: new merged type-1 and -3 files, using                        *
*                 original file prefix with new rootcode                     *
*                                                                            *
*  Written June-September 2009 by T. Cappallo                                *
*  modified to get rid of multiple type1 data arrays   rjc  2011.2.18        *
*                                                                            *
*  (c) 2009  MIT Haystack Observatory                                        *
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "fileset.h"
#include "mk4_data.h"
#include "mk4_sizes.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#include "fourmer.h"

    struct mk4_corel cdataA,        // corel data for file A,
                     cdataB,        //   for B,
                     cdataC;        //   and the merged output data
    
    struct mk4_sdata sdataA,        // same as above, for station data
                     sdataB,
                     *sdataC;

static void relabel_chan_ids(struct type_101 *, int isA);

int do_record_merge(char *fileAName, char *fileBName,
                    char *rootfile, char *rcode)
    {
    
    struct fileset fsetA, fsetB;
    fstruct *fsA, *fsB;
    
    struct type_101 *t101,
                    *t101a,
                    *t101b;
    struct type_120 **t120;
    
    struct index_tag *idxa,
                     *idxb,
                     *idxc;
    
    int i = -1,
        j = -1,
        k = -1,
        n = 0, new_idx,
        rc,
        nindex_A_save,
        ap = 0;
    
    int index_map[NUM_CH_MAP][2];    // for index reassignment
                                     // [...][0] old #  [...][1] new #

    char fileAFullName[MAX_FPATH],   // absolute paths to files
         fileBFullName[MAX_FPATH],
         fileFullName[MAX_FPATH],
         temp[MAX_FPATH],
         temp2[MAX_FPATH],
         *pchar,
         fullname[MAX_FPATH];
         
    msg("beginning do_record_merge, MAX_CHAN_PP == %d", 0, MAX_CHAN_PP);
         
    get_abs_path (fileAName, fileAFullName);
    get_abs_path (fileBName, fileBFullName);
    
    fileset (fileAFullName, &fsetA);
                        // loop through all type-1 files related to A
    msg ("fsetA.maxfile is %d", 1, fsetA.maxfile);
    while (fsetA.file[++k].type > 0)
        {
        fsA = fsetA.file + k;
        msg("[%d] foundA: %s\ttype: %d", 1, k, fsA->name, fsA->type);
        if (fsA->type == 1)
            {
                                    // initialize index_map
            for (n = 0; n < NUM_CH_MAP; n++)
                index_map[n][1] = index_map[n][0] = -1;
            i = j = -1;
            n = ap = 0;
                
            strcpy (temp2, fsA->name);
            strcpy (fullname, fsetA.scandir);
            strcat (fullname, "/");
            strcat (fullname, fsA->name);
            rc = read_mk4corel (fullname, &cdataA);
            msg ("  read_mk4corel() returned %d", 1, rc);
            msg ("fsA->name == %s", 1, fsA->name);

            msg ("Scanning for file B...", 1);
    
            i = -1;
            fileset (fileBFullName, &fsetB);
            msg ("fsetB.maxfile is %d", 1, fsetB.maxfile);
    
                            // find matching (same baseline) file for B
            rc = -2;
            while (fsetB.file[++i].type > 0)
                {
                fsB = fsetB.file + i;
                msg("[%d] foundB: %s\ttype: %d", 1, i, fsB->name, fsB->type);
                if (fsB->type == 1)
                    {
                    if (!(fsA->name[0] == fsB->name[0] &&
                          fsA->name[1] == fsB->name[1]))
                        continue;
                    strcpy (fullname, fsetB.scandir);
                    strcat (fullname, "/");
                    strcat (fullname, fsB->name);
                    rc = read_mk4corel (fullname, &cdataB);
                    msg("  read_mk4corel() returned %d", 1, rc);
                    msg("fsB->name == %s", 1, fsB->name);
                    break;  // got it!
                    }
                }

            if (rc < 0)
                {
                msg("Unable to find a match for %s, continuing", 2, fsA->name);
                // fprintf(stderr, "Unable to find a match for A?\n");
                // read_mk4corel() does an internal clear_mk4corel ()
                continue;
                }

            msg("----> index_map[0]=%x, index_map[MAX]=%x, A,B nindex=%d,%d",
                0, index_map[0], index_map[NUM_CH_MAP-1],
                cdataA.t100->nindex, cdataB.t100->nindex);

            if (cdataA.t100->nindex + cdataB.t100->nindex > NUM_CH_MAP)
                {
                fprintf(stderr, "Too many indices to remap\n");
                return(1);
                }

                            // initialize index_map with A's indices
            for (i = 0; i < cdataA.t100->nindex; i++)
                {
                idxa = cdataA.index + i;
                if ((t101a = idxa->t101) == NULL) 
                    continue;
                msg("encountered A index %d", 0, t101a->index);
                index_map[i][0] = index_map[i][1] = t101a->index;
                }

                            // if indices collide, make B's unique
            for (j = 0; j < cdataB.t100->nindex; j++)
                {
                idxb = cdataB.index + j;
                if ((t101b = idxb->t101) == NULL)
                    continue;
                msg("encountered B index %d", 0, t101b->index);
                new_idx = index_map[i+j][0] = t101b->index;
                for (n = 0; n < NUM_CH_MAP; n++)
                    while (new_idx == index_map[n][1])
                        new_idx += IDX_INC;
                index_map[i+j][1] = new_idx;
                }
        
                            // debug info
            for (n = 0; n < NUM_CH_MAP; n++)
                if (index_map[n][0] != -1)
                    msg("index_map[%d] == %d->%d", 0,
                        n, index_map[n][0], index_map[n][1]);
    
                            // rewrite actual indices in the data structure
            for (j = 0; j < cdataB.t100->nindex; j++)
                {
                idxb = cdataB.index + j;
                if ((t101 = idxb->t101) == NULL) 
                    continue;
                for (n = i; n < NUM_CH_MAP; n++)
                    if (t101->index == index_map[n][0])
                        {
                        msg("i=%d: changed %d to %d",
                            0, j, t101->index, index_map[n][1]);
                        t101->index = index_map[n][1];
                        }
                }
                            // rewrite indices in the t120s as well
            for (j = 0; j < cdataB.t100->nindex; j++)
                {
                idxb = cdataB.index + j;
                if (idxb->t101 == NULL) 
                    continue;
                if ((t120 = idxb->t120) == NULL) 
                    continue;
        
                for (ap = 0; ap < idxb->ap_space; ap++)
                    {
                    if (t120[ap] == NULL) continue;
            
                    for (n = i; n < NUM_CH_MAP; n++)
                        if (t120[ap]->index == index_map[n][0])
                            t120[ap]->index = index_map[n][1];
                    }
                }


                            // copy A's data into C
            memcpy(&cdataC, &cdataA, sizeof (struct mk4_corel));
    
                            // save len of A before clobbering it by C version
            nindex_A_save = cdataA.t100->nindex;
                            // C is the sum of A and B for records and indices
            cdataC.t100->ndrec = cdataA.t100->ndrec + cdataB.t100->ndrec;
            cdataC.t100->nindex = cdataA.t100->nindex + cdataB.t100->nindex;

            msg ("new ndrec: %d = %d + %d", 0, cdataC.t100->ndrec,
                cdataC.t100->ndrec - cdataB.t100->ndrec, cdataB.t100->ndrec);
            msg ("new nindex: %d = %d + %d", 0, cdataC.t100->nindex,
                cdataC.t100->nindex - cdataB.t100->nindex, cdataB.t100->nindex);

                            // adjust index space to new number of index items
            while (cdataC.t100->nindex > cdataC.index_space)
                {
                cdataC.index_space += IDX_INC;
                cdataA.index = (struct index_tag *)
                    realloc ((void *)cdataA.index, 
                        cdataC.index_space * sizeof (struct index_tag));
                }

                            // point C copy to (possibly) enlarged index array 
            cdataC.index = cdataA.index;
            msg ("old index_space %d new index_space %d", 0, 
                    cdataA.index_space, cdataC.index_space);
            
                            // copy B's data onto end of C and adjust markers
            memcpy((void *) (cdataC.index + nindex_A_save
                            ),//* sizeof (struct index_tag)),
                    cdataB.index,
                    cdataB.t100->nindex * sizeof (struct index_tag));
    
            for (i=0; i< cdataC.t100->nindex; i++)
                {
                idxc = cdataC.index + i;
                relabel_chan_ids(idxc->t101, i < nindex_A_save);
                msg ("i %d index %hd primary %hd ref %s rem %s", 0,
                        i, idxc->t101->index, idxc->t101->primary,
                           idxc->t101->ref_chan_id, idxc->t101->rem_chan_id);
                }
                            // initialize unused index space to null
            for (i=cdataC.t100->nindex; i<cdataC.index_space; i++)
                {
                idxc = cdataC.index + i;
                idxc->t101 = NULL;
                idxc->ap_space = 0;
                }

            pchar = strrchr (rootfile, '/');
            msg ("new rootname = %s", 1, pchar+1);
            strcpy(cdataC.t100->rootname, pchar);
    
            strncpy(cdataC.id->name + strlen(cdataC.id->name) - 6, rcode, 6);
    
            strncpy(temp2 + strlen(temp2) - 6, rcode, 6);
            msg("new filename = %s", 1, temp2);

                            // a rather verbose diagnostic
            if (msglev < 0)
                print_cdata_cmp(fsA->name, fsB->name, &cdataA, &cdataB);

            msg("Overwriting t120 rootcodes", 1);
            for (i = 0; i < cdataC.t100->nindex; i++)
                {
                idxc = cdataC.index + i;
                if (idxc->t101 == NULL) 
                    continue;
                if ((t120 = idxc->t120) == NULL) 
                    continue;
        
                for (ap = 0; ap < idxc->ap_space; ap++)
                    {
                    if (t120[ap] == NULL)
                        continue;
                    strncpy(t120[ap]->rootcode, rcode, 6);
                    }
                }
            rc = write_mk4corel (&cdataC, temp2);
            msg ("write_mk4corel() returned %d\n", 1, rc);
                                    // data written, now clean up memory arrays
            clear_mk4corel (&cdataA);
            cdataA.index = NULL;
                                    // don't try to free A's former memory
            cdataB.nalloc = 0; 
            cdataB.index_space = 0; 
            cdataB.index = NULL;

            clear_mk4corel (&cdataB);
                                    // don't try to free A's former memory
            cdataC.nalloc = 0; 
            cdataC.index_space = 0; 
            cdataC.index = NULL;

            clear_mk4corel (&cdataC);
            }
        }                           // bottom of type 1 file while loop

    msg("Finished with type 1 (corel) files, on to type 3 (station) files", 1);

    i = j = -1;
    fileset(fileAFullName, &fsetA);
    fileset(fileBFullName, &fsetB);
    
                            // loop through type-3 (station) files
    while (fsetA.file[++i].type > 0)
        {
        fsA = fsetA.file + i;
        fsB = fsetB.file + i;
        
        msg("[%d] foundA: %s\ttype: %d", 1, i, fsA->name, fsA->type);
        if (fsA->type == 3)
            {
            strcpy (fullname, fsetA.scandir);
            strcat (fullname, "/");
            strcat (fullname, fsA->name);
            msg("  read_mk4sdata(%s) returned %d\n", 1,
                fsA->name, read_mk4sdata(fullname, &sdataA));

            j = 0;
            do              // find matching B file
                {
                fsB = fsetB.file + j++;
                if (fsB->type < 0) break;
                strcpy(temp, fsA->name);
                strncpy(temp + strlen(temp) - 6, rcode, 6);
                strcpy(temp2, fsB->name);
                strncpy(temp2 + strlen(temp2) - 6, rcode, 6);
                }
            while (strcmp(temp, temp2) != 0);

            if (fsB->type < 0)
                {
                msg("No match for %s, continuing", 2, fsA->name);
                continue;
                }
            
            strcpy (fullname, fsetB.scandir);
            strcat (fullname, "/");
            strcat (fullname, fsB->name);
            msg("  read_mk4sdata(%s) returned %d", 1,
                fsB->name, read_mk4sdata(fullname, &sdataB));
            msg("new filename = %s", 1, temp);
            msg("&sdataA = %p, &(sdataA.id) = %p", 1, &sdataA, &(sdataA.id));
            msg("char = %c", -1, sdataA.id->record_id[0]);
            msg("testgrab of 301.delay_spline = %f", 0,
                sdataA.model[0].t301[0]->delay_spline[0]);

            get_abs_path(temp, fileFullName);
                            // copy A's data into C
            sdataC = malloc(sizeof(sdataA));
            memcpy(sdataC, &sdataA, sizeof(sdataA));
                            // append copy B to copy C
                            // editing channel names as required
            append_sdata(sdataC, &sdataB);

            msg("  write_mk4sdata(C) returned %d", 1,
                write_mk4sdata(sdataC, fileFullName));
            }
        }

    return 0;
    }

/*
 * Update the reference and remote channel id's with gen_new_chan_id()
 * which returns a (null-terminated) name for the new channel.
 */
static void relabel_chan_ids(struct type_101 *t101, int isA)
{
    strcpy(t101->ref_chan_id, gen_new_chan_id(t101->ref_chan_id, isA, 0));
    strcpy(t101->rem_chan_id, gen_new_chan_id(t101->rem_chan_id, isA, 0));
}

/*
 * eof
 */
