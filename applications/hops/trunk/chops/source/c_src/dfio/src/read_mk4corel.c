/************************************************************************/
/*                                                                      */
/* This routine takes a corel filename, and returns a filled-in         */
/* mk4_corel memory structure, ready for use by the caller.             */
/*                                                                      */
/*      Inputs:         filename        Better be a valid corel file    */
/*                                                                      */
/*      Output:         corel           Filled in mk4_corel struct      */
/*                      return value    0 means OK, !=0 means error     */
/*                                                                      */
/* Created 16 December 1996 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define REQUIRE_POWER_TWO 1
#if REQUIRE_POWER_TWO
/* The number of lags is required to be a power of two */
#warning "The number of lags is restricted only by size."
static int is_power_two_required(void) { return(0); }
#else /* REQUIRE_POWER_TWO */
/* Transitional code to allow use in testing. */
static int
is_power_two_required(void)
    {
        char *ev = getenv("HOPS_REQUIRE_POWER_OF_TWO");
        if (!ev) return(1);
        return(atoi(ev));
    }
#warning "The number of lags is controlled by HOPS_REQUIRE_POWER_OF_TWO."
#endif /* REQUIRE_POWER_TWO */

int
read_mk4corel (char *filename,
               struct mk4_corel *corel)
    {
    int i, n, idx, ap, totbytes, type, bytes, size, rec_id, version;
    int lags, blocks, idx_list[MAXIND], nidx, index_val;
    int require_power_two = is_power_two_required();
    void *alloc_ptr;
    char *ptr;
    struct type_101 *temp101;
    struct type_120 *temp120;
    FILE *fp;
    struct type_100 *addr_100 (short, void *, int *);
    struct type_101 *addr_101 (short, void *, int *);
    struct type_120 *addr_120 (short, void *, int *);
                                        /* Standard Mk4 file open call */
    if (open_datafile (filename, &type, &fp) != 0)
        {
        msg ("Cannot open '%s'", 3, filename);
        return (1);
        }
                                        /* Check for major goof */
    if (type != 1)
        {
        msg ("File '%s' fed to read_mk4corel:", 3, filename);
        msg ("This not a type 1 file (type = %d)", 3, type);
        fclose (fp);
        return (1);
        }
                                        /* Read entire file into memory */
                                        /* read_mk4file() takes care */
                                        /* of allocating file_image */
    clear_mk4corel (corel);
    bytes = read_mk4file (fp, &(corel->file_image));
    fclose (fp);
    if (bytes <= 0)
        {
        msg ("Error reading file '%s'", 3, filename);
        return (1);
        }
    corel->allocated[corel->nalloc] = corel->file_image;
    corel->nalloc++;
                                        /* Loop through memory image of */
                                        /* file, setting pointers to records */
                                        /* in corel structure */
    for (i=0; i<MAXIND; i++) idx_list[i] = -1;
    nidx = 0;
    ptr = corel->file_image;
    totbytes = 0;
    lags = blocks = type = 0;
    while (totbytes < bytes)
        {
        n = sscanf (ptr, "%3d%2d", &rec_id, &version);
        if (n != 2)
            {
            msg ("Unrecognized record type in corel file, '%5s'", 2, ptr);
            break;
            }
                                        /* Decode each record */
        alloc_ptr = NULL;
        switch (rec_id)
            {
            case 000:
                corel->id = (struct type_000 *)ptr;
                size = 64;
                break;

            case 100:
                corel->t100 = (struct type_100 *)addr_100 (version, ptr, &size);
                if (corel->t100 != (struct type_100 *)ptr) alloc_ptr = corel->t100;
                lags = MAXLAG;
                if (require_power_two)  /* original, trusted implementation */
                    {
                    while (lags >= 8)
                        {
                        if (corel->t100->nlags == lags) break;
                        lags /= 2;
                        }
                    if (lags < 8)
                        {
                        msg ("Invalid number of lags, %d", 2, corel->t100->nlags);
                        return (1);
                        }
                    }
                else                    /* new era: arbitrary number of lags allowed */
                    {
                    if (lags < corel->t100->nlags)
                        {
                        msg ("Too many lags, %d", 2, corel->t100->nlags);
                        return (1);
                        }
                    lags = corel->t100->nlags;
                    }
                blocks = corel->t100->nblocks;
                break;

            case 101:
                blocks = corel->t100->nblocks;
                temp101 = (struct type_101 *)addr_101 (version, ptr, &size);

                index_val = temp101->index;
                for (idx=0; idx<nidx; idx++)
                    if (idx_list[idx] == index_val) break;
                if (idx == nidx)
                    {
                    if (nidx >= MAXIND)
                        {
                        msg ("Invalid type 101 record in corel file", 2);
                        return (-1);
                        }
                    idx_list[nidx] = index_val;
                    nidx++;
                    }

                if ((temp101->nblocks != blocks) && (blocks > 0))
                    {
                    msg ("Inconsistent type 101 record in corel file", 2);
                    return (-1);
                    }
                                        /*Sanity check that the channel names are present */
                if ( strlen(temp101->ref_chan_id) == 0 || strlen(temp101->rem_chan_id) == 0 )
                    {
                    msg ("Inconsistent type 101 record in corel file, missing channel ID labels", 2);
                    return (-1);
                    }
                                        /* Expand index array if necessary */
                if (corel_alloc (corel, nidx, 0) != 0) return (-1);

                if (temp101 != (struct type_101 *)ptr) alloc_ptr = temp101;
                corel->index[idx].t101 = temp101;
                break;

            case 120:
                                        /* temp copy, get index and AP number */
                temp120 = (struct type_120 *)addr_120 (version, ptr, &size);
                                        /* Check indices first */
                index_val = temp120->index;
                for (idx=0; idx<nidx; idx++)
                    if (idx_list[idx] == index_val) break;
                if (idx == nidx)
                    {
                    if (nidx >= MAXIND)
                        {
                        msg ("Invalid type 101 record in corel file", 2);
                        return (-1);
                        }
                    idx_list[nidx] = index_val;
                    nidx++;
                    }
                                        /* Altogether too many APs */
                ap = temp120->ap;
                if (ap < 0)
                    {
                    msg ("Ignoring illegal type 120 record in corel file, "
                         "idx,ap = %d,%d", 2, idx, ap);
                         break;
                    }

                if (ap > MAXAP)
                    {
                    msg ("Invalid type 120 record in corel file, "
                         "idx,ap = %d,%d", 2, idx, ap);
                    return (-1);
                    }
                if (temp120->nlags != lags)
                    if (lags > 0)
                        {
                        msg ("Inconsistent type 120 record in corel file %d != %d", 2,
                            temp120->nlags, lags);
                        return (-1);
                        }
                                        /* Expand arrays if necessary */
                if (corel_alloc (corel, nidx, ap) != 0) return (-1);

                if (temp120 != (struct type_120 *)ptr) alloc_ptr = temp120;
                corel->index[idx].t120[ap] = temp120;
                break;

                                        /* Ignore and skip KAD/JAB diagnostic */
                                        /* records */
            case 130:
                size = sizeof (T1_R130);
                break;
            case 141:
                size = sizeof (T1_R141);
                break;
            case 142:
                size = sizeof (T1_R142);
                break;
            case 143:
                size = sizeof (T1_R143);
                break;
            case 144:
                size = sizeof (T1_R144);
                break;
            case 150:
                size = sizeof (T1_R150);
                break;

            default:
                msg ("Inappropriate record type %d in corel file", 2, rec_id);
                return (-1);
            }
                                        /* Keep track of allocated pointers */
        if (alloc_ptr != NULL)
            {
            corel->allocated[corel->nalloc] = alloc_ptr;
            corel->nalloc++;
            }
                                        /* Adjust pointer and get next rec. */
        ptr += size;
        totbytes += size;
        }

    if (totbytes != bytes)
        msg ("Last record in '%s' truncated; expected %d got %d bytes",
                2, filename, bytes, totbytes);

    msg ("read_mk4corel read %d bytes from %s", -1, totbytes, filename);
    return (0);
    }
