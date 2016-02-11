/************************************************************************/
/*                                                                      */
/* This routine takes a sdata filename, and returns a filled-in         */
/* mk4_sdata memory structure, ready for use by the caller.             */
/*                                                                      */
/*      Inputs:         filename        Better be a valid sdata file    */
/*                                                                      */
/*      Output:         sdata           Filled in mk4_sdata struct      */
/*                      return value    0 means OK, !=0 means error     */
/*                                                                      */
/* Created 11 March 1998 by CJL                                         */
/* Added type_309 record support       rjc  2006.2.6                    */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "mk4_sizes.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
read_mk4sdata (char *filename,
               struct mk4_sdata *sdata)
    {
    int i, n, totbytes, type, bytes, size, rec_id, chan;
    short int version;
    void *alloc_ptr;
    char *ptr, *dummy, chan_id[32];
    FILE *fp;
    struct type_301 *temp_301;
    struct type_302 *temp_302;
    struct type_303 *temp_303;
    struct type_304 **ptr_304;
    struct type_306 **ptr_306;
    struct type_307 **ptr_307;
    struct type_308 **ptr_308;
    struct type_309 **ptr_309;
                                        // prototypes
    struct type_300 *addr_300 (short, void *, int *);
    struct type_301 *addr_301 (short, void *, int *);
    struct type_302 *addr_302 (short, void *, int *);
    struct type_303 *addr_303 (short, void *, int *);
    struct type_304 *addr_304 (short, void *, int *);
    struct type_305 *addr_305 (short, void *, int *);
    struct type_306 *addr_306 (short, void *, int *);
    struct type_307 *addr_307 (short, void *, int *);
    struct type_308 *addr_308 (short, void *, int *);
    struct type_309 *addr_309 (short, void *, int *);
                                        /* Standard Mk4 file open call */
    if (open_datafile (filename, &type, &fp) != 0)
        {
        msg ("Cannot open '%s'", 3, filename);
        return (1);
        }
                                        /* Check for major goof */
    if (type != 3)
        {
        msg ("File '%s' fed to read_mk4sdata:", 3, filename);
        msg ("This not a type 3 file (type = %d)", 3, type);
        fclose (fp);
        return (1);
        }
                                        /* Read entire file into memory */
                                        /* read_mk4file() takes care */
                                        /* of allocating file_image */
    clear_mk4sdata (sdata);
    bytes = read_mk4file (fp, &(sdata->file_image));
    fclose (fp);
    if (bytes <= 0)
        {
        msg ("Error reading file '%s'", 3, filename);
        return (1);
        }
    sdata->allocated[sdata->nalloc] = sdata->file_image;
    sdata->nalloc++;
                                        /* Loop through memory image of */
                                        /* file, setting pointers to records */
                                        /* in corel structure */
    ptr = sdata->file_image;
    totbytes = 0;
    while (totbytes < bytes)
        {
        n = sscanf (ptr, "%3d%2hd", &rec_id, &version);
        if (n != 2)
            {
            msg ("Unrecognized record type in sdata file, '%c%c%c%c%c'", 
                    2, *ptr, *(ptr+1), *(ptr+2), *(ptr+3), *(ptr+4));
            break;
            }
                                        /* Decode each record */
        alloc_ptr = NULL;
        switch (rec_id)
            {
                                        /* Id record trivial */
            case 000:
                sdata->id = (struct type_000 *)ptr;
                size = 64;
                break;

            case 300:
                sdata->t300 = addr_300 (version, ptr, &size);
                if (sdata->t300 != (struct type_300 *)ptr) alloc_ptr = sdata->t300;
                break;

            case 301:
            case 302:
            case 303:
                                        /* Do 301/302/303 together due to */
                                        /* identical logic */
                if (rec_id == 301)
                    {
                    temp_301 = addr_301 (version, ptr, &size);
                    if (temp_301 != (struct type_301 *)ptr) alloc_ptr = temp_301;
                    strcpy (chan_id, temp_301->chan_id);
                    }
                else if (rec_id == 302)
                    {
                    temp_302 = addr_302 (version, ptr, &size);
                    if (temp_302 != (struct type_302 *)ptr) alloc_ptr = temp_302;
                    strcpy (chan_id, temp_302->chan_id);
                    }
                else if (rec_id == 303)
                    {
                    temp_303 = addr_303 (version, ptr, &size);
                    if (temp_303 != (struct type_303 *)ptr) alloc_ptr = temp_303;
                    strcpy (chan_id, temp_303->chan_id);
                    }
                                        /* Identify channel and catch array */
                                        /* overflow condition */
                chan = -1;
                for (i=0; i<MAX_CHAN; i++)
                    {
                    if (sdata->model[i].chan_id[0] == '\0') break;
                    if (strcmp (chan_id, sdata->model[i].chan_id) == 0)
                        {
                        chan = i;       /* This is one we want; note & quit loop */
                        break;
                        }
                    }
                if (i == MAX_CHAN)
                    {
                    msg ("Error, too many channel ids found %s", 2, filename);
                    return (-1);
                    }
                                        /* This is a previously unencountered */
                                        /* channel identifier */
                if (chan < 0)
                    {
                    chan = i;
                    strcpy (sdata->model[chan].chan_id, chan_id);
                    }
                                        /* Find next empty entry in spline */
                                        /* array, catching array overflow */
                for (i=0; i<MAXSPLINES; i++)
                    {
                    if (rec_id == 301)
                        {
                        if (sdata->model[chan].t301[i] == NULL)
                            break;
                        }
                    else if (rec_id == 302)
                        {
                        if (sdata->model[chan].t302[i] == NULL) 
                            break;
                        }
                    else if (rec_id == 303)
                        {
                        if (sdata->model[chan].t303[i] == NULL) 
                            break;
                        }
                    }
                if (i == MAXSPLINES)
                    {
                    msg ("Error, too many splines found in %s", 2, filename);
                    return (-1);
                    }
                                        /* Assign pointer to correct location */
                if (rec_id == 301) 
                    sdata->model[chan].t301[i] = temp_301;
                else if (rec_id == 302) 
                    sdata->model[chan].t302[i] = temp_302;
                else if (rec_id == 303) 
                    sdata->model[chan].t303[i] = temp_303;
                break;

                                        /* Types 304/6/8 all of the same form */
                                        /* Do not attempt to sort them, just add */
                                        /* them in as they occur */
            case 304:
                if (sdata->n304 >= MAXSTATPER)
                    {
                    msg ("Error, too many type 304 records in %s", 2, filename);
                    return (-1);
                    }
                ptr_304 = sdata->t304 + sdata->n304;
                *ptr_304 = addr_304 (version, ptr, &size);
                if (*ptr_304 != (struct type_304 *)ptr) alloc_ptr = *ptr_304;
                sdata->n304 += 1;
                break;

            case 306:
                if (sdata->n306 >= MAXSTATPER)
                    {
                    msg ("Error, too many type 306 records in %s", 2, filename);
                    return (-1);
                    }
                ptr_306 = sdata->t306 + sdata->n306;
                *ptr_306 = addr_306 (version, ptr, &size);
                if (*ptr_306 != (struct type_306 *)ptr) alloc_ptr = *ptr_306;
                sdata->n306 += 1;
                break;

            case 307:
                if (sdata->n308 >= MAXSTATPER)
                    {
                    msg ("Error, too many type 307 records in %s", 2, filename);
                    return (-1);
                    }
                ptr_307 = sdata->t307 + sdata->n307;
                *ptr_307 = addr_307 (version, ptr, &size);
                if (*ptr_307 != (struct type_307 *)ptr) alloc_ptr = *ptr_307;
                sdata->n307 += 1;
                break;

            case 308:
                if (sdata->n308 >= MAXSTATPER)
                    {
                    msg ("Error, too many type 308 records in %s", 2, filename);
                    return (-1);
                    }
                ptr_308 = sdata->t308 + sdata->n308;
                *ptr_308 = addr_308 (version, ptr, &size);
                if (*ptr_308 != (struct type_308 *)ptr) alloc_ptr = *ptr_308;
                sdata->n308 += 1;
                break;
            
            case 309:
                if (sdata->n309 >= MAXSTATPER)
                    {
                    msg ("Error, too many type 309 records in %s", 2, filename);
                    return (-1);
                    }
                ptr_309 = sdata->t309 + sdata->n309;
                *ptr_309 = addr_309 (version, ptr, &size);
                if (*ptr_309 != (struct type_309 *)ptr) alloc_ptr = *ptr_309;
                sdata->n309 += 1;
                break;

                                        /* Throw away raw records for now */
            case 305:
                dummy = (char *)addr_305 (version, ptr, &size);
                if (dummy != ptr) free (dummy);
                break;
                
            default:
                msg ("Inappropriate record type %d in sdata file", 2, rec_id);
                totbytes = bytes;
                size = 0;
                break;
            }
                                        /* Keep track of allocated pointers */
        if (alloc_ptr != NULL)
            {
            sdata->allocated[sdata->nalloc] = alloc_ptr;
            sdata->nalloc++;
            }
                                        /* Adjust pointer and get next rec. */
        ptr += size;
        totbytes += size;
        }

    if (totbytes != bytes)
        msg ("Last record in '%s' truncated (%d, %d)", 2, filename,
                totbytes, bytes);

    return (0);
    }
