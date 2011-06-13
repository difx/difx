/************************************************************************/
/*                                                                      */
/* This routine takes a fringe filename, and returns a filled-in        */
/* mk4_fringe memory structure, ready for use by the caller.            */
/*                                                                      */
/*      Inputs:         filename        Better be a valid fringe file   */
/*                                                                      */
/*      Output:         fringe          Filled in mk4_fringe struct     */
/*                      return value    0 means OK, !=0 means error     */
/*                                                                      */
/* Created 2 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
read_mk4fringe (char *filename,
                struct mk4_fringe *fringe)
    {
    int i, n, totbytes, type, bytes, size, rec_id, version;
    void *alloc_ptr;
    char *ptr;
    FILE *fp;
                                        // prototypes
    struct type_200 *addr_200 (short, void *, int *);
    struct type_201 *addr_201 (short, void *, int *);
    struct type_202 *addr_202 (short, void *, int *);
    struct type_203 *addr_203 (short, void *, int *);
    struct type_204 *addr_204 (short, void *, int *);
    struct type_205 *addr_205 (short, void *, int *);
    struct type_206 *addr_206 (short, void *, int *);
    struct type_207 *addr_207 (short, void *, int *);
    struct type_208 *addr_208 (short, void *, int *);
    struct type_210 *addr_210 (short, void *, int *);
    struct type_212 *addr_212 (short, void *, int *);
    struct type_221 *addr_221 (short, void *, int *);
    struct type_230 *addr_230 (short, void *, int *);
                                        /* Standard Mk4 file open call */
    if (open_datafile (filename, &type, &fp) != 0)
        {
        msg ("Cannot open '%s'", 3, filename);
        return (1);
        }
                                        /* Check for major goof */
    if (type != 2)
        {
        msg ("File '%s' fed to read_mk4fringe:", 3, filename);
        msg ("This not a type 2 file (type = %d)", 3, type);
        fclose (fp);
        return (1);
        }
                                        /* Read entire file into memory */
                                        /* read_mk4file() takes care */
                                        /* of allocating file_image */
    clear_mk4fringe (fringe);
    bytes = read_mk4file (fp, &(fringe->file_image));
    fclose (fp);
    if (bytes <= 0)
        {
        msg ("Error reading file '%s'", 3, filename);
        return (1);
        }
    fringe->allocated[fringe->nalloc] = fringe->file_image;
    fringe->nalloc++;
    msg ("Adding memory block %d to allocated list", -1, fringe->file_image);
                                        /* Loop through memory image of */
                                        /* file, setting pointers to records */
                                        /* in fringe structure */
    ptr = fringe->file_image;
    totbytes = 0;
    while (totbytes < bytes)
        {
        n = sscanf (ptr, "%3d%2d", &rec_id, &version);
        msg ("fringe record has %d fields with id %d version %d", 0, n, rec_id, version);
        if (n != 2)
            {
            msg ("Unrecognized record type in fringe file, '%c%c%c%c%c'", 
                2, *ptr, *(ptr+1), *(ptr+2), *(ptr+3), *(ptr+4));
            break;
            }
        msg ("Found record %03d, version %02d", 0, rec_id, version);
                                        /* Decode each record */
        alloc_ptr = NULL;
        switch (rec_id)
            {
                                        /* Id record trivial */
            case 000:
                fringe->id = (struct type_000 *)ptr;
                size = 64;
                msg ("Type 0 record address = %X", -2, fringe->id);
                break;

            case 200:
                fringe->t200 = (struct type_200 *)addr_200 (version, ptr, &size);
                if (fringe->t200 != (struct type_200 *)ptr) alloc_ptr = fringe->t200;
                msg ("Type 200 record address = %X", -2, fringe->t200);
                break;

            case 201:
                fringe->t201 = (struct type_201 *)addr_201 (version, ptr, &size);
                if (fringe->t201 != (struct type_201 *)ptr) alloc_ptr = fringe->t201;
                msg ("Type 201 record address = %X", -2, fringe->t201);
                break;

            case 202:
                fringe->t202 = (struct type_202 *)addr_202 (version, ptr, &size);
                if (fringe->t202 != (struct type_202 *)ptr) alloc_ptr = fringe->t202;
                msg ("Type 202 record address = %X", -2, fringe->t202);
                break;

            case 203:
                fringe->t203 = (struct type_203 *)addr_203 (version, ptr, &size);
                if (fringe->t203 != (struct type_203 *)ptr) alloc_ptr = fringe->t203;
                msg ("Type 203 record address = %X", -2, fringe->t203);
                break;

            case 204:
                fringe->t204 = (struct type_204 *)addr_204 (version, ptr, &size);
                if (fringe->t204 != (struct type_204 *)ptr) alloc_ptr = fringe->t204;
                msg ("Type 204 record address = %X", -2, fringe->t204);
                break;

            case 205:
                fringe->t205 = (struct type_205 *)addr_205 (version, ptr, &size);
                if (fringe->t205 != (struct type_205 *)ptr) alloc_ptr = fringe->t205;
                msg ("Type 205 record address = %X", -2, fringe->t205);
                break;

            case 206:
                fringe->t206 = (struct type_206 *)addr_206 (version, ptr, &size);
                if (fringe->t206 != (struct type_206 *)ptr) alloc_ptr = fringe->t206;
                msg ("Type 206 record address = %X", -2, fringe->t206);
                break;

            case 207:
                fringe->t207 = (struct type_207 *)addr_207 (version, ptr, &size);
                if (fringe->t207 != (struct type_207 *)ptr) alloc_ptr = fringe->t207;
                msg ("Type 207 record address = %X", -2, fringe->t207);
                break;

            case 208:
                fringe->t208 = (struct type_208 *)addr_208 (version, ptr, &size);
                if (fringe->t208 != (struct type_208 *)ptr) alloc_ptr = fringe->t208;
                msg ("Type 208 record address = %X", -2, fringe->t208);
                break;

            case 210:
                fringe->t210 = (struct type_210 *)addr_210 (version, ptr, &size);
                if (fringe->t210 != (struct type_210 *)ptr) alloc_ptr = fringe->t210;
                msg ("Type 210 record address = %X", -2, fringe->t210);
                break;

            case 212:
                i = fringe->n212;
                fringe->t212[i] = (struct type_212 *)addr_212 (version, ptr, &size);
                if (fringe->t212[i] != (struct type_212 *)ptr) alloc_ptr = fringe->t212[i];
                fringe->n212 += 1;
                msg ("Type 212 record number %d address = %X", -2, i, fringe->t212[i]);
                break;

            case 221:
                fringe->t221 = (struct type_221 *)addr_221 (version, ptr, &size);
                msg ("type 221 record size = %d", 0, size);
                if (fringe->t221 != (struct type_221 *)ptr) alloc_ptr = fringe->t221;
                msg ("Type 221 record address = %X", -2, fringe->t221);
                break;

            case 230:
                i = fringe->n230;
                fringe->t230[i] = (struct type_230 *)addr_230 (version, ptr, &size);
                if (fringe->t230[i] != (struct type_230 *)ptr) alloc_ptr = fringe->t230[i];
                fringe->n230 += 1;
                msg ("Type 230 record number %d address = %X", -2, i, fringe->t230[i]);
                break;

            default:
                msg ("Inappropriate record type %d in fringe file", 2, rec_id);
                totbytes = bytes;
                size = 0;
                break;
            }
                                        /* Keep track of allocated pointers */
        if (alloc_ptr != NULL)
            {
            fringe->allocated[fringe->nalloc] = alloc_ptr;
            fringe->nalloc++;
            msg ("Adding memory block %d to allocated list", -1, alloc_ptr);
            }
                                        /* Adjust pointer and get next rec. */
        ptr += size;
        totbytes += size;
        }

    if (totbytes != bytes)
        msg ("Last record in '%s' truncated", 2, filename);

    msg ("Number of type 230 records = %d", -2, fringe->n230);
    return (0);
    }
