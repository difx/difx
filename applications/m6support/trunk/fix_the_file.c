/*
 * $Id: fix_the_file.c 5211 2021-07-29 19:28:17Z gbc $
 *
 * Code to pull good packets from a bad file.
 */

/* needed for mremap() */
#define _GNU_SOURCE

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "fix_the_file.h"

/* stub out something not needed */
void sg_advice_term(int mmfd) { return; }

/*
 * Like it says: open the two files, one for reading, the other for writing.
 */
int fx_open(Fixer *fxp)
{
    struct stat mm_stb;
    int mode = S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP;

    /* open and mmap the input file */
    if (stat(fxp->ifile, &mm_stb) || mm_stb.st_size <= 0)
        return(perror("fx_open:stat:ifile"),1);
    fxp->smi.mmfd = open(fxp->ifile, O_RDONLY|O_CLOEXEC);
    if (fxp->smi.mmfd < 0)
        return(perror("fx_open:open:ifile"),2);
    fxp->smi.start = mmap(0, (fxp->smi.size = mm_stb.st_size),
        PROT_READ, MAP_SHARED|MAP_NORESERVE, fxp->smi.mmfd, 0);
    if (fxp->smi.start == MAP_FAILED) {
        perror("fx_open:mmap:ifile");
        close(fxp->smi.mmfd);
        return(3);
    }
    fxp->smi.eomem = fxp->smi.start + fxp->smi.size;

    /* creat(e) the output file with some handy junk of known size */
    fxp->smo.mmfd = open(fxp->ofile, O_CREAT|O_WRONLY|O_TRUNC, mode);
    if (fxp->smo.mmfd < 0) {
        perror("fx_open:open:ofile");
        return(4);
    }
    if (sizeof(mm_stb) != write(fxp->smo.mmfd, &mm_stb, sizeof(mm_stb))) {
        perror("fx_open:write:ofile");
        return(5);
    }
    if (close(fxp->smo.mmfd))
        return(perror("fx_open:close:ofile"),6);

    /* open and mmap the output file */
    fxp->smo.mmfd = open(fxp->ofile, O_RDWR|O_CLOEXEC);
    if (fxp->smo.mmfd < 0)
        return(perror("fx_open:open:ofile"),7);
    fxp->smo.start = mmap(0, sizeof(mm_stb),
        PROT_WRITE, MAP_SHARED|MAP_NORESERVE, fxp->smo.mmfd, 0);
    if (fxp->smo.start == MAP_FAILED) {
        perror("fx_open:mmap:ofile");
        close(fxp->smo.mmfd);
        return(8);
    }
    /* extend the size of the file */
    if (ftruncate(fxp->smo.mmfd, mm_stb.st_size)) {
        perror("fx_open:ftruncate:ofile");
        return(9);
    }
    fxp->smo.start = mremap(fxp->smo.start, sizeof(mm_stb),
        (fxp->smo.size = mm_stb.st_size), MREMAP_MAYMOVE);
    if (fxp->smo.start == MAP_FAILED) {
        perror("fx_open:mremap:ofile");
        close(fxp->smo.mmfd);
        return(10);
    }

    fxp->smo.eomem = fxp->smo.start + fxp->smo.size;
    return(0);
}

/*
 * Start the process by copying the first byte.
 */
int fx_init(Fixer *fxp)
{
    VDIFHeader *vhi = (VDIFHeader *)fxp->smi.start;
    VDIFHeader *vho = (VDIFHeader *)fxp->smo.start;
    VDIFHeader *vhf;
    uint64_t vscheck;

    /* copy the first packet; size in bytes, len in uint64_ts */
    fxp->pktsize = vhi->w3.df_len * 8;
    memcpy(vho, vhi, fxp->pktsize);
    fxp->good = 1;

    /* bump input offset and set the offset for subsequent output */
    fxp->clone = fxp->pktsize;
    fxp->start += fxp->pktsize;
    if (fxp->verb) fprintf(stdout,
        "Assuming %d packets / second, with packet size %lu B\n",
        fxp->prate, fxp->pktsize);

    /* compute a signature as a determinant of validity */
    if (fxp->verb) fprintf(stdout, "Signature from %s\n", fxp->ifile);
    fxp->vsign.word = sg_get_vsig((uint32_t *)vhi, (void *)0, fxp->verb,
        "fixer: ", (VDIFsigu *)0, (short *)0);

    /* just being anal here */
    if (fxp->verb) fprintf(stdout, "Signature from %s\n", fxp->ofile);
    vscheck = sg_get_vsig((uint32_t *)vho, (void *)0, fxp->verb,
        "fixer: ", &fxp->vsign, (short *)0);
    if (vscheck != fxp->vsign.word) {
        fprintf(stderr, "Original %lu and copy %lu signature mismatch\n",
            fxp->vsign.word, vscheck);
        return(1);
    }

    /* create a fill packet once */
    fxp->fill = (uint64_t*)calloc(1, fxp->pktsize);
    vhf = (VDIFHeader *)fxp->fill;
    if (!fxp->fill) {
        perror("fx_init:calloc:fill");
        return(2);
    }
    /* only the invalid bit and frame length is needed */
    vhf->w1.invalid = 0x1;
    vhf->w3.df_len = vhi->w3.df_len;
    /* but we'll populate signature bits as well */
    vhf->w1.legacy = vhi->w1.legacy;
    vhf->w2.ref_epoch = vhi->w2.ref_epoch;
    vhf->w2.UA = vhi->w2.UA;
    vhf->w3.num_channels = vhi->w3.num_channels;
    vhf->w3.ver = vhi->w3.ver;
    vhf->w4.stationID = vhi->w4.stationID;
    vhf->w4.bps = vhi->w4.bps;
    vhf->w4.dt = vhi->w4.dt;

    if (fxp->verb) fprintf(stdout, "Signature from fill\n");
    vscheck = sg_get_vsig((uint32_t *)fxp->fill, (void *)0, fxp->verb,
        "fixer: ", &fxp->vsign, (short *)0);
    if (vscheck != fxp->vsign.word) {
        fprintf(stderr, "Original %lu and fill %lu signature mismatch\n",
            fxp->vsign.word, vscheck);
        return(3);
    }

    return(0);
}

void report_good(Fixer *fxp, void *early, void *later)
{
    VDIFHeader *vhl = (VDIFHeader *)later;
    VDIFHeader *vhe = (VDIFHeader *)early;
    if (vhl == vhe) return;     /* nothing to report */
    long secs = (vhl->w1.secs_inre - vhe->w1.secs_inre);
    long frms = (vhl->w2.df_num_insec - vhe->w2.df_num_insec);
    if (frms < 0) {
        frms += fxp->prate;
        secs -= 1;
    }
    if (fxp->verb > 1) fprintf(stdout,
        "Good: from %d+%06d to %d+%06d\n",
        vhe->w1.secs_inre, vhe->w2.df_num_insec,
        vhl->w1.secs_inre, vhl->w2.df_num_insec);
}

/*
 * Estimate the number of fill packets based on the seconds
 * in reference epoch, the frame numbers in the second and
 * hope for the best...
 */
long number_fill_packets(Fixer *fxp, void *later, void *early)
{
    VDIFHeader *vhl = (VDIFHeader *)later;
    VDIFHeader *vhe = (VDIFHeader *)early;
    long secs = (vhl->w1.secs_inre - vhe->w1.secs_inre);
    long frms = (vhl->w2.df_num_insec - vhe->w2.df_num_insec);
    long pkts;
    if (frms < 0) {
        frms += fxp->prate;
        secs -= 1;
    }
    pkts = secs * fxp->prate + frms;
    if (fxp->verb > 1) fprintf(stdout,
        "Fill: from %d+%06d to %d+%06d (%ld packets %ld+%06ld)\n",
        vhe->w1.secs_inre, vhe->w2.df_num_insec,
        vhl->w1.secs_inre, vhl->w2.df_num_insec,
        pkts, secs, frms);
    return(pkts);
}

/*
 * The required fill will run off the end of the file, so we need to
 * truncate (making it bigger) and remap the file to use more space.
 * Since the remapped file may move, we need to update the mapping
 * and return the current pointer.
 */
void *extend_outfile(Fixer *fxp, void *ou, long bytes)
{
    long used = ou - fxp->smo.start;
    if (fxp->verb) fprintf(stdout, "Extending by another %lu B\n", bytes);

    /* make it bigger */
    if (ftruncate(fxp->smo.mmfd, used + bytes)) {
        perror("extend_outfile:ftruncate:ofile");
        return(0);
    }

    /* make the bigger file visible */
    fxp->smo.start = mremap(fxp->smo.start, fxp->smo.size,
        used + bytes, MREMAP_MAYMOVE);
    if (fxp->smo.start == MAP_FAILED) {
        perror("extend_outfile:mremap:ofile");
        close(fxp->smo.mmfd);
        return(0);
    }

    /* update the mapping data */
    fxp->smo.size = used + bytes;
    fxp->smo.eomem = fxp->smo.start + fxp->smo.size;
    ou = fxp->smo.start + used;
    return(ou);
}

/*
 * This routine does the heavylifting.  As long as we see packets
 * with valid signatures, we copy them and advance pointers.  Otherwise
 * we step forward through memory looking for a valid signature.  When
 * something is found, insert fill packets (marked invalid).
 */
int fx_copy(Fixer *fxp)
{
    void *in = fxp->smi.start + fxp->start;
    void *ou = fxp->smo.start + fxp->clone;
    uint64_t vscheck;
    long steps, tofill, goodpkts = 0;
    void *first = 0, *final = 0, *lastin = 0;

    while (in < fxp->smi.eomem && ou < fxp->smo.eomem) {
        /* check signature */
        vscheck = sg_get_vsig((uint32_t *)in, (void *)0,
            fxp->verb > 1 ? fxp->verb - 2 : 0,
            "copy:  ", &fxp->vsign, (short *)0);
        if (vscheck == fxp->vsign.word) {   /* -> valid: copy, update */
            memcpy(ou, in, fxp->pktsize);
            fxp->good ++;
            if (!first) {
                first = ou;
                fxp->patches ++;
            }
            final = ou;
            lastin = in;
            if (++goodpkts >= fxp->prate) {
                if (fxp->verb)
                    fprintf(stdout, "a run of %ld goodpkts\n", goodpkts);
                goodpkts = 0;
            }
        } else {                            /* -> invalid: search & fill */
            goodpkts = 0;
            report_good(fxp, first, final);
            for (steps = 0; in < fxp->smi.eomem; steps++) {
                /* we will assume we are at least word aligned */
                in += sizeof(uint32_t);
                vscheck = sg_get_vsig((uint32_t *)in, (void *)0,
                    fxp->verb > 1 ? fxp->verb - 2 : 0,
                    "step:  ", &fxp->vsign, (short *)0);
                if (vscheck == fxp->vsign.word) break;
            }
            /* we are done if at end of input file */
            if (in >= fxp->smi.eomem) break;
            /* otherwise we found a good packet */
            tofill = number_fill_packets(fxp, in, ou - fxp->pktsize);
            if (tofill <= 0) {
                fprintf(stdout,
                    "Backwards? at offset %#lX (%#lX), tofill is %+ld\n",
                        in - fxp->smi.start, lastin - fxp->smi.start, tofill);
                in += sizeof(uint32_t);
                first = final;
                continue;
            }
            fxp->nfil += tofill;
            if ((ou + (tofill+1)*fxp->pktsize) > fxp->smo.eomem) {
                ou = extend_outfile(fxp, ou, (tofill+fxp->extra)*fxp->pktsize);
                if (!ou) return(2);
            }
            while (tofill-- > 0) {
                if (ou > fxp->smo.eomem) {
                    fprintf(stderr, "error--ran off end of file.\n");
                    return(1);
                }
                memcpy(ou, fxp->fill, fxp->pktsize);
                ou += fxp->pktsize;
            }
            /* now copy that good packet */
            memcpy(ou, in, fxp->pktsize);
            fxp->good ++;
            final = first = ou;
            fxp->patches ++;
        }
        in += fxp->pktsize;
        ou += fxp->pktsize;
    }
    report_good(fxp, first, final);

    fxp->start = in - fxp->smi.start;
    fxp->clone = ou - fxp->smo.start;
    return(0);
}

/*
 * Truncate the output file to declared length and close up files.
 */
int fx_close(Fixer *fxp)
{
    if (ftruncate(fxp->smo.mmfd, fxp->clone)) {
        perror("fx_open:ftruncate:ofile");
        return(9);
    }
    if (fxp->verb) fprintf(stdout, "Truncated to %lu B\n", fxp->clone);

    if (close(fxp->smi.mmfd)) perror("close:ifile");
    if (munmap(fxp->smi.start, fxp->smi.size)) perror("munmap:ifile");
    if (close(fxp->smo.mmfd)) perror("close:ofile");
    if (munmap(fxp->smo.start, fxp->smo.size)) perror("munmap:ofile");
    return(0);
}

/* Main entry */
int fix_the_file(Fixer *fxp)
{
    int             error;

    if (fxp->verb) fprintf(stdout,
        "Fixing file %s at offset %lu into file %s\n",
        fxp->ifile, (long unsigned)fxp->start, fxp->ofile);

    sg_logger(stdout);

    /* open the files */
    if ((error = fx_open(fxp))) {
        fprintf(stderr, "Unable to open the working files\n");
        return(10 + error);
    }

    /* get ourselves started */
    if ((error = fx_init(fxp))) {
        fprintf(stderr, "Unable to find and copy first packet\n");
        return(30 + error);
    }

    /* all the heavy lifting is here */
    if ((error = fx_copy(fxp))) {
        fprintf(stderr, "Insoluble problem copying packets\n");
        return(40 + error);
    }

    /* close the files */
    if ((error = fx_close(fxp))) {
        fprintf(stderr, "Unable to close the working files\n");
        return(50 + error);
    }

    if (fxp->verb) fprintf(stdout,
        "Completed: %lu good %lu fill in %lu patches\n",
        fxp->good, fxp->nfil, fxp->patches);
    return(0);
}

/*
 * eof
 */
