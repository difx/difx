/*
 * codif_merge.c
 *
 * Merges two CODIF format files into a third. Output is predominantly from
 * fileA, with a small random fraction of frames replaced by frames from fileB.
 * FileB frames are marked invalid in their headers.
 *
 * Usage:
 *   codif_merge [options] fileA fileB outfile
 *
 *   fileA        : Primary input file (most frames come from here)
 *   fileB        : Secondary input file (occasional frames, marked invalid)
 *   outfile      : Output file
 *   -o outfile   : Output file (alternative to positional argument)
 *   -p percent   : Probability (%) of choosing a run from fileB [default: 2]
 *   -n maxframes : Max frames in a run from fileB (1..N chosen randomly) [default: 10]
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <codifio.h>   

/* ── tunables ─────────────────────────────────────────────────────────────── */
#define BUFFER_TARGET_BYTES (10 * 1024 * 1024)   /* ~10 MB per file buffer   */

/* ── helpers ──────────────────────────────────────────────────────────────── */

/* Return a random integer in [lo, hi] (inclusive). */
static int rand_range(int lo, int hi) {
    return lo + (int)((double)(hi - lo + 1) * rand() / ((double)RAND_MAX + 1.0));
}

/* Return 1 with probability (pct/100), else 0. */
static int rand_percent(int pct) {
    return (rand_range(1, 100) <= pct);
}

/*
 * get_frame_size()
 *
 * Read the first CODIF header from fp and return the total frame size
 * (header + data) in bytes, then rewind fp to the beginning.
 *
 * Returns frame size on success, -1 on error.
 */
static ssize_t get_frame_size(FILE *fp) {
    codif_header hdr;

    if (fread(&hdr, 1, CODIF_HEADER_BYTES, fp) != CODIF_HEADER_BYTES) {
        fprintf(stderr, "Error: could not read first CODIF header\n");
        return -1;
    }

    /*
     * TODO: use the codifio API to extract the data size from the header.
     * Something like:
     *   size_t data_bytes = getCODIFFrameBytes(&hdr);   // or similar call
     * then:
     *   size_t frame_bytes = CODIF_HEADER_BYTES + data_bytes;
     *
     * Replace the placeholder below with the real call.
     */
    size_t data_bytes = getCODIFFrameBytes(&hdr);
    if (data_bytes == 0) {
      fprintf(stderr, "Error: data size from header is zero – check API call\n");
      return -1;
    }

    rewind(fp);
    return (ssize_t)(CODIF_HEADER_BYTES + data_bytes);
}

/*
 * check_headers_consistent()
 *
 * Compare the CODIF headers of two frames and return 1 if they are
 * "compatible" (same channel, thread, epoch, etc.), 0 otherwise.
 *
 */
static int check_headers_consistent(const codif_header *ha,
                                    const codif_header *hb) {

  if (getCODIFThreadID(ha)      != getCODIFThreadID(hb))      return 0;
  if (getCODIFGroupID(ha)       != getCODIFGroupID(hb))       return 0;
  if (getCODIFSecondaryID(ha)   != getCODIFSecondaryID(hb))   return 0;
  if (getCODIFEpoch(ha)         != getCODIFEpoch(hb))         return 0;
  if (getCODIFFrameBytes(ha)    != getCODIFFrameBytes(hb))    return 0;
  if (getCODIFComplex(ha)       != getCODIFComplex(hb))       return 0;
  if (getCODIFHeaderBytes(ha)   != getCODIFHeaderBytes(hb))   return 0;
  if (getCODIFPeriod(ha)        != getCODIFPeriod(hb))        return 0;
  if (getCODIFSync(ha)          != getCODIFSync(hb))          return 0;
  if (getCODIFTotalSamples(ha)  != getCODIFTotalSamples(hb))  return 0;
  if (getCODIFBitsPerSample(ha) != getCODIFBitsPerSample(hb)) return 0;
  if (getCODIFFrameEpochSecOffset(ha) != getCODIFFrameEpochSecOffset(hb)) return 0;
  return 1;
}

/*
 * mark_invalid()
 *
 * Set the "invalid" bit in the CODIF header at the start of `frame`.
 *
 * TODO: use the codifio API, e.g.:
 *   setCODIFInvalid((codif_header_t *)frame, 1);
 */
static void mark_invalid(uint8_t *frame) {
    /* setCODIFInvalid((codif_header_t *)frame, 1); */
    (void)frame; /* TODO: remove once real call is in place */
}

/* ── main ─────────────────────────────────────────────────────────────────── */

static void usage(const char *prog) {
    fprintf(stderr,
        "Usage: %s [-p percent] [-n maxframes] fileA fileB outfile\n"
        "  fileA        Primary input  (most output frames come from here)\n"
        "  fileB        Secondary input (occasional bursts, marked invalid)\n"
        "  outfile      Output file\n"
        "  -p percent   %% chance each burst comes from fileB [default 2]\n"
        "  -n maxframes Max frames per fileB burst (random 1..N) [default 10]\n",
        prog);
}

int main(int argc, char *argv[]) {

    /* ── option parsing ───────────────────────────────────────────────────── */
    const char *path_a   = NULL;
    const char *path_b   = NULL;
    const char *path_out = NULL;
    int         opt_pct       = 2;    /* % chance of choosing fileB burst  */
    int         opt_maxframes = 10;   /* max frames per fileB burst        */

    int opt;
    while ((opt = getopt(argc, argv, "p:n:h")) != -1) {
        switch (opt) {
        case 'p': opt_pct       = atoi(optarg); break;
        case 'n': opt_maxframes = atoi(optarg); break;
        case 'h': usage(argv[0]); return 0;
        default:  usage(argv[0]); return 1;
        }
    }

    /* Remaining positional arguments: fileA  fileB  outfile */
    if (argc - optind != 3) {
        fprintf(stderr, "Error: expected exactly 3 positional arguments (fileA fileB outfile).\n");
        usage(argv[0]);
        return 1;
    }
    path_a   = argv[optind];
    path_b   = argv[optind + 1];
    path_out = argv[optind + 2];

    if (opt_pct < 0 || opt_pct > 100) {
        fprintf(stderr, "Error: -p percent must be 0–100.\n");
        return 1;
    }
    if (opt_maxframes < 1) {
        fprintf(stderr, "Error: -n maxframes must be >= 1.\n");
        return 1;
    }

    srand((unsigned)time(NULL));

    /* ── open files ───────────────────────────────────────────────────────── */
    FILE *fa  = fopen(path_a,   "rb");
    FILE *fb  = fopen(path_b,   "rb");
    FILE *fout = fopen(path_out, "wb");

    if (!fa)   { perror(path_a);   return 1; }
    if (!fb)   { perror(path_b);   return 1; }
    if (!fout) { perror(path_out); return 1; }

    /* ── determine frame size from fileA ──────────────────────────────────── */
    ssize_t frame_size = get_frame_size(fa);
    if (frame_size <= 0) return 1;

    printf("CODIF frame size: %zd bytes\n", frame_size);

    /* ── allocate buffers (multiple of frame_size, ~10 MB each) ──────────── */
    size_t frames_per_buf = (size_t)BUFFER_TARGET_BYTES / (size_t)frame_size;
    if (frames_per_buf == 0) frames_per_buf = 1;
    size_t buf_bytes = frames_per_buf * (size_t)frame_size;

    uint8_t *buf_a = malloc(buf_bytes);
    uint8_t *buf_b = malloc(buf_bytes);
    if (!buf_a || !buf_b) {
        fprintf(stderr, "Error: failed to allocate %zu byte buffers.\n", buf_bytes);
        return 1;
    }

    printf("Buffer: %zu frames (%zu bytes) per file\n", frames_per_buf, buf_bytes);

    /* ── merge loop ───────────────────────────────────────────────────────── */
    size_t total_frames_a = 0;   /* frames written from fileA */
    size_t total_frames_b = 0;   /* frames written from fileB */

    /*
     * frames_left_from_b: when > 0 we are mid-burst from fileB.
     * We reset to 0 at the start of each buffer so a burst never
     * "carries over" more frames than are physically available.
     */
    int frames_left_from_b = 0;

    for (;;) {  /* outer loop over buffer pairs */

        /* Read a full (or partial) buffer from each file */
        size_t bytes_a = fread(buf_a, 1, buf_bytes, fa);
        size_t bytes_b = fread(buf_b, 1, buf_bytes, fb);

        /* EOF or read error on either file → stop */
        if (bytes_a == 0 || bytes_b == 0) break;

        /* How many complete frames did we actually get? */
        size_t frames_a = bytes_a / (size_t)frame_size;
        size_t frames_b = bytes_b / (size_t)frame_size;
        size_t frames_this_buf = (frames_a < frames_b) ? frames_a : frames_b;

        if (frames_this_buf == 0) break;   /* only partial header – done */

        /*
         * Reset any in-progress burst from fileB.  A burst that started near
         * the end of the previous buffer is abandoned; we start fresh here so
         * we always write whole runs from contiguous buffer data.
         */
        frames_left_from_b = 0;

        /* Walk through frames in this buffer */
        for (size_t i = 0; i < frames_this_buf; /* incremented inside */) {

            if (frames_left_from_b > 0) {
                /*
                 * We are in a fileB burst.  Clamp to the frames remaining in
                 * this buffer (the spec says "just write the 5 frames and
                 * reset on the next buffer").
                 */
                size_t burst = (size_t)frames_left_from_b;
                size_t remaining = frames_this_buf - i;
                if (burst > remaining) burst = remaining;

                for (size_t f = 0; f < burst; f++, i++) {
                    uint8_t *fa_frame = buf_a + i * (size_t)frame_size;
                    uint8_t *fb_frame = buf_b + i * (size_t)frame_size;

                    /* Validate headers are compatible */
                    if (!check_headers_consistent((codif_header*)fa_frame, (codif_header *)fb_frame)) {
		      fprintf(stderr, "Error: header mismatch at frame %zu – skipping fileB frame\n",
                            total_frames_a + total_frames_b + i);
                        /* Fall back to fileA for this frame */
		      exit(1);
                    }

                    /* Mark the fileB frame invalid before writing */
		    setCODIFFrameInvalid((codif_header*)fb_frame, 1);
                    fwrite(fb_frame, 1, (size_t)frame_size, fout);
                    total_frames_b++;
                }

                frames_left_from_b = 0;  /* burst finished (or truncated) */

            } else {
                /* Decide whether to start a fileB burst at this frame */
                if (rand_percent(opt_pct)) {
                    /* Start a new burst: choose length 1..opt_maxframes */
                    frames_left_from_b = rand_range(1, opt_maxframes);
                    /* (loop continues; next iteration handles the burst) */
                } else {
                    /* Write this frame from fileA */
                    uint8_t *fa_frame = buf_a + i * (size_t)frame_size;
                    fwrite(fa_frame, 1, (size_t)frame_size, fout);
                    total_frames_a++;
                    i++;
                }
            }
        }

        /* If we got less than a full buffer, at least one file is at EOF */
        if (frames_a < frames_per_buf || frames_b < frames_per_buf) break;
    }

    if (ferror(fa))   fprintf(stderr, "Warning: read error on %s\n", path_a);
    if (ferror(fb))   fprintf(stderr, "Warning: read error on %s\n", path_b);
    if (ferror(fout)) fprintf(stderr, "Warning: write error on %s\n", path_out);

    printf("Done.  Frames from A: %zu  |  Frames from B (invalid): %zu  |  "
           "Total: %zu\n",
           total_frames_a, total_frames_b,
           total_frames_a + total_frames_b);

    fclose(fa);
    fclose(fb);
    fclose(fout);
    free(buf_a);
    free(buf_b);

    return 0;
}
