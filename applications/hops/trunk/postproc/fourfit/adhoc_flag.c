/*
 * Machinery to support adhoc_flag() capability
 */

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mk4_util.h"

#include "param_struct.h"

#define ADHOC_FLAG_IMPLEMENTATION 1
#include "adhoc_flag.h"

#if USE_ADHOC_FLAG

/* might as well do hashed searching correctly */
// #define _GNU_SOURCE 1
#define __USE_GNU 1
#include <search.h>
static struct hsearch_data htable;
/* a place to hold flag table data once read */
static AHFFC_Entry flag_table[MAX_NUM_FLAG_FILES];
/* this points to the next open index in flag_table */
static int flag_table_open = 0;
/* this increments beyond unity on error */
static int flag_table_init = 0;

/*
 * Initialize the machinery with a zero-ed table and a hash for filenames.
 */
static void init_flag_table(void)
{
    int rv;
    memset(flag_table, 0, sizeof(flag_table));
    flag_table_init = 1;
    rv = hcreate_r(MAX_NUM_FLAG_FILES, &htable);
    if (!rv) {
        perror("init_flag_table:hcreate_r");
        msg("Init flag tables failed(%d), no flagging should result", 3, rv);
        flag_table_init ++;
    }
}

/*
 * Provide a string for msg purposes; each byte has two nibbles.
 */
static char *table_report(char *e, int n)
{
    static char buf[2*MAX_NUM_FREQS + 2];
    unsigned char *b = buf - 1;
    while (n-- > 0){
        *(++b) = (*e & 0xF0)>>4;
        if (*b < 10) *b += '0'; else *b += 'A' - 10;
        *(++b) = (*e++ & 0x0F);
        if (*b < 10) *b += '0'; else *b += 'A' - 10;
    }
    *b = 0;         /* null terminate string */
    return(buf);
}

/*
 * Read the file and transfer its data to the table entry.
 * If we run into any issues in the process we nuke the table
 * (preventing further use) and return null (for no flagging).
 *
 * MAX_NUM_FREQS and NUM_FLAG_LINES serve as starting allocations
 * but we will realloc as needed.
 * FIXME: sort the table (as coded requires a reload).
 * FIXME: work out actual number of freqs really needed.
 */
static AHFFC_Entry *populate_entry(AHFFC_Entry *table, char *file)
{
    static char buf[MAX_LEN_FLAG_LINE];
    FILE *fp = fopen(file, "r");
    char *bytes = malloc(MAX_NUM_FREQS + 2);
    char *name = malloc(strlen(file)+2);
    double *times = (double*)malloc((NUM_FLAG_LINES+2) * sizeof(double));
    int load, offset, nibble, bread, addr, usb, lastusb, lastlsb, limit;
    int loaded = NUM_FLAG_LINES + 2, readable = MAX_NUM_FREQS + 2;

    /* verify pointers */
    if (!bytes || !times || !name) {
        perror("populate_entry:malloc");
        msg("Unable to store flag file %s", 3, file);
        flag_table_init++;
        return((AHFFC_Entry *)0);
    }
    strcpy((table->file = name), file);
    if (!fp) {
        perror("populate_entry:fopen");
        msg("Unable to access flag file %s", 2, file);
        /* this failure doesn't shut us down */
        return((AHFFC_Entry *)0);
    }
    msg("Loading Flag Table[%d] from %s", 1, table - flag_table, file);

    /* load the file */
    for (load = 0, bread = 0; fgets(buf, sizeof(buf), fp); load++) {
        buf[limit = (strlen(buf)-1)] = 0;   /* nuke nl & be sure of end */
        if (buf[0] == AHCOMMENT) {
            msg(" entry: %s (ignored)", 1, buf);
            load--;
            continue;  /* ignore comments */
        }
        msg(" entry: %s", 1, buf);
        if (load >= loaded) {
            loaded += NUM_FLAG_LINES;
            times = realloc(times, loaded);
            if (!times) {
                perror("populate_entry:realloc:times");
                msg("Unable to expand flag file %s", 3, file);
                flag_table_init++;
                return((AHFFC_Entry *)0);
            }
        }
        if (bread + MAX_NUM_FREQS >= readable) {
            readable += MAX_NUM_FREQS;
            bytes = realloc(bytes, readable);
            if (!bytes) {
                perror("populate_entry:realloc:bytes");
                msg("Unable to expand flag file %s", 3, file);
                flag_table_init++;
                return((AHFFC_Entry *)0);
            }
        }
        sscanf(buf, "%lf%n", &times[load], &offset);
        while (buf[offset] && isspace(buf[offset])) offset++;
        /*
         * scan the bytes into the table one nibble at a time
         * and note that we continue with the last byte once
         * we hit the end-of-line null.
         */
        msg(" with: %.12f %d %s", 0, times[load], offset, buf + offset);
        lastusb = offset;
        lastlsb = offset+1;
        for (addr = 0, usb = 1; addr < MAX_NUM_FREQS; usb = !usb) {
            if (buf[offset]) nibble = buf[offset];
            else if (usb)    nibble = buf[lastusb];
            else             nibble = buf[lastlsb];
            if (nibble >= '0' && nibble <= '9') nibble -= '0';
            else if (nibble >= 'A' && nibble <= 'F') nibble -= ('A' - 10);
            else if (nibble >= 'a' && nibble <= 'f') nibble -= ('a' - 10);
            else {
                msg("Illegal %x line %d char %d of %s, disabling flag", 2,
                    nibble, load, offset, file);
                nibble = 0xF;
            }
            if (usb) bytes[bread + addr]    = nibble << 4;
            else     bytes[bread + addr++] |= nibble;
            if (buf[offset]) {
                if (usb) lastusb = offset;
                else     lastlsb = offset;
            }
            if (offset < limit) offset++;
        }
        bread += addr;
        bytes[bread] = 0;
        msg(" (raw): %.12f %s", 0, times[load], bytes + bread - addr);
        msg("  into: %.12f %128.128s", 1, times[load],
            table_report(bytes + bread - addr, addr));
    }

    /* finally, fully populate the entry */
    table->tfirst = times[0];
    table->tfinal = times[load-1];
    table->ntimes = load;
    table->lindex = 0;      /* start at the beginning */
    table->nbytes = MAX_NUM_FREQS;
    table->times = times;
    table->table = bytes;
    /* table->file was set above */
    msg("Table has %d entries from %.12f to %.12f", 1,
        load, table->tfirst, table->tfinal);
    return(table);
}

/*
 * Open the file and read its contents.
 * An AHFFC_Entry suitable for further use will be created
 * and returned no matter what.
 */
static AHFFC_Entry *import_file(char *file)
{
    int rv;
    ENTRY try, *erv;
    AHFFC_Entry *usable;

    /* initialization and sanity checks */
    if (flag_table_init == 0) init_flag_table();
    if (flag_table_init > 1) return((AHFFC_Entry *)0);

    /* first try to find it */
    try.key = file;
    try.data = (void*)(long)flag_table_open;
    rv = hsearch_r(try, ENTER, &erv, &htable);
    if (!rv) {
        perror("import_file:hsearch_r");
        msg("Disabling all flagging activity", 3);
        flag_table_init++;
        return((AHFFC_Entry *)0);
    }

    /* if it was found, then we can just use it provided it is valid */
    rv = (int)(long)(erv->data);
    usable = flag_table + rv;
    if (rv < flag_table_open)
        return(usable->table ? usable : (AHFFC_Entry *)0);
    
    /* check to see if we have run out of space */
    if (++flag_table_open >= MAX_NUM_FLAG_FILES) {
        msg("Exhausted flag table storage, disabling activity", 3);
        flag_table_init++;
        return((AHFFC_Entry *)0);
    }

    /* load the table and prepare for first-time use */
    return(populate_entry(usable, file));
}

/*
 * Binary search for the correct time.  The first and final indexes
 * are guaranteed to be in bounds.  It is not clear if this is needed.
 * (If files are always accessed in time order, the if..else logic
 * used in the caller should suffice.)
 */
static int binary_search(int first, int final, double *times, double thyme)
{
    int middle;
    assert(final > first);
    while (final - first > 1) {
        middle = (first + final) / 2;
        if (times[middle] >= thyme) final = middle;
        else                        first = middle;
    }
    msg("assert %.12f <= %.12f < %.12f", 1, times[first], thyme, times[final]);
    assert(times[first] <= thyme && thyme < times[final]);
    return(first);
}

/*
 * This function provides the flagging data.
 *
 * On the first invocation, it locates the file, and if found proceeds
 * to allocate and initialize the AHFFC_Entry for future use.  If the
 * file is not found, an invalid table entry is created to prevent
 * wasting time on lookups in subsequent invocations.
 *
 * If something suitable is not located, a null pointer is returned.
 */
static unsigned char *locate_entry(char *file, double thyme)
{
    AHFFC_Entry *ep = import_file(file);
    int index;
    msg("ahf: locate_entry %p file %s for %.12f < %.12f < %.12f", 0,
        ep, ep->file, ep->tfirst, thyme, ep->tfinal);
    if (!ep) return(0);     /* no file means no flagging */

    /* out of bounds means no flagging */
    if (thyme < ep->tfirst || thyme > ep->tfinal) return(0);

    /*
     * ok, we're looking for something actually in the table
     * we're optimizing for the likely case that we're stepping
     * through the APs in time order
     */
    if (thyme >= ep->times[ep->lindex] &&
        thyme  < ep->times[ep->lindex + 1]) {           /* no-op, really */
        index = ep->lindex;
        msg("ahf:%.12f -> %d (case 1)", 0, thyme, index);
    } else if (thyme >= ep->times[ep->lindex + 1] &&
               thyme  < ep->times[ep->lindex + 2]) {    /* try next time */
        index = ep->lindex+1;
        msg("ahf:%.12f -> %d (case 2)", 0, thyme, index);
    } else if (thyme < ep->times[1]) {                  /* back to start */
        index = 0;
        msg("ahf:%.12f -> %d (case 3)", 0, thyme, index);
    } else if (thyme < ep->times[ep->lindex]) {         /* search early */
        index = binary_search(0, ep->lindex, ep->times, thyme);
        msg("ahf:%.12f -> %d (case 4)", 0, thyme, index);
    } else if (thyme >= ep->times[ep->lindex + 1]) {    /* search late */
        index = binary_search(ep->lindex + 1, ep->ntimes - 1, ep->times, thyme);
        msg("ahf:%.12f -> %d (case 5)", 0, thyme, index);
    } else {
        msg ("Flag table indexing error", 3);       /* should not happen */
        return(0);
    }

    /* ep->lindex is the entry to use */
    return(ep->table + (ep->lindex = index) * (ep->nbytes));
}

/*
 * This function provides the flagging byte
 * 0xFF == no flagging, appropriate if nothing applicable is found.
 */
static int locate_flag(char *file, double thyme, int fr)
{
    unsigned char *flag_bytes = locate_entry(file, thyme);
    /* no entry means no flagging */
    if (!flag_bytes) return(0xFF);
    msg("ahf bytes: %p = %X", 0, flag_bytes, (unsigned int)flag_bytes[fr]);
    /* otherwise send the appropriate byte */
    return((unsigned int)flag_bytes[fr]);
}

/*
 * Main entry to adhoc flagging of data via files.
 */
void adhoc_flag(struct type_param *pp,
    int datum_flag, int fr, int ap, int *uflag, int *lflag)
{
    double thyme;
    unsigned int usb_flag, lsb_flag;
    unsigned char ref_entry, rem_entry;
    /* the invoking logic presumes at least one of these exists */
    assert(pp->ah_flag_files[0][0] || pp->ah_flag_files[1][0]);

    /* use param to convert AP to target time in days from boy */
    thyme = ((ap + 0.5) * pp->acc_period + pp->start) / 8.64e4;
    msg ("ahf thyme: %.12f ap %d per %f start %f", 1,
        thyme, ap, pp->acc_period, pp->start);

    /* locate the data flag byte; first invocation initializes entry */
    ref_entry = (pp->ah_flag_files[0][0])
              ? locate_flag(pp->ah_flag_files[0], thyme, fr) : 0xFF;
    rem_entry = (pp->ah_flag_files[1][0])
              ? locate_flag(pp->ah_flag_files[1], thyme, fr) : 0xFF;

    /* separate the bits for messaging */
    usb_flag = ref_entry & rem_entry & 0x55;
    lsb_flag = ref_entry & rem_entry & 0xAA;

    /* and finally mask the bits */
    *uflag = datum_flag & usb_flag;
    *lflag = datum_flag & lsb_flag;

    msg ("fr-%d ap-%d r(%02X/%02X)usb(%02X&%02X->%02X)lsb(%02X&%02X->%02X)", 1,
        fr, ap, ref_entry, rem_entry,
        datum_flag, usb_flag, *uflag, datum_flag, lsb_flag, *lflag);
}

#endif /* USE_ADHOC_FLAG */

/*
 * eof
 */
