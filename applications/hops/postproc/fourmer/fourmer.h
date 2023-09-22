/*
 * $Id: fourmer.h 1342 2016-05-31 14:01:08Z gbc $
 *
 * support for fourmer
 */

#include "mk4_data.h"
#include "mk4_util.h"

#define MAX_LINES   4096        // max lines in an input file
#define MAX_CHARS   512         // max chars in a line
#define MAX_BUFFER  256         // max lines in a merged section

#define IDX_INC	    100		    // how much to shift indices
#define NUM_CH_MAP  2*MAX_CHAN      // size of index remapping
#define MAX_FPATH   1024	    // MAX_PATH?

extern int msglev;

/*
 * Generate a new channel id based on the old, whether it is the
 * A file (or the B file) and whether it is the root or post-root.
 */
extern char *gen_new_chan_id(char *old, int isA, int isroot);

/*
 * Relabel a channel within a line based on whether it is the A or B file.
 */
extern void relabel_chan_def(char *line, int isA);

/*
 * A verbose comparison diagnostic if needed
 */
extern void print_cdata_cmp(char *AName, char *BName,
			    struct mk4_corel *cdataA,
			    struct mk4_corel *cdataB);

/*
 * Append sdataB to sdataC and edit channel names in the process.
 */
extern void append_sdata(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB);

/*
 * Reconciliation of scan name
 */
extern void scan_name_edit(char lineA[][MAX_CHARS], int nA,
			   char lineB[][MAX_CHARS], int nB);

/*
 * eof
 */
