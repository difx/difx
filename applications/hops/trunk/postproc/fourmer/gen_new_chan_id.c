/*
 * $Id: gen_new_chan_id.c 782 2012-11-13 19:45:21Z rjc $
 *
 * support for fourmer--relabelling of channel ids
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "fourmer.h"

/*
 * For now, this is an undocumented testing hack.
 */
static void freq_group_override(char *cid)
{
    static int once = 1;
    static char *fg = 0;
    if (once) { fg = getenv("HOPS_FOURMER_FG"); once = 0; }
    if (fg) *cid = *fg;
}

/*
 * Generate a new channel id based on the old, whether it is the
 * A file (or the B file) and whether it is invoked for the root,
 * or later for the other files.  Here we use the simplest scheme
 * which inserts 'A' or 'B' after the frequency group.
 * Note that the old label might not be null terminated.
 *
 * Channel identifiers are 8 chars in some places and 32 chars elsewhere.
 * We'll silently truncate to 8 chars until we have a better plan.
 */
char *gen_new_chan_id(char *old, int isA, int isroot)
{
    static char mine[9];
    /* if (!isroot && (end = get_cache(old, isA))) return(end); */
    memset(mine, 0, sizeof(mine));
    if (strlen (old))
        {
        freq_group_override(old);
        mine[0] = *old++;
        mine[1] = isA ? 'A' : 'B';
        strncpy(mine + 2, old, 6);
        /* if (isroot) cache_old(old, isA); */
        }
    return(mine);
}

/*
 * Relabel a channel within a line based on whether it is the A or B file.
 * This is used for surgery on the root file FREQ section.
 *
 * We presume ... chan_def =[ ]*S00L[ ]*: and we want
 * to carve out the label so that we can modify it.
 */
void relabel_chan_def(char *line, int isA)
{
    static char mine[MAX_CHARS];
    char *colon = strchr(line, ':');
    if (!colon) return;
    mine[0] = ' ';
    strcpy(mine + 1, colon);    /* save rest of the line, starting w/ colon */
    do --colon; while (isspace(*colon) && (colon > line));
    if (colon <= line) return;
    *++colon = 0;	    /* null terminate the label */
    do --colon; while (!isspace(*colon) && (*colon != '=') && (colon > line));
    if (colon++ <= line) return;
    strcpy(colon, gen_new_chan_id(colon, isA, 1));
    strcat(colon, mine);
}

/*
 * eof
 */
