/*
 * $Id: scan_name_edit.c 495 2011-11-01 19:29:32Z gbc $
 *
 * Reconcile the scan names and update the lines where found.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fourmer.h"

/*
 * How to reconcile depends on what we have.
 * If the new name is the same as A's, return nonzero
 * and we can skip the rest of the work.
 */
static int reconcile_names(char *scanA, char *scanB, char *scanC)
    {
    int left = 32;
    char *join = getenv("HOPS_FOURMER_JOIN");
    while (*scanA && *scanB && (*scanA == *scanB) && (*scanA != ';') && left--)
	{
	*scanC++ = *scanA++; 
	scanB++; 
	}
    snprintf(scanC, left, join ? join : "JOIN");
    return(!strcmp(scanA, scanC));
    }

static void revise(char *line, char *old, int olen, char *new)
    {
    static char mine[MAX_CHARS];
    char *at, *rest;
    if ((at = strstr(line, old)) == NULL) return;		// not here
    if (strlen(line) - olen + strlen(new) > MAX_CHARS) return;	// danger!
    *at = 0;
    rest = at + olen - 1;
    sprintf(mine, "%s%s", new, rest);
    strcat(line, mine);
    }

/*
 * Looking for something like this.
 * ^$SCHED;$
 * ^scan <AorBname>;$
 *
 * *IF* we find it, generate a new scan name from the common characters.
 * Scan names are at most 32 characters.
 */
void scan_name_edit(char lA[][MAX_CHARS], int nA,
		    char lB[][MAX_CHARS], int nB)
    {
    int	iA, iB, len;
    char scanA[33], scanB[33], scanC[33];

    for (iA = 0; iA < nA; iA++)
	if (!strcmp(lA[iA], "$SCHED;")) break;
    if (iA == nA || !strcmp(lA[++iA], "scan ")) return;
    if (1 != sscanf(lA[iA], "scan %32s;", scanA)) return;
    msg("Scan from A is %s", 1, scanA);

    for (iB = 0; iB < nB; iB++)
	if (!strcmp(lB[iB], "$SCHED;")) break;
    if (iB == nB || !strcmp(lB[++iB], "scan ")) return;
    if (1 != sscanf(lB[iB], "scan %32s;", scanB)) return;
    msg("Scan from B is %s", 1, scanB);

    if (reconcile_names(scanA, scanB, scanC)) return;
    msg("New Scan name is %s", 2, scanC);

    len = strlen(scanA);
    for ( ; iA < nA; iA++) revise(lA[iA], scanA, len, scanC);
    len = strlen(scanB);
    for ( ; iB < nB; iB++) revise(lB[iB], scanB, len, scanC);
    }

/*
 * eof
 */
