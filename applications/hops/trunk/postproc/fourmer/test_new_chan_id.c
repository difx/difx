/*
 * $Id: test_new_chan_id.c 252 2011-05-17 12:45:39Z gbc $
 *
 * Test new channel renaming methods
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "fourmer.h"

char lines[][MAX_CHARS] = {
    " chan_def = S00L : &X : ...; \t* Left w/ 1 space\n",
    " chan_def = S01L: &X : ...;  \t* Left no space after\n",
    "  chan_def =S02L : &X : ...;  \t* Left no space before\n",
    "  chan_def =S03L: &X : ...;   \t* Left no spaces\n",
    "done\n"
};

int main(int argc, char **argv)
{
    static char work[MAX_CHARS];
    char *vb = getenv("testverb");
    int	verb = vb ? atoi(vb) : 0;
    int ii, ab, errs = 0, ll, nl;

    for (ab = 1; ab >= 0; ab--) {
	for (ii = 0; ii < sizeof(lines)/MAX_CHARS; ii++) {
	    if (!strcmp(lines[ii], "done\n")) continue;
	    strcpy(work, lines[ii]);
	    if (verb>0) fprintf(stdout, "  Was: %s", work);
	    relabel_chan_def(work, ab);
	    if (verb>0) fprintf(stdout, "  Now: %s", work);
	    if (strlen(work) < strlen(lines[ii]) + 1) errs ++;
	    if (verb>0 && errs) fprintf(stdout, "!! %d\n", errs);
	}
    }
    if (errs) fprintf(stderr,
	"%d lines did not increase by at least one.\n", errs);

    strcpy(work, lines[0]);
    ll = strlen(work);
    if (verb>0) fprintf(stdout, "  Was: %s", work);
    for (ab = 0; ab < 8; ab++) {
	relabel_chan_def(work, ab & 0x1);
	nl = strlen(work);
	if (verb>0) fprintf(stdout, " Now%d: %s", ab, work);
	if (ab < 4) {
	    if (nl != ll + 1) errs ++;
	} else {
	    if (nl != ll + 0) errs ++;
	}
	if (verb>0 && errs) fprintf(stdout, "!! %d (%d %d)\n", errs, ll, nl);
	ll = nl;
    }
    if (errs) fprintf(stderr,
	"or %d more lines didn't increment as expected.\n", errs);

    return(errs);
}

/*
 * eof
 */
