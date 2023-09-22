/*
 * $Id: append_sdata.c 1197 2015-09-10 12:38:41Z gbc $
 *
 * Append sdataB to sdataC and edit channel names in the process.
 */

#include <string.h>

#include "fourmer.h"

#define revise_cid(CID,ISA,ISR) \
    strncpy(CID, gen_new_chan_id(CID,ISA,ISR), 8)

/*
 * append B's data to C for type_301 and type_302
 */
static void append_model(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    int n, nA, nC, sp, isA;

    for (n = 0; n < MAX_CHAN; n++)
	if (sdataC->model[n].chan_id[0] == '\0') break;
    nA = n;
    for (n = 0; n < MAX_CHAN; n++)
	{
	if (sdataB->model[n].chan_id[0] == '\0') break;
	memcpy(&(sdataC->model[nA + n]),
	    &(sdataB->model[n]), sizeof(sdataB->model[n]));
	}
    nC = nA + n;
    for (n = 0; n < nC; n++)
	{
	isA = (n < nA ? 1 : 0);
	revise_cid(sdataC->model[n].chan_id, isA, 0);
	for (sp = 0; sp < MAXSPLINES; sp++)
	    {
	    if (sdataC->model[n].t301[sp])
		revise_cid(sdataC->model[n].t301[sp]->chan_id, isA, 0);
	    if (sdataC->model[n].t302[sp])
		revise_cid(sdataC->model[n].t302[sp]->chan_id, isA, 0);
	    if (sdataC->model[n].t303[sp])
		revise_cid(sdataC->model[n].t303[sp]->chan_id, isA, 0);
	    }
	}
    msg("fixed %d (%d + %d) model records", 1, nC, nA, nC - nA);
    }

/*
 * append B's data to C for type_304 -- NO chan id renaming required
 */
static void append_t304(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    int n, nA = sdataC->n304;
    for (n = 0; n < sdataB->n304; n++)
	sdataC->t304[nA + n] = sdataB->t304[n];
    sdataC->n304 += sdataB->n304;
    msg("joined %d (%d + %d) 304 records", 1, sdataC->n304, nA, sdataB->n304);
    }


/*
 * append B's data to C for type_305 -- NO chan id renaming required
 */
static void append_t305(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    int n, nA = sdataC->n305;
    for (n = 0; n < sdataB->n305; n++)
	sdataC->t305[nA + n] = sdataB->t305[n];
    sdataC->n305 += sdataB->n305;
    msg("joined %d (%d + %d) 305 records", 1, sdataC->n305, nA, sdataB->n305);
    }

/*
 * append B's data to C for type_306 -- chan id renaming IS required
 */
static void append_t306(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    int n, nA = sdataC->n306, sc;
    for (n = 0; n < sdataB->n306; n++)
	sdataC->t306[nA + n] = sdataB->t306[n];
    sdataC->n306 += sdataB->n306;
    for (n = 0; n < sdataC->n306; n++)
	for (sc = 0; sc < 16; sc++)
	    revise_cid(sdataC->t306[n]->stcount[sc].chan_id,
		(n < nA ? 1 : 0), 0);
    msg("fixed %d (%d + %d) 306 records", 1, sdataC->n306, nA, sdataB->n306);
    }

/*
 * append B's data to C for type_307 -- NO chan id renaming required
 */
static void append_t307(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    int n, nA = sdataC->n307;
    for (n = 0; n < sdataB->n307; n++)
	sdataC->t307[nA + n] = sdataB->t307[n];
    sdataC->n307 += sdataB->n307;
    msg("joined %d (%d + %d) 307 records", 1, sdataC->n307, nA, sdataB->n307);
    }

/*
 * append B's data to C for type_308 -- chan id renaming IS required
 */
static void append_t308(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    int n, nA = sdataC->n308, pc;
    for (n = 0; n < sdataB->n308; n++)
	sdataC->t308[nA + n] = sdataB->t308[n];
    sdataC->n308 += sdataB->n308;
    for (n = 0; n < sdataC->n308; n++)
	for (pc = 0; pc < 32; pc++)
	    revise_cid(sdataC->t308[n]->pcal[pc].chan_id,
		(n < nA ? 1 : 0), 0);
    msg("fixed %d (%d + %d) 308 records", 1, sdataC->n308, nA, sdataB->n308);
    }

/*
 * append B's data to C for type_309 -- chan id renaming IS required
 * v0: 16 chans
 * v1: 64 chans
 */
static void append_t309(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    int n, nA = sdataC->n309, pc, pcn;
    for (n = 0; n < sdataB->n309; n++)
	sdataC->t309[nA + n] = sdataB->t309[n];
    sdataC->n309 += sdataB->n309;
    for (n = 0; n < sdataC->n309; n++)
	{
	pcn = (sdataC->t309[n]->version_no[0] == 0) ? 16 : 64;
	for (pc = 0; pc < 64; pc++)
	    revise_cid(sdataC->t309[n]->chan[pc].chan_name,
		(n < nA ? 1 : 0), 0);
	}
    msg("fixed %d (%d + %d) 309 records", 1, sdataC->n309, nA, sdataB->n309);
    }

/*
 * Handle all the types of 30x records in the sdata structure
 */
void append_sdata(struct mk4_sdata *sdataC, struct mk4_sdata *sdataB)
    {
    append_model(sdataC, sdataB);
    if (sdataB->n304 > 0) append_t304(sdataC, sdataB);
    if (sdataB->n305 > 0) append_t305(sdataC, sdataB);
    if (sdataB->n306 > 0) append_t306(sdataC, sdataB);
    if (sdataB->n307 > 0) append_t307(sdataC, sdataB);
    if (sdataB->n308 > 0) append_t308(sdataC, sdataB);
    if (sdataB->n309 > 0) append_t309(sdataC, sdataB);
    }

/*
 * eof
 */
