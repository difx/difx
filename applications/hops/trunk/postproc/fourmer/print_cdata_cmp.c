/*
 * $Id: print_cdata_cmp.c 254 2011-05-17 17:51:30Z gbc $
 *
 * A verbose diagnostic, if needed
 */

#include <stdio.h>

#include "fourmer.h"

void print_cdata_cmp(char *AName, char *BName,
		     struct mk4_corel *cdataAp,
                     struct mk4_corel *cdataBp)
    {
    struct type_101 *t101a, *t101b;
    struct index_tag *idxa, *idxb;
    int i;

    printf("\nDetailed Comparison of A(%s) and B(%s) begins:\n",
	AName, BName);

    printf("\nComparing t100 records...\n");
    if (strncmp(cdataAp->t100->record_id, cdataBp->t100->record_id, 3))
	printf("  Discrepancy in t100.record_id\n");

    if (strncmp(cdataAp->t100->version_no, cdataBp->t100->version_no, 2))
	printf("  Discrepancy in t100.version_no\n");
    if (strncmp(cdataAp->t100->unused1, cdataBp->t100->unused1, 3))
	printf("  Discrepancy in t100.unused1\n");
    if (strncmp(cdataAp->t100->baseline, cdataBp->t100->baseline, 2))
	printf("  Discrepancy in t100.baseline\n");
    if (strncmp(cdataAp->t100->rootname, cdataBp->t100->rootname, 34))
	printf("  Discrepancy in t100.rootname\n");
    if (strncmp(cdataAp->t100->qcode, cdataBp->t100->qcode, 2))
	printf("  Discrepancy in t100.qcode\n");
    if (strncmp(cdataAp->t100->unused2, cdataBp->t100->unused2, 6))
	printf("  Discrepancy in t100.unused2\n");

    if (cdataAp->t100->pct_done != cdataBp->t100->pct_done)
	printf("  Discrepancy in t100.pct_done\n");
    if (cdataAp->t100->ndrec != cdataBp->t100->ndrec)
	printf("  Discrepancy in t100.ndrec\n");
    if (cdataAp->t100->nindex != cdataBp->t100->nindex)
	printf("  Discrepancy in t100.nindex\n");
    if (cdataAp->t100->nlags != cdataBp->t100->nlags)
	printf("  Discrepancy in t100.nlags\n");
    if (cdataAp->t100->nblocks != cdataBp->t100->nblocks)
	printf("  Discrepancy in t100.nblocks\n");
    msg("cdataAp->t100->nblocks = %d\n", 1, cdataAp->t100->nblocks);

    if (memcmp(&(cdataAp->t100->procdate),
	       &(cdataBp->t100->procdate),
		sizeof(struct date)))
	printf("  Discrepancy in t100.procdate\n");
    if (memcmp(&(cdataAp->t100->start),
	       &(cdataBp->t100->start),
		sizeof(struct date)))
	printf("  Discrepancy in t100.start\n");
    if (memcmp(&(cdataAp->t100->stop),
	       &(cdataBp->t100->stop),
		sizeof(struct date)))
	printf("  Discrepancy in t100.stop\n");

    printf("\nComparing t101 records...\n");

    for (i = 0; i < cdataAp->t100->nindex; i++)
	{
	idxa = cdataAp->index + i;
	idxb = cdataBp->index + i;
	if ((t101a = idxa->t101) == NULL) continue;
	if ((t101b = idxb->t101) == NULL) continue;

	if (strncmp(t101a->record_id, t101b->record_id, 3))
	    printf("  Discrepancy in t101[%d].record_id\n", i);
	if (strncmp(t101a->version_no, t101b->version_no, 2))
	    printf("  Discrepancy in t101[%d].version_no\n", i);
	if (strncmp(t101a->ref_chan_id, t101b->ref_chan_id, 8))
	    printf("  Discrepancy in t101[%d].ref_chan_id\n", i);
	if (strncmp(t101a->rem_chan_id, t101b->rem_chan_id, 8))
	    printf("  Discrepancy in t101[%d].rem_chan_id\n", i);

	if (t101a->status != t101b->status)
	    printf("  Discrepancy in t101[%d].status\n", i);
	if (t101a->nblocks != t101b->nblocks)
	    printf("  Discrepancy in t101[%d].nblocks\n", i);
	if (t101a->index != t101b->index)
	    printf("  Discrepancy in t101[%d].index\n", i);
	if (t101a->primary != t101b->primary)
	    printf("  Discrepancy in t101[%d].primary\n", i);
	if (t101a->corr_board != t101b->corr_board)
	    printf("  Discrepancy in t101[%d].corr_board\n", i);
	if (t101a->corr_slot != t101b->corr_slot)
	    printf("  Discrepancy in t101[%d].corr_slot\n", i);
	if (t101a->ref_chan != t101b->ref_chan)
	    printf("  Discrepancy in t101[%d].ref_chan\n", i);
	if (t101a->rem_chan != t101b->rem_chan)
	    printf("  Discrepancy in t101[%d].rem_chan\n", i);
	if (t101a->post_mortem != t101b->post_mortem)
	    printf("  Discrepancy in t101[%d].post_mortem\n", i);
	if (t101a->blocks[0] != t101b->blocks[0])
	    printf("  Discrepancy in t101[%d].blocks[0]\n", i);
	}

    printf("\nDetailed Comparison of A(%s) and B(%s) concluded.\n",
	AName, BName);
    }

/*
 * eof
 */
