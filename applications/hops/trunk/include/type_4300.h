#ifndef TYPE_4300_H
#define TYPE_4300_H

struct type_4300 {
	short		record_id;		/* record identifier */
	short		baseline_no;		/* baseline # as defined in root */
	short		spare[4];		/*  */
	char		src_name[8];		/* Radio source name */
	char		refname[8];		/* Reference station name */
	char		remname[8];		/* Remote station name */
	char		corel_name[6];		/* HP corel filename (obsolete) */
	char		reftape_id[8];		/* Reference station tape ID */
	char		remtape_id[8];		/* Remote station tape ID */
	char		frnge_vers[6];		/* FRNGE version, YYMMDD */
	char		runcode[8];		/* Run code, e.g. 329-1300 */
	char		quality_code[2];	/* FRNGE quality code e.g. " D" */
	char		freq_group[2];		/* frequency group code */
	char		corel_orig[6];		/* Orig. corel filename (obsolete) */
	char		tape_qcode[6];		/* Tape Q code */
	char		spare2[8];		/*  */
	char		ref_occucode[8];	/* reference station occupation code */
	char		rem_occucode[8];	/* remote station occupation code */
	char		unused[144];
};
	
#endif
