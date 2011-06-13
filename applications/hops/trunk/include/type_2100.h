/* Changed Jan 3 1994 by CJL to accommodate a record of what has been */
/* fourfitted in this baseline.  Previously unused space after */
/* source now contains up to 4 ascii characters, which represent */
/* the frequency subgroups which have been successfully fourfitted */

struct type_2100 {
	short		record_id;		/*  */
	short		baseline_no;		/*  */
	char		baseline_id[2];		/*  */
	short		unused;			/*  */
	char		ref_station[8];		/*  */
	char		rem_station[8];		/*  */
	char		source[8];		/*  */
	char		sg_fitted[4];		/* New for fourfit */
	short		unused2[2];		/*  */
	char		ascii_date[8];		/*  */
	char		program_name[8];	/*  */
	char		run_name[8];		/*  */
	char		corel_procdate[8];	/*  */
	char		corel_rev[8];		/*  */
	char		ref_tape_label[8];	/*  */
	char		rem_tape_label[8];	/*  */
	char		ref_occ_code[8];	/*  */
	char		rem_occ_code[8];	/*  */
	short		comment_len;		/*  */
	char		comment[142];		/*  */
};
