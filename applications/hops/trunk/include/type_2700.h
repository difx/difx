struct loffset 
	{
	int		ref;			/*  */
	int		rem;			/*  */
        };

struct type_2700
	{
	short		record_id;		/*  */
	short		baseline_no;		/*  */
	char		baseline_id[2];		/*  */
	short		no_freq_pairs;		/*  */
	short		no_freqs;		/*  */
        short		unused[3];		/*  */
	struct loffset	lo_offset[28];		/* in mHz  */
        short           unuzed[8];              /* should be 8!! rjc  */
        };
