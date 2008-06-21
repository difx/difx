c Common area for source coordinates in string form - req'd by DUMMY_SYNCH
c

	integer*4	MAX_SOURCES
	parameter	( MAX_SOURCES = 500 )

	integer*4	num_srcs_in_list
	character*16	src_name( MAX_SOURCES )
	character*16	src_ra_str( MAX_SOURCES ), src_dec_str( MAX_SOURCES )

	common / source_catalogue / num_srcs_in_list
	common / source_catalogue  / src_name
	common / source_catalogue  / src_ra_str, src_dec_str

