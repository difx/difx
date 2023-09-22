
***********************************************************

	logical*4 function find_source( source, src_ra, src_dec )

*  Looks for source from the current catalogue

	implicit none

	character*(*) 	source, src_ra, src_dec

	include 'CALCDB.i'

c Following defines area into which the source list has been loaded
	include 'SRC_COORD_STR.i'

	real*8	coord_str_to_rads

	integer*4 i

*-------------------------------------------------------------

	i = 1
	do while ((i.le.num_srcs_in_list) .and. (source.ne.src_name(i)))
                i = i + 1
	end do

	if( i .gt. num_srcs_in_list ) then
		find_source = .FALSE.
	else
		find_source = .TRUE.
		src_ra = src_ra_str( i )
		src_dec = src_dec_str( i )

c Fill CALC "database"
		strnam( 1 ) = source
		star20( 1, 1 ) = coord_str_to_rads( 0, src_ra_str( i ) )
		star20( 2, 1 ) = coord_str_to_rads( 1, src_dec_str( i ) )
		NSTARS = 1

	end if

	return

	end
