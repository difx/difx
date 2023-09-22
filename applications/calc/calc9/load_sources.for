
	integer*4 function load_sources( filename, message )

c***********************************************************
c
c	Loads the current source catalogue from file "filename"
c
c	Returns 0 if successful, 1 otherwise
c
c***********************************************************

	implicit none

c PARAMETERS

	character*(*)	filename
	character*(*)	message

c COMMON AREAS

	include		'SRC_COORD_STR.i'

c EXTERNAL ROUTINES

	real*8		coord_str_to_rads

c LOCAL VARIABLES

	integer*4	i, j, n, inx

	real*8		dx

	character*80 	str

c BEGIN

	open (unit=1, file=filename, status='old', ERR=950 )

	n = 0
	do while (.true.)

	  read ( 1, '(A)', end=900 ) str

	  call get_next_field( str, 1, i, j )
	  if( i .gt. 0 ) then
c Detect comment lines
	    if( str( i:i ) .ne. ';' ) then
		n = n + 1

		if( n .gt. MAX_SOURCES ) go to 960

c		strnam(n) = str( i:j )
		src_name( n ) = str( i:j )
c		call STR$UPCASE( src_name( n ), src_name( n ) )
c RA
		call get_next_field( str, j+1, i, j )
		if( i .le. 0 ) go to 999
		src_ra_str( n ) = str( i:j )

c Following is done as a check of format only
		dx = coord_str_to_rads( 0, str( i:j ) )
		if( dx .gt. 10.0 ) go to 999
c		star20(1,n) = dx

c DEC		
		call get_next_field( str, j+1, i, j )
		if( i .le. 0 ) go to 999
		src_dec_str( n ) = str( i:j )

c Following is done as a check of format only
		dx = coord_str_to_rads( 1, str( i:j ) )
		if( dx .gt. 10.0 ) go to 999
c		star20(2,n) = dx

	    end if		
	  end if
 	end do

 900	continue

c	nstars = n
	num_srcs_in_list = n

	print 123, num_srcs_in_list, filename
123	format( 1X, I4, ' SOURCES read from ', A ) 

	close (1)
	load_sources = 0
	return

 950	continue
	message = ' ERROR opening FILE ' // filename
	load_sources = 1
	return

 960	continue
	write( message, 
     1	'( '' TOO MANY sources ( Max. '', I4, '' ) in '', A )',
     1			ERR=961 ) MAX_SOURCES, filename

c	message = ' TOO MANY sources ( Max. 100 ) in ' // filename

961	continue
	load_sources = 1
	close (1)
	return

 999	continue
	message = ' FORMAT ERROR in SOURCE table ' // filename
	load_sources = 1
	close (1)
	return

	end
