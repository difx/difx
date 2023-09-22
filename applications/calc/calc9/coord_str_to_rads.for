
	real*8	function  coord_str_to_rads( mode, str )

	implicit	none

	integer*4	mode
	character*(*)	str

c********************************************************************
c
c	Converts coordinate string e.g. 12:23:30.333 to radians
c	Mode .eq. 0, h:m:s
c	Mode .ne. 0, d:m:s
c
c********************************************************************

c EXTERNAL FUNCTIONS


c LOCAL VARIABLES

	integer*4	i, j, l
	integer*4	ii, jj
	real*4		xx

	real*8		dx, sgn

	real*8		PI
	parameter	( PI = 3.1415926535897932D0 )

c BEGIN

	l = LEN( str )
	i = 1
	do while( i .le. l .and. str(i:i) .eq. ' ' )
	   i = i + 1
	end do
	if( i .gt. l ) go to 999

	j = INDEX( str, ':' )
	if( j .le. 0 ) go to 999
	j = j - 1
	read( str( i:j ), *, ERR=999, END=999 ) ii
	if( str( i:i ) .eq. '-' .or. ii .lt. 0 ) then
	  sgn = -1.0
	else
	  sgn = +1.0
	end if

	i = j + 2
	j = INDEX( str(i:), ':' )
	if( j .le. 0 ) go to 999
	j = i + j - 2
	read( str( i:j ), *, ERR=999, END=999 ) jj

	i = j + 2
	read( str( i: ), *, ERR=999, END=999 ) xx

	ii = iabs( ii )
	dx = ii + jj / 60.0 + xx / 3600.0
	if( mode .eq. 0 ) then
		coord_str_to_rads = ( dx / 12.0 ) * PI
	else
		coord_str_to_rads = ( dx / 180.0 ) * PI
	end if

	coord_str_to_rads = coord_str_to_rads * sgn

	return

999	continue
	coord_str_to_rads = 999.0
	return

	end

