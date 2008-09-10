

	subroutine  get_next_field( string, start, first, last )

	implicit none

c**************************************************************
c
c	Returns in first and last the position of the next 
c	white-space delimited field in the input string
c	beginning the search at string(start:)
c	If no field found, returns 0,0
c
c**************************************************************

	character*(*)	string
	integer*4	start, first, last

c EXTERNAL ROUTINES

c LOCAL

	integer*4       i, j, k
	logical*4       found
	character*5	SEPARATORS
c SPACE, COMMA, TAB, LF, CR
c	parameter	( SEPARATORS = ( ' ,' // char(9) //
c	1    char(10) // char(13) ) )

c BEGIN

	SEPARATORS = ' ,' // CHAR(9) // CHAR(10) // CHAR(13)

	i = start
	j = LEN( string )
	found = .FALSE.
	do while( i .le. j .and. .not. found )
	   k = 1
	   do while( k .le. 4 .and. string(i:i) .ne. SEPARATORS(k:k) )
	      k = k + 1
	   end do
	   if( k .gt. 4 ) then
	      found = .TRUE.
	   else
	      i = i + 1
	   end if
	end do

	if( found ) then
	   first = i
	   i = i + 1
	   found = .FALSE.
	   do while( i .le. j .and. .not. found )
	      k = 1
	      do while( k .le. 4 .and. string(i:i) .ne. SEPARATORS(k:k) )
		 k = k + 1
	      end do
	      if( k .le. 4 ) then
		 found = .TRUE.
	      else
		 i = i + 1
	      end if
	   end do
	   if( found ) then 
	      last = i
	   else
	      last = j
	   end if
	else
	   first = 0
	   last = 0
	end if

	return

	end

