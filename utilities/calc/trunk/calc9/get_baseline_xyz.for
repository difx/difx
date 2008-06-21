********************************************************

	integer*4 function get_baseline_xyz( num_ants, 
	1			ant_name, ant_x, ant_y, ant_z )

	implicit none

	include 'CALCDB.i'

	integer*4	num_ants
	character*(*) 	ant_name(*)
	real*8  	ant_x(*), ant_y(*), ant_z(*)

	integer*4	i, j

*--------------------------------------
	 
	get_baseline_xyz = 0

	do i = 1, num_ants
	    j = 1
	    do while( j .le. nsites .and. 
	1		ant_name( i )(1:8) .ne. sitnam( j )(1:8) )
		j = j + 1
	    end do
	    if( j .gt. nsites ) then
		get_baseline_xyz = i
		return
	    else
		ant_x( i ) = sitere( 1,j )
		ant_y( i ) = sitere( 2,j )
		ant_z( i ) = sitere( 3,j )
	    end if		
	end do

	end

