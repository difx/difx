
      subroutine inform( station )

      implicit none

      character*(*)	station

      include 'CALCDB.i'

      integer*4 i, j

      print *, ' '
c      print 3355, nsites
c3355  format( ' INFORM: Number of SITES = ', I3 )
c
      do j = 1, nsites
	if( sitnam(j)(1:6) .eq. station .or. sitnam(j) .eq. 'LBAREF  ' ) 
     - then
		print 3356, j, sitnam(j), ( sitere(i,j), i = 1, 3 )
	end if
      end do

3356  format( ' INFORM: SITE ', I2, ': "', A, '" ', 3E15.8 )

	print *, ' '
      do j = 1, nstars
	print 3357, j, strnam(j), star20(1,j), star20(2,j)
      end do
3357  format( ' INFORM: SOURCE ', I2, ' "', A, '" ', 2E20.8 )

      print *, ' '

      end
