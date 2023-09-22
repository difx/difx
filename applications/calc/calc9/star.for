
      subroutine star( num_stars, star_ra, star_dec )

      implicit none

      integer*4 num_stars
      real*8 star_ra, star_dec

      include 'CALCDB.i'

      num_stars = nstars
      star_ra = star20( 1, 1 )
      star_dec = star20( 2, 1 )

      end

