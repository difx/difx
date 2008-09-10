C
C Temporary !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      subroutine flinear_mean_square_fit( x, y, n, a, b, rms )

      implicit none

      real*4  x(*), y(*), a, b, rms
      integer*4 n

      a = 0.0
      b = x(1)
      rms = 999.0

      return

      end

