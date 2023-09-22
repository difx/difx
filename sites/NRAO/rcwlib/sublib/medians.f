      subroutine medians( n, x, tcal, tcallast, xmed, div, yr, yrmin )
C
C     Get median value of x for all x with test(i) .eq. test(n).
C     The intent is to get the median value for all measurements since
C     the last receiver change.
C     There is a 500 point maximum for now.
C     Actually set up to get element N/DIV.  This will be the
C     median if DIV=2 but will be something else if not.
C     Only use points with latest Tcal and with year (yr) greater
C     than yrmin.
C
C     Originally used in PLOTSUM, but replaced with iterative average
C     that down weights deviant points.   RCW
C
      integer    maxpts, ipt
      parameter  (maxpts=500)
      integer    n, i, npt
      real       x(n), tcal(n), xmed, div, yr(n), yrmin
      real       xu(maxpts), yrtest, tcallast 
C-------------------------------------------------------------------------
C     Set up to get old points if there are no new ones.
C
      yrtest = yrmin
C
C     Protect against too large arrays.
C
      if( n .gt. maxpts ) then
         write(*,*) 'Too many points for median determination'
         xmed = 0.0
C
C     Array small enough
C
      else if( n .ge. 1 ) then
C
C        Get the points to use by testing on the tcals.  This is
C        very specific to PLOTSUM. 
C
         npt = 0
10       continue
            do i = 1, n
               if( tcal(i) .lt. tcallast * 1.05 .and.
     +             tcal(i) .gt. tcallast * 0.95 .and.
     +             yr(i) .ge. yrtest ) then
                  npt = npt + 1
                  xu(npt) = x(i)
               end if
            end do
C
C           If nothing, go after older points.
C
            if( npt .eq. 0 .and. yrtest .ne. 0.0 ) then
               yrtest = 0.0
               go to 10
            end if
C
C
C        Now get the median.  If there is a small number of 
C        points, protect against taking the first.
C
         if( npt .ge. 2 ) then
            call sort( npt, xu )
            ipt = npt/div + 0.1
            if( ipt .lt. 1 ) ipt = 1
            if( npt .ge. 3 .and. ipt .lt. 2 ) ipt = 2
            xmed = xu(ipt)
         else if( npt .eq. 1 ) then
            xmed = xu(1)
         else
            xmed = 0.0
         end if
      end if
      return
      end
