
      subroutine deli( n, str )

      implicit none

      integer*4 n
      character *(*) str

      return

      end



      integer*4 function gethostname( str )

      implicit none

      character *(*) str

      str = 'ATLBA_Correlator'
      gethostname = 0

      end


      integer*4 function trimlen( str )

      implicit none

      character *(*) str

      trimlen = Len_Trim( str )

      end


      subroutine kai
      return
      end

      subroutine phist
      return
      end

