      subroutine rjusty(text)
C
C     A.J. Hunt  -  August, 1981
C     H.May      -  Feb 1994 - modified to work correctly under unix.
C     N.Killeen  -  Mar 1994 - declared variable i
C
C  call rjusty (text)
C
C     RJUSTY is a subroutine for right justifying a text string 
C     (i.e. removing the trailing spaces and shifting them to 
C      the beginning).
 
      character *(*) text
      integer lastsp, NCHAR, ist, i
 
      if (text .eq. ' ') return
      if (len(text) .eq. nchar(text)) return
 
      lastsp = LEN (text) - NCHAR (text)
      ist = lastsp + 1
 
      do i = len(text),ist,-1
         text(i:i) = text(i-lastsp:i-lastsp)
      end do
C     text(ist:) = text
      if (lastsp .ge. 1) text (:lastsp) = ' '
 
      return
      end
