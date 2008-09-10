      subroutine ljusty(text)
C
C     A.J. Hunt  -  August, 1981
C     H.May - August 1987, fix handling of blank string
C     JER 03/01/91; blank-fill null strings.
C
C  call ljusty (text)
C
C     LJUSTY is a subroutine for left justifying a text string 
C     (i.e. removing the leading spaces and shifting them to 
C      the end).
 
      character *(*) text
      integer ist
 
      do ist=1,len(text)
          if (ichar(text(ist:ist)) .eq. 0) text(ist:ist) = ' '
          if (text(ist:ist).ne.' ') goto 100
      end do
      return

100   text = text(ist:)

      return
      end
